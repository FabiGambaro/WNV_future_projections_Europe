suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(stringi)
  library(tidyr); library(purrr); library(sf)
})
sf_use_s2(FALSE)

# Masking in PADI-web texts: runs of 3+ dashes or asterisks hide words (often place names)
MASK_RE <- "-{3,}|\\*{3,}"

# Lowercase, strip accents, keep letters/digits only, collapse spaces
norm_txt <- function(x) {
  # Greek script: modern pronunciation (Evvoia, Rodopi, Kavala), not the classical default (Euboia, Rhodope, Kabala)
  x <- stri_trans_general(x, "NFD; [:Nonspacing Mark:] Remove; NFC; Lower")
  x <- stri_replace_all_fixed(x, c("ου", "αυ", "ευ", "β", "η", "υ", "ω"), c("ou", "av", "ev", "v", "i", "i", "o"), vectorize_all = FALSE)
  x <- stri_trans_general(x, "Any-Latin; Latin-ASCII")
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  str_squish(x)
}

# Strip Serbian/Greek/Slovene case and adjective endings so "Južnobačkog" = "Južnobačka", "Πιερίας" = "Pieria"
stem_name <- function(k) {
  st <- function(w) str_replace(w, "^(\\w{3,}?)(skog|skom|ska|ski|sko|og|om|ou|as|is|es|os|on|i|a|o|e|s)$", "\\1")
  words <- str_split(k, " ")
  c(collapsed = st(str_remove_all(k, " ")), by_word = map_chr(words, ~ paste(st(.x), collapse = "")))
}

# Strip generic admin words so "Suceava County" matches "Suceava"
strip_admin_words <- function(x) {
  x <- norm_txt(x)
  x <- str_replace_all(x, "\\b(east|west|north|south)ern\\b", "\\1")   # "Eastern Attica" = "East Attica"
  x <- str_replace_all(x, "\\b(regional unit of|regional unit|metropolitan unit of|mitropolitiki enotita|perifereiaki enotita|health area|prefecture of|province of|county of|district of|municipality of|the|county|province|provincia|district|okrug|oblast|judet|judetul|prefecture|region|department|departement|nomos)\\b", " ")
  str_squish(x)
}

# Google News RSS links wrap the real article URL in base64
decode_google_news <- function(u) {
  if (!str_detect(u, "news\\.google\\.com/.*/articles/")) return(u)
  p <- str_match(u, "/articles/([^?]+)")[, 2]
  p <- chartr("-_", "+/", p)
  p <- paste0(p, strrep("=", (4 - nchar(p) %% 4) %% 4))
  raw <- tryCatch(jsonlite::base64_dec(p), error = function(e) NULL)
  if (is.null(raw)) return(u)
  s <- rawToChar(raw[raw >= as.raw(0x21) & raw <= as.raw(0x7e)])
  m <- str_extract(s, "https?://[!-~]+")
  if (is.na(m)) u else str_remove(m, "R?$")
}

read_articles <- function(path) {
  a <- read_csv(path, col_types = cols(.default = col_character()), progress = FALSE)
  stopifnot(all(c("id", "text", "url", "publication_date") %in% names(a)))
  a
}

# NUTS3 polygons: GISCO 1:1M if reachable, else Eurostat Nuts2json 1:10M (coarser boundaries)
load_nuts3 <- function(year, ref_dir) {
  f1 <- file.path(ref_dir, sprintf("NUTS_RG_01M_%s_4326_LEVL_3.geojson", year))
  f2 <- file.path(ref_dir, sprintf("nutsrg_3_%s_10M.json", year))
  if (!file.exists(f1)) {
    try(download.file(sprintf("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_%s_4326_LEVL_3.geojson", year),
                      f1, quiet = TRUE, mode = "wb"), silent = TRUE)
  }
  if (file.exists(f1) && file.size(f1) > 1e5) {
    x <- st_read(f1, quiet = TRUE) |> transmute(nuts3 = NUTS_ID, nuts3_name = NAME_LATN)
    attr(x, "source") <- "GISCO 1:1M"
    return(st_make_valid(x))
  }
  if (!file.exists(f2)) {
    try(download.file(sprintf("https://raw.githubusercontent.com/eurostat/Nuts2json/master/pub/v2/%s/4326/10M/nutsrg_3.json", year),
                      f2, quiet = TRUE, mode = "wb"), silent = TRUE)
  }
  if (!file.exists(f2) || file.size(f2) < 1e5) stop("Could not get NUTS ", year, " polygons")
  x <- st_read(f2, quiet = TRUE) |> transmute(nuts3 = id, nuts3_name = str_trim(na))
  attr(x, "source") <- "Nuts2json 1:10M (fallback; check points near borders)"
  st_make_valid(x)
}

# GeoNames cities1000 (places with > 1,000 inhabitants)
load_geonames <- function(ref_dir) {
  f <- file.path(ref_dir, "cities1000.txt")
  if (!file.exists(f)) {
    z <- file.path(ref_dir, "cities1000.zip")
    ok <- tryCatch({download.file("https://download.geonames.org/export/dump/cities1000.zip", z, quiet = TRUE, mode = "wb"); TRUE},
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok || !file.exists(z) || file.size(z) < 1e5) return(NULL)
    unzip(z, exdir = ref_dir)
  }
  g <- read_tsv(f, col_names = c("geonameid","name","asciiname","alternatenames","lat","lon","fclass","fcode",
                                 "country","cc2","admin1","admin2","admin3","admin4","population","elevation","dem","tz","moddate"),
                col_types = cols(.default = col_character(), lat = col_double(), lon = col_double(), population = col_double()),
                quote = "", progress = FALSE)
  # one row per (normalised name, point)
  g |>
    mutate(names = paste(name, asciiname, alternatenames, sep = ",")) |>
    select(geonameid, country, lat, lon, population, names) |>
    separate_rows(names, sep = ",") |>
    mutate(key = norm_txt(names)) |>
    filter(key != "") |>
    distinct(geonameid, key, .keep_all = TRUE) |>
    select(key, country, lat, lon, population)
}

# ISO country code -> NUTS prefix
iso_to_nuts <- function(x) recode(x, GR = "EL", GB = "UK", .default = x)
