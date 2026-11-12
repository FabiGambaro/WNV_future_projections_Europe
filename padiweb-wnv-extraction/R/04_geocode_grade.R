# Steps 3-5: verify quotes, assign NUTS3 (2024 + 2021), grade evidence, flag records for review.
source("R/config.R"); source("R/utils.R")

art  <- read_articles(cfg$ARTICLES_CSV)
rec  <- read_csv(cfg$RECORDS_CSV, col_types = cols(.default = "c"))
mask <- if (file.exists(file.path(cfg$WORK_DIR, "masked_articles.csv")))
  read_csv(file.path(cfg$WORK_DIR, "masked_articles.csv"), col_types = cols(.default = "c")) else tibble(id = character(), text_for_extraction = character())
aliases <- read_csv(cfg$ALIASES_CSV, col_types = cols(.default = "c"))
fixes   <- read_csv(cfg$CORRECTIONS_CSV, col_types = cols(.default = "c"))

# records missed by the readers and field fixes after review (both documented, with reasons, in data/);
# a fix only applies when the record's current value matches `old_value`, so a fresh extraction is never overwritten blindly
if (file.exists(cfg$ADDED_RECORDS_CSV)) rec <- bind_rows(rec, read_csv(cfg$ADDED_RECORDS_CSV, col_types = cols(.default = "c")) |> select(-any_of("reason")))
rec$infection_uncertain <- FALSE
if (file.exists(cfg$RECORD_FIXES_CSV)) {
  rfix <- read_csv(cfg$RECORD_FIXES_CSV, col_types = cols(.default = "c"))
  for (j in seq_len(nrow(rfix))) {
    f <- rfix[j, ]; hit <- which(rec$article_id == f$article_id & rec$place == f$place)
    if (f$field != "infection_uncertain") hit <- hit[coalesce(rec[[f$field]][hit], "") == coalesce(f$old_value, "")]
    if (!length(hit)) { cat("record fix not applied (no match):", f$article_id, f$place, f$field, "\n"); next }
    rec[[f$field]][hit] <- if (f$field == "infection_uncertain") TRUE else f$new_value
  }
}

rec <- rec |> left_join(select(art, article_id = id, title, text, publication_date, source_country = country), by = "article_id")

# 3. Quotes must be exact substrings of the text the record was extracted from ----------------
# (records extracted from an original article or a duplicate carry text_source and are checked there)
src_text <- function(article_id, text_source, text, title) {
  if (is.na(text_source) || text_source == "padiweb") return(paste(title, text))
  if (text_source == "padiweb+locations") { f <- file.path(cfg$WORK_DIR, "filled_texts", paste0(article_id, ".txt"))
    return(if (file.exists(f)) paste(title, paste(readLines(f, warn = FALSE), collapse = "\n")) else NA_character_) }
  if (startsWith(text_source, "duplicate:")) { d <- str_remove(text_source, "duplicate:"); a <- art[art$id == d, ]; return(paste(a$title, a$text)) }
  f <- file.path(cfg$WORK_DIR, "original_articles", paste0(article_id, ".txt"))
  if (file.exists(f)) paste(readLines(f, warn = FALSE), collapse = "\n") else NA_character_
}
# "local" must be said with a literal term (autochthonous / locally acquired / domestic case, in any of the
# article's languages) or an explicit statement that the person had not travelled; paraphrases such as
# "local mosquitoes" or "no evidence that they resided abroad" do not count
literal_local <- regex(paste0("autochthon|autocton|aut[o\u00f3]cton|autohton|\u03b1\u03c5\u03c4\u03cc\u03c7\u03b8\u03bf\u03bd|",
  "local(ly)?[- ]acquired|acquired locally|local acquisition|lokal erworben|",
  "domestic(ally)?[- ]?(acquired|cases?|origin|transmission|infections?|patients?)|of domestic origin|",
  "\u03b5\u03b3\u03c7\u03ce\u03c1\u03b9|hazai|doma\u0107|\u0434\u043e\u043c\u0430\u045b|indigen|",
  # an explicit statement of no travel is the surveillance definition of a locally acquired case
  "(did|had|has|have|would) not (have )?(report(ed)? (any )?)?(recently )?(travel|been on a trip|been abroad|left)|",
  "no (recent )?(history of )?travel(l?ing)?( history)? (was |were )?(reported|outside|abroad|in the)|no (recent )?travel history|",
  "without (any )?(recent )?travel|not report(ed)? (any )?(recent )?(travel|trips)|denied (any )?(recent )?travel|",
  "non (ha|aveva|avevano|hanno) (effettuato |fatto )?viagg|nessun viaggio|",
  "no (ha|hab[i\u00ed]a|hab[i\u00ed]an) viajado|sin antecedentes de viaje|",
  "nu a c\u0103l\u0103torit|nu au c\u0103l\u0103torit|f\u0103r\u0103 istoric de c\u0103l\u0103torie|nem utazott|",
  "\u03b4\u03b5\u03bd (\u03b5\u03af\u03c7\u03b5|\u03b5\u03af\u03c7\u03b1\u03bd) \u03c4\u03b1\u03be\u03b9\u03b4|nije putova|nisu putova|nicht gereist|keine reiseanamnese"), ignore_case = TRUE)
rec$literal_local <- pmap_lgl(list(rec$article_id, rec$text_source, rec$text, rec$title, rec$quote),
  function(a, s, t, ti, q) { st <- src_text(a, s, t, ti); str_detect(paste(st, q), literal_local) })
# a place of infection given with hedged wording ("probably infected in Hamburg", "most likely infected in the
# Pomurska region", "presumed place of infection", "believed to have been infected", "allegedly infected") is
# grade B, not A (decision 2026-10-20)
hedged_re <- regex(paste0("\\b(probabl[ey]|most likely|likely|presum(ed|ably|ptive)|possibl[ey]|suspected|believed|allegedly|apparently|assumed|supposed(ly)?|",
  "(thought|said|considered) to have|",
  "(may|might|could) have (been|become|got(ten)?) infected|(may|might|could) have (contracted|caught|acquired)|wahrscheinlich|vermutlich|vermutet|mutma\u00dflich|offenbar|angenommen|probabilmente|presumibilmente|",
  "probablemente|presumiblemente|probablement|probabil|verjetno|val\u00f3sz\u00edn\u0171|vjerojatno|verovatno|",
  "\u0432\u0435\u0440\u043e\u0432\u0430\u0442\u043d\u043e|\u03c0\u03b9\u03b8\u03b1\u03bd)"), ignore_case = TRUE)
rec$infection_uncertain <- rec$infection_uncertain | (rec$role == "infection" & str_detect(coalesce(rec$quote, ""), hedged_re))
rec$quote_verified <- pmap_lgl(list(rec$article_id, rec$text_source, rec$text, rec$title, rec$quote),
  function(a, s, t, ti, q) { st <- src_text(a, s, t, ti); !is.na(st) && str_detect(st, fixed(q)) })
cat("Quotes verified:", sum(rec$quote_verified), "of", nrow(rec), "\n")

# 4. NUTS3 ------------------------------------------------------------------------------------
nuts <- setNames(lapply(cfg$NUTS_YEARS, load_nuts3, ref_dir = cfg$REF_DIR), cfg$NUTS_YEARS)
cat("NUTS polygons:", attr(nuts[["2024"]], "source"), "\n")

# lookup of NUTS3 names, including each part of merged Greek units ("Karditsa, Trikala")
name_keys <- st_drop_geometry(nuts[["2024"]]) |>
  mutate(part = str_split(nuts3_name, ",\\s*")) |> unnest(part) |>
  bind_rows(st_drop_geometry(nuts[["2024"]]) |> mutate(part = nuts3_name)) |>
  mutate(key = strip_admin_words(part)) |> filter(key != "") |> distinct(nuts3, key)
alias_keys <- aliases |> transmute(nuts3, key = strip_admin_words(alias))
single_nuts3 <- st_drop_geometry(nuts[["2024"]]) |> count(cc = substr(nuts3, 1, 2)) |> filter(n == 1) |>
  left_join(st_drop_geometry(nuts[["2024"]]) |> mutate(cc = substr(nuts3, 1, 2)), by = "cc") |> with(setNames(as.list(nuts3), cc))

single_child <- st_drop_geometry(nuts[["2024"]]) |> add_count(n2 = substr(nuts3, 1, 4)) |> filter(n == 1) |> pull(nuts3)
stem_keys <- bind_rows(name_keys, alias_keys) |> mutate(s = map(key, stem_name)) |> unnest(s) |> distinct(nuts3, s)
resolve_name <- function(x, country = NA) {
  if (is.na(x) || x == "") return(NA_character_)
  k <- strip_admin_words(x)
  a <- unique(alias_keys$nuts3[alias_keys$key == k])
  if (length(a) > 1 && !is.na(country)) a <- a[substr(a, 1, 2) == country]
  if (length(a) == 1 && (is.na(country) || substr(a, 1, 2) == country)) return(a)
  m <- unique(name_keys$nuts3[name_keys$key == k])
  if (length(m) > 1 && !is.na(country)) m <- m[substr(m, 1, 2) == country]
  if (length(m) == 1) return(m)
  # inflected or hyphenated forms within the same country, only when the stem matches one unit
  if (is.na(country)) return(NA_character_)
  sk <- stem_name(k); sk <- sk[nchar(sk) >= 4]
  if (!length(sk)) return(NA_character_)
  m <- unique(stem_keys$nuts3[stem_keys$s %in% sk & substr(stem_keys$nuts3, 1, 2) == country])
  if (length(m) == 1) m else NA_character_
}

# gazetteer: GeoNames cities1000, else cached points (article_id, place, lon, lat)
geo   <- load_geonames(cfg$REF_DIR)
cache <- if (nzchar(cfg$GAZETTEER_CACHE)) read_csv(cfg$GAZETTEER_CACHE, col_types = cols(.default = "c", lon = "d", lat = "d")) else NULL
locs  <- if (file.exists(cfg$LOCATIONS_CSV)) read_csv(cfg$LOCATIONS_CSV, col_types = cols(.default = "c"), progress = FALSE) |>
  transmute(article_id = article, key = norm_txt(location), lon = as.numeric(longitude), lat = as.numeric(latitude)) |> distinct() else NULL
cat("Gazetteer:", if (!is.null(geo)) "GeoNames cities1000" else if (!is.null(cache)) "cached points (GeoNames not reachable)" else "none", "\n")

geo_idx <- if (!is.null(geo)) split(seq_len(nrow(geo)), geo$key) else NULL   # index by name for speed
candidates <- function(article_id, place, cc = NA) {
  k <- norm_txt(place)
  if (!is.null(geo) && !is.null(geo_idx[[k]])) {
    g <- geo[geo_idx[[k]], ]; g <- g[is.na(cc) | iso_to_nuts(g$country) == cc, ]
    if (nrow(g)) return(transmute(g, lon, lat, country = iso_to_nuts(country), population, method = "GeoNames point-in-polygon"))
  }
  if (!is.null(cache)) {
    g <- cache[cache$article_id == article_id & norm_txt(cache$place) == k, ]
    if (nrow(g)) return(transmute(g, lon, lat, country = NA_character_, population = NA_real_, method = "cached GeoNames point-in-polygon"))
  }
  if (!is.null(locs)) {
    g <- locs[locs$article_id == article_id & locs$key == k, ]
    if (nrow(g)) return(transmute(g, lon, lat, country = NA_character_, population = NA_real_, method = "PADI-web location point-in-polygon"))
  }
  NULL
}

# Province named in the text, strongest first:
#  1. admin_hint recorded at extraction ("Bosanci, Suceava County")
#  2. "(Province)" right after the place in the quote ("Villanueva del Rey (Córdoba)")
#  3. the only NUTS3 name of the same country in the quote's sentence that contains the place
# 1-2 also constrain the gazetteer search; 3 is used only when the place is not found, and is flagged.
strong_hint <- function(place, admin_hint, quote) {
  if (!is.na(admin_hint) && admin_hint != "") return(admin_hint)
  if (is.na(quote)) return(NA_character_)
  str_match(quote, regex(paste0(stri_replace_all_regex(place, "([\\W])", "\\\\$1"), "\\s*\\(([^)]+)\\)"), ignore_case = TRUE))[, 2]
}
all_keys <- bind_rows(name_keys, alias_keys) |> filter(nchar(key) > 3)
sentence_hint <- function(place, quote, cc) {
  if (is.na(quote)) return(NA_character_)
  sents <- str_split(quote, "(?<=[.;!?])\\s+")[[1]]
  s <- sents[str_detect(sents, fixed(place, ignore_case = TRUE))]
  if (!length(s)) return(NA_character_)
  ns <- paste0(" ", norm_txt(s[1]), " ")
  keys <- all_keys[is.na(cc) | substr(all_keys$nuts3, 1, 2) == cc, ]
  hits <- unique(keys$nuts3[stri_detect_fixed(ns, paste0(" ", keys$key, " "))])
  self <- resolve_name(place, cc)
  hits <- setdiff(hits, self)
  if (length(hits) == 1) hits else NA_character_   # returns a NUTS3 code
}

# point-in-polygon; points just outside a generalised coastline (< ~5 km) take the nearest polygon
pip <- function(lon, lat, layer) {
  p <- st_as_sf(tibble(lon, lat), coords = c("lon", "lat"), crs = 4326)
  out <- suppressMessages(st_join(p, layer, join = st_within))$nuts3
  miss <- which(is.na(out))
  if (length(miss)) {
    pm <- st_transform(p[miss, ], 3035); lm <- layer_3035(layer)
    nf <- st_nearest_feature(pm, lm)
    d  <- as.numeric(st_distance(pm, lm[nf, ], by_element = TRUE))
    out[miss] <- ifelse(d < 5000, layer$nuts3[nf], NA)
  }
  out
}
# distances are computed in metres on the ETRS89 / LAEA Europe projection
layer_3035 <- local({ cache <- list(); function(layer) {
  k <- paste(nrow(layer), attr(layer, "source")); if (is.null(cache[[k]])) cache[[k]] <<- st_transform(layer, 3035); cache[[k]] } })
# distance (km) from a point to the nearest OTHER NUTS3 region (coastlines do not count)
border_km <- function(lon, lat, code, layer) {
  lm <- layer_3035(layer)
  key <- paste(lon, lat, code); u <- which(!duplicated(key) & !is.na(lon) & !is.na(code))   # each distinct point once
  d <- map_dbl(u, function(i) {
    others <- lm[lm$nuts3 != code[i], ]
    pt <- st_transform(st_sfc(st_point(c(lon[i], lat[i])), crs = 4326), 3035)
    as.numeric(st_distance(pt, others[st_nearest_feature(pt, others), ])) / 1000
  })
  unname(setNames(d, key[u])[key])
}

# gazetteer candidates with their NUTS3, memoised (GeoNames results do not depend on the article)
loc_memo <- new.env()
located <- function(article_id, place, cc) {
  cand <- candidates(article_id, place, cc)
  if (is.null(cand) || !nrow(cand)) return(NULL)
  key <- paste(if (startsWith(cand$method[1], "GeoNames")) "" else article_id, norm_txt(place), cc, sep = "|")
  if (!is.null(loc_memo[[key]])) return(loc_memo[[key]])
  cand$nuts3 <- pip(cand$lon, cand$lat, nuts[["2024"]])
  cand <- filter(cand, !is.na(nuts3))
  assign(key, cand, envir = loc_memo); cand
}
others_nuts2 <- list()   # NUTS2 of each article's already-placed records, used to break ties
geocode_one <- function(i) {
  r <- rec[i, ]; cc <- iso_to_nuts(coalesce(r$country, r$source_country))
  out <- list(nuts3 = NA_character_, method = NA_character_, lon = NA_real_, lat = NA_real_, hint = NA_character_)
  lvl <- r$place_level
  if (!is.na(cc) && cc %in% names(single_nuts3)) return(modifyList(out, list(nuts3 = single_nuts3[[cc]], method = "country has a single NUTS3")))
  if (lvl == "country") return(modifyList(out, list(method = "coarser than NUTS3")))
  # a whole country named as if it were a province ("AP Kosovo (one case)") is country level
  if (strip_admin_words(r$place) %in% c("kosovo", "ap kosovo", "kosovski", "kosova")) return(modifyList(out, list(method = "coarser than NUTS3")))
  # a region that is also a single NUTS3 unit (city-states such as Berlin, Hamburg, Vienna) is matched by name
  if (lvl == "region") { n <- resolve_name(r$place, cc)
    if (!is.na(n) && n %in% single_child) return(modifyList(out, list(nuts3 = n, method = "name match (region that is a single NUTS3)")))
    return(modifyList(out, list(method = "coarser than NUTS3"))) }
  # route 1: the place is itself a NUTS3 unit
  if (lvl == "province") {
    n <- resolve_name(r$place, cc)
    if (!is.na(n)) return(modifyList(out, list(nuts3 = n, method = "name match (NUTS3)")))
  }
  # route 2: municipality -> point -> polygon, constrained to the province named in the text
  hint <- strong_hint(r$place, r$admin_hint, r$quote)
  hint_nuts <- if (is.na(hint)) NA_character_ else resolve_name(hint, cc)
  cand <- located(r$article_id, r$place, cc)
  if (!is.null(cand) && nrow(cand)) {
    if (!is.na(hint_nuts)) cand <- filter(cand, nuts3 == hint_nuts)
    if (nrow(cand) && n_distinct(cand$nuts3) > 1 && !is.na(cc) && any(substr(cand$nuts3, 1, 2) == cc)) cand <- filter(cand, substr(nuts3, 1, 2) == cc)
    if (nrow(cand) && n_distinct(cand$nuts3) > 1 && length(others_nuts2[[r$article_id]])) {
      same <- filter(cand, substr(nuts3, 1, 4) %in% others_nuts2[[r$article_id]]); if (nrow(same)) cand <- same }
    if (nrow(cand)) {
      cand <- arrange(cand, desc(population))
      return(modifyList(out, list(nuts3 = cand$nuts3[1], method = cand$method[1], lon = cand$lon[1], lat = cand$lat[1], hint = hint)))
    }
  }
  # route 3: municipality not found -> province named in the text
  if (!is.na(hint_nuts)) return(modifyList(out, list(nuts3 = hint_nuts, method = paste0("via province named in text (", hint, ")"), hint = hint)))
  # the municipality's own name is a NUTS3 unit or an alias (Larissa -> EL612, Seville -> ES618, Greek municipalities in the alias table)
  if (lvl != "province") { n <- resolve_name(r$place, cc)
    if (!is.na(n)) return(modifyList(out, list(nuts3 = n, method = "municipality name matches a NUTS3 unit or alias"))) }
  if (lvl == "province") return(modifyList(out, list(method = "failed: province name not matched")))   # never borrow another province
  sh <- sentence_hint(r$place, r$quote, cc)
  if (!is.na(sh)) return(modifyList(out, list(nuts3 = sh, method = paste0("via the only province in the same sentence (", sh, ")"), hint = sh)))
  if (lvl == "province") return(modifyList(out, list(method = "failed: province name not matched")))
  modifyList(out, list(method = "failed: not in gazetteer and no province in text"))
}

geo_res <- vector("list", nrow(rec))
for (i in seq_len(nrow(rec))) {
  if (i %% 2000 == 0) cat("geocoded", i, "of", nrow(rec), format(Sys.time(), "%H:%M:%S"), "\n")
  geo_res[[i]] <- geocode_one(i)
  n3 <- geo_res[[i]]$nuts3
  if (!is.na(n3)) others_nuts2[[rec$article_id[i]]] <- unique(c(others_nuts2[[rec$article_id[i]]], substr(n3, 1, 4)))
}
rec <- bind_cols(rec, bind_rows(geo_res) |> rename(nuts3_2024 = nuts3, geo_method = method, province_hint = hint))

# manual corrections (documented in data/manual_corrections.csv)
rec <- rec |> left_join(rename(fixes, fix_nuts3 = nuts3_2024, fix_reason = reason), by = c("article_id", "place")) |>
  mutate(geo_method = if_else(!is.na(fix_nuts3), paste0("manual correction (was ", nuts3_2024, ")"), geo_method),
         nuts3_2024 = coalesce(fix_nuts3, nuts3_2024)) |> select(-fix_nuts3)

# names and 2021 codes (point if available, else a point inside the 2024 polygon)
lab <- st_drop_geometry(nuts[["2024"]])
rec <- rec |> left_join(lab, by = c("nuts3_2024" = "nuts3"))
sur <- suppressWarnings(st_point_on_surface(nuts[["2024"]])) |> mutate(slon = st_coordinates(geometry)[, 1], slat = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |> select(nuts3, slon, slat)
rec <- rec |> left_join(sur, by = c("nuts3_2024" = "nuts3")) |>
  mutate(plon = if_else(!is.na(lon) & !str_starts(geo_method, "manual"), lon, slon),
         plat = if_else(!is.na(lat) & !str_starts(geo_method, "manual"), lat, slat))
rec$nuts3_2021 <- NA_character_
ok <- !is.na(rec$plon)
rec$nuts3_2021[ok] <- pip(rec$plon[ok], rec$plat[ok], nuts[["2021"]])
rec <- select(rec, -slon, -slat, -plon, -plat)
rec$border_km <- round(border_km(as.numeric(rec$lon), as.numeric(rec$lat), rec$nuts3_2024, nuts[["2024"]]), 1)

europe_cc <- c("AD","AL","AT","BA","BE","BG","BY","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IS","IT",
               "LI","LT","LU","LV","MC","MD","ME","MK","MT","NL","NO","PL","PT","RO","RS","RU","SE","SI","SK","SM","UA","UK","VA","XK")
# generalised 1:10M boundaries can be off by a few km; GISCO 1:1M by a few hundred metres
border_flag_km <- if (startsWith(attr(nuts[["2024"]], "source"), "GISCO")) 1 else 3

# 5. Evidence grade and review flags -------------------------------------------------------------
pub <- as.Date(substr(rec$publication_date, 1, 10))
rec <- rec |> mutate(
  evidence_grade = case_when(
    human_case != "yes"                          ~ "exclude: human case unclear",
    geo_method == "coarser than NUTS3"           ~ "no NUTS3 (coarser level)",
    is.na(nuts3_2024)                            ~ "no NUTS3 (geocoding failed)",
    role %in% c("hospital", "possible_exposure") | certainty == "suspected" | is.na(year_infection) ~
      "C: review (hospital / uncertain exposure / suspected / unknown year)",
    role == "infection" & infection_uncertain    ~ "B: place of infection hedged or uncertain",
    role == "infection"                          ~ "A: place of infection stated",
    status == "autochthonous"                    ~ "B: autochthonous, located by residence/reporting area",
    TRUE                                         ~ "B-: residence/reporting area, status not stated"),
  # local acquisition is explicit when the text says so (status) or states where the person was infected (role infection)
  # study area = continental Europe: European countries only, without the EU outermost regions
  # (FRY* overseas departments, ES70 Canarias, PT20 Azores, PT30 Madeira)
  in_study_area = iso_to_nuts(country) %in% europe_cc & !coalesce(str_detect(nuts3_2024, "^(FRY|ES70|PT20|PT30)"), FALSE),
  explicit_autochthonous = in_study_area & ((status == "autochthonous" & literal_local) | role == "infection"),
  # presence needs a stated place of infection (grade A); explicit local cases placed only by residence or
  # reporting area (grade B) form a separate layer
  in_presence = explicit_autochthonous & str_detect(evidence_grade, "^A:"),
  in_b_layer  = explicit_autochthonous & str_detect(evidence_grade, "^B:"),
  # a stated place of infection counts wherever the case was reported ("exposed in Milan" in a Romanian
  # bulletin is a Milan presence); "imported" only says the case was imported into the reporting country,
  # and PADI-web's source country is the outlet's, not the bulletin's, so it is not flagged
  review_reason = pmap_chr(list(evidence_grade, geo_method, year_basis, format(pub, "%m"), article_id, border_km, note),
    function(g, m, yb, mo, a, bk, nt) {
      x <- c(if (startsWith(g, "C")) "grade C",
             if (startsWith(g, "no NUTS3 (geocoding failed)")) "no NUTS3 code found",
             if (!is.na(m) && startsWith(m, "manual")) "NUTS3 corrected by hand",
             if (!is.na(m) && str_detect(m, "same sentence")) "NUTS3 from a province in the same sentence; check",
             if (!is.na(yb) && startsWith(yb, "corrected")) "year corrected",
             if (!is.na(bk) && bk < border_flag_km) paste0("point within ", border_flag_km, " km of a NUTS3 border; check"),
             if (!is.na(nt) && str_detect(nt, regex("northern cyprus|TRNC|occupied", ignore_case = TRUE))) "Northern Cyprus: keep or drop",
             if (!is.na(yb) && yb == "publication" && !is.na(mo) && mo %in% c("11", "12", "01", "02")) "published Nov-Feb; check year",
             NULL)
      if (length(x)) paste(x, collapse = "; ") else NA_character_
    }))

# human review (data/review_decisions.csv): the reviewer's decision overrides the grade of that record
# (A = place of infection confirmed, B = local but place of infection not explicit, exclude = not counted)
if (file.exists(cfg$REVIEW_DECISIONS_CSV)) {
  rev <- read_csv(cfg$REVIEW_DECISIONS_CSV, col_types = cols(.default = "c"))
  miss <- anti_join(rev, rec, by = c("article_id", "place", "quote"))
  if (nrow(miss)) { cat("review decisions with no matching record:\n"); print(miss) }
  rec <- rec |> left_join(select(rev, article_id, place, quote, decision, reviewer_note), by = c("article_id", "place", "quote")) |>
    mutate(evidence_grade = case_when(
             decision == "A" ~ "A: place of infection stated (confirmed by review)",
             decision == "B" ~ "B: place of infection not explicit (set by review)",
             decision == "exclude" ~ "excluded by review",
             TRUE ~ evidence_grade),
           explicit_autochthonous = if_else(!is.na(decision), in_study_area & decision != "exclude", explicit_autochthonous),
           in_presence = explicit_autochthonous & str_detect(evidence_grade, "^A:"),
           in_b_layer  = explicit_autochthonous & str_detect(evidence_grade, "^B:"),
           review_reason = if_else(!is.na(decision), paste0(if_else(is.na(review_reason), "", paste0(review_reason, "; ")), "reviewed: ", decision, " (", reviewer_note, ")"), review_reason)) |>
    select(-decision, -reviewer_note)
}

write_csv(select(rec, -text, -title), file.path(cfg$WORK_DIR, "records_geocoded.csv"))
cat("Records:", nrow(rec), "\n"); print(count(rec, evidence_grade)); print(count(rec, geo_method, sort = TRUE))
