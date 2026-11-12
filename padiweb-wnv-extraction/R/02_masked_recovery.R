# Masked articles: (1) the original article, from pages downloaded beforehand with tools/download_masked.R
# (ORIGINALS_DIR) or fetched now; (2) if it can't be read, flag it and look for an unmasked duplicate in the
# corpus. A duplicate is used ONLY if its text is identical to the masked article outside the masked parts
# (after normalising case, accents, punctuation and spacing); near duplicates are listed but never used.
# (3) Otherwise fill masks from the PADI-web location file: a location whose character position falls inside
# a masked span is the hidden name (PADI-web extracted locations before masking the export).
source("R/config.R"); source("R/utils.R")
suppressPackageStartupMessages(library(httr2))

art    <- read_articles(cfg$ARTICLES_CSV)
sample <- read_csv(file.path(cfg$WORK_DIR, "sample_articles.csv"), col_types = cols(.default = "c"))
masked <- filter(sample, str_detect(text, MASK_RE))
orig_dir <- file.path(cfg$WORK_DIR, "original_articles"); dir.create(orig_dir, showWarnings = FALSE)

# (1) original article -------------------------------------------------------
fetch_original <- function(id, u) {
  # pages saved beforehand on a computer that can reach the news sites
  pre <- if (nzchar(cfg$ORIGINALS_DIR)) file.path(cfg$ORIGINALS_DIR, paste0(id, ".html")) else ""
  if (nzchar(pre) && file.exists(pre)) {
    file.copy(pre, file.path(orig_dir, paste0(id, ".html")), overwrite = TRUE)
    return(tibble(id, access = "ok", http_status = 200L, detail = paste("downloaded beforehand:", pre)))
  }
  if (nzchar(cfg$ORIGINALS_DIR)) return(tibble(id, access = "not downloaded", http_status = NA_integer_, detail = "not in ORIGINALS_DIR (see its download_log.csv)"))
  u <- strsplit(u, "[` ]+")[[1]][1]   # some url fields hold several links
  resp <- tryCatch(
    request(u) |> req_user_agent("Mozilla/5.0 (research; PADI-web WNV validation)") |>
      req_timeout(30) |> req_error(is_error = \(r) FALSE) |> req_perform(),
    error = function(e) e)
  if (inherits(resp, "error")) return(tibble(id, access = "blocked", http_status = NA_integer_, detail = conditionMessage(resp)))
  st <- resp_status(resp)
  if (st != 200) return(tibble(id, access = if (st %in% c(401, 403, 451)) "blocked" else "error", http_status = st, detail = ""))
  html <- resp_body_string(resp)
  writeLines(html, file.path(orig_dir, paste0(id, ".html")))
  tibble(id, access = "ok", http_status = st, detail = paste0(id, ".html"))
}
access <- map2_dfr(masked$id, map_chr(masked$url, decode_google_news), fetch_original)

# (2) unmasked duplicate in the corpus --------------------------------------
unmasked <- art |> filter(!str_detect(text, MASK_RE)) |>
  mutate(ntext = norm_txt(text))
uwords <- map(str_split(unmasked$ntext, " "), unique)
jaccard <- function(a, b) length(intersect(a, b)) / length(union(a, b))

find_duplicate <- function(mid) {
  m <- filter(art, id == mid)
  t <- str_replace_all(m$text, MASK_RE, " qqmaskqq ")
  nt <- norm_txt(t)
  segs <- str_split(nt, "\\s*qqmaskqq\\s*")[[1]]
  lead <- startsWith(nt, "qqmaskqq"); trail <- endsWith(nt, "qqmaskqq")
  segs <- segs[segs != ""]
  if (!length(segs)) return(tibble(id = mid, duplicate_id = NA_character_, rule = "no text"))
  # candidates: same story = at least half of the words in common (different translations included)
  w <- setdiff(unique(str_split(paste(segs, collapse = " "), " ")[[1]]), "")
  sim <- map_dbl(uwords, jaccard, b = w)
  cand <- unmasked[sim >= 0.5 & unmasked$id != mid, ]
  cand$sim <- sim[sim >= 0.5 & unmasked$id != mid]
  if (!nrow(cand)) return(tibble(id = mid, duplicate_id = NA_character_, rule = "no candidate"))
  # identical outside the masks: each masked span may hold 0-12 words, everything else must match exactly
  # (normalised text contains only [a-z0-9 ], so segments need no regex escaping)
  gap <- "(?:[a-z0-9]+ ){0,12}"
  pat <- paste0("^", if (lead) gap else "", paste(segs, collapse = paste0(" ", gap)), if (trail) "(?: [a-z0-9]+){0,12}" else "", "$")
  ok <- cand$id[str_detect(cand$ntext, regex(pat))]
  if (length(ok)) tibble(id = mid, duplicate_id = paste(ok, collapse = "; "), rule = "identical outside masks")
  else tibble(id = mid, duplicate_id = NA_character_, rule = paste0("near duplicate(s) not used: ", paste0(cand$id, " (", round(cand$sim, 2), " word overlap)", collapse = "; ")))
}

dups <- map_dfr(masked$id[access$access != "ok"], find_duplicate)

# (3) masks filled from the PADI-web location file ----------------------------
fill_dir <- file.path(cfg$WORK_DIR, "filled_texts"); dir.create(fill_dir, showWarnings = FALSE)
locs <- if (file.exists(cfg$LOCATIONS_CSV)) read_csv(cfg$LOCATIONS_CSV, col_types = cols(.default = "c"), progress = FALSE) else tibble(article = character())
fill_masks <- function(mid) {
  m <- filter(art, id == mid); sp <- str_locate_all(m$text, MASK_RE)[[1]]
  l <- filter(locs, article == mid, !is.na(suppressWarnings(as.integer(position))))
  if (!nrow(sp) || !nrow(l)) return(tibble(id = mid, n_masks = nrow(sp), n_filled = 0L, filled_names = NA_character_))
  pos <- as.integer(l$position) + 1L   # position is 0-based
  hit <- map(seq_len(nrow(sp)), \(k) unique(l$location[pos >= sp[k, 1] - 2 & pos <= sp[k, 2] + 2]))
  n <- sum(lengths(hit) > 0)
  if (n) {
    txt <- m$text
    for (k in rev(which(lengths(hit) > 0)))   # keep the mask, add the hidden name after it: "-----[Dos Hermanas]"
      txt <- paste0(substr(txt, 1, sp[k, 2]), "[", paste(hit[[k]], collapse = " / "), "]", substr(txt, sp[k, 2] + 1, nchar(txt)))
    writeLines(txt, file.path(fill_dir, paste0(mid, ".txt")))
  }
  tibble(id = mid, n_masks = nrow(sp), n_filled = n, filled_names = if (n) paste(unique(unlist(hit)), collapse = "; ") else NA_character_)
}
fills <- map_dfr(masked$id, fill_masks)

res <- masked |> select(id, url, publication_date) |>
  mutate(url_original = map_chr(url, decode_google_news)) |>
  left_join(access, by = "id") |>
  left_join(dups, by = "id") |>
  left_join(fills, by = "id") |>
  mutate(text_for_extraction = case_when(
    access == "ok" ~ "original article",
    !is.na(duplicate_id) ~ paste("unmasked duplicate", duplicate_id),
    n_filled > 0 ~ sprintf("PADI-web text with %d of %d masks filled from the location file (flag for review)", n_filled, n_masks),
    TRUE ~ "PADI-web text (masked; flag for review)"))

write_csv(res, file.path(cfg$WORK_DIR, "masked_articles.csv"))
cat("Masked articles:", nrow(res), "\n"); print(count(res, access)); print(count(res, text_for_extraction))
