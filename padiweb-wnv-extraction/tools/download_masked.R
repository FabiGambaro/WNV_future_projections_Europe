# Download the original web pages of the masked PADI-web articles, so their hidden place names can be read.
# Run on your own computer (not behind a restrictive proxy). Needs only R and the packages httr2 and readr.
#   Rscript tools/download_masked.R full_run/masked_to_download.csv originals
# Re-running skips articles already saved or already tried, so it can be stopped and resumed.
# Output: originals/<id>.html (one page per article) and originals/download_log.csv (one row per attempt).
# Share the originals folder privately (zip it): the pages are publishers' content, never commit them to GitHub.
suppressPackageStartupMessages({ library(httr2); library(readr) })
args <- commandArgs(trailingOnly = TRUE)
list_csv <- if (length(args) >= 1) args[1] else "full_run/masked_to_download.csv"
out_dir  <- if (length(args) >= 2) args[2] else "originals"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
todo <- read_csv(list_csv, col_types = cols(.default = "c"))
log_file <- file.path(out_dir, "download_log.csv")

fetch <- function(u) {
  resp <- tryCatch(
    request(u) |>
      req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36") |>
      req_headers(`Accept-Language` = "en,es,it,el,sr,ro,de,fr,hu;q=0.8") |>
      req_timeout(40) |> req_retry(max_tries = 2) |> req_error(is_error = \(r) FALSE) |> req_perform(),
    error = function(e) e)
  if (inherits(resp, "error")) return(list(status = NA_integer_, final_url = NA_character_, html = NULL, detail = conditionMessage(resp)))
  # keep the page as raw bytes: some pages declare one encoding and use another, which breaks text handling
  body <- if (resp_status(resp) == 200) tryCatch(resp_body_raw(resp), error = function(e) NULL) else NULL
  list(status = resp_status(resp), final_url = resp$url, html = body, detail = "")
}

# articles already tried in an earlier run are skipped too (set RETRY_FAILED=1 to try the failed ones again)
tried <- if (file.exists(log_file) && Sys.getenv("RETRY_FAILED") != "1") unique(read_csv(log_file, col_types = cols(.default = "c"))$id) else character()
cat("Already saved:", length(list.files(out_dir, "\\.html$")), "| already tried:", length(tried), "\n")
for (i in seq_len(nrow(todo))) {
  id <- todo$id[i]; f <- file.path(out_dir, paste0(id, ".html"))
  if (file.exists(f) || id %in% tried) next
  # some url fields hold several links joined by a backtick or a space: try each in turn
  urls <- strsplit(todo$url[i], "[` ]+")[[1]]; urls <- urls[nzchar(urls)]
  for (u in urls) {
    r <- fetch(u)
    ok <- isTRUE(length(r$html) > 2000)
    if (ok) ok <- !inherits(tryCatch(writeBin(r$html, f), error = function(e) e), "error")
    write_csv(data.frame(id = id, url = u, final_url = r$final_url, http_status = r$status,
                         result = if (ok) "ok" else if (is.na(r$status)) "error" else if (r$status %in% c(401, 403, 429, 451)) "blocked" else if (r$status == 404) "not_found" else "failed",
                         detail = substr(r$detail, 1, 200), time = format(Sys.time())),
              log_file, append = file.exists(log_file))
    if (ok) break
    Sys.sleep(1)
  }
  if (i %% 50 == 0) cat(i, "of", nrow(todo), "\n")
  Sys.sleep(1)   # be polite to the news sites
}
log <- read_csv(log_file, col_types = cols(.default = "c"))
cat("Saved pages:", length(list.files(out_dir, "\\.html$")), "of", nrow(todo), "\n")
print(table(log$result))
