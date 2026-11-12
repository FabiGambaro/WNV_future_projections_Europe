# Step 2 at scale: extract human WNV case places from each article with the Claude API (Message Batches,
# 50% cheaper, results within 24 h). Needs ANTHROPIC_API_KEY in the environment.
# Usage: Rscript R/03_extract_claude_batch.R dry-run  -> writes one example request, sends nothing
#        Rscript R/03_extract_claude_batch.R submit   -> writes work/batch_ids.txt
#        Rscript R/03_extract_claude_batch.R collect  -> polls, downloads, writes RECORDS_CSV and OUTCOMES_CSV
source("R/config.R"); source("R/utils.R")
suppressPackageStartupMessages({ library(httr2); library(jsonlite) })

API <- "https://api.anthropic.com/v1/messages/batches"
mode <- commandArgs(trailingOnly = TRUE)[1]   # submit | collect | dry-run (writes the first request, sends nothing)
key <- Sys.getenv("ANTHROPIC_API_KEY"); if (!nzchar(key) && !identical(mode, "dry-run")) stop("Set ANTHROPIC_API_KEY")
api <- function(url) request(url) |> req_headers(`x-api-key` = key, `anthropic-version` = "2023-06-01") |>
  req_retry(max_tries = 5, is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 529))
nullable <- function(type, ...) list(anyOf = list(c(list(type = type), list(...)), list(type = "null")))
record_schema <- list(
  type = "object", additionalProperties = FALSE,
  required = c("place","country","place_level","admin_hint","role","status","human_case","certainty",
               "n_cases","year_infection","year_basis","quote","note"),
  properties = list(
    place = list(type = "string"), country = nullable("string"),
    place_level = list(type = "string", enum = c("municipality","province","region","country")),
    admin_hint = nullable("string"),
    role = list(type = "string", enum = c("infection","residence","reporting_area","hospital","possible_exposure")),
    status = list(type = "string", enum = c("autochthonous","imported","not_stated")),
    human_case = list(type = "string", enum = c("yes","unclear")),
    certainty = list(type = "string", enum = c("confirmed","suspected")),
    n_cases = nullable("integer"), year_infection = nullable("integer"),
    year_basis = list(type = "string", enum = c("explicit","onset","diagnosis","publication")),
    quote = list(type = "string"), note = nullable("string")))
schema <- list(type = "object", additionalProperties = FALSE, required = c("article_outcome", "records"),
               properties = list(article_outcome = list(type = "string"),
                                 records = list(type = "array", items = record_schema)))
system_prompt <- paste(readLines("data/extraction_prompt.md", warn = FALSE), collapse = "\n")

# text sent for each article: PADI-web text, plus the original article (step 02) or the identical duplicate
html_to_text <- function(f) {
  # pages that are valid UTF-8 are read as UTF-8 (libxml otherwise falls back to Latin-1 when the charset tag is late or missing)
  b <- readBin(f, "raw", file.size(f)); s <- suppressWarnings(rawToChar(b[b != as.raw(0)]))
  doc <- if (validUTF8(s)) xml2::read_html(s, encoding = "UTF-8") else xml2::read_html(f)
  xml2::xml_remove(xml2::xml_find_all(doc, "//script|//style|//noscript|//nav|//footer|//header"))
  ps <- xml2::xml_text(xml2::xml_find_all(doc, "//h1|//h2|//h3|//p|//li"))
  paste(str_squish(ps[nzchar(str_squish(ps))]), collapse = "\n")
}
article_input <- function(a, mk) {
  txt <- paste0("Article id: ", a$id, "\nPublished: ", a$publication_date, "\nSource country: ", a$country,
                "\n\nTitle: ", a$title, "\n\nText (PADI-web English translation):\n", a$text)
  m <- mk[mk$id == a$id, ]
  if (nrow(m) && m$access == "ok") {
    f <- file.path(cfg$WORK_DIR, "original_articles", paste0(a$id, ".html"))
    o <- html_to_text(f); writeLines(o, sub("\\.html$", ".txt", f))
    txt <- paste0(txt, "\n\nOriginal article (fills masked places; quote from here when a place is only visible here):\n", o)
  } else if (nrow(m) && !is.na(m$duplicate_id)) {
    d <- art[art$id == str_split(m$duplicate_id, "; ")[[1]][1], ]
    txt <- paste0(txt, "\n\nUnmasked duplicate ", d$id, " (identical outside the masked spans):\n", d$text)
  } else if (nrow(m) && file.exists(f <- file.path(cfg$WORK_DIR, "filled_texts", paste0(a$id, ".txt")))) {
    txt <- paste0(txt, "\n\nSame text with hidden names restored from the PADI-web location file, shown in [brackets] after the mask ",
                  "(quote from here when a place is only visible here):\n", paste(readLines(f, warn = FALSE), collapse = "\n"))
  }
  txt
}

if (mode %in% c("submit", "dry-run")) {
  art <- read_articles(cfg$ARTICLES_CSV)
  smp <- read_csv(file.path(cfg$WORK_DIR, "sample_articles.csv"), col_types = cols(.default = "c"))
  mk  <- read_csv(file.path(cfg$WORK_DIR, "masked_articles.csv"), col_types = cols(.default = "c"))
  reqs <- map(seq_len(nrow(smp)), function(i) list(
    custom_id = smp$id[i],
    params = list(model = cfg$MODEL, max_tokens = 16000,
                  system = system_prompt,
                  output_config = list(effort = "high", format = list(type = "json_schema", schema = schema)),
                  messages = list(list(role = "user", content = article_input(smp[i, ], mk))))))
  if (mode == "dry-run") {
    write_json(reqs[[1]], file.path(cfg$WORK_DIR, "batch_request_example.json"), auto_unbox = TRUE, null = "null", pretty = TRUE)
    cat("Wrote", file.path(cfg$WORK_DIR, "batch_request_example.json"), "\n"); quit(save = "no")
  }
  ids <- c()
  for (chunk in split(reqs, ceiling(seq_along(reqs) / 2000))) {
    b <- api(API) |> req_body_json(list(requests = chunk), auto_unbox = TRUE, null = "null") |> req_perform() |> resp_body_json()
    ids <- c(ids, b$id); cat("Submitted batch", b$id, "with", length(chunk), "requests\n")
  }
  writeLines(ids, file.path(cfg$WORK_DIR, "batch_ids.txt"))
}

if (identical(mode, "collect")) {
  ids <- readLines(file.path(cfg$WORK_DIR, "batch_ids.txt"))
  smp <- read_csv(file.path(cfg$WORK_DIR, "sample_articles.csv"), col_types = cols(.default = "c"))
  mk  <- read_csv(file.path(cfg$WORK_DIR, "masked_articles.csv"), col_types = cols(.default = "c"))
  recs <- list(); outs <- list()
  for (id in ids) {
    repeat {
      b <- api(paste0(API, "/", id)) |> req_perform() |> resp_body_json()
      if (b$processing_status == "ended") break
      cat(id, b$processing_status, "- processing:", b$request_counts$processing, "\n"); Sys.sleep(60)
    }
    lines <- api(b$results_url) |> req_perform() |> resp_body_string() |> str_split("\n") |> pluck(1)
    for (l in lines[nzchar(lines)]) {
      r <- fromJSON(l, simplifyVector = FALSE); cid <- r$custom_id   # results come in any order: key by custom_id
      if (r$result$type != "succeeded") { outs[[cid]] <- tibble(article_id = cid, outcome = paste("API", r$result$type)); next }
      msg <- r$result$message
      if (!identical(msg$stop_reason, "end_turn")) { outs[[cid]] <- tibble(article_id = cid, outcome = paste("stop_reason", msg$stop_reason)); next }
      txt <- paste(map_chr(keep(msg$content, ~ .x$type == "text"), "text"), collapse = "")
      j <- fromJSON(txt, simplifyVector = FALSE)
      outs[[cid]] <- tibble(article_id = cid, outcome = j$article_outcome)
      m <- mk[mk$id == cid, ]
      src <- if (nrow(m) && m$access == "ok") "original" else if (nrow(m) && !is.na(m$duplicate_id)) paste0("duplicate:", str_split(m$duplicate_id, "; ")[[1]][1]) else
             if (nrow(m) && file.exists(file.path(cfg$WORK_DIR, "filled_texts", paste0(cid, ".txt")))) "padiweb+locations" else "padiweb"
      if (length(j$records)) recs[[cid]] <- map_dfr(j$records, ~ as_tibble(map(.x, ~ if (is.null(.x)) NA else .x))) |>
        mutate(article_id = cid, .before = 1) |>
        # a quote found only in the original/duplicate text is checked there; otherwise against PADI-web text
        mutate(text_source = if_else(str_detect(paste(smp$title[smp$id == cid], smp$text[smp$id == cid]), fixed(quote)), "padiweb", src))
    }
  }
  write_csv(bind_rows(recs), cfg$RECORDS_CSV); write_csv(bind_rows(outs), cfg$OUTCOMES_CSV)
  cat("Records:", nrow(bind_rows(recs)), "from", length(outs), "articles\n")
}
