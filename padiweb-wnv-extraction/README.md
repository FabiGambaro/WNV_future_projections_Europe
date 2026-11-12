# PADI-web WNV extraction

Extracts **explicitly autochthonous human West Nile virus cases** from PADI-web news articles and summarises them as **NUTS3 region x year of infection**, with a verbatim quote behind every record.

`SKILL.md` describes the method, so Claude can apply it as a skill. The `R/` scripts implement it.

## Layout
```
SKILL.md                     method (also usable as a Claude skill)
R/config.R                   paths and settings (each can be set with an environment variable)
R/utils.R                    shared helpers (normalisation, masking, NUTS and GeoNames loaders)
R/01_prepare.R               read articles, pre-filter, flag masking, draw or fix the sample
R/02_masked_recovery.R       masked articles: try the original URL, else an identical unmasked duplicate
R/03_extract_claude_batch.R  extraction with the Claude API (Message Batches): dry-run | submit | collect
R/04_geocode_grade.R         quote check, NUTS3 2024/2021, evidence grade, review flags
R/05_outputs.R               presence table + Excel workbook (explicit autochthonous only)
data/extraction_prompt.md    extraction instructions sent to Claude
data/nuts3_aliases.csv       exonyms and press spellings -> NUTS3 code (extend as needed)
data/manual_corrections.csv  documented hand corrections
data/record_corrections.csv  field fixes to extracted records
data/added_records.csv       records the readers missed, added by hand
data/review_decisions.csv    final grades set by the human reviewer (applied in R/04)
full_run/                    inputs and outputs of the full run (all articles)
full_run/records_extracted.csv   all records extracted by Claude, with verbatim quotes
full_run/article_outcomes.csv    one row per article: relevant / not relevant, masking route
full_run/masked_to_download.csv, download_log.csv, originals_check.csv
                             masked articles, download attempts and check of the original pages
full_run/output/             final outputs: NUTS3_year_presence_explicit.csv (grade A),
                             NUTS3_year_B_residence.csv (grade B), padiweb_wnv_explicit.xlsx
tools/                       helpers for the run (masked-page download, batch merge, quote check, reader brief)
```

## Requirements
R >= 4.1 with `readr dplyr stringr stringi tidyr purrr sf writexl httr2 jsonlite xml2`.
Step 03 also needs an `ANTHROPIC_API_KEY`.
Reference data is downloaded into `ref/` on first use: GISCO NUTS3 polygons at 1:1M and GeoNames `cities1000`.

The PADI-web articles are news texts under publishers' copyright. Keep them in `data-raw/`, which is git-ignored, and don't commit them.

## Run
```sh
export ARTICLES_CSV=data-raw/articles.csv LOCATIONS_CSV=data-raw/locations.csv
Rscript R/01_prepare.R                    # SAMPLE_N=0 for all filtered articles, or SAMPLE_IDS=file.csv
Rscript tools/download_masked.R full_run/masked_to_download.csv data-raw/originals   # once, on a computer that can reach the news sites
export ORIGINALS_DIR=data-raw/originals
Rscript R/02_masked_recovery.R
Rscript R/03_extract_claude_batch.R dry-run   # check one request, sends nothing
Rscript R/03_extract_claude_batch.R submit
Rscript R/03_extract_claude_batch.R collect   # waits for the batch, writes work/records_extracted.csv
Rscript R/04_geocode_grade.R
Rscript R/05_outputs.R                    # output/NUTS3_year_presence_explicit.csv, output/padiweb_wnv_explicit.xlsx
```
`pilot/gazetteer_cache.csv` holds the GeoNames points used in the pilot. Pass it with `GAZETTEER_CACHE=` only if GeoNames can't be downloaded.
