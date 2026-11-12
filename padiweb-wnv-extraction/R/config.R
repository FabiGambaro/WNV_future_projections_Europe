# Paths and settings for the PADI-web WNV pipeline.
# Every path can be overridden with an environment variable of the same name.

cfg <- list(
  # Inputs from PADI-web
  ARTICLES_CSV  = Sys.getenv("ARTICLES_CSV",  "data-raw/articles.csv"),
  LOCATIONS_CSV = Sys.getenv("LOCATIONS_CSV", "data-raw/locations.csv"),  # fallback for coordinates and for masked names
  # folder of original pages saved with tools/download_masked.R (empty = try to fetch them now)
  ORIGINALS_DIR = Sys.getenv("ORIGINALS_DIR", ""),

  # Sampling: a file with an `id` column fixes the article set; otherwise a random sample is drawn
  SAMPLE_IDS = Sys.getenv("SAMPLE_IDS", ""),
  SAMPLE_N   = as.integer(Sys.getenv("SAMPLE_N", "100")),   # 0 = all filtered articles
  SEED       = as.integer(Sys.getenv("SEED", "42")),

  # Extraction output (step 2): one row per place x case cluster, plus one row per article outcome
  RECORDS_CSV  = Sys.getenv("RECORDS_CSV",  "work/records_extracted.csv"),
  OUTCOMES_CSV = Sys.getenv("OUTCOMES_CSV", "work/article_outcomes.csv"),

  # Reference data (downloaded once into ref/)
  REF_DIR   = Sys.getenv("REF_DIR", "ref"),
  NUTS_YEARS = c("2024", "2021"),
  # Optional cached gazetteer (place, country, lon, lat) used when GeoNames cannot be downloaded
  GAZETTEER_CACHE = Sys.getenv("GAZETTEER_CACHE", ""),

  # Data tables shipped with the pipeline
  ALIASES_CSV     = Sys.getenv("ALIASES_CSV", "data/nuts3_aliases.csv"),
  CORRECTIONS_CSV = Sys.getenv("CORRECTIONS_CSV", "data/manual_corrections.csv"),          # NUTS3 fixes
  RECORD_FIXES_CSV = Sys.getenv("RECORD_FIXES_CSV", "data/record_corrections.csv"),        # field fixes to extracted records
  ADDED_RECORDS_CSV = Sys.getenv("ADDED_RECORDS_CSV", "data/added_records.csv"),           # records the readers missed
  REVIEW_DECISIONS_CSV = Sys.getenv("REVIEW_DECISIONS_CSV", "data/review_decisions.csv"),  # final grades set by a human reviewer

  WORK_DIR = Sys.getenv("WORK_DIR", "work"),
  OUT_DIR  = Sys.getenv("OUT_DIR", "output"),

  # Claude API batch extraction (step 2, full corpus)
  MODEL = Sys.getenv("CLAUDE_MODEL", "claude-opus-5-5")
)

dir.create(cfg$WORK_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(cfg$OUT_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(cfg$REF_DIR,  showWarnings = FALSE, recursive = TRUE)
