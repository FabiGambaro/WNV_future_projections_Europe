# Step 0-1: read the PADI-web articles, apply the loose pre-filter, flag masking, draw the sample.
source("R/config.R"); source("R/utils.R")

art <- read_articles(cfg$ARTICLES_CSV)
cat("Articles:", nrow(art), "\n")
cat("Publication dates:", min(art$publication_date, na.rm = TRUE), "to", max(art$publication_date, na.rm = TRUE), "\n")

wnv_re   <- regex("West Nile|WNV|Nile fever|Nile virus", ignore_case = TRUE)
human_re <- regex("\\b(?:patients?|persons?|people|m[ae]n|wom[ae]n|humans?|hospitali[sz]ed|neuroinvasive|died|deaths?|cases? of (?:infection|disease))\\b",
                  ignore_case = TRUE)

art <- art |>
  mutate(f_wnv   = str_detect(text, wnv_re),
         f_human = f_wnv & str_detect(text, human_re),
         masked  = str_detect(text, MASK_RE),
         url_original = map_chr(url, decode_google_news))

cat("Mention WNV:", sum(art$f_wnv), "\n")
cat("Mention WNV and a human term:", sum(art$f_human), "\n")
cat("Contain masking (-{3,} or *{3,}):", sum(art$masked), "\n")

keep <- filter(art, f_human)
if (nzchar(cfg$SAMPLE_IDS)) {
  ids <- read_csv(cfg$SAMPLE_IDS, col_types = cols(.default = "c"))$id
  sample <- filter(art, id %in% ids)
  cat("Sample fixed by", cfg$SAMPLE_IDS, ":", nrow(sample), "articles\n")
} else if (cfg$SAMPLE_N > 0 && cfg$SAMPLE_N < nrow(keep)) {
  set.seed(cfg$SEED)
  sample <- slice_sample(keep, n = cfg$SAMPLE_N)
  cat("Random sample: n =", nrow(sample), "seed =", cfg$SEED, "\n")
} else {
  sample <- keep
}

write_csv(select(art, -text), file.path(cfg$WORK_DIR, "articles_flags.csv"))
write_csv(sample, file.path(cfg$WORK_DIR, "sample_articles.csv"))
cat("Masked articles in sample:", sum(sample$masked), "\n")
