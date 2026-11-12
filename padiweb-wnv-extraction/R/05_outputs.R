# Step 6: outputs. The presence table holds records that state the place of infection (grade A:
# "contracted the disease in Utrera"). Records that say the case was local ("autochthonous", "locally
# acquired", "domestic") but only give residence or reporting area (grade B) form the B_residence layer.
# Both are explicit and appear in Records and Review. Everything else is kept in Not_explicit for traceability.
source("R/config.R"); source("R/utils.R")
suppressPackageStartupMessages(library(writexl))

rec  <- read_csv(file.path(cfg$WORK_DIR, "records_geocoded.csv"), col_types = cols(.default = "c"))
mask <- read_csv(file.path(cfg$WORK_DIR, "masked_articles.csv"), col_types = cols(.default = "c"))
outc <- if (file.exists(cfg$OUTCOMES_CSV)) read_csv(cfg$OUTCOMES_CSV, col_types = cols(.default = "c")) else tibble()
smp  <- read_csv(file.path(cfg$WORK_DIR, "sample_articles.csv"), col_types = cols(.default = "c"))

cols <- c("article_id","publication_date","source_country","place","place_level","admin_hint","role","status","human_case","certainty",
          "n_cases","year_infection","year_basis","nuts3_2024","nuts3_name","nuts3_2021","geo_method","province_hint","lon","lat",
          "evidence_grade","quote","quote_verified","text_source","note","review_reason")
# clickable link to each article (first link of the url field; Google News links decoded to the publisher's URL)
art <- read_articles(cfg$ARTICLES_CSV) |> transmute(article_id = id,
  url = map_chr(url, ~ if (is.na(.x)) NA_character_ else decode_google_news(strsplit(.x, "[` ]+")[[1]][1])))
rec <- rec |> left_join(art, by = "article_id")
cols <- c(cols[1], "url", cols[-1])
explicit <- rec |> filter(explicit_autochthonous == "TRUE") |> select(any_of(cols))
other    <- rec |> filter(explicit_autochthonous != "TRUE") |> select(any_of(cols))
review   <- explicit |> filter(!is.na(review_reason))

to_cells <- function(x) x |>
  mutate(country = substr(nuts3_2024, 1, 2)) |>
  group_by(country, nuts3_2024, nuts3_2021, nuts3_name, year_infection) |>
  summarise(n_articles = n_distinct(article_id), n_records = n(),
            places_mentioned = paste(sort(unique(place)), collapse = "; "),
            article_ids = paste(sort(unique(article_id)), collapse = "; "), .groups = "drop") |>
  arrange(country, nuts3_2024, year_infection)
# grade A: the text states where the person was infected
presence <- rec |> filter(in_presence == "TRUE") |> to_cells()
# grade B: the text says the case was local but places it only by residence / reporting area;
# NUTS3 x years already in the presence table are left out
b_layer <- rec |> filter(in_b_layer == "TRUE") |> to_cells() |>
  anti_join(presence, by = c("nuts3_2024", "year_infection"))

# masked articles are reviewed per article (the risk is a hidden place, not a wrong record)
mask <- mask |> select(-any_of(c("detail"))) |>
  left_join(rec |> group_by(id = article_id) |>
              summarise(n_records = n(), n_explicit_records = sum(explicit_autochthonous == "TRUE")), by = "id") |>
  mutate(across(c(n_records, n_explicit_records), ~ coalesce(.x, 0L)),
         to_check = if_else(str_starts(text_for_extraction, "PADI-web text"),
                            "open url_original and look for human cases in the masked spans", NA_character_)) |>
  arrange(desc(n_explicit_records), desc(n_records))

n_art <- nrow(smp); n_with <- n_distinct(rec$article_id)
readme <- tibble(
  item = c("Sample", "Rule", "NUTS3_year", "B_residence", "Records", "Review", "Not_explicit", "Masked_articles", "Counts"),
  description = c(
    sprintf("%d articles; %d produced at least one human case place record.", n_art, n_with),
    "Presence requires the text to state where the person was infected ('contracted the disease in <town>', 'exposed in Milan'): grade A records with a NUTS3 code. Records that say the case was local (autochthonous / locally acquired / domestic) but give only where the person lives or where the case was recorded are grade B and kept apart.",
    "One row per NUTS3 x year of infection with a stated place of infection (presence, not case counts). Same content as NUTS3_year_presence_explicit.csv.",
    "Sensitivity layer: NUTS3 x years supported only by grade B records (explicitly local, placed by residence or reporting area, place of infection not stated); NUTS3 x years already in NUTS3_year are not repeated. Same content as NUTS3_year_B_residence.csv.",
    "All explicit records, with quote, NUTS3 2024/2021, how the code was found, and grade.",
    "Explicit records that need a human check; the reason is in review_reason.",
    "Records without explicit local acquisition or a stated place of infection (residence, reporting area or hospital with status not stated, paraphrase only, or imported), and all records outside continental Europe (other continents, EU outermost regions such as Guadeloupe), kept for traceability only.",
    "Every masked article in the sample: whether the original could be opened, whether an identical unmasked duplicate was found, and how many records it produced. Masked articles whose original could not be read must be checked at url_original.",
    sprintf("%d records in total; %d explicit; %d NUTS3 x year presences (grade A); %d more NUTS3 x years from grade B only; %d to review.", nrow(rec), nrow(explicit), nrow(presence), nrow(b_layer), nrow(review))))

linkify <- function(d) { if ("url" %in% names(d)) d$url <- xl_hyperlink(coalesce(d$url, ""), coalesce(d$url, "")); d }
write_xlsx(list(README = readme, NUTS3_year = presence, B_residence = b_layer, Records = linkify(explicit), Review = linkify(review),
                Not_explicit = other, Masked_articles = mask, Articles = outc),
           file.path(cfg$OUT_DIR, "padiweb_wnv_explicit.xlsx"))
write_csv(presence, file.path(cfg$OUT_DIR, "NUTS3_year_presence_explicit.csv"))
write_csv(b_layer, file.path(cfg$OUT_DIR, "NUTS3_year_B_residence.csv"))

cat(readme$description[9], "\n")
print(count(presence, country, sort = TRUE))
