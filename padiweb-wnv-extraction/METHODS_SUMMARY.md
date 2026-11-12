# PADI-web WNV extraction: summary for replication

## Inputs
- **PADI-web articles CSV** (main source): 4,555 articles after the West Nile / human-case pre-filter, text machine-translated to English. The `country` column is a PADI-web tag and is not used.
- **PADI-web location file**: used only to fill masked place names (its `position` points into the text) and as a fallback for coordinates.
- **Original pages of masked articles** (`originals` folder, `ORIGINALS_DIR`): 2,090 articles had masked spans (`-----`). 1,690 pages were downloaded, and 1,583 were usable. These are publishers' copyright, so keep them private and never on GitHub.
- **Reference data**: Eurostat GISCO NUTS3 2024 and 2021 polygons at 1:1M (`NUTS_RG_01M_<year>_4326_LEVL_3.geojson`, downloaded automatically or placed in `REF_DIR`), and GeoNames `cities1000`.
- **Package files**: `R/`, `data/` (name aliases, manual corrections, extraction prompt) and `tools/`. These are needed together with `SKILL.md`.

## Processing
1. **Prepare** (`R/01`): pre-filter articles and flag masked text.
2. **Masked articles** (`R/02`): use the routes below in order. In the full run:
   - Read the original page: 1,583 articles.
   - Use an identical unmasked duplicate: 2.
   - Fill masks from the location file: 29.
   - Read the masked text as is: 476 (listed for manual checking).
3. **Extraction**: each article is read in full. Every human case place becomes a record with:
   - `role`: infection / residence / reporting area / hospital / possible exposure.
   - `status`: autochthonous / imported / not stated.
   - Year of infection.
   - A verbatim quote.

   Done with the Claude API (`R/03`) or with parallel reader agents (`tools/reader_brief.md`, `tools/check_quotes.py`, `tools/merge_batches.py`). The full run produced 18,600 records from 2,919 articles; one missed record was added and 7 fields were corrected after an independent replication (`data/added_records.csv`, `data/record_corrections.csv`).
4. **Quote check, NUTS3, grading** (`R/04`):
   - Every quote must appear verbatim in its source text.
   - Places get a NUTS3 code by one of these routes: name match, aliases, Greek/Serbian normalisation, GeoNames point-in-polygon, then the province named in the text.
   - Records are graded:
     - **A**: the text states where the person was infected ("infected in Utrera", "exposed in Milan"). This includes travel cases infected in another country.
     - **B (hedged)**: place of infection given with hedged wording ("probably / most likely / presumed / believed / allegedly infected in X"), or a sentence that may refer to mosquitoes (Pančevo, Požarevac 2022).
     - **B**: explicitly local (a literal term such as autochthonous, locally acquired or domestic case, or an explicit "had not travelled"), but located only by residence or reporting area.
     - **B-**: residence or reporting area with no explicit local wording.
     - **C**: hospital only, competing exposure places, suspected case or unknown year.
   - Paraphrases ("local mosquitoes", "no evidence of residing abroad") do not count as local.
5. **Human review**: the Review tab was checked by hand (FG, 2026-10-21). The 23 decisions (6 confirmed A, 9 set to B, 8 excluded) are in `data/review_decisions.csv` and override the automatic grade; no NUTS3-year changed.
6. **Outputs** (`R/05`).

## Outputs (`full_run/output/`)
- `NUTS3_year_presence_explicit.csv`: **grade A presences**, 105 NUTS3 x years. One row per NUTS3 x year of infection (presence, not case counts).
- `NUTS3_year_B_residence.csv`: **grade B sensitivity layer**, 145 more NUTS3 x years not already in the A table.
- `padiweb_wnv_explicit.xlsx`:

| Tab | Rows | Content |
|---|---|---|
| README | 9 | Rules and counts of the run |
| NUTS3_year | 105 | Same as the presence CSV (grade A) |
| B_residence | 145 | Same as the B CSV |
| Records | 7,190 | Every explicit record in continental Europe (grade A or explicitly local), whatever its NUTS3 result |
| Review | 0 | Explicit records that still need a human check (all 23 have been reviewed; decisions in `data/review_decisions.csv` and `review_reason`) |
| Not_explicit | 11,411 | All other records (no local wording, paraphrase only, imported, or outside continental Europe), for traceability |
| Masked_articles | 2,090 | Masked articles: how each was read and what to check |
| Articles | 4,555 | Outcome per article (records found, or why none) |

**Presence table columns** (NUTS3_year, B_residence):

| Column | Meaning |
|---|---|
| `country` | NUTS country prefix (EL = Greece) |
| `nuts3_2024`, `nuts3_2021` | NUTS3 codes in the 2024 and 2021 versions |
| `nuts3_name` | NUTS3 name |
| `year_infection` | Year of infection |
| `n_articles`, `n_records` | Supporting articles and records |
| `places_mentioned` | Places as written in the articles |
| `article_ids` | IDs of the supporting articles |

**Record columns** (Records, Review, Not_explicit):

| Column | Meaning |
|---|---|
| `article_id`, `url` | Article ID and a clickable link to the publisher's page |
| `publication_date` | Publication date |
| `source_country` | PADI-web tag, unreliable and not used |
| `place` | Place as written in the text |
| `place_level` | municipality / province (= a NUTS3 unit) / region / country |
| `admin_hint` | Province named with a municipality |
| `role` | infection / residence / reporting_area / hospital / possible_exposure |
| `status` | Wording in the text: autochthonous / imported / not_stated. A stated place of infection without such a word stays `not_stated` and still counts as grade A |
| `human_case` | yes / unclear |
| `certainty` | confirmed / suspected |
| `n_cases` | Number of cases |
| `year_infection`, `year_basis` | Year of infection and how it was set (explicit, onset, diagnosis or publication) |
| `nuts3_2024`, `nuts3_name`, `nuts3_2021` | NUTS3 result |
| `geo_method` | How the NUTS3 code was found |
| `province_hint` | Province used to place a municipality |
| `lon`, `lat` | Point used for point-in-polygon |
| `evidence_grade` | A / B / B- / C, or why there is no NUTS3 |
| `quote`, `quote_verified` | Verbatim supporting text and whether it was found in the source |
| `text_source` | padiweb / original / duplicate:<id> / padiweb+locations |
| `note` | The reader's comment |
| `review_reason` | Why the record is in Review |

**Masked_articles columns**:

| Column | Meaning |
|---|---|
| `url_original` | Original article address |
| `access`, `http_status` | Download result |
| `duplicate_id`, `rule` | Identical unmasked duplicate, if one was used |
| `n_masks`, `n_filled`, `filled_names` | Masks filled from the location file |
| `text_for_extraction` | Which text was read |
| `n_records`, `n_explicit_records` | Records produced |
| `to_check` | What to look at manually |

## Study area
Continental Europe only. Records from other continents (for example travel cases infected in the US, Egypt or India) and from EU outermost regions (Guadeloupe and the other FRY regions, the Canaries, the Azores, Madeira) go to Not_explicit. Cyprus is kept as an EU member state.

## Limitations
- 476 masked articles could only be read with their masks.
- 263 records in the study area got no NUTS3 code, and none of them is explicitly local, so they don't affect the tables. They are mostly garbled machine-translated Serbian district names, Italian health districts (ULSS) and areas ("Polesine", "Lodigiano"), misspelt Spanish towns, and places in Ukraine and Moldova, which have no NUTS codes. Kosovo named as a whole is treated as country level.
- Points within 1 km of a NUTS3 border are flagged for review.
- Two readings of the same article can differ slightly record by record, so compare presences rather than records.
