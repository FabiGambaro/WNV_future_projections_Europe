# Task: extract human West Nile virus case places from a batch of news articles

You are one of many readers working in parallel on the same study; every reader must apply the rules identically.

Inputs (read them fully; do not skip any article; the batch file is long, read it in consecutive chunks until the end):
- Instructions (authoritative): /mnt/project-files/padiweb-wnv-extraction/data/extraction_prompt.md
- Your articles: {BATCH}.txt (articles separated by "=== ARTICLE <id> ==="). Some masked articles are followed by a second text: the ORIGINAL article (original language), an UNMASKED DUPLICATE, or the same text with hidden names restored in [brackets] from the PADI-web location file.
- Format reference (good records from the pilot): /mnt/project-files/padiweb-wnv-extraction/pilot/records_extracted.csv (first ~60 lines only)

Read every article yourself (no keyword scripts to decide records). Apply the instructions exactly. Reminders:
- role "infection" whenever the text says where the person was infected / contracted the disease / was exposed ("contracted the disease in the municipalities of Seville" -> each town, role infection). A Romanian "territorial unit of exposure" list is role infection.
- status "autochthonous" ONLY with explicit wording (autochthonous, locally acquired, local transmission, domestic case, indigenous, native case, acquired in <this country/area>, had not travelled / did not travel). A place of infection alone stays "not_stated" (its role infection is enough). Imported/travel = "imported". In bulletins that say "N domestic cases" and then list the areas of those cases (e.g. Greek EODY lists), each listed area is autochthonous.
- "Infected by the bite of a domestic/native mosquito" ("heimische Mücke") said ABOUT THE CASE counts as autochthonous; a general sentence that WNV is carried by domestic mosquitoes does not.
- Cases summed over several years with no per-year split: year_infection null, year_basis "explicit", range in note.
- Residence is not place of infection. Hospital-only location -> role "hospital".
- Masked spans ("-----", "*****"): if the second text reveals the hidden place, record it and take the quote from that second text; otherwise never guess. Mention masks in note.
- Skip animal-only, mosquito-only, country-total-only content and other diseases. Never make a region/country record when a smaller place for the same cases is given. Do not record places named only in menus or other headlines of the original web page.
- quote: copied character for character from ONE source text of that article (check: it must be an exact substring). Keep quotes short (one sentence or a list fragment). text_source names that source: "padiweb" (the PADI-web English text or title), "original", "duplicate:<id>" or "padiweb+locations" (exactly as given in the section header). Prefer "padiweb" when the place is visible there.
- place: as written in the chosen source; for a place in Greek/Cyrillic script you may give the Latin form in note.
- country: ISO2 of the PLACE (GR for Greece), not of the news source.

Output, written with Python (json.dumps per line, ensure_ascii=False), in the same folder as the batch:
1. {BATCH}_records.jsonl — one JSON object per record with keys: article_id, place, country, place_level, admin_hint, role, status, human_case, certainty, n_cases, year_infection, year_basis, quote, note, text_source (null where empty; n_cases and year_infection integers or null).
2. {BATCH}_outcomes.jsonl — one object per article: {"article_id": ..., "outcome": "n place record(s)" or "no usable location: <reason>"}.

Then run: python3 -I {DIR}/check_quotes.py {BATCH}_texts.json {BATCH}_records.jsonl {BATCH}_outcomes.jsonl
Fix every problem it reports until it reports 0 problems and all outcomes. Delete any helper script you wrote; write nothing else anywhere.
Final message (short): articles, records, records with status autochthonous, records with role infection, and at most 5 articles you were unsure about (id + one line why).
