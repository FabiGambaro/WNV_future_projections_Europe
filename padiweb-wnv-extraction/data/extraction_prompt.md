You extract human West Nile virus (WNV) cases from one news article (machine-translated to English by PADI-web; the original-language text may follow). Return every place that the article links to one or more HUMAN WNV cases.

One record per place x case cluster. Skip: animal cases (birds, horses), mosquito pools, surveillance or risk areas without human cases, country totals with no smaller place, and general information.

Fields:
- place: the place exactly as written in the text (municipality, town, province, county, regional unit, region or country).
- country: ISO 3166-1 alpha-2 code of the country the place is in (GR for Greece), or null if unknown.
- place_level: municipality | province | region | country. "province" means a unit that is itself a NUTS3 region (Italian province, Spanish province, Romanian county, Greek regional unit, Serbian district, Hungarian county, French department, Turkish province).
- admin_hint: the province/county/regional unit the text gives for a municipality (e.g. "Bosanci, Suceava County" -> "Suceava"), else null.
- role: infection (the text says the person was infected / contracted the disease / was exposed at this place, e.g. "contracted the disease in the municipality of Coria del Río"; use infection for a place abroad too, e.g. a Romanian patient infected in Milan -> place Milan, country IT, role infection, status imported) | residence | reporting_area (cases listed or reported by area) | hospital (only where the patient is treated) | possible_exposure (one of several candidate places of infection).
- status: autochthonous ONLY when the text explicitly says the case was locally acquired: words such as "autochthonous", "locally acquired", "local transmission", "domestic case", "indigenous", "native case", "acquired in <this country/area>", "had not travelled" / "did not travel". In bulletins that report "N domestic cases" and then list the areas of those cases (e.g. Greek EODY lists), each listed area is autochthonous. imported when the text says the infection was acquired abroad or during travel. not_stated otherwise (a place of infection alone, without such wording, is not_stated; its role infection already records it).
- human_case: yes | unclear (host not clearly human).
- certainty: confirmed | suspected (probable, suspected, under investigation, not yet confirmed).
- n_cases: number of cases at this place if given, else null.
- year_infection: year of infection. Use, in order: the season or year stated, the symptom-onset date, the diagnosis date, the publication date. Retrospective summaries and articles published November-February often describe an earlier season.
- year_basis: explicit | onset | diagnosis | publication.
- quote: the sentence(s) supporting the record, copied character for character from the text (it will be checked as an exact substring). Do not translate, fix or shorten inside the quote.
- note: anything a reviewer should know (masked place names "-----" near this place, conflicting dates, residence vs infection place, a place in Northern Cyprus), else null.

Rules:
- Residence is not place of infection. "A woman from Navarra infected in Utrera" -> one record, place Utrera, role infection.
- A case with two candidate exposure places -> one record per place, role possible_exposure.
- Masked text ("-----" or "*****") may hide place names: never guess them; mention the mask in note.
- article_outcome: "n place record(s)" or "no usable location: <reason>".
