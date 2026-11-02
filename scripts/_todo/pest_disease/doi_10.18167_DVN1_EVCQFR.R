# R script for "carob"
# license: GPL (>=3)

## NOTES
# 110 cotton surveys in Guinea. One row per species with a rating > 0.
# Floristique is the DOC 1-9 scale. Note_globale is that same 1-9 note and is
# stored as weed_cover. weed_species is the 5-letter code. Location is Lieu
# (Dabola, Kankan, or Koundara-Gaoual).

## ISSUES
# Title says 1996; the file also has 1997-08-01. The DOC calendar line
# (23 September-5 October 2018) does not match the Date serials. No coordinates.
# write_files: missing variables: yield, yield_moisture, yield_part, latitude,
# longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.


## NOT PROCESSED
# Facteurs not mapped: Type_culture, Climat, Epoque/cycle, jour_de_mission,
# Observation_du_jour. Méta Données and the origine sheet repeat the survey.
# FLO, FAC, and PA repeat the workbook or the DOC.


carob_script <- function(path) {

"
Weed survey in cotton crop in Guinea (1996)

Characterization of weed flora of cotton crop in Guinea in 1996, within the framework of the projects PCGK (Projet Coton Gaoual - Koundara) and PCK (Projet Coton Kankan). Abundance index rating 1-9. This dataset covers 110 plant surveys for 158 weed species.
"

	uri <- "doi:10.18167/DVN1/EVCQFR"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIRAD",
		publication = NA,
		project = "PCGK; PCK",
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;ground_cover;weed_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "GUI-MAR-1996-COT-AD.xlsx"]
	r1b <- carobiner::read.excel(f, sheet="Facteurs")
	r1c <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1b[r1b[[1]] == label, -1]))

	# GUI-MAR-1996-COT-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(cotonnier = "cotton")
	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Note_globale")))
	# Date is an Excel serial (1996-10-23, 1996-11-01, 1997-08-01).
	obs <- as.numeric(get_row("Date"))
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))]),
		date = as.character(as.Date(obs, origin="1899-12-30")),
		country = "Guinea", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- FALSE
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE
	d1b$geo_from_source <- FALSE

	present <- which(!is.na(r1c[, -1]) & r1c[, -1] > 0, arr.ind=TRUE)
	scores <- as.integer(as.numeric(r1c[, -1][present]))
	d <- data.frame(
		trial_id = names(r1c)[-1][present[, "col"]],
		weed_species = trimws(as.character(r1c[[1]][present[, "row"]])),
		ground_cover = rating_to_cover[match(scores, 1:9)]
	)
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
