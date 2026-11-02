# R script for "carob"
# license: GPL (>=3)

## NOTES
# 210 plot surveys in south-west Madagascar (1999). One row per species with a
# rating > 0. Floristique notes are the DOC 1-9 scale (Ab/Dom already transformed).
# Enherbement is that same 1-9 note. weed_species is the 5-letter code.
# mais/manioc is recorded as one crop, maize_cassava.

## ISSUES
# No coordinates. DOC names Tuléar; Lieu is Ankazoabo, Antseva, Manombo, Sakaraha,
# or Vineta. Five floristic cells are 11 or 22, outside the DOC 1-9 table, so
# ground_cover is NA there. write_files: missing variables: yield,
# yield_moisture, yield_part, latitude, longitude, K_fertilizer, N_fertilizer,
# P_fertilizer, planting_date, harvest_date.


## NOT PROCESSED
# Facteurs not mapped: ages, Assolement, Type_culture, Climat, Date_semis,
# Date_observation, Drainage, Fumure (qualitative), Intensification,
# Intensite_sarclage, Labour, Profondeur_sol, Structure, Type_sol, Zone.
# Méta Données, FLO, FAC, and PA repeat the workbook or the DOC.


carob_script <- function(path) {

"
Weed survey in various cropping systems in Madagascar (1999)

Characterization of weed communities in the cotton rotation (cotton, maize, cassava, groundnut) in South West Madagascar, within the framework of the PSO (Projet Sud-Ouest). Phytoecological study in abundance scale 1-9. This dataset covers 210 plant surveys for 140 weed species.
"

	uri <- "doi:10.18167/DVN1/XZ4CBA"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIRAD",
		publication = NA,
		project = "Projet Sud-Ouest",
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

	f <- ff[basename(ff) == "MAD-JA2-1999-DIV-AD.xlsx"]
	r1b <- carobiner::read.excel(f, sheet="Facteurs")
	r1c <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1b[r1b[[1]] == label, -1]))

	# MAD-JA2-1999-DIV-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(
		arachide = "groundnut", cotonnier = "cotton", "mais/manioc" = "maize_cassava"
	)
	prev_map <- c(
		arachide = "groundnut", cotonnier = "cotton", "jachère" = "none",
		"maïs" = "maize", mais = "maize", manioc = "cassava",
		"manioc-maïs" = "cassava;maize"
	)

	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))]),
		previous_crop = unname(prev_map[tolower(trimws(as.character(get_row("Precedent"))))]),
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Madagascar", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- grepl("_", d1b$crop)
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
