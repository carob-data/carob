# R script for "carob"
# license: GPL (>=3)

## NOTES
# 363 plot surveys of rainfed rice and maize at Ban Cuon (1999). One row per
# species with a rating > 0. Floristique is the DOC 1-9 scale. No Enherbement
# row. weed_species is the 5-letter code. Traitement is kept as the source code.

## ISSUES
# No coordinates. geographicCoverage is "Viet Nam"; country is the terminag name
# Vietnam. One species code is ZANSP?. Traitement mixes weeding, mulch, and
# fertilizer codes; 21 plots are blank. write_files: metadata: missing values
# in treatment variable treatment. missing variables: yield, yield_moisture,
# yield_part, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer,
# planting_date, harvest_date.


## NOT PROCESSED
# Facteurs not mapped: Parcelle, Age_parcelle, Type_culture, Climat, Observation,
# Pente, Precedent (forest and pasture descriptions, not a crop name),
# Toposequence, Type_sol, Type_suivi. Méta Données, FLO, FAC, and PA repeat
# the workbook or the DOC.


carob_script <- function(path) {

"
Weed survey in upland rice in Northern Viet Nam (1999)

Characterization of weed communities in rain-fed crops on slopes (rice, corn) in the context of the use of cover plants. Northern Viet Nam, Bac Kan Province, Ban Cuon Valley. Study by phytoecological surveys with abundance scale. This dataset covers 363 plant surveys for 113 weed species.
"

	uri <- "doi:10.18167/DVN1/MDP0H5"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "treatment",
		response_vars = "weed_species;ground_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "VIE-STE-1999-RIZ-AD.xlsx"]
	r1b <- carobiner::read.excel(f, sheet="Facteurs")
	r1c <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1b[r1b[[1]] == label, -1]))

	# VIE-STE-1999-RIZ-AD-DOC.txt: Ab/Dom 1-6 stored as 1-9 notes; same % table.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(mais = "maize", riz = "rice")
	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))]),
		treatment = trimws(as.character(get_row("Traitement"))),
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Vietnam", # geographicCoverage "Viet Nam"
		location = trimws(as.character(get_row("Lieu"))),
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
