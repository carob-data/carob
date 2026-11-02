# R script for "carob"
# license: GPL (>=3)

## NOTES
# 198 plot surveys in south-west Madagascar (1998). One row per species with a
# rating > 0. Floristique and Enherbement are the DOC 1-9 notes (not percents);
# both use the DOC percent table. weed_species is the 5-letter code.

## ISSUES
# No coordinates in the file. DOC names Tuléar; Lieu is Ankazoabo, Antseva,
# Manombo, Sakaraha, or Vineta. Culture_origine2 "légumineuses" is not a crop
# species. write_files: missing variables: yield, yield_moisture, yield_part,
# latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date,
# harvest_date. invalid terms: intercrops: légumineuses.


## NOT PROCESSED
# Facteurs not mapped: Parcelle, ages, Altitude, Assolement, Type_culture, Climat,
# Date de semis, Date labour, Drainage, Etat de surface, Facteur hydrique, Fumure
# (qualitative, no nutrient rate), soil surface and texture, crop height and cover,
# labour and weeding intensity, Pente, Profondeur, Zone. Méta Données, FLO, FAC,
# and PA repeat the workbook or the DOC.


carob_script <- function(path) {

"
Weed survey in various cropping systems in Madagascar (1998)

Characterization of weed communities in the cotton rotation (cotton, maize, cassava, groundnut) in South West Madagascar, within the framework of the PSO (Projet Sud-Ouest). Phytoecological study in abundance scale 1-9. This dataset covers 198 plant surveys for 120 weed species.
"

	uri <- "doi:10.18167/DVN1/O1EHV2"
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

	f <- ff[basename(ff) == "MAD-JAR-1998-DIV-AD.xlsx"]
	r1b <- carobiner::read.excel(f, sheet="Facteurs")
	r1c <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1b[r1b[[1]] == label, -1]))

	# MAD-JAR-1998-DIV-AD-DOC.txt, same 1-9 note to % cover table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(
		arachide = "groundnut", cotonnier = "cotton", mais = "maize", manioc = "cassava"
	)
	prev_map <- c(
		arachide = "groundnut", cotonnier = "cotton", "jachère" = "none",
		"maïs" = "maize", mais = "maize", manioc = "cassava",
		"manioc-maïs" = "cassava;maize"
	)
	assoc <- tolower(trimws(as.character(get_row("Culture_origine2"))))
	assoc[is.na(assoc) | !nzchar(assoc) | assoc %in% c("na", "-")] <- NA_character_
	# légumineuses is a group, not a species name in the sheet.
	assoc_map <- c("maïs" = "maize", mais = "maize", "légumineuses" = "légumineuses")

	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))]),
		intercrops = unname(assoc_map[assoc]),
		previous_crop = unname(prev_map[tolower(trimws(as.character(get_row("Précédent"))))]),
		date = as.character(as.integer(as.numeric(get_row("Année")))),
		country = "Madagascar", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- !is.na(d1b$intercrops)
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE
	# No coordinates in the workbook.
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
