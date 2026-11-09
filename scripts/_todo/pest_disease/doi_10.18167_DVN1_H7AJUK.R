# R script for "carob"
# license: GPL (>=3)

## NOTES
# 52 muskwari (flood-recession sorghum) surveys in northern Cameroon. One row
# per species with a rating > 0. Floristique and Enherbement are the DOC 1-9
# notes. weed_species is the 5-letter code. The crop column says muskwari.

## ISSUES
# The Annee row is the sequence 1999-2050, not the survey year. Observation
# dates from the date row are in 1999 and 2000. No coordinates. Lieu includes
# both Djapai and Sjapai, kept as written. write_files: missing variables:
# yield, yield_moisture, yield_part, latitude, longitude, K_fertilizer,
# N_fertilizer, P_fertilizer, planting_date, harvest_date.


## NOT PROCESSED
# Facteurs not mapped: Annee (the 1999-2050 sequence), Type_culture, Climat,
# parcelle, village codes, karal codes, Wsol, position, diguettes, an.culture,
# code date, and the long inondation notes (Irrigation is used instead).
# Donnée originales, Méta Données, FLO, FAC, and PA repeat the workbook or the DOC.
# CAM-MAR-1999-MSK-AD-FLO.txt: not read. columns: "CAM-MAR-01" "CAM-MAR-02" "CAM-MAR-03" "CAM-MAR-04" "CAM-MAR-05" "CAM-MAR-06" "CAM-MAR-07" "CAM-MAR-08" "CAM-MAR-09" "CAM-MAR-10" "CAM-MAR-11" "CAM-MAR-12" "CAM-MAR-13" "CAM-MAR-14" "CAM-MAR-15" "CAM-MAR-16" "CAM-MAR-17" "CAM-MAR-18" "CAM-MAR-19" "CAM-MAR-20" "CAM-MAR-21" "CAM-MAR-22" "CAM-MAR-23" "CAM-MAR-24" "CAM-MAR-25" "CAM-MAR-26" "CAM-MAR-27" "CAM-MAR-28" "CAM-MAR-29" "CAM-MAR-30" "CAM-MAR-31" "CAM-MAR-32" "CAM-MAR-33" "CAM-MAR-34" "CAM-MAR-35" "CAM-MAR-36" "CAM-MAR-37" "CAM-MAR-38" "CAM-MAR-39" "CAM-MAR-40" "CAM-MAR-41" "CAM-MAR-42" "CAM-MAR-43" "CAM-MAR-44" "CAM-MAR-45" "CAM-MAR-46" "CAM-MAR-47" "CAM-MAR-48" "CAM-MAR-49" "CAM-MAR-50" "CAM-MAR-51" "CAM-MAR-52". Observation records are missing from d until this file is standardized.
# CAM-MAR-1999-MSK-FAC.txt: not read. columns: "CAM-MAR-01" "CAM-MAR-02" "CAM-MAR-03" "CAM-MAR-04" "CAM-MAR-05" "CAM-MAR-06" "CAM-MAR-07" "CAM-MAR-08" "CAM-MAR-09" "CAM-MAR-10" "CAM-MAR-11" "CAM-MAR-12" "CAM-MAR-13" "CAM-MAR-14" "CAM-MAR-15" "CAM-MAR-16" "CAM-MAR-17" "CAM-MAR-18" "CAM-MAR-19" "CAM-MAR-20" "CAM-MAR-21" "CAM-MAR-22" "CAM-MAR-23" "CAM-MAR-24" "CAM-MAR-25" "CAM-MAR-26" "CAM-MAR-27" "CAM-MAR-28" "CAM-MAR-29" "CAM-MAR-30" "CAM-MAR-31" "CAM-MAR-32" "CAM-MAR-33" "CAM-MAR-34" "CAM-MAR-35" "CAM-MAR-36" "CAM-MAR-37" "CAM-MAR-38" "CAM-MAR-39" "CAM-MAR-40" "CAM-MAR-41" "CAM-MAR-42" "CAM-MAR-43" "CAM-MAR-44" "CAM-MAR-45" "CAM-MAR-46" "CAM-MAR-47" "CAM-MAR-48" "CAM-MAR-49" "CAM-MAR-50" "CAM-MAR-51" "CAM-MAR-52". Observation records are missing from d until this file is standardized.
# CAM-MAR-1999-MSK-PA-FLO.txt: not read. columns: "CAM-MAR-01" "CAM-MAR-02" "CAM-MAR-03" "CAM-MAR-04" "CAM-MAR-05" "CAM-MAR-06" "CAM-MAR-07" "CAM-MAR-08" "CAM-MAR-09" "CAM-MAR-10" "CAM-MAR-11" "CAM-MAR-12" "CAM-MAR-13" "CAM-MAR-14" "CAM-MAR-15" "CAM-MAR-16" "CAM-MAR-17" "CAM-MAR-18" "CAM-MAR-19" "CAM-MAR-20" "CAM-MAR-21" "CAM-MAR-22" "CAM-MAR-23" "CAM-MAR-24" "CAM-MAR-25" "CAM-MAR-26" "CAM-MAR-27" "CAM-MAR-28" "CAM-MAR-29" "CAM-MAR-30" "CAM-MAR-31" "CAM-MAR-32" "CAM-MAR-33" "CAM-MAR-34" "CAM-MAR-35" "CAM-MAR-36" "CAM-MAR-37" "CAM-MAR-38" "CAM-MAR-39" "CAM-MAR-40" "CAM-MAR-41" "CAM-MAR-42" "CAM-MAR-43" "CAM-MAR-44" "CAM-MAR-45" "CAM-MAR-46" "CAM-MAR-47" "CAM-MAR-48" "CAM-MAR-49" "CAM-MAR-50" "CAM-MAR-51" "CAM-MAR-52". Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Weed surveys in muskwari sorghum in Northern Cameroon (1999)

Study of the weed flora of flood sorghum (Muskwari) in the karal soil areas in Northern Cameroon, within the framework of the PRASAC project. Abundance index rating 1-9. This dataset covers 52 plant surveys for 64 weed species.
"

	uri <- "doi:10.18167/DVN1/H7AJUK"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIRAD",
		publication = NA,
		project = "PRASAC",
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

	f <- ff[basename(ff) == "CAM-MAR-1999-MSK-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Facteurs")
	r2 <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	# CAM-MAR-1999-MSK-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	# muskwari is flood-recession sorghum (DOC: sorgho de décrue).
	crop_map <- c(muskwari = "sorghum")
	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	# date is an Excel serial. Annee is not used (it is a 1999-2050 fill).
	obs <- as.numeric(get_row("date"))
	d1b <- data.frame(
		trial_id = names(r1)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))]),
		date = as.character(as.Date(obs, origin="1899-12-30")),
		country = "Cameroon", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- FALSE
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE
	d1b$geo_from_source <- FALSE

	present <- which(!is.na(r2[, -1]) & r2[, -1] > 0, arr.ind=TRUE)
	scores <- as.integer(as.numeric(r2[, -1][present]))
	d <- data.frame(
		trial_id = names(r2)[-1][present[, "col"]],
		weed_species = trimws(as.character(r2[[1]][present[, "row"]])),
		ground_cover = rating_to_cover[match(scores, 1:9)]
	)
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
