# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cotton weeding experiment at Mafa-Kilda (2001), 1217 surveys (plot x quadrat x
# date). One row per species with a rating > 0. Floristique and Enherbement are
# the DOC 1-9 notes. Modalité att = mechanical (attelé) weeding, man = hand
# weeding. ITK lab = ploughing, sd = semis direct (no tillage). weed_species is
# the 5-letter code.

## ISSUES
# One floristic cell (HPYSU) is 11, outside the DOC 1-9 table, so ground_cover
# is NA there. No coordinates. on_farm is TRUE because the records are field
# plots; the sheet does not say whether Mafa-Kilda is a station. write_files:
# missing variables: yield, yield_moisture, yield_part, latitude, longitude,
# K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.


## NOT PROCESSED
# Facteurs not mapped: Stagiaire, Epoque, Parcelle, Placette (quadrat, not used
# as rep), Sol codes. Méta données, FLO, FAC, and PA repeat the workbook or the DOC.


carob_script <- function(path) {

"
Effect of mechanical weeding compared to hand weeding on cotton weed flora in Northern Cameroon (2001)

Comparative experimental study of the effect of mechanical weeding compared to manual weeding on the weed flora of cotton in Northern Cameroon, within the framework of the project PRASAC. Abundance index rating 1-9. This dataset covers 1217 plant surveys for 71 weed species.
"

	uri <- "doi:10.18167/DVN1/9PDXYC"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIRAD",
		publication = NA,
		project = "PRASAC",
		design = "comparison of mechanical and hand weeding",
		data_type = "experiment",
		treatment_vars = "weeding_method;land_prep_method",
		response_vars = "weed_species;ground_cover;weed_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "CAM-VAL-2001-COT-AD.xlsx"]
	r1b <- carobiner::read.excel(f, sheet="Facteurs")
	r1c <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1b[r1b[[1]] == label, -1]))

	# CAM-VAL-2001-COT-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(cotonnier = "cotton")
	prev_map <- c(
		arachide = "groundnut", "jachère" = "none", "maïs" = "maize",
		"niébé" = "cowpea", sorgho = "sorghum"
	)
	weed_map <- c(att = "mechanical weeding", man = "hand weeding")
	prep_map <- c(lab = "ploughing", sd = "none")

	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	# Date is an Excel serial in the 2001 cotton season.
	obs <- as.numeric(get_row("Date"))
	mod <- tolower(trimws(as.character(get_row("Modalité"))))
	itk <- tolower(trimws(as.character(get_row("ITK"))))
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))]),
		previous_crop = unname(prev_map[tolower(trimws(as.character(get_row("Precedent"))))]),
		weeding_method = unname(weed_map[mod]),
		land_prep_method = unname(prep_map[itk]),
		date = as.character(as.Date(obs, origin="1899-12-30")),
		country = "Cameroon", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- FALSE
	d1b$on_farm <- TRUE
	d1b$is_survey <- FALSE
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
