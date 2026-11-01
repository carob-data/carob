# R script for "carob"
# license: GPL (>=3)

## NOTES
# 540 plot surveys of the cotton rotation in northern Cameroon. One row per
# species with a rating > 0. Floristique is the DOC 1-9 scale. There is no
# Enherbement row. weed_species is the 5-letter code. Crop is Type_culture.
# Location is the Lieu value on each plot.

## ISSUES
# Title spans 1988-1992; Annee and Date_obs are all in 1990. Floristique has
# 155 species rows; the description says 157. No coordinates. write_files:
# missing variables: yield, yield_moisture, yield_part, latitude, longitude,
# K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.


## NOT PROCESSED
# Facteurs not mapped: Parcelle, Sol, Climat, Age_parcelle, Labour, Fumure
# (product rates without an NPK grade), Stade_culture, Herbicide, Sarclage,
# Butage, Anciennete_herbi, Herbicide-type, Sol_nu, Epoque_obs, Altitude,
# Prof sol, pH, MO, Argile, Limon, Sable, rainfall. Méta données, Facteurs
# origine, FLO, FAC, and PA repeat the workbook or the DOC.


carob_script <- function(path) {

"
Weed survey in cotton rotation in Northern Cameroon (1988-1992)

Characterization of weed communities in cotton rotation (cotton, maiz, sorghum, groundnut) in Northern Cameroon, within the framework of the Projet FAC Regional Malherbologie Garoua Cameroun. Phytoecological study in abundance scale 1-5 transformed to scale 1-9. This dataset covers 540 surveys of plants and environmental factors for 157 weed species.
"

	uri <- "doi:10.18167/DVN1/5ZEQGO"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "Projet FAC régional malherbologie Garoua Cameroun",
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;ground_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "CAM-LEB-1990-DIV-AD.xlsx"]
	r1b <- carobiner::read.excel(f, sheet="Facteurs")
	r1c <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1b[r1b[[1]] == label, -1]))

	# CAM-LEB-1990-DIV-AD-DOC.txt: transformed 1-9 note to % cover, Benin table.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(
		arachide = "groundnut", cotonnier = "cotton", mais = "maize", sorgho = "sorghum"
	)
	prev_map <- c(
		arachide = "groundnut", cotonnier = "cotton", jachere = "none",
		mais = "maize", sorgho = "sorghum"
	)

	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	# Date_obs is an Excel serial (1 Jun, 1 Aug, 1 Oct 1990).
	obs <- as.numeric(get_row("Date_obs"))
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Type_culture"))))]),
		previous_crop = unname(prev_map[tolower(trimws(as.character(get_row("Precedent"))))]),
		date = as.character(as.Date(obs, origin="1899-12-30")),
		country = "Cameroon", # geographicCoverage
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
