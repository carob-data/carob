# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Abstract states 140 surveys (2011-2014) and 389 species, whereas
#    `Facteurs` and `CDI-AWA-2011-VIV-AD-DOC.txt` have 240 plots (all dated
#    2011 in `Facteurs`) and `Floristique` has 366 species.
# 2. Per-plot crop species are not recorded (`Culture_origine = "vivrier"`,
#    generic food crops; abstract mentions cassava, maize, okra), so `crop` is
#    set to `"unknown"` with `land_use = "food crops"`.
# 3. Coordinates are not in the source; midpoint of Dimbokro and Bongouanou is
#    used with 30 km uncertainty.
# 4. Three EPPO codes in `Floristique` (DAGWE, MZUBN, SCPRU) do not appear in
#    sheet "Données d'origine" and are retained as codes in `weed_species`.

## MISSING
# yield, yield_moisture, yield_part, weed_cover, planting_date, harvest_date.

## NOTES
# 240 on-farm weed surveys in rainfed extensive food crops in the
# Dimbokro-Bongouanou area (Center-East Côte d'Ivoire, 2011-2014). Wide table
# `d` holds plot-level metadata; `long` holds species x plot weed cover (%)
# converted from the 1-9 scale with Latin names from "Données d'origine".

## NOT PROCESSED
# Facteurs: Auteur, Type_culture (all vivrier), Intensification (all extensif),
# Climat (all tropical). Duplicate text exports (CDI-AWA-2011-VIV-AD-FLO.txt,
# -FAC.txt, -PA-FLO.txt) and documentation (CDI-AWA-2011-VIV-AD-DOC.txt,
# sheet "Méta données").


carob_script <- function(path) {

"
Weed surveys in food crops in Center-East Côte d'Ivoire (2011-2014)

Inventory of weeds of food crops (cassava, maize, okra) in the Center-East of Côte d'Ivoire from 2011 to 2014. Rating in abundance index 1-5 transformed into scale 1-9. This dataset covers 140 plant surveys for 389 weed species.
"

	uri <- "doi:10.18167/DVN1/ZH4W5M"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "UFHB;CIRAD",
		publication = NA,
		project = "Amatrop",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "CDI-AWA-2011-VIV-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Facteurs")
	r2 <- carobiner::read.excel(f, sheet="Floristique")
	r3 <- carobiner::read.excel(f, sheet="Données d'origine", col_names=FALSE)

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	## 1-9 abundance scale to % cover (CDI-AWA-2011-VIV-AD-DOC.txt)
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	d <- data.frame(
		trial_id = names(r1)[-1],
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Côte d'Ivoire",
		adm1 = "Lacs",
		location = trimws(as.character(get_row("Lieu"))),
		crop = "unknown",
		land_use = "food crops",
		irrigated = FALSE,
		irrigation_method = "none",
		stress = "weeds",
		on_farm = TRUE,
		is_survey = TRUE
	)

	## Midpoint between Dimbokro (6.647, -4.705) and Bongouanou (6.652, -4.204)
	d$latitude <- 6.649
	d$longitude <- -4.455
	d$geo_uncertainty <- 30000
	d$geo_source <- "Midpoint of GeoNames Dimbokro and Bongouanou"
	d$geo_from_source <- FALSE

	## Species lookup (EPPO code -> Latin name) from "Données d'origine"
	hdr <- which(trimws(as.character(r3[[2]])) == "code EPPO")[1]
	spp <- data.frame(
		code = trimws(as.character(r3[[2]][(hdr + 2):nrow(r3)])),
		weed_species = trimws(as.character(r3[[1]][(hdr + 2):nrow(r3)]))
	)
	spp <- spp[!is.na(spp$code) & nzchar(spp$code), ]
	spp <- spp[!duplicated(spp$code), ]

	score <- as.matrix(r2[, -1])
	present <- which(!is.na(score) & score > 0, arr.ind=TRUE)
	long <- data.frame(
		trial_id = colnames(score)[present[, "col"]],
		code = trimws(as.character(r2[[1]][present[, "row"]])),
		weed_species_cover = rating_to_cover[match(as.integer(as.numeric(score[present])), 1:9)]
	)
	long <- merge(long, spp, by="code", all.x=TRUE)
	miss <- is.na(long$weed_species) | !nzchar(long$weed_species)
	long$weed_species[miss] <- long$code[miss]
	long$code <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
