# R script for "carob"
# license: GPL (>=3)

## NOTES
# brocaarbre is bored coffee fruits on a tree. antarbre is ant counts by species on a tree. Zero ant counts are kept.
# Dates such as April18 are month and year (2018-04). Farm labels Sustainable, Organic, and Conventional are the three practice types in the description.
# Country Nicaragua, adm1 Matagalpa, and location El Tuma - La Dalia are geographicCoverage. Nicaglobale X and Z are described as spatial coordinates but are projected metres, not longitude and latitude.

## ISSUES
# Ants were counted on trees in the coffee plots. antarbre does not name the tree species.
# No yield in kg/ha is in the tree-count files.
# Three plant-date pairs are entered twice in antarbre. visit_ keeps both.
# write_files: missing yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# unknown variables: plant_id_ (plot_id?), farm_practice_, fruits_sampled_, bored_fruits_, visit_, record_type_. invalid terms: pest_species for Hypothenemus hampei and the ant species names.

## NOT PROCESSED
# Nicaglobale.xlsx: shade, DBH, temperature, and diversity indices. Fruit counts there repeat brocaarbre. X and Z were not converted.
# ExplicationNicaglobal.docx and the analysis script.
# Readme.txt: documentation or a codebook, not an observation table.
# Script_BagnyBeilhe_et_al_2020.R: not read. .r file. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Raw data: Bagny Beilhe et al 2020: \"Pest-regulating networks of the coffee berry borer (Hypothenemus hampei) in agroforestry systems. Crop Protection, 131 : 10 p.\"

Données collectées à La Dalia, Nicaragua, de 2017 à 2018. Données issues de 3 systèmes agroforestiers à base de caféiers avec des niveaux de diversification végétale différents et trois types de pratiques (conventionnelles, durables et organiques). Dans chaque ferme 20 parcelles ont été suivies. Données collectées durant une année et demi pour suivre la dynamique des fruits scolytés dans les parcelles, les fourmis arboricoles, l'évolution de la température, de l'ombrage, du rendement et des dégâts associés aux scolytes dans les parcelles.
"

	uri <- "doi:10.18167/DVN1/QVTOBA"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence;bored_fruits_",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 45,
		carob_effort = 0.5
	)

	month_year <- function(x) {
		mon <- c(january="01", february="02", march="03", april="04", may="05", june="06", july="07", august="08", september="09", october="10", november="11", december="12")
		mm <- tolower(gsub("[0-9]", "", x))
		yy <- gsub("[^0-9]", "", x)
		paste0("20", yy, "-", unname(mon[mm]))
	}

	f1 <- ff[basename(ff) == "brocaarbre.csv"]
	r <- read.csv(f1, sep=";", check.names=FALSE)
	d1 <- data.frame(
		trial_id = trimws(as.character(r$farm_id)),
		hhid = trimws(as.character(r$farm_id)),
		plot_id = trimws(as.character(r$plot_id)),
		plant_id_ = trimws(as.character(r$plant_id)),
		date = month_year(trimws(as.character(r$date))),
		farm_practice_ = trimws(as.character(r$farm_id)),
		crop = "coffee",
		pest_species = "Hypothenemus hampei",
		fruits_sampled_ = as.numeric(r$fruit_nb),
		bored_fruits_ = as.numeric(r$fruit_CBB),
		pest_incidence = NA_integer_,
		visit_ = 1L,
		record_type_ = "coffee berry borer"
	)

	f2 <- ff[basename(ff) == "antarbre.xlsx"]
	r1 <- carobiner::read.excel(f2, sheet="antarbre")
	# Three plant-date pairs are entered twice, with different counts for some species.
	r1$visit_ <- ave(seq_len(nrow(r1)), r1$plant_id, r1$date, FUN=seq_along)
	idcols <- c("plant_id", "date", "plant_id2", "farm_id", "plot_id", "visit_")
	spp <- r1[, !(names(r1) %in% idcols)]
	n <- nrow(r1)
	d2 <- data.frame(
		trial_id = rep(trimws(as.character(r1$farm_id)), ncol(spp)),
		hhid = rep(trimws(as.character(r1$farm_id)), ncol(spp)),
		plot_id = rep(trimws(as.character(r1$plot_id)), ncol(spp)),
		plant_id_ = rep(trimws(as.character(r1$plant_id)), ncol(spp)),
		date = rep(month_year(trimws(as.character(r1$date))), ncol(spp)),
		farm_practice_ = rep(trimws(as.character(r1$farm_id)), ncol(spp)),
		crop = "coffee",
		pest_species = rep(gsub("_", " ", names(spp), fixed=TRUE), each=n),
		pest_incidence = as.integer(unlist(spp, use.names=FALSE)),
		fruits_sampled_ = NA_real_,
		bored_fruits_ = NA_real_,
		visit_ = rep(r1$visit_, ncol(spp)),
		record_type_ = "ant"
	)

	d <- rbind(d1, d2)
	d$country <- "Nicaragua"
	d$adm1 <- "Matagalpa"
	d$location <- "El Tuma - La Dalia"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$stress <- "pests"

	carobiner::write_files(path, meta, d)
}
