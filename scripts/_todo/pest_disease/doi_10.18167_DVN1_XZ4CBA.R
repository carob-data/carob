# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Coordinates are not in the source; georeferenced to the five named zones
#    in Atsimo-Andrefana (SW Madagascar: Antseva, Manombo Sud, Vineta,
#    Sakaraha, Ankazoabo) with 15 km uncertainty.
# 2. Five cells in `Floristique` have values 11 (4 cells) or 22 (1 cell),
#    outside the 1-9 abundance scale; `weed_species_cover` is set to NA for
#    those five entries.
# 3. `Floristique` uses 5-letter Amatrop/EPPO species codes and the dataset
#    does not include a code-to-Latin-name lookup table (taxonomic reference
#    is in CIRAD's separate "Reference Weed Flora" dataset).
# 4. `Intensite_sarclage` category "4 sarclages et plus" is mapped to
#    `weeding_times = 4` (lower bound).

## NEW
# plot_age_: plot cultivation age class ("1-10 years" or ">=11 years")
# rotation_pattern_: cropping rotation pattern ("monoculture", "crop/fallow", "same crop every 2-4 years")
# planting_timing_: relative sowing time ("early" or "late")
# soil_depth_class_: categorical soil depth ("<10 cm", "10-15 cm", ">=15 cm")
# soil_structure_: qualitative topsoil structure description

## MISSING
# yield, yield_moisture, yield_part, planting_date, harvest_date.

## NOTES
# 210 on-farm weed surveys (1999) in the cotton-based rotation (cotton,
# maize/cassava, groundnut) of SW Madagascar (Projet Sud-Ouest, FOFIFA/CIRAD).
# Wide table `d` holds all plot-level environmental and management factors;
# `long` holds species x plot weed cover (%) converted from the 1-9 scale.

## NOT PROCESSED
# Facteurs: Auteur, Type_culture, Climat (all tropical-sec), Intensification
# (composite of crop + fertilizer/tillage, already captured by individual
# columns). Duplicate text exports (MAD-JA2-1999-DIV-AD-FLO.txt, -FAC.txt,
# -PA-FLO.txt) and documentation (MAD-JA2-1999-DIV-AD-DOC.txt, Méta Données).


carob_script <- function(path) {

"
Weed survey in various cropping systems in Madagascar (1999)

Characterization of weed communities in the cotton rotation (cotton, maize, cassava, groundnut) in South West Madagascar, within the framework of the PSO (Projet Sud-Ouest). Phytoecological study in abundance scale 1-9. This dataset covers 210 plant surveys for 140 weed species.
"

	uri <- "doi:10.18167/DVN1/XZ4CBA"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "FOFIFA;CIRAD",
		publication = "https://agritrop.cirad.fr/490840/",
		project = "Projet Sud-Ouest",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_cover;weed_species;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "MAD-JA2-1999-DIV-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Facteurs")
	r2 <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	## 1-9 abundance scale to % cover (MAD-JA2-1999-DIV-AD-DOC.txt)
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	cult <- tolower(trimws(as.character(get_row("Culture_origine"))))
	crop_map <- c(arachide="groundnut", cotonnier="cotton", "mais/manioc"="maize")
	prev_map <- c(
		arachide = "groundnut", cotonnier = "cotton", "jachère" = "none",
		"maïs" = "maize", mais = "maize", manioc = "cassava",
		"manioc-maïs" = "cassava;maize"
	)

	lab <- tolower(trimws(as.character(get_row("Labour"))))
	lp_method <- ifelse(lab == "zéro labour", "none", "ploughing")
	lp_tract <- c(
		"zéro labour" = "none",
		"charrue à traction bovine" = "animal",
		"labour au tracteur" = "tractor"
	)

	sarc <- tolower(trimws(as.character(get_row("Intensite_sarclage"))))
	weed_n <- c(
		"minimum (1 sarclage)" = 1L,
		"2 sarclages" = 2L,
		"3 sarclages" = 3L,
		"4 sarclages et plus" = 4L
	)

	drain_map <- c(
		faible = "poorly drained",
		moyen = "moderately well drained",
		fort = "well drained"
	)

	soil_map <- c(
		vertisols = "Vertisol",
		"sables roux hydromorphes" = "hydromorphic red sand",
		"sables roux typiques" = "typical red sand",
		"sols fersiallitiques" = "fersiallitic soil",
		"sables roux arenacés" = "arenaceous red sand"
	)

	depth_map <- c(
		"inférieur à 10 cm" = "<10 cm",
		"10 à 15 cm" = "10-15 cm",
		"15 cm et plus" = ">=15 cm"
	)

	age_map <- c(
		"1 à 1O ans" = "1-10 years",
		"1 à 10 ans" = "1-10 years",
		"11 ans et plus" = ">=11 years"
	)

	rot_map <- c(
		monoculture = "monoculture",
		"culture / jachère" = "crop/fallow",
		"même culture tous les 2- 4 ans" = "same crop every 2-4 years"
	)

	semis_map <- c("précoce"="early", tardive="late")
	stage_map <- c("début cycle"="early cycle", "fin cycle"="late cycle")
	fum <- tolower(trimws(as.character(get_row("Fumure"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))

	d <- data.frame(
		trial_id = names(r1)[-1],
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Madagascar",
		adm1 = "Atsimo-Andrefana",
		location = trimws(as.character(get_row("Lieu"))),
		site = trimws(as.character(get_row("Zone"))),
		crop = unname(crop_map[cult]),
		intercrops = ifelse(cult == "mais/manioc", "cassava", NA_character_),
		intercropped = cult == "mais/manioc",
		previous_crop = unname(prev_map[tolower(trimws(as.character(get_row("Precedent"))))]),
		growth_stage = unname(stage_map[tolower(trimws(as.character(get_row("Date_observation"))))]),
		planting_timing_ = unname(semis_map[tolower(trimws(as.character(get_row("Date_semis"))))]),
		plot_age_ = unname(age_map[trimws(as.character(get_row("Age_parcelle")))]),
		rotation_pattern_ = unname(rot_map[tolower(trimws(as.character(get_row("Assolement"))))]),
		irrigated = FALSE,
		irrigation_method = "none",
		fertilizer_used = fum == "fumure minérale",
		land_prep_method = lp_method,
		land_prep_traction = unname(lp_tract[lab]),
		weeding_done = TRUE,
		weeding_times = unname(weed_n[sarc]),
		soil_drainage = unname(drain_map[tolower(trimws(as.character(get_row("Drainage"))))]),
		soil_type = unname(soil_map[tolower(trimws(as.character(get_row("Type_sol"))))]),
		soil_depth_class_ = unname(depth_map[tolower(trimws(as.character(get_row("Profondeur_sol"))))]),
		soil_structure_ = trimws(as.character(get_row("Structure"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		stress = "weeds",
		on_farm = TRUE,
		is_survey = TRUE
	)

	## SW Madagascar locality centroids (Atsimo-Andrefana / Tuléar region)
	geo <- data.frame(
		location = c("Antseva", "Manombo", "Vineta", "Sakaraha", "Ankazoabo"),
		adm2 = c("Toliara II", "Toliara II", "Sakaraha", "Sakaraha", "Ankazoabo"),
		latitude = c(-22.6333, -22.9500, -23.0167, -22.9000, -22.2833),
		longitude = c(43.6167, 43.4667, 44.5167, 44.5333, 44.5167)
	)
	d <- merge(d, geo, by="location", all.x=TRUE)
	d$geo_uncertainty <- 15000
	d$geo_source <- "GeoNames / Mindat (SW Madagascar locality centroids)"
	d$geo_from_source <- FALSE

	score <- as.matrix(r2[, -1])
	present <- which(!is.na(score) & score > 0, arr.ind=TRUE)
	sc <- as.integer(as.numeric(score[present]))
	long <- data.frame(
		trial_id = colnames(score)[present[, "col"]],
		weed_species = trimws(as.character(r2[[1]][present[, "row"]])),
		weed_species_cover = rating_to_cover[match(sc, 1:9)]
	)

	carobiner::write_files(path, meta, d, long=long)
}
