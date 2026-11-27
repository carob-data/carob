# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. No coordinates in the source. Plot coordinates are assigned from the five
#    named localities (Lieu) in Atsimo-Andrefana, SW Madagascar (geo_uncertainty
#    = 10000 m).
# 2. The Facteurs row "Irrigation" is "pluvial" for all 198 plots, whereas
#    "Facteur hydrique" records 12 plots as "irrigation partielle" and 186 as
#    "précipitation". irrigated is derived from Facteur hydrique.
# 3. No botanical species lookup sheet is provided in the raw files; weed_species
#    retains the 5-letter EPPO/CIRAD codes from the Floristique sheet.

## MISSING
# yield, yield_part, yield_moisture, planting_date, harvest_date, N_fertilizer,
# P_fertilizer, K_fertilizer (weed survey; quantitative rates/dates not recorded).

## NOTES
# 198 weed surveys (120 weed species) across cotton, maize, cassava, and groundnut
# fields in SW Madagascar (1998, Projet Sud-Ouest; PhD thesis Randriamampianina
# 2001). Wide table d holds plot-level crop, soil, and management factors; long
# holds species-level cover (%) converted from the 1-9 abundance scale.

## NOT PROCESSED
# Facteurs categorical/class variables without exact numeric equivalents:
# Age après la dernière jac, Age dela parcelle, Altitude (ranges), Assolement,
# Type_culture, Climat, Date de semis (early/normal/late), Date labour, Etat de
# surface, Gravier, Hauteur culture1/2 (ranges), Intensification, Nature sarclage1-3,
# Orientation, Pente (classes), Profondeur (ranges), Recouvrement culture2,
# Roche mère, Ruissellement, Structure, Surface de la parcelle (ranges), Zone.
# Redundant exports duplicating the workbook: MAD-JAR-1998-DIV-AD-FLO.txt,
# MAD-JAR-1998-DIV-FAC.txt, MAD-JAR-1998-DIV-PA-FLO.txt, MAD-JAR-1998-DIV-AD-DOC.txt.

carob_script <- function(path) {

"
Weed survey in various cropping systems in Madagascar (1998)

Characterization of weed communities in the cotton rotation (cotton, maize, cassava, groundnut) in South West Madagascar, within the framework of the PSO (Projet Sud-Ouest). Phytoecological study in abundance scale 1-9. This dataset covers 198 plant surveys for 120 weed species.
"

	uri <- "doi:10.18167/DVN1/O1EHV2"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIRAD;FOFIFA",
		publication = NA,
		project = "Projet Sud-Ouest",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_cover;weed_species;weed_species_cover;ground_cover",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "MAD-JAR-1998-DIV-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Facteurs", col_types = "text")
	r2 <- carobiner::read.excel(f, sheet = "Floristique")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	# 1-9 abundance scale to % cover (MAD-JAR-1998-DIV-AD-DOC.txt / Méta Données)
	cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(arachide = "groundnut", cotonnier = "cotton", mais = "maize", manioc = "cassava")
	prev_map <- c(
		arachide = "groundnut", cotonnier = "cotton", "jach\u00e8re" = "none",
		"ma\u00efs" = "maize", mais = "maize", manioc = "cassava",
		"manioc-ma\u00efs" = "cassava;maize"
	)
	assoc_map <- c("ma\u00efs" = "maize", mais = "maize", "l\u00e9gumineuses" = "legume")

	drain_map <- c(
		"tr\u00e8s faible" = "very poorly drained",
		"faible" = "poorly drained",
		"moyen" = "moderately well drained",
		"fort" = "well drained"
	)
	tex_map <- c(
		"tr\u00e8s fine" = "fine",
		"fine" = "fine",
		"moyenne" = "medium",
		"grossi\u00e8re" = "coarse",
		"tr\u00e8s grossi\u00e8re" = "coarse"
	)
	topo_map <- c(
		"terrain plat" = "flat",
		"haut de pente" = "upper slope",
		"milieu de pente" = "middle slope",
		"bas de pente" = "lower slope"
	)
	lab_method_map <- c(
		"z\u00e9ro labour" = "none",
		"charrue attel\u00e9e" = "ploughing",
		"labour tracteur" = "ploughing"
	)
	lab_tract_map <- c(
		"z\u00e9ro labour" = "none",
		"charrue attel\u00e9e" = "animal",
		"labour tracteur" = "tractor"
	)
	plant_map <- c(
		"poquet ligne manuel" = "dibbling;line sowing;manual",
		"poquet diffus manuel" = "dibbling;manual",
		"poquets tr\u00e8s espac\u00e9s (manioc m" = "dibbling"
	)

	loc <- trimws(get_row("Lieu"))
	geo <- data.frame(
		location = c("Ankazoabo", "Antseva", "Manombo", "Sakaraha", "Vineta"),
		longitude = c(44.5167, 43.6167, 43.4667, 44.5264, 44.2579),
		latitude = c(-22.2833, -22.6333, -22.9500, -22.9106, -23.0560)
	)
	gidx <- match(loc, geo$location)

	assoc <- tolower(trimws(get_row("Culture_origine2")))
	hydr <- tolower(trimws(get_row("Facteur hydrique")))
	fum <- tolower(trimws(get_row("Fumure")))
	lab <- tolower(trimws(get_row("Labour")))
	sar <- tolower(trimws(get_row("Sarclage")))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	rec1 <- as.integer(as.numeric(get_row("Recouvrement culture1")))

	d <- data.frame(
		trial_id = names(r1)[-1],
		plot_id = trimws(get_row("Parcelle")),
		date = as.character(as.integer(as.numeric(get_row("Ann\u00e9e")))),
		country = "Madagascar",
		adm1 = "Atsimo-Andrefana",
		location = loc,
		longitude = geo$longitude[gidx],
		latitude = geo$latitude[gidx],
		crop = unname(crop_map[tolower(trimws(get_row("Culture_origine")))]),
		intercrops = unname(assoc_map[assoc]),
		previous_crop = unname(prev_map[tolower(trimws(get_row("Pr\u00e9c\u00e9dent")))]),
		irrigated = hydr == "irrigation partielle",
		soil_drainage = unname(drain_map[tolower(trimws(get_row("Drainage")))]),
		soil_texture = unname(tex_map[tolower(trimws(get_row("Texture")))]),
		soil_type = trimws(get_row("Type de sol")),
		landscape_position = unname(topo_map[tolower(trimws(get_row("Topos\u00e9quence")))]),
		fertilizer_used = ifelse(is.na(fum), NA, fum != "sans fumure"),
		fertilizer_type = ifelse(fum == "sans fumure", "none",
			ifelse(fum == "fumure compl\u00e8te", "NPK",
			ifelse(fum %in% c("fumure azot\u00e9e", "autres"), "unknown", NA_character_))),
		land_prep_method = unname(lab_method_map[lab]),
		land_prep_traction = unname(lab_tract_map[lab]),
		planting_method = unname(plant_map[tolower(trimws(get_row("Type de semis")))]),
		weeding_done = sar != "non sarcl\u00e9",
		weeding_times = ifelse(sar == "non sarcl\u00e9", 0L,
			ifelse(sar == "moyenne (2 sarclages)", 2L, NA_integer_)),
		weed_cover = cover[match(enh, 1:9)],
		ground_cover = cover[match(rec1, 1:9)],
		stress = "weeds"
	)
	d$intercropped <- !is.na(d$intercrops)
	d$geo_from_source <- FALSE
	d$geo_uncertainty <- 10000
	d$geo_source <- "GeoNames (SW Madagascar town centers)"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	## Species-level (long) records
	score <- as.matrix(r2[, -1])
	j <- which(!is.na(score) & score > 0, arr.ind = TRUE)
	long <- data.frame(
		trial_id = colnames(score)[j[, "col"]],
		weed_species = trimws(as.character(r2[[1]][j[, "row"]])),
		weed_species_cover = cover[match(as.integer(score[j]), 1:9)]
	)

	carobiner::write_files(path, meta, d, long = long)
}
