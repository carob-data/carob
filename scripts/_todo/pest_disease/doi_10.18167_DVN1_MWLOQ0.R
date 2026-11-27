# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The dataset title, DOC file, and Facteurs sheet state year 2020, whereas the
#    repository abstract states 2019. Year is set to "2020" from the data.
# 2. No coordinates in the source. Plot coordinates are assigned from the six
#    named towns (Lieu) in Mauritius (geo_uncertainty = 3000 m).
# 3. Two cultivated plots (MAU-PAY-04, MAU-PAY-05; bordure/culture = "c") have
#    missing Culture_origine and are assigned crop = "unknown".
# 4. Five genus-level EPPO codes in Floristique (ABUS4, MUBS1, PYJS1, RUBS1,
#    SEBS4) correspond to the five "*SS" genus codes in Données origine (ABUSS,
#    MUBSS, PYJSS, RUBSS, SEBSS) and are mapped accordingly.

## NEW
# weed_family_: botanical family of the weed species

## MISSING
# yield, yield_part, yield_moisture, planting_date, harvest_date, N_fertilizer,
# P_fertilizer, K_fertilizer (weed community survey; not recorded).

## NOTES
# 69 weed surveys (131 weed species) across vegetable plots and field borders in
# Mauritius (2020), under the Smart Agriculture Project. Plot-level metadata are
# in d; species-level cover (%) converted from the 1-9 abundance scale (DOC) are
# in long with full botanical names and families from the Données origine sheet.

## NOT PROCESSED
# Facteurs: Auteur (Payet), n° (survey sequence), Type_culture (all maraichage),
# Climat (all tropical-humide).
# Redundant exports duplicating the workbook: MAU-PAY-2020-MAR-AD-FLO.txt,
# MAU-PAY-2020-MAR-FAC.txt, MAU-PAY-2020-MAR-PA-FLO.txt, MAU-PAY-2020-MAR-AD-DOC.txt.

carob_script <- function(path) {

"
Weed survey in vegetable growing in Mauritius (2020)

Characterization of weed communities of vegetable crops in Mauritius in 2019 within the framework of the Smart Agriculture Project.
Abundance scale notation 1-9. This dataset covers 69 plant surveys for 131 weed species.
"

	uri <- "doi:10.18167/DVN1/MWLOQ0"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "Smart Agriculture Project",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_cover;weed_species;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 95,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "MAU-PAY-2020-MAR-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Facteurs", col_types = "text")
	r2 <- carobiner::read.excel(f, sheet = "Floristique")
	r3 <- carobiner::read.excel(f, sheet = "Donn\u00e9es origine", col_types = "text")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	# 1-9 abundance scale to % cover (MAU-PAY-2020-MAR-AD-DOC.txt / Méta Données)
	cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	src <- trimws(get_row("Culture_origine"))
	bc <- trimws(get_row("bordure/culture"))

	crop_map <- c(
		"Calebasse" = "bottle gourd",
		"Carotte" = "carrot",
		"Carotte fin de culture" = "carrot",
		"Choux" = "cabbage",
		"Choux fin de culture" = "cabbage",
		"Giromon" = "pumpkin",
		"Haricot, carotte" = "common bean",
		"Aubergine lalo manioc" = "eggplant",
		"Lalo manioc piment" = "okra",
		"Lalo, aubergine" = "okra",
		"Ma\u00efs" = "maize",
		"Manioc cultiv\u00e9" = "cassava",
		"Papaye" = "papaya",
		"Poireau" = "leek",
		"Roquette" = "rocket",
		"Salade, br\u00e8de" = "lettuce",
		"Br\u00e8de" = "vegetable",
		"Serre aubergine" = "eggplant",
		"Serre butturnut" = "winter squash",
		"Serre cacahouette" = "groundnut",
		"Serre salade" = "lettuce",
		"Songe maurice" = "taro",
		"Tomate" = "tomato",
		"Voeme" = "cowpea",
		"Fruitier" = "fruit",
		"Haie fruiti\u00e8re" = "fruit"
	)
	inter_map <- c(
		"Haricot, carotte" = "carrot",
		"Aubergine lalo manioc" = "okra;cassava",
		"Lalo manioc piment" = "cassava;chili pepper",
		"Lalo, aubergine" = "eggplant",
		"Salade, br\u00e8de" = "vegetable"
	)
	uncropped <- c(
		"A venir", "A venir cotonmili", "A venir courgette",
		"Bord de ravine", "Champ non travaill\u00e9", "Champ travaill\u00e9",
		"D\u00e9sherbant", "Entre deux serres", "Foss\u00e9",
		"Future p arcelle SMARTinoccup\u00e9e", "Haie", "Haie cultiv\u00e9e",
		"Haie et foss\u00e9", "Haie proche ravine", "Le long du chemin",
		"Non cultiv\u00e9", "Serre vide butturnut \u00e0 venir"
	)

	crop <- unname(crop_map[src])
	crop[src %in% uncropped | (is.na(src) & bc == "b")] <- "none"
	crop[is.na(src) & bc == "c"] <- "unknown"

	intercrops <- unname(inter_map[src])

	land_use <- ifelse(bc == "b", "field border", "cropland")
	land_use[grepl("^Haie", src)] <- "hedge"
	land_use[src %in% c("Foss\u00e9", "Bord de ravine")] <- "ditch/ravine border"
	land_use[grepl("^Serre", src)] <- "greenhouse"
	land_use[src %in% c("Non cultiv\u00e9", "Future p arcelle SMARTinoccup\u00e9e",
		"A venir", "A venir cotonmili", "A venir courgette")] <- "fallow"

	prev_map <- c(
		"calebasse" = "bottle gourd",
		"canne \u00e0 sucre" = "sugarcane",
		"carotte" = "carrot",
		"laitue" = "lettuce",
		"oignon" = "onion",
		"salade" = "lettuce"
	)

	loc <- trimws(get_row("Lieu"))
	geo <- data.frame(
		location = c("Bel Ombre", "Flic-en-Flac", "Mapou", "Petite-Rivi\u00e8re",
			"Rivi\u00e8re des Anguilles", "Saint-Pierre"),
		adm1 = c("Savanne", "Black River", "Rivi\u00e8re du Rempart",
			"Black River", "Savanne", "Moka"),
		longitude = c(57.4132, 57.3708, 57.6057, 57.4459, 57.5508, 57.5208),
		latitude = c(-20.5049, -20.2804, -20.0789, -20.1955, -20.4853, -20.2175)
	)
	gidx <- match(loc, geo$location)

	irr <- tolower(trimws(get_row("Irrigation")))
	enh <- as.integer(as.numeric(get_row("Enherbement")))

	d <- data.frame(
		trial_id = names(r1)[-1],
		date = as.character(as.integer(as.numeric(get_row("Ann\u00e9e")))),
		country = "Mauritius",
		adm1 = geo$adm1[gidx],
		location = loc,
		site = trimws(get_row("Exploitation")),
		longitude = geo$longitude[gidx],
		latitude = geo$latitude[gidx],
		elevation = as.numeric(get_row("Altitude (m)")),
		land_use = land_use,
		crop = crop,
		intercrops = intercrops,
		intercropped = !is.na(intercrops),
		previous_crop = unname(prev_map[tolower(trimws(get_row("Pr\u00e9cedent")))]),
		land_prep_method = ifelse(src == "Champ travaill\u00e9", "tillage",
			ifelse(src == "Champ non travaill\u00e9", "none", NA_character_)),
		herbicide_used = ifelse(src == "D\u00e9sherbant", TRUE, NA),
		irrigated = irr != "pluvial",
		irrigation_method = ifelse(irr == "pluvial", "none", NA_character_),
		weed_cover = cover[match(enh, 1:9)],
		stress = "weeds"
	)
	d$geo_from_source <- FALSE
	d$geo_uncertainty <- 3000
	d$geo_source <- "GeoNames (Mauritius town centers)"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	## Species lookup from Données origine (rows 10..141: col 1 = EPPO, col 2 = Espèce, col 3 = Famille)
	i <- which(!is.na(r3[[1]]) & seq_len(nrow(r3)) >= 10)
	spp <- data.frame(
		code = trimws(r3[[1]][i]),
		weed_species = trimws(r3[[2]][i]),
		weed_family_ = trimws(r3[[3]][i])
	)
	# Align 5 genus-level codes in Floristique with *SS codes in Données origine
	alias <- c(ABUS4 = "ABUSS", MUBS1 = "MUBSS", PYJS1 = "PYJSS", RUBS1 = "RUBSS", SEBS4 = "SEBSS")

	## Species-level (long) records
	score <- as.matrix(r2[, -1])
	j <- which(!is.na(score) & score > 0, arr.ind = TRUE)
	codes <- trimws(as.character(r2[[1]][j[, "row"]]))
	mapped_codes <- ifelse(codes %in% names(alias), unname(alias[codes]), codes)
	sidx <- match(mapped_codes, spp$code)

	long <- data.frame(
		trial_id = colnames(score)[j[, "col"]],
		weed_species = spp$weed_species[sidx],
		weed_family_ = spp$weed_family_[sidx],
		weed_species_cover = cover[match(as.integer(score[j]), 1:9)]
	)

	carobiner::write_files(path, meta, d, long = long)
}
