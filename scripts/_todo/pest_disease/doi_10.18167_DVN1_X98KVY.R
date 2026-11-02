# R script for "carob"
# license: GPL (>=3)

## NOTES
# 22 plot-level weed surveys (Zonmon, 2013). One row per species observed
# (rating > 0). Source 1-9 scale converted to % cover. Plot-level Enherbement
# is total weed cover (%).

## ISSUES
# Facteurs/Floristique have 22 plots (BEN-MAR-03..26, some numbers skipped);
# metadata says 26 surveys. Latin names not in a clean lookup; weed_species
# is the 5-letter code from Floristique (parcelles chem p100 has names in an
# irregular layout). Author should georeference Zonmon. write_files:
# missing yield, yield_moisture, yield_part, N/P/K_fertilizer, planting_date,
# harvest_date (survey in pest_disease group).


carob_script <- function(path) {

"
Weed surveys in food crops and irrigated rice in the Zonmon region, Benin (2013)

Characterization of weed communities of food crops (groundnut, maize, sweet potato, cassava, fallow land) and irrigated rice in the Zonmon region, Benin in 2013. Phytoecological study with abundance scale 1-9. This dataset covers 26 plant surveys for 86 weed species.
"

	uri <- "doi:10.18167/DVN1/X98KVY"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIRAD",
		publication = NA,
		project = "Amatrop",
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;weed_cover",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "BEN-MAR-2013-DIV-AD.xlsx"]
	r1b <- carobiner::read.excel(f1, sheet="Facteurs")
	r1c <- carobiner::read.excel(f1, sheet="Floristique")

	get_row <- function(label) unlist(r1b[r1b[[1]] == label, -1])

	crop_map <- c(
		arachide = "groundnut", patate = "sweetpotato", manioc = "cassava",
		mais = "maize", riz = "rice", friche = "none"
	)
	assoc_map <- c(
		"maïs" = "maize", mais = "maize", manioc = "cassava",
		arachide = "groundnut", gombo = "okra", patate = "sweetpotato",
		palmier = "oil palm"
	)

	assoc <- get_row("Cultures_associees")
	assoc[assoc %in% c("-", "NA")] <- NA
	intercrops <- sapply(assoc, function(x) {
		if (is.na(x) || !nzchar(trimws(x))) return(NA_character_)
		toks <- unlist(strsplit(tolower(x), "[[:space:]/]+"))
		toks <- assoc_map[toks]
		toks <- toks[!is.na(toks)]
		if (length(toks) == 0) return(NA_character_)
		paste(unique(toks), collapse=";")
	})

	irrig <- tolower(get_row("Irrigation"))
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		crop = unname(crop_map[tolower(get_row("Culture_origine"))]),
		intercrops = unname(intercrops),
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Benin",
		location = "Zonmon",
		weed_cover = as.numeric(get_row("Enherbement")),
		irrigated = irrig %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- !is.na(d1b$intercrops)
	# Zonmon village, Zagnanado commune (Zou). Author should confirm on Google Maps.
	d1b$longitude <- 2.35
	d1b$latitude <- 7.22
	d1b$geo_uncertainty <- 10000
	d1b$geo_source <- "estimated from Zonmon, Benin"
	d1b$geo_from_source <- FALSE
	d1b$adm1 <- "Zou"
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE

	# rating 1-9 -> % cover (from BEN-MAR-2013-DIV-AD-DOC.txt)
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)
	present <- which(!is.na(r1c[, -1]) & r1c[, -1] > 0, arr.ind=TRUE)
	d <- data.frame(
		trial_id = names(r1c)[-1][present[, "col"]],
		weed_species = r1c[[1]][present[, "row"]],
		ground_cover = rating_to_cover[match(as.numeric(r1c[, -1][present]), 1:9)]
	)
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
