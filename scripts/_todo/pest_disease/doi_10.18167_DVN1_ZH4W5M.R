# R script for "carob"
# license: GPL (>=3)

## NOTES
# 240 plot-level weed surveys (Dimbokro-Bongouanou, 2011). One row per species
# observed (rating > 0). Floristique is already on the 1-9 scale; converted to
# % cover. Latin names from Données d'origine (EPPO code + species).

## ISSUES
# Abstract says 140 surveys / 389 species; Facteurs has 240 plots. Crop is
# only "vivrier" (food crop) per plot; cassava/maize/okra not distinguished.
# Coords estimated for the Dimbokro-Bongouanou area. write_files: missing
# yield, yield_moisture, yield_part, N/P/K_fertilizer, planting_date,
# harvest_date (survey in pest_disease group). Crop is NA (only "vivrier").


carob_script <- function(path) {

"
Weed surveys in food crops in Center-East Côte d'Ivoire (2011-2014)

Inventory of weeds of food crops (cassava, maize, okra) in the Center-East of Côte d'Ivoire from 2011 to 2014. Rating in abundance index 1-5 transformed into scale 1-9. This dataset covers 140 plant surveys for 389 weed species.
"

	uri <- "doi:10.18167/DVN1/ZH4W5M"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "Amatrop",
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "CDI-AWA-2011-VIV-AD.xlsx"]
	r1b <- carobiner::read.excel(f1, sheet="Facteurs")
	r1c <- carobiner::read.excel(f1, sheet="Floristique")
	r1d <- carobiner::read.excel(f1, sheet="Données d'origine")

	get_row <- function(label) unlist(r1b[r1b[[1]] == label, -1])

	irrig <- tolower(get_row("Irrigation"))
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Côte d'Ivoire",
		location = "Dimbokro-Bongouanou",
		irrigated = irrig %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	# Midpoint of Dimbokro and Bongouanou. Author should refine.
	d1b$longitude <- -4.45
	d1b$latitude <- 6.65
	d1b$geo_uncertainty <- 40000
	d1b$geo_source <- "estimated from Dimbokro-Bongouanou"
	d1b$geo_from_source <- FALSE
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE

	header_row <- which(r1d[[2]] == "code EPPO")[1]
	species_lookup <- data.frame(
		code = r1d[[2]][(header_row + 2):nrow(r1d)],
		weed_species = r1d[[1]][(header_row + 2):nrow(r1d)]
	)
	species_lookup <- species_lookup[!is.na(species_lookup$code) & nzchar(trimws(as.character(species_lookup$code))), ]

	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)
	present <- which(!is.na(r1c[, -1]) & r1c[, -1] > 0, arr.ind=TRUE)
	d <- data.frame(
		trial_id = names(r1c)[-1][present[, "col"]],
		code = r1c[[1]][present[, "row"]],
		ground_cover = rating_to_cover[match(as.numeric(r1c[, -1][present]), 1:9)]
	)
	d <- merge(d, species_lookup, by="code", all.x=TRUE)
	# keep EPPO code when Latin name is missing so rows stay distinct
	miss <- is.na(d$weed_species) | !nzchar(trimws(as.character(d$weed_species)))
	d$weed_species[miss] <- d$code[miss]
	d$code <- NULL
	d$crop <- NA_character_
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
