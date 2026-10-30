# R script for "carob"

## NOTES
# Organic rainfed trial at Villa Masini (Ravenna, Emilia-Romagna). Fifteen species-year means (not plot-level).
# Yield is t/ha; converted to kg/ha. No synthetic fertilizer: N/P/K inorganic = 0. Green manure used (amount not in file).
# Coordinates converted from 44°15'59.5"N, 12°07'46.5"E in the associated paper.

## ISSUES
# Records are treatment means over 3 RCBD replicates, not plot data.
# Sowing and harvest dates are in paper Table 2, not in the spreadsheet; planting_date is year only.
# Green-manure amount/type not in the file.
# Weed/pest scores and panicle traits not fully mapped (weed_cover and plant_height are).
# Millet typed as proso millet from the paper (Panicum miliaceum).
# write_files: harvest_date and yield_moisture not in file; UNIBO may not be in org vocab.

carob_script <- function(path) {

"
Field trial data set of millet, sorghum and maize

The present study examined for the first time the environmental and economic impacts, as well as agronomic performances, of rainfed cultivated proso millet, sorghum and maize over a three-year period under challenging climatic conditions in Emilia-Romagna region, Italy.
"

	uri <- "doi:10.5281/zenodo.14732549"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
		data_organization = "UNIBO",
		publication = "doi:10.1016/j.fcr.2024.109563",
		project = NA,
		design = "RCBD; 3 replicates (means only in file)",
		data_type = "experiment",
		treatment_vars = "crop",
		response_vars = "yield;plant_height",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 65,
		carob_effort = 0.6
	)

	f <- ff[grepl("MILLET", basename(ff), ignore.case=TRUE)]
	r <- carobiner::read.excel(f)

	# Yield column name has two spaces before (
	yc <- grep("^Yield", names(r), value=TRUE)

	crop <- ifelse(grepl("Millet", r$Species), "proso millet",
		ifelse(grepl("Sorghum", r$Species), "sorghum", "maize"))
	variety_type <- ifelse(grepl("hybrid", r$Species), "hybrid",
		ifelse(grepl("population", r$Species), "landrace",
		ifelse(grepl("variety", r$Species), "variety", NA)))

	d <- data.frame(
		trial_id = paste0("VillaMasini_", r$Year),
		crop = crop,
		variety_type = variety_type,
		treatment = r$Species,
		plant_height = r$`Plant Height (cm)`,
		weed_cover = r$`Weed Incidence (%)`,
		# yield t/ha to kg/ha
		yield = r[[yc]] * 1000,
		planting_date = as.character(r$Year),
		country = "Italy",
		adm1 = "Emilia-Romagna",
		location = "Villa Masini",
		latitude = 44 + 15/60 + 59.5/3600,
		longitude = 12 + 7/60 + 46.5/3600,
		geo_from_source = TRUE,
		N_fertilizer = 0,
		P_fertilizer = 0,
		K_fertilizer = 0,
		OM_used = TRUE,
		irrigated = FALSE,
		on_farm = TRUE,
		is_survey = FALSE,
		yield_part = "grain"
	)

	carobiner::write_files(path, meta, d)
}
