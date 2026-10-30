# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm sorghum varietal demonstrations under ATASP-1 in the same three Nigerian SCPZs as doi:10.21421/D2/ORJQWZ.
# The source column "Treatment" is the variety (not a fertilizer treatment).
# Yield is already kg/ha (GHvYld_C_kgha). A few rows are 0; left as recorded.

## ISSUES
# get_data() can time out on dataverse.icrisat.org over IPv6; files were retrieved with IPv4 (curl -4).
# Fertilizer, planting/harvest dates, and plot size are not in the table.
# Communities are not georeferenced. Human should georeference; LGA names G/Mallam and M/Madori need expanding.
# Variety spelling variants were merged (ICSV111/ICSV 111, ICSV400/ICSV 400, samsorg 44/Samsorg 44, local/Local).
# Description is a .docx; not parsed here.

carob_script <- function(path) {

"
Enhancing farmer's access to technology  for varietal demonstration trial ATASP-1 increased Sorghum productivity in the selected staple crop processing zones

The Agricultural Transformation Agenda Support Program Phase 1 (AT ASP-1) of the Federal Government of Nigeria was launched m 2015 as a follow up to the previous. Agricultural  Transformation Agenda (ATA). It is expected to create in 120.000jobs along the value chain of priority commodities and add additional 20 million metric tons of food. Project activities included thematic training, on-farm technology  demonstrations, community seed production and formation of Innovation Platforms for market linkages. The project has made remarkable progress in enhancing access to quality seeds and other inputs to over 34.300 farmers while expanding knowledge of best-bet productron technologies in over 100 communities across three staple crop processing zones (SCPZ).
"

	uri <- "doi:10.21421/D2/SJQNAI"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = "ATASP-1",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Data file of ICRISAT on Varietal demonstration trial ATASP-1.xlsx"]
	r <- carobiner::read.excel(f)

	var <- trimws(r$Treatment)
	var[var %in% c("ICSV111", "ICSV 111")] <- "ICSV 111"
	var[var %in% c("ICSV400", "ICSV 400")] <- "ICSV 400"
	var[tolower(var) == "samsorg 44"] <- "Samsorg 44"
	var[tolower(var) == "local"] <- "local"

	d <- data.frame(
		country = "Nigeria",
		adm1 = carobiner::fix_name(r$State, "title"),
		adm2 = carobiner::fix_name(r$LGA, "title"),
		location = r$Community,
		variety = var,
		treatment = var,
		yield = as.numeric(r$GHvYld_C_kgha),
		crop = "sorghum",
		yield_part = "grain",
		planting_date = "2016",
		on_farm = TRUE,
		is_survey = FALSE
	)

	d$trial_id <- paste(d$adm1, d$adm2, d$location, sep = "_")
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
