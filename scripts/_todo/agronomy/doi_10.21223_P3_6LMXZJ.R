# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm potato yields at three sectors in Northern Rwanda. Each treatment was replicated four times. Season is 1 or 2. The year is not in the file.
# KIRUNDO and KINIGI are fresh tuber yield in t/ha, stored as one row per variety. Yield is t/ha * 1000.
# Season 1 is stored as first and season 2 as second.
# The dictionary defines CTRL as no fungicide and no insecticide with clean seed, F as fungicide only with clean seed, I as insecticide only with clean seed, and F+I as both with clean seed.
# The data value Farmer is not in that list. The dictionary's remaining definition is FF+I, fungicide and insecticide with farmer seed, so Farmer is read that way.
# Altitude was measured with a GPS. The file has no coordinates.

## ISSUES
# No coordinates. The three sectors (Gataraga, Ntaruka, Nyange) still need a manual georeference.
# No planting date and no year.
# No fertilizer rate and no irrigation statement.
# No yield moisture.
# The dictionary says four treatments and then defines five, including FF+I, which is not a value in the table.
# write_files: missing yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. NA in yield.

## NOT PROCESSED
# Farmers name.
# Data_dictionary.xls: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: On-farm potato tuber yields along an altitude gradient in Northern Rwanda

On-farm potato tuber yields of KIRUNDO and KINIGI along an altitude gradient in Northern Rwanda. Treatments are combinations of fungicide, insecticide, and seed source, replicated four times at each sector.
"

	uri <- "doi:10.21223/P3/6LMXZJ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;fungicide_used;insecticide_used;seed_source",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Data_onfarm_yield_Rwanda.xls"]
	r <- carobiner::read.excel(f, na=".")

	# Dictionary order: CTRL, F, I, F+I, and Farmer read as the FF+I definition.
	code <- c("CTRL", "F", "I", "F+I", "Farmer")
	leg <- data.frame(
		treatment = code,
		fungicide_used = c(FALSE, TRUE, FALSE, TRUE, TRUE),
		insecticide_used = c(FALSE, FALSE, TRUE, TRUE, TRUE),
		seed_source = c("clean", "clean", "clean", "clean", "farmer")
	)
	i <- match(trimws(as.character(r$Treatment)), leg$treatment)

	one <- function(variety, yield) {
		data.frame(
			location = carobiner::fix_name(r$Location, "title"),
			season = c("1" = "first", "2" = "second")[as.character(r$Season)],
			elevation = as.numeric(r$"Altitude (m)"),
			rep = as.integer(r$Rep),
			treatment = trimws(as.character(r$Treatment)),
			fungicide_used = leg$fungicide_used[i],
			insecticide_used = leg$insecticide_used[i],
			seed_source = leg$seed_source[i],
			variety = variety,
			crop = "potato",
			yield = as.numeric(yield) * 1000,
			yield_part = "tubers",
			yield_isfresh = TRUE,
			country = "Rwanda",
			trial_id = paste(carobiner::fix_name(r$Location, "title"), c("1" = "first", "2" = "second")[as.character(r$Season)], sep="_"),
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	d <- rbind(
		one("KIRUNDO", r$"KIRUNDO (t/ha)"),
		one("KINIGI", r$"KINIGI (t/ha)")
	)
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
