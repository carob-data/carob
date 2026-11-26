# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm potato yields of KIRUNDO and KINIGI at three sectors in Northern Rwanda (Gataraga, Ntaruka, Nyange) across two seasons. Each treatment was replicated on four farmers' fields per sector (Farmer1..Farmer12).
# KIRUNDO and KINIGI are fresh tuber yields in t/ha, reshaped to one row per variety and converted to kg/ha (* 1000).
# Season 1 is mapped to "first" and season 2 to "second".
# Dictionary defines: CTRL = no fungicide, no insecticide, clean seed; F = fungicide only, clean seed; I = insecticide only, clean seed; F+I = fungicide + insecticide, clean seed; FF+I = fungicide + insecticide, farmer seed (coded as "Farmer" in the data table).
# Sector administrative units (Northern Province; Musanze and Burera districts) and approximate sector-center coordinates are estimated with geo_from_source = FALSE and geo_uncertainty = 5000 m. Altitude (m) was measured by GPS in the raw file.

## ISSUES
# No planting date, harvest date, or calendar year in the raw data or metadata.
# No fertilizer rate, irrigation statement, or yield moisture reported.
# Many rows have NA yield (e.g., plots not harvested or failed); retained as recorded.
# The dictionary states four treatments and lists five codes, labeling farmer practice as FF+I while the data table uses "Farmer".
# write_files: missing carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date; NA in yield.

## NOT PROCESSED
# Data_dictionary.xls: codebook used to interpret treatments and units.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0
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

	# Estimated sector centers in Northern Province, Rwanda
	geo <- data.frame(
		location = c("Gataraga", "Ntaruka", "Nyange"),
		adm1 = "Northern",
		adm2 = c("Musanze", "Burera", "Musanze"),
		latitude = c(-1.483, -1.460, -1.500),
		longitude = c(29.556, 29.740, 29.660)
	)
	loc <- carobiner::fix_name(r$Location, "title")
	g <- match(loc, geo$location)
	seas <- unname(c("1" = "first", "2" = "second")[as.character(r$Season)])
	farmer <- trimws(as.character(r[["Farmers name"]]))

	one <- function(variety, yield) {
		data.frame(
			trial_id = paste(loc, seas, sep="_"),
			plot_id = paste(farmer, seas, trimws(as.character(r$Treatment)), variety, sep="_"),
			country = "Rwanda",
			adm1 = geo$adm1[g],
			adm2 = geo$adm2[g],
			location = loc,
			latitude = geo$latitude[g],
			longitude = geo$longitude[g],
			geo_from_source = FALSE,
			geo_uncertainty = 5000,
			elevation = as.numeric(r[["Altitude (m)"]]),
			season = seas,
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
			on_farm = TRUE,
			is_survey = FALSE
		)
	}

	d <- rbind(
		one("KIRUNDO", r[["KIRUNDO (t/ha)"]]),
		one("KINIGI", r[["KINIGI (t/ha)"]])
	)
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
