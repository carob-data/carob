# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Coordinates are from the GeoNames links in 4296_Localities.xlsx. For UoN, GeoNames 6533921 points to the University of Nairobi main campus; trials may have been at the Kabete field station (~9 km west), covered by geo_uncertainty = 10000 m.
# Only marketable tuber yield (MTYA) is reported in the source; it is stored in both yield_marketable and yield.
# UoN has two 2011 trials coded YEAR = 2011a (SEASON = LR) and YEAR = 2011b (SEASON = LR2); both are assigned planting_date = "2011" and season = "long rains", and kept separate in trial_id.

## NEW
# fungicide_product: phosphonate (not in values_fungicides.csv; generic phosphonate / potassium phosphonate fungicide).

## MISSING
# harvest_date, irrigated, on_farm, yield_moisture, N_fertilizer, P_fertilizer, K_fertilizer.

## NOTES
# Multi-site potato late blight (Phytophthora infestans) management trials in Kenya (Koibatek, Limuru, Njabini, University of Nairobi) across 2010-2014 comparing 10 varieties and 3 fungicide regimes (Control, Metalaxyl + Mancozeb, Phosphonate).
# Raw AUDPC values (0-4645, dictionary upper limit 6000; rAUDPC = AUDPC / ((last - first) * 100)) are in percent-days and are divided by 100 to fraction-days.
# RLB is a fixed variety-level resistance class (Resistant, Moderately resistant, Susceptible, Highly Susceptible) rather than a plot-level severity score, and is mapped to variety_traits.

## NOT PROCESSED
# 4296_Data.xlsx (Data sheet): Disease_Pressure, Pressure (site-year pressure categories), Disease_reduction, Relative_to_Asante, Yield_increase_relative_to_control (derived comparisons).
# 4296_Data.xlsx (Calculate_Efficacy and results sheets): derived treatment efficacy and susceptibility summaries.


carob_script <- function(path) {

"
Dataset for: Integrated management of late blight on potato

Integrated management of late blight on potato
"

	uri <- "doi:10.21223/6RV436"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;fungicide_product",
		response_vars = "yield;yield_marketable;AUDPC;rAUDPC",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "4296_Data.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")

	# Coordinates from GeoNames links in 4296_Localities.xlsx; Kenyan counties mapped to GADM adm1
	sites <- data.frame(
		SITE = c("UoN", "Koibatek", "Njabini", "Limuru"),
		adm1 = c("Nairobi", "Baringo", "Nyandarua", "Kiambu"),
		adm2 = c(NA_character_, "Koibatek", NA_character_, "Limuru"),
		location = c("University of Nairobi", "Koibatek", "Njabini", "Limuru"),
		longitude = c(36.8225, 35.8600, 36.6667, 36.6421),
		latitude = c(-1.2794, 0.1300, -0.7167, -1.1136),
		geo_uncertainty = c(10000, 20000, 5000, 5000),
		geo_source = c(
			"GeoNames 6533921 (from 4296_Localities.xlsx)",
			"GeoNames 7909894 (from 4296_Localities.xlsx)",
			"GeoNames 183371 (from 4296_Localities.xlsx)",
			"GeoNames 189386 (from 4296_Localities.xlsx)"
		)
	)
	idx <- match(r$SITE, sites$SITE)
	if (anyNA(idx)) stop("unmapped site: ", paste(unique(r$SITE[is.na(idx)]), collapse=", "))

	fung <- trimws(as.character(r$TREATMENT))
	fmap <- c(
		"Control" = "none",
		"Metalaxyl + Mancozeb" = "metalaxyl;mancozeb",
		"Phosphonate" = "phosphonate"
	)
	fung_product <- unname(fmap[fung])
	if (anyNA(fung_product)) stop("unmapped fungicide treatment")

	year <- trimws(as.character(r$YEAR))
	season_code <- trimws(as.character(r$SEASON))

	d <- data.frame(
		country = "Kenya",
		adm1 = sites$adm1[idx],
		adm2 = sites$adm2[idx],
		location = sites$location[idx],
		longitude = sites$longitude[idx],
		latitude = sites$latitude[idx],
		geo_uncertainty = sites$geo_uncertainty[idx],
		geo_source = sites$geo_source[idx],
		geo_from_source = FALSE,
		trial_id = paste(r$SITE, year, season_code, sep="_"),
		planting_date = substr(year, 1, 4),
		season = "long rains",
		crop = "potato",
		variety = trimws(as.character(r$ACCENAME)),
		variety_code = trimws(as.character(r$ACCENUMB)),
		variety_traits = paste(tolower(trimws(as.character(r$RLB))), "to late blight"),
		treatment = fung,
		fungicide_product = fung_product,
		fungicide_used = fung != "Control",
		rep = as.integer(r$REP),
		# MTYA is marketable tuber yield adjusted (t/ha -> kg/ha)
		yield = as.numeric(r$MTYA) * 1000,
		yield_marketable = as.numeric(r$MTYA) * 1000,
		yield_part = "tubers",
		yield_isfresh = TRUE,
		# Raw AUDPC is in percent-days; divide by 100 to fraction-days
		AUDPC = as.numeric(r$AUDPC) / 100,
		rAUDPC = as.numeric(r$rAUDPC),
		disease = "potato late blight",
		pathogen = "Phytophthora infestans",
		stress = "disease",
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
