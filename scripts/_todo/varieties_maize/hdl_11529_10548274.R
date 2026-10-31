# R script for "carob"
# license: GPL (>=3)

## NOTES
# Microwave popping tests on 873 popcorn accessions from the CIMMYT maize collection, compiled in 2014. This is a laboratory assay of genebank seed, not a field yield trial.
# Passport longitude for Mexico is unsigned in the file. It is stored as degrees west.
# The metadata production place is El Batan, Mexico. That is not stored as the row location, because the coordinates in the file are collection sites.
# Thousand-kernel weight is initial sample weight divided by the kernel count in that sample, times 1000.
# Time to first pop is an Excel time. It is stored as seconds from midnight.
# Flake shape and pericarp retention are 1 to 5 scores. The scale is in the phenotyping manual, which is a PDF in the dataset.

## ISSUES
# write_files: missing variables: yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# write_files: unknown variable: time_to_first_pop_s_ (time?)
# write_files: unknown variables: kernels_in_sample_, expansion_volume_ml_, expandability_, unpopped_kernels_, unpopped_kernel_weight_g_, unpopped_percent_, flake_shape_, pericarp_retention_, primary_race_
# write_files: coordinates not on land: Mexico
# write_files: duplicate coordinates: fewer coordinates than locations
# write_files: NA detected: latitude, longitude
# Some Mexico passport coordinates are still not on land after storing unsigned longitude as west. They are kept as published.

## NEW VARIABLES
# expansion_volume_ml_: volume of the popped sample, ml. The description says the sample is 30 g.
# expandability_: expansion volume divided by the initial grain weight.
# unpopped_kernels_: kernels that did not pop.
# unpopped_kernel_weight_g_: weight of kernels that did not pop, g.
# unpopped_percent_: percent of kernels that did not pop.
# kernels_in_sample_: kernels in the initial sample.
# time_to_first_pop_s_: seconds from the start of microwaving to the first pop.
# flake_shape_: flake shape score, 1 to 5.
# pericarp_retention_: pericarp retained after popping, score 1 to 5.
# primary_race_: primary race name from the passport record.

## NOT PROCESSED
# Final sample weight, flake color, accession identifiers other than the accession number, collection date, donor numbers, and grain-color and grain-type lists.
# The phenotyping video.


carob_script <- function(path) {

"
The Global Popcorn Project

The Global Popcorn Project began in 2014, by asking how many popcorn accessions are in the CIMMYT maize collection. This file is the 2014 popping-trait dataset for 873 accessions, measured with the method in the phenotyping guides.
"

	uri <- "hdl:11529/10548274"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Global Popcorn Project",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "seed_weight",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "PopcornDataset.873accs.2014.xlsx"]
	r <- carobiner::read.excel(f, sheet="873 accessions PopCorn2014")

	seconds <- function(x) {
		if (inherits(x, "POSIXt")) {
			h <- as.numeric(format(x, "%H"))
			m <- as.numeric(format(x, "%M"))
			s <- as.numeric(format(x, "%S"))
			return(h * 3600 + m * 60 + s)
		}
		as.numeric(x)
	}
	num <- function(x) as.numeric(as.character(x))
	wt <- num(r[["Initial Weight (grams)"]])
	nk <- num(r[["Kernel number in 30 grams"]])
	lat <- num(r$Latitude)
	lon <- num(r$Longitude)
	country <- trimws(as.character(r$Country))
	country[country == ""] <- NA_character_
	sw <- wt / nk * 1000
	sw[!is.finite(sw)] <- NA_real_

	d <- data.frame(
		trial_id = "popcorn 2014",
		crop = "maize",
		variety = trimws(as.character(r[["Accession Number"]])),
		treatment = trimws(as.character(r[["Accession Number"]])),
		country = country,
		location = trimws(as.character(r$Province_Site)),
		latitude = lat,
		longitude = lon,
		elevation = num(r$Elevation),
		geo_from_source = !is.na(lat) & !is.na(lon),
		seed_weight = sw,
		kernels_in_sample_ = nk,
		expansion_volume_ml_ = num(r[["Expansion Volume (milliliters)"]]),
		expandability_ = num(r$Expandability),
		unpopped_kernels_ = num(r[["Number of unpopped kernels"]]),
		unpopped_kernel_weight_g_ = num(r[["Weight of unpopped kernels (grams)"]]),
		unpopped_percent_ = num(r[["Percent unpopped kernels"]]),
		time_to_first_pop_s_ = seconds(r[["Time to first pop (seconds)"]]),
		flake_shape_ = num(r[["Flake shape"]]),
		pericarp_retention_ = num(r[["Pericarp retention"]]),
		primary_race_ = trimws(as.character(r[["Primary Race"]])),
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$location <- carobiner::fix_name(d$location, "title")
	d$location[d$location == ""] <- NA_character_
	mex <- !is.na(d$country) & d$country == "Mexico" & !is.na(d$longitude) & d$longitude > 0
	d$longitude[mex] <- -d$longitude[mex]
	d$primary_race_[d$primary_race_ == ""] <- NA_character_
	d$variety_type <- "popcorn"

	carobiner::write_files(path, meta, d)
}
