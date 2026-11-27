# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Source columns Long (~4.11-4.42 N) and Lat (~9.05-9.29 E) are swapped throughout the CSV; swapped here so latitude and longitude match the Mount Cameroon area.
# 2. Starting at ID 373 (2017-02-09 through 2017-08-10), Altitude, Long, and Lat are misaligned/shifted across villages and traps (e.g., Kotto 2 Trap 4 has Woteva coordinates; 7 Kotto 1 rows have Trap = NA; 6 Woteva Trap 5 rows have blank coordinates and altitude). Source values are kept as recorded except for the 6 missing Woteva coordinates, which are filled using Woteva's 2015-2016 mean source coordinates.
# 3. From July 2015 to January 2017 (ID 86-367), Bomana Trap 2 has two rows per visit date with identical coordinates/altitude and different weevil counts (likely a mislabeled Trap 3).
# 4. Farmer name (Name) is entirely blank, so hhid is constructed from village and trap number (or row ID when Trap is missing). Improved plantain varieties (IITA/FHIA) and Cosmolure lure mentioned in the abstract are not recorded in the data.

## NEW
# cluster_: Mount Cameroon project cluster ("Buea" or "Bomboko"; not an administrative unit)
# pest_species: "Cosmopolites sordidus" (banana weevil; not yet in values_pest.csv)

## MISSING
# yield, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date

## NOTES
# Monthly adult banana weevil (Cosmopolites sordidus) counts per Cosmolure pheromone trap across 5 villages (Bwassa, Woteva, Bomana, Kotto 1, Kotto 2) around Mount Cameroon National Park, Sud-Ouest, Cameroon, Jan 2015 to Aug 2017 (467 records).
# Season "Rainy" is mapped to "wet". Six Woteva rows missing GPS coordinates are georeferenced with the village mean of Woteva's 2015-2016 trap coordinates (geo_from_source = FALSE).

## NOT PROCESSED
# banana-weevil-dynamic-_psmnr-project.csv: Name (farmer name, 100% NA).

carob_script <- function(path) {

"
Dynamic of banana weevil in plantain farms in South West region of Cameroon

The overall objective of the project was to transfer and promote improved plantain production technologies to enhance and profitability of plantain production in PSMNR target villages around the MCNP, thereby improving the livelihood of involved farmers. A planning. The experiments were set up to evaluate in participatory manner the performance of cosmolure trap against banana weevils in the demonstration plots planted with improved varieties of plantain from IITA and FHIA.
"

	uri <- "doi:10.25502/zp91-ca97/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Plantain Production and Capacity Building for PSMNR Target Villages around Mount Cameroon National Park",
		design = "Monthly pheromone-trap (Cosmolure) monitoring of adult banana weevils on participatory plantain demonstration plots",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "banana-weevil-dynamic-_psmnr-project.csv"]
	r <- read.csv(f, check.names = FALSE)
	names(r) <- trimws(names(r))

	season <- tolower(trimws(r$Season))
	season[season == "rainy"] <- "wet"

	loc <- carobiner::fix_name(trimws(r$Village), "title")
	clust <- carobiner::fix_name(trimws(r$Cluster), "title")

	# Source Long (~4.1-4.4) is latitude and source Lat (~9.1-9.3) is longitude
	lat <- as.numeric(r$Long)
	lon <- as.numeric(r$Lat)
	has_geo <- !is.na(lat) & !is.na(lon)

	# Fill 6 Woteva rows with missing coordinates using Woteva's 2015-2016 source mean
	miss_woteva <- !has_geo & loc == "Woteva"
	lat[miss_woteva] <- 4.22248
	lon[miss_woteva] <- 9.28430

	trap <- as.integer(r$Trap)
	hhid <- ifelse(is.na(trap), paste0(loc, "_row", r$ID), paste0(loc, "_trap", trap))

	d <- data.frame(
		country = "Cameroon",
		adm1 = "Sud-Ouest",
		adm2 = ifelse(clust == "Buea", "Fako", NA_character_),
		location = loc,
		cluster_ = clust,
		latitude = lat,
		longitude = lon,
		elevation = as.numeric(r$Altitude),
		geo_from_source = has_geo,
		geo_uncertainty = ifelse(miss_woteva, 2000, NA_real_),
		geo_source = ifelse(miss_woteva, "village mean of source trap coordinates", NA_character_),
		date = as.character(as.Date(trimws(r$Evaluation_Date), format = "%d/%m/%Y")),
		season = season,
		rep = trap,
		hhid = hhid,
		crop = "plantain",
		yield_part = "fruit",
		pest_species = "Cosmopolites sordidus",
		pest_incidence = as.integer(r$weevil_month),
		stress = "pests",
		trial_id = paste(loc, r$Year, sep = "_"),
		on_farm = TRUE,
		is_survey = TRUE
	)
	d$record_id <- seq_len(nrow(d))

	carobiner::write_files(path, meta, d)
}
