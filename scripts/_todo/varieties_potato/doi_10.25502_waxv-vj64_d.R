# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm potato tricot, Rwanda, first season (package codes 1-150). Registration coordinates are in Farm_Geolocation as "latitude longitude elevation precision".
# The spread of coordinates (about 1.50-2.45 S, 29.39-30.02 E) is across the potato districts, so they were kept as farm GPS (geo_from_source TRUE).
# Varieties are blinded as option A, B and C. The package-to-variety key is not in this download. Each package is expanded to three rows.
# Questions such as "which variety gives the best yield" name an option. They are not kilogram yields. best_yield_ is TRUE for the option the farmer picked.
# Farmer name and the separate GPS_Point_Longitude columns are empty. The first form date is a submission date (16/09/2020 is day/month/year), not a verified planting date.
# Country is Rwanda from the dictionary coverage field. Creator metadata is IITA and CIP.

## ISSUES
# No variety names and no weighed yield. A randomization list is needed before variety can be filled.
# Bacterial-wilt and preference questions that are not the best/worst yield or overall choice were not reshaped (see NOT PROCESSED).
# N/P/K, irrigation, planting date and harvest date are not in the file.
# write_files: missing yield (and N/P/K, irrigated, planting_date, harvest_date). Variety values are A/B/C.

## NOT PROCESSED
# Telephone, consent, device IMEI, submission UUID, enumerator fields.
# Ranking questions other than best/worst growing, best/worst yield, and best/worst overall (storage, dormancy, taste, tuber size, bacterial wilt, marketability). They name an option, not a measured rate.
# GPS_Point_Longitude / Latitude on the last form: empty or "None". Farm_Geolocation was used.
# data dictionary.

## NEW VARIABLES
# package_id_: tricot package code.
# best_growing_, worst_growing_, best_yield_, worst_yield_, best_overall_, worst_overall_: TRUE when that blinded option was the one named in the matching question.


carob_script <- function(path) {

"
Results from the first season of analysis for 13 potato varieties

A challenge observed is the nature of RTB crops which demand more strategic logistics as the ‘seed’ of RTB crops are not grains but cuttings, suckers or vines. These are bulkier and can be stored for a limited amount of time. Because of this also less pronounced seed systems exist for RTB crops, which can also be considered an advantage to use tricot as an extra important variety dissemination strategy when compared to the approach within grain crops. Participatory variety evaluation efforts have sometimes failed in the past because of their failure to work within established research and extension systems. A main challenge for the project will be to forge lasting collaborations between research and extension around the tricot approach, and to embed the tricot approach in work routines. This means that to the extension system we need to demonstrate that tricot facilitates their work and provides them with tools to generate recommendation domains. It also means we need to demonstrate to research institutes the scientific value of the results (data quality, representation of target population environments, validity of the results).
"

	uri <- "doi:10.25502/waxv-vj64/d"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; CIP",
		publication = NA,
		project = NA,
		design = "tricot",
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "best_yield_",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.7,
		notes = NA
	)

	f <- ff[basename(ff) == "report_data_pot21a-4.csv"]
	r <- read.csv(f, na.strings=c("", "NA", "None"), check.names=TRUE)
	gps <- strsplit(trimws(r$Farm_Geolocation), "\\s+")
	lat <- as.numeric(vapply(gps, function(z) z[1], ""))
	lon <- as.numeric(vapply(gps, function(z) z[2], ""))
	elev <- as.numeric(vapply(gps, function(z) z[3], ""))

	opt <- function(x) {
		x <- trimws(x)
		ifelse(grepl("A", x), "A", ifelse(grepl("B", x), "B", ifelse(grepl("C", x), "C", NA)))
	}
	# "Option A" / "Option_C" -> A/B/C. Combined answers like "Variety A - Variety B" are not a single winner.
	one <- function(x) {
		x <- gsub("_", " ", trimws(x))
		hit <- grepl("^Option [ABC]$", x)
		out <- rep(NA_character_, length(x))
		out[hit] <- substr(x[hit], 8, 8)
		out
	}

	base <- data.frame(
		package_id_ = as.character(r$Package_code),
		latitude = lat,
		longitude = lon,
		elevation = elev,
		best_g = one(r$Which_is_the_best_growing_variety),
		worst_g = one(r$Which_is_the_worst_growing_variety),
		best_y = one(r$Which_variety_gives_the_best_yield),
		worst_y = one(r$Which_variety_gives_the_worst_yield),
		best_o = one(r$Which_variety_did_you_like_best_overall),
		worst_o = one(r$Which_variety_did_you_like_least_overall)
	)

	d <- do.call(rbind, lapply(c("A", "B", "C"), function(v) {
		data.frame(
			package_id_ = base$package_id_,
			variety = v,
			treatment = v,
			latitude = base$latitude,
			longitude = base$longitude,
			elevation = base$elevation,
			best_growing_ = base$best_g == v,
			worst_growing_ = base$worst_g == v,
			best_yield_ = base$best_y == v,
			worst_yield_ = base$worst_y == v,
			best_overall_ = base$best_o == v,
			worst_overall_ = base$worst_o == v
		)
	}))
	d$country <- "Rwanda"
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$trial_id <- paste0("POT21A_", d$package_id_)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE

	carobiner::write_files(path, meta, d)
}
