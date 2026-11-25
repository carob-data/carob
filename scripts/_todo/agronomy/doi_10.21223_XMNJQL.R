# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm potato validation trials for AKILIMO fertilizer recommendations in Rwanda (country code RW; 638 plots across 109 fields identified by KEY1).
# Coordinates (Latitude, Longitude) and Altitude (m) are plot GPS points recorded in the file.
# tuberY matches tubersFW / (plotLength * plotWidth) * 10 exactly, confirming tuberY is fresh tuber yield in t/ha -> yield = tuberY * 1000 (kg/ha).
# Harvest evaluation date comes from ODK column `today` (dd-Mon-yy, 2020-12-16 to 2021-07-18).
# Rwanda AKILIMO potato validation treatments denote 50-kg bags/ha of standard Rwanda fertilizers (NPK 17-17-17, DAP 18-46-0, Urea 46-0-0, MOP/KCl 0-0-60):
#   NPK6       = 300 kg/ha NPK 17-17-17 (blanket recommendation) -> N=51, P=22.27, K=42.32 kg/ha
#   NPK11      = 550 kg/ha NPK 17-17-17 -> N=93.5, P=40.83, K=77.59 kg/ha
#   NPK4_MOP2  = 200 kg/ha NPK 17-17-17 + 100 kg/ha KCl (60% K2O) -> N=34, P=14.85, K=78.00 kg/ha
#   NPK4_DAP2  = 200 kg/ha NPK 17-17-17 + 100 kg/ha DAP (18-46-0) -> N=52, P=34.94, K=28.21 kg/ha
#   NPK4_UREA2 = 200 kg/ha NPK 17-17-17 + 100 kg/ha urea (46-0-0) -> N=80, P=14.85, K=28.21 kg/ha
#   Elemental conversions: P = P2O5 / 2.29; K = K2O / 1.2051.

## ISSUES
# Two rows have very high tuberY (>100 t/ha, up to 249 t/ha); they match tubersFW / plot area exactly and are kept as recorded.
# Site1 and Site2 are generic labels rather than place names; exact GPS coordinates are provided.
# No planting date, irrigation statement, variety name, or yield moisture reported.
# write_files: missing carob_contributor, yield_moisture, irrigated, planting_date; out of bounds / high crop yield (up to 249132 kg/ha).

## NOT PROCESSED
# 01_Scaling_AKILIMO_yield_data.csv: score_vigor, score_weeds, tubersSmallFW, tubersDiseasedFW, tubersMarketableFW, tubersSmallNr, tubersDiseasedNr, tubersMarketableNr are 100% empty (0 non-NA rows). PD_count and Pdtubers_count are 0 in all rows. tubersFWss is subsample fresh weight without matching dry weight.
# ODK administrative/device columns (uuid, deviceid, start, end, SubmissionDate, instanceID, username, ENID, lookup, barcode).
# 02_Dictionary_Scaling_AKILIMO_yield_data.csv: codebook.


carob_script <- function(path) {

"
Dataset for: Validation trials to validate AKILIMO recommendations in Rwanda

A set of 260 validation trials was conducted from the season 2021B onwards to compare AKILIMO recommendations with the blanket fertilizer recommendations in Rwanda
"

	uri <- "doi:10.21223/XMNJQL"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = "AKILIMO",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;fertilizer_type",
		response_vars = "yield;tuber_density",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "01_Scaling_AKILIMO_yield_data.csv"]
	r <- read.csv(f, check.names=FALSE)
	country <- trimws(r$country)
	if (any(country != "RW")) stop("unexpected country code")

	# Rwanda AKILIMO 50 kg bag/ha protocol: NPK 17-17-17, DAP 18-46-0, Urea 46-0-0, MOP/KCl 0-0-60
	fert_tab <- data.frame(
		treat = c("NPK6", "NPK11", "NPK4_MOP2", "NPK4_DAP2", "NPK4_UREA2"),
		N = c(300*0.17, 550*0.17, 200*0.17, 200*0.17 + 100*0.18, 200*0.17 + 100*0.46),
		P = c(300*0.17, 550*0.17, 200*0.17, 200*0.17 + 100*0.46, 200*0.17) / 2.29,
		K = c(300*0.17, 550*0.17, 200*0.17 + 100*0.60, 200*0.17, 200*0.17) / 1.2051,
		amount = c(300, 550, 300, 300, 300),
		ftype = c("NPK", "NPK", "NPK;KCl", "NPK;DAP", "NPK;urea")
	)
	tr <- trimws(as.character(r$treat))
	fi <- match(tr, fert_tab$treat)

	# Parse ODK harvest evaluation date (dd-Mon-yy, English month abbreviations)
	lct <- Sys.getlocale("LC_TIME")
	Sys.setlocale("LC_TIME", "C")
	hdate <- as.character(as.Date(trimws(as.character(r$today)), "%d-%b-%y"))
	Sys.setlocale("LC_TIME", lct)

	area_m2 <- as.numeric(r$plotLength) * as.numeric(r$plotWidth)
	d <- data.frame(
		crop = "potato",
		country = "Rwanda",
		location = trimws(as.character(r$site)),
		latitude = as.numeric(r$Latitude),
		longitude = as.numeric(r$Longitude),
		elevation = as.numeric(r$Altitude),
		geo_from_source = TRUE,
		harvest_date = hdate,
		treatment = tr,
		fertilizer_used = TRUE,
		fertilizer_type = fert_tab$ftype[fi],
		fertilizer_amount = fert_tab$amount[fi],
		N_fertilizer = fert_tab$N[fi],
		P_fertilizer = fert_tab$P[fi],
		K_fertilizer = fert_tab$K[fi],
		trial_id = trimws(as.character(r$KEY1)),
		plot_id = as.character(r$numeration),
		plot_area = area_m2,
		plot_length = as.numeric(r$plotLength),
		plot_width = as.numeric(r$plotWidth),
		plant_density = as.numeric(r$nrPlants) / area_m2 * 10000,
		tuber_density = as.numeric(r$tubersNr) / area_m2 * 10000,
		yield = as.numeric(r$tuberY) * 1000,
		yield_part = "tubers",
		yield_isfresh = TRUE,
		on_farm = TRUE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
