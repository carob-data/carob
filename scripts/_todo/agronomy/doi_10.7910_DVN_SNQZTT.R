# R script for "carob"
# license: GPL (>=3)

## NOTES
# Site-level compilation of drought breeding network sites (Bangladesh, India, Nepal), 2012-2014 wet seasons:
#   average rice yield under well-watered (control) and reproductive-stage drought stress, with site soil properties.
# Each site-year gives two records (drought_stress "none" and "reproductive stage").
# Only the sheet "all data for PCA" is used; the "imputed" sheet contains imputed values and is ignored.
# Country/adm1 are assigned from the station names (admin names, no coordinates).

## ISSUES
# Yields are averages over the screening entries; varieties, number of entries/reps, planting dates,
#   fertilizer and yield moisture are not given. Yield unit assumed kg/ha (not stated).
# Coordinates not provided; georeference stations: Barwale (Jalna, Maharashtra), Sabour and Patna (Bihar),
#   Cuttack (Odisha), IIRR (Hyderabad, Telangana), Faizabad (Uttar Pradesh), Hazaribag (Jharkhand),
#   Raipur (Chhattisgarh), Rewa (Madhya Pradesh), Coimbatore (Tamil Nadu), Tripura (India), Varanasi (Uttar Pradesh);
#   Hardinath, Nepalgunj, Tarahara (Nepal); Rajshahi (Bangladesh).
# Soil sampling depths for pH, P, K, texture not given. P extraction method not given (soil_P_method not set);
#   "Exch-K ppm" mapped to soil_K (mg/kg).
# Water retention values: unit not given (volumetric fraction?); Faizabad (3, 5, 15 bar) and Nepalgunj (15 bar)
#   have values > 7, likely entry errors; kept as is.
# Not mapped: daily rainfall / tensiometer / water table series (60-100 DAS) and penetrometer depth profiles
#   (only the site-level summaries in the PCA sheet are used); Paramakudi appears only in the soil texture file.
# Remaining messages: missing variety, yield_moisture, coordinates, NPK, planting and harvest dates; new variables.

## NEW VARIABLES
# yield_reduction_: % yield reduction by drought (same value on both records of a site-year)
# rain_60_100DAS_: rainfall between 60 and 100 days after sowing (mm)
# tensiom_avg_60_100DAS_, tensiom_max_60_100DAS_: average and maximum soil water potential 60-100 DAS (-kPa)
# water_table_60_100DAS_: average water table depth 60-100 DAS (cm)
# soil_bd_25_30_: bulk density at 25-30 cm (g/cm3); soil_bd is at 5-10 cm
# water_ret_10kPa_, water_ret_300kPa_, water_ret_500kPa_, water_ret_1500kPa_: water retention at 5-10 cm
# penetrom_max_: maximum cone index (kPa); penetrom_max_depth_: depth of maximum cone index (cm); penetrom_30cm_: cone index at 30 cm (kPa)


carob_script <- function(path) {

"
Replication data for: Depth of soil compaction predominantly affects rice yield reduction by reproductive-stage drought at varietal screening sites in Bangladesh, India, and Nepal

Drought is the major constraint to rainfed rice productivity in South Asia, but few reports provide detailed characterization of the soil properties related to drought stress severity in the region. The aim of the study was to provide a compilation of drought breeding network sites and their respective levels of drought stress, and to relate soil parameters with yield reduction by drought. Methods This study characterized levels of drought stress and soil nutrient and physical properties at 18 geographically distributed research station sites.
"

	uri <- "doi:10.7910/DVN/SNQZTT"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "IRRI",
		publication = "doi:10.1007/s11104-017-3265-2",
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "drought_stress",
		response_vars = "yield", 
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 50,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "all data for PCA w imputed.xlsx"]
	r <- carobiner::read.excel(f, sheet="all data for PCA")
	r <- r[!is.na(r$location), ]

	d <- data.frame(
		planting_date = as.character(r$Year),
		location = r$location,
		yield_reduction_ = r[["% yield reduction"]],
		rain_60_100DAS_ = r[["Rainfall 60-100 DAS"]],
		tensiom_avg_60_100DAS_ = r[["ave tensiom 60-100"]],
		tensiom_max_60_100DAS_ = r[["max tensiom 60-100"]],
		water_table_60_100DAS_ = r[["aver water table 60-100 DAS"]],
		soil_bd = r[["bd 5-10 cm"]],
		soil_bd_25_30_ = r[["bd 25-30 cm"]],
		soil_pH = r$pH,
		soil_P = r[["Avail-P ppm"]],
		soil_K = r[["Exch-K ppm"]],
		soil_clay = r[["% Clay"]],
		soil_silt = r[["% Silt"]],
		soil_sand = r[["% Sand"]],
		water_ret_10kPa_ = r[["0.1 bar 5-10 cm"]],
		water_ret_300kPa_ = r[["3 bar 5-10 cm"]],
		water_ret_500kPa_ = r[["5 bar 5-10 cm"]],
		water_ret_1500kPa_ = r[["15 bar 5-10 cm"]],
		penetrom_max_ = r[["max penetrom"]],
		penetrom_max_depth_ = r[["depth of max penetrom"]],
		penetrom_30cm_ = r[["penetrom 30 cm"]]
	)
	d$trial_id <- paste(d$location, d$planting_date, sep="_")

	dc <- data.frame(d, drought_stress="none", treatment="well-watered", yield=r[["Ave yield control"]])
	ds <- data.frame(d, drought_stress="reproductive stage", treatment="drought stress", yield=r[["Ave yield stress"]])
	d <- rbind(dc, ds)
	d <- d[!is.na(d$yield), ]

	geo <- data.frame(
		location = c("Barwale", "Sabour", "Cuttack", "IIRR", "Faizabad", "Hardinath", "Hazaribag", "Nepalgunj", 
			"Patna", "Raipur", "Rajshahi", "Rewa", "Tarahara", "Coimbatore", "Tripura", "Varanasi"),
		country = c("India", "India", "India", "India", "India", "Nepal", "India", "Nepal",
			"India", "India", "Bangladesh", "India", "Nepal", "India", "India", "India"),
		adm1 = c("Maharashtra", "Bihar", "Odisha", "Telangana", "Uttar Pradesh", NA, "Jharkhand", NA,
			"Bihar", "Chhattisgarh", "Rajshahi", "Madhya Pradesh", NA, "Tamil Nadu", "Tripura", "Uttar Pradesh")
	)
	d <- merge(d, geo, by="location", all.x=TRUE)

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$season <- "wet"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$geo_from_source <- NA

	carobiner::write_files(path, meta, d)
}
