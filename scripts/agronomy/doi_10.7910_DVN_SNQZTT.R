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
# Station coordinates added for all 16 South Asian drought-breeding network stations (geo_from_source = FALSE).
# Soil sampling depths for pH, P, K, texture not given. P extraction method not given (soil_P_method not set);
#   "Exch-K ppm" mapped to soil_K (mg/kg).
# Water retention values: unit not given (volumetric fraction?); Faizabad (3, 5, 15 bar) and Nepalgunj (15 bar)
#   have values > 7, likely entry errors; kept as is.
# Not mapped: daily rainfall / tensiometer / water table series (60-100 DAS) and penetrometer depth profiles
#   (only the site-level summaries in the PCA sheet are used); Paramakudi appears only in the soil texture file.
# Remaining messages: missing variety, yield_moisture, coordinates, NPK, planting and harvest dates; new variables.

## NEW VARIABLES
# yield_reduction: % yield reduction by drought (same value on both records of a site-year)
# tensiom_: soil water potential (-kPa)
# water_table:  water table depth (cm)
# soil_bd_25_30_: bulk density at 25-30 cm (g/cm3); soil_bd is at 5-10 cm
# soil_SWP (soil water potential) at  10kPa, 300kPa, 500kPa and 1500kPa at 5-10 cm
# penetrom_max: maximum cone index (kPa); penetrom_max_depth: depth of maximum cone index (cm); penetrom_30cm_: cone index at 30 cm (kPa)



## NOT PROCESSED
# Variable Descriptions.txt: not read. columns: Year - Year of the experiment was conducted. Observation records are missing from d until this file is standardized.
# soil texture.csv ( because it's already included in all data for PCA w imputed.xlsx)

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
		treatment_vars = "stress",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-11-27",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 80,
		carob_effort = 2
	)

	
	f <- ff[basename(ff) == "all data for PCA w imputed.xlsx"]
	r <- carobiner::read.excel(f, sheet="all data for PCA")
	r <- r[!is.na(r$location), ]
	f1 <- ff[basename(ff) == "rainfall tensiom water table.csv"]
	r1 <- read.csv(f1)
	
	
	d <- data.frame(
	  planting_date = as.character(r$Year),
	  location = r$location,
	  yield_reduction_ = r[["% yield reduction"]],
	  #rain_60_100DAS_ = r[["Rainfall 60-100 DAS"]],
	  #tensiom_avg_60_100DAS_ = r[["ave tensiom 60-100"]],
	  #tensiom_max_60_100DAS_ = r[["max tensiom 60-100"]],
	  #water_table_60_100DAS_ = r[["aver water table 60-100 DAS"]],
	  soil_bd = r[["bd 5-10 cm"]],
	  #soil_bd_25_30_ = r[["bd 25-30 cm"]],
	  soil_pH = r$pH,
	  soil_P = r[["Avail-P ppm"]],
	  soil_K = r[["Exch-K ppm"]],
	  soil_clay = r[["% Clay"]],
	  soil_silt = r[["% Silt"]],
	  soil_sand = r[["% Sand"]],
	  penetrom_max = r[["max penetrom"]],
	  penetrom_depth = r[["depth of max penetrom"]],
	  #penetrom_30cm = r[["penetrom 30 cm"]],
	  depth_bottom = 10,
	  depth_top = 5
	)
	d$trial_id <- paste(d$location, d$planting_date, sep="_")
	
	dc <- data.frame(d, stress="none", treatment="well-watered", yield=r[["Ave yield control"]])
	ds <- data.frame(d, stress="drought", treatment="reproductive stage-drought stress", yield=r[["Ave yield stress"]])
	d <- rbind(dc, ds)
	d <- d[!is.na(d$yield), ]
	
	geo <- data.frame(
	  location = c("Barwale", "Sabour", "Cuttack", "IIRR", "Faizabad", "Hardinath", "Hazaribag", "Nepalgunj", 
	               "Patna", "Raipur", "Rajshahi", "Rewa", "Tarahara", "Coimbatore", "Tripura", "Varanasi"),
	  country = c("India", "India", "India", "India", "India", "Nepal", "India", "Nepal",
	              "India", "India", "Bangladesh", "India", "Nepal", "India", "India", "India"),
	  adm1 = c("Maharashtra", "Bihar", "Odisha", "Telangana", "Uttar Pradesh", NA, "Jharkhand", NA,
	           "Bihar", "Chhattisgarh", "Rajshahi", "Madhya Pradesh", NA, "Tamil Nadu", "Tripura", "Uttar Pradesh"),
	  latitude = c(19.841, 25.242, 20.454, 17.320, 26.775, 26.806, 24.010, 28.050,
	               25.595, 21.236, 24.375, 24.536, 26.701, 11.012, 23.907, 25.268),
	  longitude = c(75.886, 87.043, 85.933, 78.394, 82.138, 85.973, 85.353, 81.617,
	                85.090, 81.701, 88.613, 81.304, 87.277, 76.935, 91.310, 82.991)
	)
	d <- merge(d, geo, by="location", all.x=TRUE)
	
	###  Keep tension water table as long variable
	water_T <- data.frame(
	  trial_id = paste(r1$location, r1$Year, sep="_"),
	  DAP = r1$DAS,
	  rain = r1$Rainfall,
	  water_table = r1$Water.table,
	  Soil_SWP = r1$tensiom # soil water potential
	)
	
	water_T <- water_T[water_T$trial_id %in% intersect(d$trial_id,water_T$trial_id),]
	
	### Capture water retention (water holding capacity) as long variable
	water_ret <- data.frame(
	  trial_id = c(rep(paste(r$location, r$Year, sep="_"), 4)),
	  soil_WHC= c(r[["0.1 bar 5-10 cm"]], r[["3 bar 5-10 cm"]], r[["5 bar 5-10 cm"]], r[["15 bar 5-10 cm"]]),
	  soil_WHC_pressure = c(rep(10, nrow(r)), rep(300,nrow(r) ), rep(500, nrow(r)), rep(1500, nrow(r)))
	)
	
	dlong  <- carobiner::bindr(water_T, water_ret)
	
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$season <- "wet"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	
	### Extracted from publication (Materials and method)
	d$yield_moisture <- NA # Not provided 
	d$yield_isfresh <- NA
	d$irrigated <- TRUE
	d$N_fertilizer <- 90
	d$N_splits <- 3L
	d$P_fertilizer <- 60
	d$K_fertilizer <- 40
	d$harvest_date <- NA
	d$geo_from_source <- FALSE

carobiner::write_files(path, meta, d, long = dlong)
}
