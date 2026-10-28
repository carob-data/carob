# R script for "carob"
# license: GPL (>=3)

# REJECTED
# Strict subset of doi:10.71682/10549435 (all 84 year-1 rows are identical).

## ISSUES
# Planting, harvest (day), and lime application dates are not in the files (only harvest year).
# Fertilizer (N/P/K) rates are not reported.
# Soil sample IDs and sampling dates are all missing.
# Yield moisture is not reported.

## NOTES
# On-station RCBD according to abstract; despite the filename suggesting otherwise. Two sites, three blocks, seven lime rates.
# lime is the onset rate (kg/ha), applied in year 1 only.

## NEW VARIABLES
# yield_outlier_: source flag for whether the yield value is an outlier (unlikely, likely, surely)


carob_script <- function(path) {

"
Field experiments on crop response to lime rates in Ethiopia (year 1)

This dataset was generated as part of the Guiding Acid Soil Management Investments in Africa (GAIA) project, which aims to support evidence-based investments in acid soil management across sub-Saharan Africa. Within this project, researcher-managed on-station trials were established to quantify crop yield responses to increasing lime application rates. 

The experimental design followed a randomized complete block design (RCBD) within each participating farm. Two farms were selected to host the trial, and seven lime application rates were tested within each farm. There were three blocks per farm. 

Crop yields measured during the first year capture the direct response to lime application. This dataset contains plot-level yield measurements for maize, wheat, faba bean, and soybean collected during the first year after trial establishment in Ethiopia. 

Predicted soil properties associated with the experimental sites are available from external datasets and can be linked through the soil sample unique identifier (SSN). 

These soil datasets are available at:
	Predicted Soil Properties Dataset:     hdl:11529/10549136
	Predicted Soil Properties Dataset:     doi:10.71682/10549328    

To facilitate interoperability and harmonization of agronomic terminology, a lookup table is provided that maps variable names and terms used in this dataset to the terminology defined in the terminag GitHub repository. 

Data from the second year are available in the related dataset: https://doi.org/10.71682/10549435.
"

	uri <- "doi:10.71682/10549433"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT; CIRAD; EIAR",
		publication = NA,
		project = "GAIA",
		design = "RCBD",
		data_type = "on-station experiment",
		treatment_vars = "lime",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Robert Hijmans",
		carob_LLM = "Cursor Grok 4.6",
		carob_date = "2026-10-15",
		carob_completion = 85,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "2026-07-24.GAIA_RCBD_Eth_on_farm_trials_yield_yr1_v0.1.csv"]
	r <- read.csv(f)

	# not processed (all missing): ssn_BP, ssn_AH, ssn_X, soil_sampling_date_BP, soil_sampling_date_AH,
	#   previous_crop, liming_date, planting_date, harvest_date
	# not processed (constant / redundant): trial_type, trial_year

	d <- data.frame(
		trial_id = paste(r$fid, r$crop, r$harvest_year, r$trial_season, sep = "_"),
		field_id = r$fid,
		country = r$country,
		adm1 = carobiner::fix_name(r$adm1, "title"),
		# concatenated source name; GADM equivalent is East Gojam
		adm2 = carobiner::fix_name(gsub("MisraqGojjam", "East Gojam", r$adm2), "title"),
		adm3 = carobiner::fix_name(r$adm3, "title"),
		longitude = r$longitude,
		latitude = r$latitude,
		geo_from_source = TRUE,
		crop = r$crop,
		block_id = r$rep,
		treatment = r$treatment,
		# onset rate from treatment code (T1_0tha ... T7_7tha); t/ha -> kg/ha
		lime = as.numeric(gsub("T[0-9]+_|tha", "", r$treatment)) * 1000,
		fertilizer_type = ifelse(r$lime_source == "CaCO3", "calcitic-lime", NA),
		harvest_date = as.character(r$harvest_year),
		season = ifelse(r$trial_season == 1, "first", "second"),
		on_farm = r$is_on_farm,
		irrigated = r$is_irrigated,
		is_survey = FALSE,
		yield = r$yield_tha * 1000,
		yield_outlier_ = r$is_outlier
	)

	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("faba bean", "soybean")] <- "seed"

	carobiner::write_files(path, meta, d)
}
