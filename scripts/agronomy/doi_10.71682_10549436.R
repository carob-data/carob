# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Planting, harvest (day), and lime application dates are not in the files (only harvest year).
# Fertilizer (N/P/K) rates are not reported.
# Soil sampling dates and after-harvest sample IDs are missing; some before-planting sample IDs are present.
# Yield moisture is not reported.

## NOTES
# On-farm RCBD, 16 farms, three blocks (some relabelled as r_4..r_7), seven lime rates.
# Year-2 records are residual effects: raw lime_tha is 0. lime is the onset rate (kg/ha) parsed from treatment.
# This file also contains the year-1 records. doi:10.71682/10549434 is in _rejected as a strict subset.

## NEW VARIABLES
# yield_outlier_: source flag for whether the yield value is an outlier (unlikely, likely, surely)


carob_script <- function(path) {

"
Field experiments on crop response to lime rates in Rwanda (year 2)

This dataset was generated as part of the Guiding Acid Soil Management Investments in Africa (GAIA) project, which aims to support evidence-based investments in acid soil management across sub-Saharan Africa. Within this project, researcher-managed on-farm trials were established to quantify crop yield responses to increasing lime application rates. The experimental design followed a randomized complete block design (RCBD) within each participating farm. 

 Sixteen farms were selected to host the trial, and seven lime application rates were tested within each farm. Lime was applied at the onset of the trial (Year 1). There were three blocks per farm. Blocks were relabelled when the former could not be identified in subsequent season or year. Incomplete, isolated or missing blocks in a farm result either from data missingness or farm misidentification. 

 Crop yields measured during the first year capture the direct response to lime application, while yields measured in the second year allow evaluation of the residual effects of the initial lime treatment. This dataset contains plot-level yield measurements for maize, common beans, and potatoes collected during the first and second years after trial establishment in Rwanda. 

 Predicted soil properties associated with the experimental sites are available from external datasets and can be linked through the soil sample unique identifier (SSN). 

These soil datasets are available at:

        Predicted Soil Properties Dataset:     hdl:11529/10549138           Predicted Soil Properties Dataset:     doi:10.71682/10549329    

 To facilitate interoperability and harmonization of agronomic terminology, a lookup table is provided that maps variable names and terms used in this dataset to the terminology defined in the terminag GitHub repository. 

 Data from the first year are available in the related dataset: https://doi.org/10.71682/10549434.
"

	uri <- "doi:10.71682/10549436"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT; CIRAD; RAB; ICRAF",
		publication = NA,
		project = "GAIA",
		design = "RCBD",
		data_type = "on-farm experiment",
		treatment_vars = "lime",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Robert Hijmans",
		carob_LLM = "Cursor Grok 4.6",
		carob_date = "2026-10-15",
		carob_completion = 85,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "2026-07-24.GAIA_RCBD_Rwa_on_farm_trials_yield_yr2_v0.1.csv"]
	r <- read.csv(f)

	# not processed (all missing): ssn_AH, ssn_X, soil_sampling_date_BP, soil_sampling_date_AH,
	#   previous_crop, liming_date, planting_date, harvest_date
	# not processed (constant / redundant): trial_type, trial_year

	d <- data.frame(
		trial_id = paste(r$fid, r$crop, r$harvest_year, r$trial_season, sep = "_"),
		field_id = r$fid,
		country = r$country,
		adm1 = carobiner::fix_name(r$adm1, "title"),
		adm2 = carobiner::fix_name(r$adm2, "title"),
		adm3 = carobiner::fix_name(r$adm3, "title"),
		longitude = r$longitude,
		latitude = r$latitude,
		geo_from_source = TRUE,
		crop = r$crop,
		block_id = r$rep,
		treatment = r$treatment,
		# onset rate from treatment code (T1_0tha ... T7_7tha); t/ha -> kg/ha
		# year-2 raw lime_tha is 0 (not reapplied)
		lime = as.numeric(gsub("T[0-9]+_|tha", "", r$treatment)) * 1000,
		fertilizer_type = ifelse(r$lime_source == "CaCO3", "calcitic-lime", NA),
		harvest_date = as.character(r$harvest_year),
		season = ifelse(r$trial_season == 1, "first", "second"),
		sample_id = r$ssn_BP,
		on_farm = r$is_on_farm,
		irrigated = r$is_irrigated,
		is_survey = FALSE,
		yield = r$yield_tha * 1000,
		yield_outlier_ = r$is_outlier
	)

	d$yield_part <- "grain"
	d$yield_part[d$crop == "common bean"] <- "seed"
	d$yield_part[d$crop == "potato"] <- "tubers"

	carobiner::write_files(path, meta, d)
}
