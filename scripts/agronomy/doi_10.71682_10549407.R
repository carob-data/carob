# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. 2 entries from raw data have NA values in yield


carob_script <- function(path) {

"
Agronomic data of on-farm trials on yield response to lime rates  in Zambia

This dataset was generated as part of the Guiding Acid Soil Management Investments in Africa (GAIA) project, which aims to support evidence-based investments in acid soil management across sub-Saharan Africa. Within this project, researcher-managed on-farm trials were established to quantify crop yield responses to increasing lime application rates. The experimental design followed a randomized complete block design (RCBD) in which each participating farm represented one replication (block). A total of 15 farms were included, and four lime application rates were tested within each farm. Lime was applied at the onset of the trial (Year 1) and crop yields measured to capture the direct response to lime application. This dataset contains plot-level yield measurements for maize and common beans collected during the first year after trial establishment in Zambia. Predicted soil properties associated with the experimental sites are available from external datasets and can be linked through the soil sample unique identifier (SSN). These soil datasets are available at: - Predicted Soil Properties Dataset  (https://doi.org/10.71682/10549395 )

To facilitate interoperability and harmonization of agronomic terminology, a lookup table is provided that maps variable names and terms used in this dataset to the terminology defined in the terminag GitHub repository. (https://github.com/reagro/terminag/)
"

	uri <- "doi:10.71682/10549407"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;CIRAD;ZARI;ICRAF",
		publication = NA,
		project = "GAIA",
		design = "Randomized Complete Block",#from description
		data_type = "experiment",
		treatment_vars = "lime",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-06-23",
		carob_completion = 100,	
		carob_effort = 2
	)

	f <- ff[basename(ff) == "GAIA_Zmb_on_farm_trials_yield_yr1_2026-03-13_v0.1.csv"]
	r <- read.csv(f)

	d <- data.frame(
	  country=r$country,
	  adm1=r$adm1,
	  adm2=r$adm2,
	  adm3=r$adm3,
	  longitude=r$longitude,
	  latitude=r$latitude,
	  harvest_date=as.character(r$harvest_date),
	  previous_crop=r$previous_crop,
	  crop=r$crop,
	  treatment=r$treatment,
	  lime=r$lime_tha,
	  fertilizer_type= "calcitic-lime", # CaCO3
	  liming_date=as.character(r$liming_date),
	  planting_date=as.character(r$planting_date),
	  on_farm=r$is_on_farm,
	  irrigated=r$is_irrigated,
	  is_survey=FALSE,
	  geo_from_source=TRUE,
	  yield=r$yield_tha*1000
	)

	d$trial_id <- paste(d$planting_date,d$adm4, sep = "_")
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- as.numeric(NA)
	d$yield_part <- ifelse(d$crop=="beans", "seed", "grain")
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	d$crop <- gsub("beans","common bean", d$crop)

	carobiner::write_files(path, meta, d)
}

