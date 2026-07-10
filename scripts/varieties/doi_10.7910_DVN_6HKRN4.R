# R script for "carob"
# license: GPL (>=3)

## ISSUES
#The crops were striga_infected


carob_script <- function(path) {

"
Advanced drought tolerant sorghum hybrids at Shiraro 2018

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for advanced drought tolerant hybrids evaluated at Shiraro, Ethiopia in 2018
"

	uri <- "doi:10.7910/DVN/6HKRN4"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;plant_height",
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-10",
		carob_completion = 70,	
		carob_effort = 5)
	

	f <- ff[basename(ff) == "Advanced drought tolerant sorghum hybrids at Shiraro 2018.xlsx"]

	r <- carobiner::read.excel(f)


	d <- data.frame(
	  country = "Ethiopia",
	  location = r$Site,
	  treatment = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  rep = as.integer(r$Replicate),
	  yield = r$`Yield Kg/Ha`,
	  plot_id = as.character(r$Plot),
	  plant_height = r$PHTMean,
	  flowering_days = r$DTF,
	  maturity_days = r$DTM,
	  str_co2 = as.numeric(r$`Striga count at 70 days`),
	  crop = "sorghum"
	  )

	d$trial_id <- "1"
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	#co-odinates from google, Sheraro not found in GADM
	
	d$longitude <- 37.773
	d$latitude <-  14.396
	d$geo_from_source <- FALSE
	
	#adm3 = c("Tahtay Adiyabo"),
	#longitude = c(37.7897),
	#latitude = c(14.4267),
	#geo_uncertainty = c(49096),
	#geo_source = c("GADM 4.1, adm3")
	
	
	
	d$planting_date <- as.character(NA)
	d$harvest_date <- as.character(NA)
	
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	
	
	carobiner::write_files(path, meta, d)
}


