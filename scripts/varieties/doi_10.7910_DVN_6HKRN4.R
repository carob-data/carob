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
		treatment_vars = "variety",
		response_vars = "yield;plant_height",
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-10",
		carob_completion = 70,	
		carob_effort = 5
	)
	
	f <- ff[basename(ff) == "Advanced drought tolerant sorghum hybrids at Shiraro 2018.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
	  country = "Ethiopia",
	  location = r$Site,
	  
	  ### wrong conversion, that was later set th NA
	  ### planting_date = as.character(as.Date(r$Sown))
	  ### excel starts counting days from 1899-12-29 (!)
	  planting_date = as.character(r$Sown + as.Date("1900-01-01") - 2),
	  treatment = as.character(r$Entry),
	  variety = r$Genotype,
	  variety_pedigree = r$Pedigree,
	  variety_type= "drought tolerant hybrids",
	  rep = as.integer(r$Replicate),
	  yield = r$`Yield Kg/Ha`,
	  plot_id = as.character(r$Plot),
	  plant_height = r$PHTMean,
	  flowering_days = r$DTF,
	  maturity_days = r$DTM,
	  crop = "sorghum"
	)

	# all 7.5 m2
	d$plot_area <- mean(10 * r$`Yield g/Plot` / r$`Yield Kg/Ha`)
	d$plant_density <- 10000 * r$StandAtHarv  / d$plot_area
	d$spike_density	<- 10000 * r$`Heads/Plot` / d$plot_area

	# One would assume that the striga counts are per plot, in which case the per m2 striga density would be 
	# s70 <- as.numeric(r$`Striga count at 70 days`) / d$plot_area
	# s90 <- as.numeric(r$`Striga count at 90 days`) / d$plot_area
    # but that seems way too high, so I am leaving them out 
	d$stress <- "striga"
	
	d$trial_id <- r$Type
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$longitude <- 37.773
	d$latitude <-  14.396
	# uncertainty from GADM adm3 = "Tahtay Adiyabo",
	d$geo_uncertainty = 49096
	d$geo_source = "Google Maps"
	d$geo_from_source <- FALSE
	
	d$harvest_date <- as.character(NA)	
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}


