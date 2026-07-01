# R script for "carob"
# license: GPL (>=3)

## ISSUES
#NA values on yield

carob_script <- function(path) {

"
Red sorghum hybrids at Shiraro 2014

Data on agronomic traits of maturity, plant height, grain yield, resistance/tolerance to biotic (insects and disease) and abiotic (drought) stress and plant aspect score collected for 36 experimental red hybrids, a hybrid and an OPV check evaluated at Sheraro (Western Tigrai, Ethiopia) in 2014
"

	uri <- "doi:10.7910/DVN/GT9OC4"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = "NA",
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield;disease_severity;pest_severity;drought_stress", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-01",
		carob_completion = 0,	
		carob_effort = 5)
	

	f <- ff[basename(ff) == "Red sorghum hybrids at Shiraro 2014.xlsx"]


	r <- carobiner::read.excel(f, sheet="Sheet1")
	d <- data.frame(
	  country = "Ethiopia",
		adm1 = "Tigrai",
		adm2 = "Western Tigrai",
		adm3 = r$Site,
		treatment = r$Genotype,
		rep = as.integer(r$Replicate),
		plant_height = r$PHTMean,
		yield = r$`YieldKg/Ha`,
		variety_pedigree = r$Pedigree,
		plot_id = as.character(r$Plot),
		flowering_days = r$DTF,
		maturity_days = r$DTM,
		yield_part = "grain",
		plot_area = as.numeric(r$PlotArea),
		disease_severity = as.character(r$DiseaseScore),
		pest_severity = as.character(r$InsectScore),
		drought_stress = as.character(r$DroughtScore),
		crop = "sorghum"
	)
	
	d$trial_id <- as.character(as.integer(as.factor( paste(d$adm3,d$planting_date,sep = "_"))))
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	d$longitude <-  37.555
	d$latitude <-  14.240
	d$geo_from_source <- FALSE

	
	d$planting_date <- as.character(as.Date("2014-07-16"))
	d$harvest_date  <- as.character(as.Date("2014-11-26"))

   d$P_fertilizer <- NA
   d$K_fertilizer <- NA
   d$N_fertilizer <- NA
   d$fertilizer_type <- NA

   yield = r$`YieldKg/Ha`
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d)
}


