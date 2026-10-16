# R script for "carob"
# license: GPL (>=3)

## REJECTED 
# exeperiment without response data 

carob_script <- function(path) {

"
Fertilizer Experiment with 18 clones at Obasanjo Farm Ajepero-Oyo-Iseyin Road

Fertilizer Experiment with 18 clones at Obasanjo Farm Ajepero-Oyo-Iseyin Road
"

	uri <- "doi:10.25502/n9as-k548"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "none", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-09-17",
		carob_completion = 80,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "2021-11-24t035610phenotype_download.csv"]
	f2 <- ff[basename(ff) == "metadata.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	d1 <- data.frame(
	  country = "Nigeria",
		plot_id = as.character(r1$plotNumber),
		planting_date = "2006", ### from the study year
		harvest_date = NA,
		#location = r1$locationName, #this variables suggests that the study was done in Ibadan
        location <- "Obasanjo Farm, Ajepero, Oyo-Iseyin Road"
		field_size = r1$fieldSize,
		plot_area = (r1$plotLength)*(r1$plotWidth),
		variety = r1$germplasmName,
		variety_code = as.character(r1$germplasmDbId),
		rep = r1$replicate,
		block_id = as.character(r1$blockNumber),
		trial_id = as.character(r1$observationUnitDbId),
		yield = NA,
		crop = "cassava"
	)

	
  d <- d1
  
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	

  d$longitude <- 3.6972
  d$latitude <- 7.9175
  d$geo_source <- "Google Maps"
  d$geo_from_source <- FALSE

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$fertilizer_type <- NA
  d$fertilizer_type <- NA

  d$yield_part <- "tubers"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}


