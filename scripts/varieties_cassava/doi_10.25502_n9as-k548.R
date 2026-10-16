# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them
### location is confusing the location names says the says the study was conducted in adm2 = Ibadan yet the study discription says it was done in adm2 = Iseyin, adm4 = Ajapero village
## missing harvest date

carob_script <- function(path) {

"
Fertilizer Experiment with 18 clones at Obasanjo Farm Ajepero-Oyo-Iseyin Road

Fertilizer Experiment with 18 clones at Obasanjo Farm Ajepero-Oyo-Iseyin Road
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

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

## select the variables of interest and assign them to the correct name


	
	d1 <- data.frame(
	  country = "Nigeria",
	  adm1 = "Oyo",
	  adm2 = "Iseyin",### obtained from studyption and name
	  location = "Ajepero",#obtained from studyption and name
		plot_id = as.character(r1$plotNumber),
		planting_date = "2006", ### from the study year
		harvest_date = NA, #not indicated in the dataset
		#location = r1$locationDbId,
		#location = r1$locationName,#this column highlights that the study was done in Ibadan
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
	

## see carobiner::geocode
	#### The geo_coordinates are for adm2 = Iseyin sourced from GADM. I could not find exact location  for Ajapero farm in google maps
	
	  d$longitude <- 3.5333
	  d$latitude <- 7.8647
	  d$geo_uncertainty <- 32578
	  d$geo_source <- "GADM 4.1, adm2"
	  d$geo_from_source <- FALSE


	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$fertilizer_type <- NA
  d$fertilizer_type <- NA

  d$yield_part <- "tubers"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}


