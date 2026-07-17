# R script for "carob"
# license: GPL (>=3)

## ISSUES
# No location below country level exists in the source.
# No planting_date in the source - only HarvestDate is available.
# ridged or not ridged was a variable, but ridges is not included in carob terminage 
# NPK was given as 90:20:37

carob_script <- function(path) {

"
Cassava maize intercropping in Nigeria

ACAI is a 5 year Bill & Melinda Gates Foundation funded project in 5 countries in Africa (Nigeria and Tanzania) providing tailored agronomic advice to small scale cassava growers in the target countries. The project delivers agronomic solutions to imporve cassava root yield and quality and the necessary knowledge base and applications for accessing this knowledge to cassava scaling partners and ultimately farmers in the target countries while instituting the necessary capacity and skills for national system scientists to engage in transformative cassava agronomy.
"

	uri <- "doi:10.25502/dvff-xx49/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)



	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "ACAI",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "intercropped;fertilizer_type",
		response_vars = "yield;yield_marketable;root_infection", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-17",
		carob_completion = 20,	
		carob_effort = 8
	)
	

	f1 <- ff[basename(ff) == "cim_ckan.csv"]
	#f2 <- ff[basename(ff) == "cim_ckan_treatments.csv"]
	#f3 <- ff[basename(ff) == "data_dictionary_cim_ckan.csv"]

	r <- read.csv(f1) |> unique()
	## f2 (cim_ckan_treatment.csv) and f3 (data_dictionary_cim_ckan.csv)
	## are reference/dictionary files, not data.
	
	#r <- read.csv(f1)
	#r2 <- read.csv(f2)
	#r3 <- read.csv(f3)
	
	d <- data.frame(
	  country = r$country,
	  trial_id = r$trial_ID,
	  plot_id = r$plot_ID,
	  treatment = r$trt_code,
	  crop = "cassava_monocrop;maize_monocrop;cassava_maize",
	  planting_date =  as.character(as.Date(NA)),
	  harvest_date = as.character(as.Date(r$HarvestDate, format = "%d/%m/%Y")),
	  #fertilizer_type = r$Fertilizer,
	  variety = ifelse(trimws(r$Variety) == "", NA, r$Variety),
	  land_prep_method = ifelse(trimws(r$flat_ridge) == "", NA, r$flat_ridge),
	  # assumed the weights provided are in tonne/ha as plant density is reported as per ha
	  cassava_density = as.numeric(r$cassava_density),
	  maize_density = as.numeric(r$maize_density),
	  yield = rowSums(r[, c("tuberizedMarketableRootsFW","tuberizedDiseasedRootsFW","tuberizedSmallRootsFW")], na.rm = FALSE),
	  yield_marketable = r$tuberizedMarketableRootsFW,
	  root_infection = r$tuberizedDiseasedRootsFW
	)  
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$intercrops <- "cassava_maize"

	d$longitude <- NA
	d$latitude <- NA
	d$geo_from_source <- NA


   d$P_fertilizer <- 20
   d$K_fertilizer <- 37
   d$N_fertilizer <- 90
   
   #The dataset says NPK and MOP and in terminage muriate of potash is named KCL
   #The f2 file says NPK was applied in form of NPK and urea
   
  d$fertilizer_type <- "NPK;KCL;urea"

  d$root_infection <- (r$tuberizedDiseasedRootsFW / d$yield) * 100

  d$yield_part <- "roots"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- TRUE

	
	carobiner::write_files(path, meta, d)
}


