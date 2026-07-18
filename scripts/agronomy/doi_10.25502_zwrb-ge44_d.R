# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Under best planting practices, the effect of land preparation on cassava root yield in Nigeria

ACAI is a 5 year Bill & Melinda Gates Foundation funded project in 5 countries in Africa (Nigeria and Tanzania) providing tailored agronomic advice to small scale cassava growers in the target countries. The project delivers agronomic solutions to improve cassava root yield and quality and the necessary knowledge base and applications for accessing this knowledge to cassava scaling partners and ultimately farmers in the target countries while instituting the necessary capacity and skills for national system scientists to engage in transformative cassava agronomy.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi.org/10.25502/zwrb-ge44/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "African Cassava Agronomy Initiative (ACAI)",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "fertilizer_used;values_land_prep;herbicide_timing",
		response_vars = "yield;yield_marketable;root_infection;plant_density", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-07-17",
		carob_completion = 100,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "bpp_ckan.csv"]
	f2 <- ff[basename(ff) == "bpp_ckan_treatment.csv"]
	f3 <- ff[basename(ff) == "data_dictionary_acai_bpp_ckan.csv"]

	r1 <- read.csv(f1)
	#r2 <- read.csv(f2) dataset dictionary
	#r3 <- read.csv(f3)

	
	d <- data.frame(
	  trial_id = as.character(r1$trial_ID),
	  country = "Nigeria",
	  adm1 = NA,
	  plot_id = as.character(r1$plotID),
	  planting_date = NA,
	  harvest_date = format(as.Date(r1$harvestDate, format = "%d/%m/%Y"), "%Y-%m-%d"),
	  treatment = as.character(r1$Trt_Code),
	  plot_area = NA,
	  plant_density = r1$plant_density_per_.ha,
	  yield = r1$tuberizedMarketableRootsFW,
	  yield_marketable = r1$tuberizedMarketableRootsFW,
	  values_land_prep = paste(r1$ploughing,r1$flat_ridge),
	  root_infection = (r1$tuberizedDiseasedRootsFW / (r1$tuberizedDiseasedRootsFW + r1$tuberizedMarketableRootsFW)) * 100,
	  fertilizer_used = ifelse(r1$Fertilizer == "Fertilizers applied", TRUE, FALSE),
	  crop = "cassava"
	)

#####These values were for land_prep
	#### assuming flat in values_land_prep = minimum tillage and zero tillage is none 
	
	d$values_land_prep <- gsub("Flat", "minimum tillage",d$values_land_prep)
	d$values_land_prep<- gsub("Ridged", "ridge tillage",d$values_land_prep)
	d$values_land_prep <- gsub("Zero tillage", "none",d$values_land_prep)
	d$values_land_prep <- gsub("Single plough", "single ploughing",d$values_land_prep)
	d$values_land_prep <- gsub("Double plough", "double ploughing",d$values_land_prep)
	
	
	##### assumed herbicide timing = wether following the protocol or farmers choice
	d$herbicide_timing[grepl("HB", d$treatment)] <- "application according to protocol"
	d$herbicide_timing[grepl("FC", d$treatment)] <- "application according to farmer's timing"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	
	d$longitude <- 8.675
	d$latitude <- 9.082
	d$geo_uncertainty = 96111 # geo_uncertainty was computed  from  adm1 = Lagos 
	d$geo_source = "Google maps"
	d$geo_from_source <- FALSE
	
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- NA
	
	d$yield_part <- "tubers"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	

	carobiner::write_files(path, meta, d)
}

