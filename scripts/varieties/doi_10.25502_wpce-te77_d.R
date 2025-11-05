# R script for "carob"

carob_script <- function(path) {
   
"Soybean (Glycine max (L.) Merrill.) is one of the most important oil crops of the world which also has tremendous importance as a food legume. The work on soybean aims at providing farmers, both commercial and subsistence, varieties with their preferred attributes to increase yield and income. These include high yield, resistance to deadly diseases, such as soybean rust, and insect pests, early maturity, good seed quality, and resistance to other stresses such as drought and soil acidity. The International Institute of Tropical Agriculture (IITA) is a key player in tropical soybean research and a partner of the Soybean Innovation Lab."
   
	uri <- "doi:10.25502/wpce-te77/d"
	group <- "varieties"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication= NA,
		data_organization = "IITA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-09-21",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project=NA 
	)
	
	r <- read.csv(ff[basename(ff)=="data.csv"])
	
	d <- data.frame(
#		ID = r$ID,
		country = r$Country,
		location = r$City,
		rep = r$REP_NO,
		variety = r$DESIGNATION,
		yield = r$YIELD,
		dmy_total = r$BIOM,
		plant_height = r$PLHT,
		seed_weight = r$SWT100,
		maturity_days = r$DM,
		flowering_days = r$DFFL
	)


	d$location <- carobiner::fix_name(d$location, "title") 
	d$country <- carobiner::fix_name(d$country, "title") 
	
	d$crop <- "soybean" 
	d$yield_part <- "seed" 

	
	d$trial_id <- "1" ## paste(d$ID, d$adm1, sep = "-") ??
#	d$ID <- NULL
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	# Nampula
	d$longitude <- 39.271
	d$latitude <- -14.967
	d$geo_from_source <- FALSE

	## Planting and harvest date are taken from metadata 
	d$planting_date <- "2018-01-20"
	d$harvest_date <- "2018-06-18"

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
	
}


