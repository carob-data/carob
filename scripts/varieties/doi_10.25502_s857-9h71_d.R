# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Genotype X Environment data

Data collected from a multi-location variety trial
"

	uri <- "doi:10.25502/s857-9h71/d"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;NCRI", # NCRI: National Cereals Research Institute
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-06",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "soybean-gxe-data-.csv"]
	#f2 <- ff[basename(ff) == "data_dictionary.csv"]

	r1 <- read.csv(f1)
	#r2 <- read.csv(f2)

###### process
	d <- data.frame(
	  location = c("IITA_Ibadan"= "Ibadan", "SeedCo_Kujama"= "Kujama", "SeedCo_Saminaka" = "Saminaka", "SeedCo_Sabuwa"= "Sabuwa", "NCRI_Makurdi"= "Makurdi", "NCRI_Mokwa"= "Mokwa", "Zaria"= "Zaria")[r1$env],
	  rep = r1$rep,
	  block_id = as.character(r1$block),
	  variety_code = r1$genotype,
	  yield = r1$yield,
	  crop = "soybean",
	  is_survey = FALSE, 
	  on_farm = TRUE, 
	  trial_id =r1$env , 
	  yield_part = "grain", 
	  country = "Nigeria", 
	  geo_from_source = FALSE, 
	  irrigated = NA, 
	  yield_moisture = NA_real_,
	  planting_date = "2024", 
	  harvest_date = NA_character_,
	  yield_isfresh = NA
	)
	
	### Adding lon and lat 
	
	geo <- data.frame(
	  location = c("Makurdi", "Zaria", "Sabuwa", "Mokwa", "Ibadan","Kujama", "Saminaka"),
	  longitude = c(8.5359, 7.7143, 7.0935, 5.1464, 3.897, 7.635, 7.4048),
	  latitude = c(7.7042, 11.0231, 11.2842, 9.2437, 7.375, 10.480, 10.516),
	  geo_uncertainty = c(18552, 16966, 22502, 87356, 4390, NA, NA),
	  geo_source = c(rep("GADM 4.1, adm2", 5), rep("Google Maps", 2))
	)
	
	d <- merge(d, geo, by ="location", all.x = TRUE)
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA) 		

	carobiner::write_files(path, meta, d)
}


