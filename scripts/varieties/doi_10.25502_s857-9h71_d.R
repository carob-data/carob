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
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-08-19",
		carob_completion = 80,	
		carob_effort = 5
	)
	

	#f1 <- ff[basename(ff) == "data_dictionary.csv"]
	f2 <- ff[basename(ff) == "soybean-gxe-data-.csv"]

	#r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	d1 <- data.frame(
	  country = "Nigeria",
	  location = r2$env,
	  plot_id = as.character(r2$ID),
	  rep = r2$rep,
	  variety = r2$genotype,
	  block_id = as.character(r2$block),
	  yield = r2$yield,
	  crop = "soybean"
	)


	geo <-  data.frame(
	  location = c("IITA_Ibadan", "SeedCo_Kujama", "SeedCo_Saminaka", "SeedCo_Sabuwa", "NCRI_Makurdi","NCRI_Mokwa", "Zaria"),
	  latitude = c(7.4999, 10.3669, 10.327, 11.284, 9.114, 9.244, 11.023),
	  longitude = c(3.908, 7.241, 8.555, 7.94, 6.141, 5.146, 7.714)
	)
	
	d <- merge(d1, geo, by = "location", all.x = TRUE)
	d$geo_from_source <- FALSE
	d$trial_id <- as.character(as.integer(as.factor(1)))
	
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$planting_date <- as.character(as.Date(NA))
	d$harvest_date  <- as.character(as.Date(NA))


   d$P_fertilizer <- NA
   d$K_fertilizer <- NA
   d$N_fertilizer <- NA
   d$fertilizer_type <- NA

   d$yield_part <- "grain"
   d$yield_moisture <- as.numeric(NA)
   d$yield_isfresh <- NA
	
	carobiner::write_files(path, meta, d)
}


