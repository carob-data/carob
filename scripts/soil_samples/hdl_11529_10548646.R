# R script for "carob"


carob_script <- function(path) {
  
"The Soil Intelligence System (SIS-India) mid-IR (MIR) dataset contains MIR spectra and wet chemistry measurements (pH, EC, OC, N, P, K, S, B, Zn, Cu, Mn, Fe) for 3182 soil samples collected from randomly selected rice-wheat farmer fields spread uniformly within a KVK (government extension system) domain/district in Bihar, India. Soil sampling was conducted during the time following kharif harvest and prior to land preparation for rabi during the 2018, 2019, and 2020 seasons. The dataset is used for the training and evaluation of machine learning models for spectral prediction of soil properties. (2022-03-03)"
  
	uri <- "hdl:11529/10548646"
	group <- "soil_samples"
	ff	 <- carobiner::get_data(uri, path, group)
  
	## data set level data 
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		project="CSISA",
		publication=NA,
		data_organization = "CIMMYT",
		carob_contributor="Andrew Sila",
		carob_effort = NA,
		carob_date="2023-09-28",
		data_type = "survey",
		response_vars = "none",
		treatment_vars = "none"
    )
  

	# read wetchem data and sample identifier tables
	f0 <- ff[basename(ff) == "sample_identifier.csv"]
	d0 <- read.csv(f0)
	
	# rename columns
	colnames(d0) <- tolower(colnames(d0))

	f1 <- ff[basename(ff) == "wetchem.csv"]
	d1 <- read.csv(f1)
	
	# Properties
		
	hd <- c('soil_pH', 'soil_EC', 'soil_SOC', 'soil_N', 'soil_P_total', 'soil_K',  'soil_Zn', 'soil_Cu', 'soil_Fe', 'soil_Mn', 'soil_S', 'soil_B')
	colnames(d1) <- c('id', hd)
	
	# No need to read table with MIR data now because we are not processing it
	## why not?
	#f2 <- ff[basename(ff) == "drychem.csv"]
	#d2 <- read.csv(f2)
	
	# merge d0 and d1 tables
	d <- merge(d0, d1, by="id")
	d$country <- 'India'
	d$geo_from_source <- TRUE
	d$location <- d$district
	d$date <- as.character(d$year)
	
	# Remove negative B values
	d$soil_B[d$soil_B < 0] <- NA
	
	# Drop id, original_id, year and district columns from d
	d$id <- d$original_id <- d$year <- d$district <- NULL
	
	## fixing long and lat coordinate 
	d$longitude[grepl("Sitamarhi", d$location)] <- 85.5027
	d$latitude[grepl("Sitamarhi", d$location)] <- 26.590
	
	carobiner::write_files(path, meta, d)
}



