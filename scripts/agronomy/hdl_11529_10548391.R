# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. dataset had several treatment variables linked to N,P,K but the values are empty including the yields, so i only picked the populated treatment variables with populated yield
#2. there are some missing values in the N,P,K columns
#3. there are some NA values in yield

carob_script <- function(path) {

"
Tanzania Performance Trials 2016

2017-18 Performance Trials in Tanzania by TAMASA
"

	uri <- "hdl:11529/10548391"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-09-24",
		carob_completion = 90,	
		carob_effort = 7
	)
	

	f <- ff[basename(ff) == "TAMASA_NE_PT_Data_Tanzania_2017_18(Blurred).xlsx"]

	r <- carobiner::read.excel(f, sheet="Data (NE database) ")

  d <- data.frame(
    row_id = seq_len(nrow(r)),
    country=r$Country,
    adm1=r$Region,
    adm2=r$District,
    adm3=r$Ward,
    adm4=r$Ward,
    crop=tolower(r$Crop),
    field_size=r$`Field size`,
    plant_density=r$FP_PD,
    soil_depth=r$Soil_depth,
    previous_crop_residue_management=tolower(r$Residue_tc),
    soil_P=r$P_soil_value,
    soil_K=r$K_soil_value,
    yield_part="grain",
    yield_isfresh=FALSE,
    yield_moisture=NA
    )

  #fertilizer data
  ctrl <- data.frame(
    row_id = seq_len(nrow(r)),
    treatment = "control",
    yield = r$GY_control,
    N_fertilizer = 0,
    P_fertilizer = 0,
    K_fertilizer = 0
  )
  
  ne <- data.frame(
    row_id = seq_len(nrow(r)),
    treatment = "nutrient expert",
    yield = r$NE_Y_actual,
    N_fertilizer = r$NE_N_actual,
    P_fertilizer = r$NE_P2O5_actual / 2.29,
    K_fertilizer = r$NE_K2O_actual / 1.2051
  )
  
  d_fert <- rbind(ctrl, ne)
  d <- merge(d, d_fert, by = "row_id")
  d$row_id <- NULL
  
  d$plant_density <- as.numeric(gsub("[^0-9]", "", d$plant_density))#density provided as estimates, removing non numeric characters
  d$soil_depth <- as.numeric(gsub("[^0-9]", "", d$soil_depth))#depth provided as estimates, removing non numeric characters

	d$trial_id <- paste(d$adm4,seq_len(nrow(r)), sep = "_")
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	d$planting_date <- NA
	d$harvest_date  <- NA
	
	#manually adding location data based on adm4(Tanzania GADM has upto 3 levels only)
	geo <- data.frame(
	  adm4 = c("Isale", "Kate", "Laela", "Magamba", "Mtunduru", "Nyimbili", "Mlowo", "Santilya", "Iyunga Mapinduzi", "Isuto", "Ulembwe", "Makoga", "Uwemba", "Maposeni",
	    "Kilagano", "Mgombasi", "Rwinga", "Ikweha", "Bumilayinga", "Igombavanu", "Ulanda", "Maboga", "Irole", "Uhambingeto", "Lembeni", "Shighatini", "Msangeni", "Kahe",
	    "Kahe Mashariki", "Siha Kati", "Siha Kaskazini", "Kikatiti", "Maji ya chai", "Kikwe", "Sepeko", "Karatu", "Qurus", "Tlawi", "Bashay", "Sanu", "Tumati", "Dareda", "Riroda", "Measkron",
	    "Endasak", "Hidet", "Njoro", "Olbolot"),
	  
	  longitude = c( 31.0762, 31.1837, 32.0427, 38.3010, 34.5140, 33.0561, 32.9949, 33.3691, 33.3879, 33.2683, 34.6280, 34.5709, 34.7876, 35.4238,
	    35.2872, 36.0587, 36.2063, 34.9189, 35.0599, 35.1235, 35.5289, 35.2713, 35.9000, 35.9732, 37.6078, 37.6435, 37.6552, 37.4332,
	    37.5254, 37.1047, 37.1239, 36.9469, 36.9237, 36.8304, 36.3562, 35.6657, 35.6351, 35.4821, 35.3499, 35.5521, 35.4397, 35.5215, 35.6449, 35.4923,
	    35.5164, 35.5258, 36.4652, 36.3007),
	  
	  latitude = c( -7.64086, -7.85418, -8.58138, -4.75779, -4.82126, -9.20269, -9.01897, -9.11111, -9.06403, -9.07271, -9.34524, -9.34118, -9.46611, -10.49900,
	    -10.40110, -10.22040, -10.47010, -8.43673, -8.38015, -8.22465, -7.82165, -7.97206, -7.79871, -7.57183, -3.78384, -3.67032, -3.64501, -3.50037,
	    -3.31906, -3.23197, -3.13050, -3.40028, -3.33673, -3.41973, -3.42236, -3.34835, -3.39242, -3.91656, -4.00012, -3.86088, -4.07794, -4.23638, -4.29954, -4.45380,
	    -4.42158, -4.49937, -5.25531, -5.17758),
	  geo_source = rep("Google Maps", 48))
	
	d <- merge(d,geo,by="adm4",all.x = TRUE)
	
	carobiner::write_files(path, meta, d)
}

