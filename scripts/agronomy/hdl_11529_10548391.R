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
    country=r$Country,
    adm1=r$Region,
    adm2=r$District,
    adm3=r$Ward,
    location=r$Village,
    crop=tolower(r$Crop),
    field_size=r$`Field size`,
    plant_density=r$FP_PD,
    soil_depth=r$Soil_depth,
    previous_crop_residue_management=tolower(r$Residue_tc),
    soil_P=r$P_soil_value,
    soil_K=r$K_soil_value,
    yield_part="grain",
    yield_isfresh=TRUE,  
    yield_moisture=NA,
    geo_from_source=FALSE
  )
  d$trial_id <- 1:nrow(d)

  # fertilizer data
  ctrl <- data.frame(
    treatment = "control",
    yield = r$GY_control,
    N_fertilizer = 0,
    P_fertilizer = 0,
    K_fertilizer = 0
  )
  
  ne <- data.frame(
    treatment = "nutrient expert",
    yield = r$NE_Y_actual,
    N_fertilizer = r$NE_N_actual,
    P_fertilizer = r$NE_P2O5_actual / 2.29,
    K_fertilizer = r$NE_K2O_actual / 1.2051
  )
  
  d <- cbind(rbind(d, d), rbind(ctrl, ne))

  #density provided as estimates, removing non numeric characters
  ## d$plant_density <- as.numeric(gsub("[^0-9]", "", d$plant_density))
  #depth provided as estimates, removing non numeric characters	
  ## d$soil_depth <- as.numeric(gsub("[^0-9]", "", d$soil_depth))

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- r$Water != "fully rainfed"
	d$planting_date <- NA
	d$harvest_date  <- NA
	
	#manually adding location data based on adm4(Tanzania GADM has upto 3 levels only)
  #Google Maps coordinates
	geo <- data.frame(
	  location = c(
	    "Ntuchi - Kanondo", "Kitosi", "Mvimwa - Mission", "Maporomoko", "Mlombo", "Nyimbili", "Hasamba", "Mpanda", "Hantesya", "Kiruru Ibweijewa", "Mbambua", "Rau river",
	    "Ghona", "Ngasinyi", "Kyomu", "Ngaritati", "Koboko", "Kikatiti", "Ngurdoto", "Kikwe", "Lendikinya", "Gyekrum-Arusha", "Qurus", "Gongali", "Tlawi", "Masqaroda",
	    "Harsha", "Silaloda", "Mangisa", "Dareda Kati", "Nakwa", "Riroda", "Measkron", "Endasiwold", "Bassotughang", "Ndaleta", "Mwitikira"
	  ),
	  latitude = c(
	    -7.714, -7.748, -7.809, -9.308, -8.403, -9.205, -9.146, -6.363, -9.213, -3.714, -3.759, -3.394,
	    -3.455, -3.239, -3.476, -3.267, -3.192, -3.4, -3.327, -3.425, -3.339, -3.351, -3.392, -3.392, -3.916, -3.895,
	    -3.995, -3.836, -4.001, -4.213, -4.247, -4.299, -4.453, -4.407, -4.343, -5.231, -6.584
	  ),
	  longitude = c(
	    31.152, 31.157, 31.122, 32.774, 32.009, 33.056, 33.006, 31.064, 32.985, 37.588, 37.603, 37.372,
	    37.529, 37.358, 37.512, 37.044, 37.077, 36.946, 36.913, 36.832, 36.362, 35.671, 35.652, 35.652, 35.482, 35.37,
	    35.342, 35.443, 35.37, 35.553, 35.702, 35.644, 35.492, 35.499, 35.312, 36.499, 35.661
	  ),
	  geo_uncertainty = rep(NA,37),
	  geo_source = rep("Google Maps",37)
	)	
	d <- merge(d, geo, by="location", all.x = TRUE)
	
	#Using point radius method for location names not found on google maps and location names with NA 
	no_coords <- is.na(d$latitude) | is.na(d$longitude)
	
	missing <- d[no_coords, ]
	geo <- data.frame(
	adm3 = c("Maboga", "Ulanda", "Irole", "Uhambingeto", "Bumilayinga", "Igombavanu", "Ikweha", "Magamba", "Msangeni", "Shighatini", "Isuto", "Iyunga Mapinduzi", "Santilya", "Uwemba", "Makoga", "Ulembwe", "Laela", "Mgombasi", "Rwinga", "Kilagano", "Maposeni", "Mtunduru", "Magamba","Siha Kati"),
	longitude = c(35.3159, 35.5286, 35.9024, 35.9739, 35.0826, 35.1137, 34.9102, 31.103, 37.626, 37.6252, 33.2568, 33.3825, 33.3923, 34.7912, 34.5651, 34.6364, 31.9902, 36.0826, 36.2022, 35.2825, 35.4496, 34.4987, 32.9134,37.0856),
	latitude = c(-7.9289, -7.819, -7.7725, -7.5688, -8.3955, -8.2315, -8.3269, -6.4313, -3.6166, -3.6667, -9.0753, -9.073, -9.1373, -9.4737, -9.334, -9.3502, -8.6096, -10.0367, -10.471, -10.4174, -10.4874, -4.8578, -8.621, -3.1887),
	geo_uncertainty = c(18441, 13891, 19683, 12957, 10467, 12765, 20473, 4735, 6779, 5535, 10868, 5990, 7536, 12589, 10832, 11787, 15617, 37083, 16201, 22605, 18046, 15815, 10943,2585),
	geo_source = rep("GADM 4.1, adm3",24))
	
  #filling only NA places 
	i <- match(d$adm3, geo$adm3)
	use <- no_coords & !is.na(i)
	
	d$longitude[use]       <- geo$longitude[i[use]]
	d$latitude[use]        <- geo$latitude[i[use]]
	d$geo_uncertainty[use] <- geo$geo_uncertainty[i[use]]
	d$geo_source[use]      <- geo$geo_source[i[use]]

	carobiner::write_files(path, meta, d)
}
