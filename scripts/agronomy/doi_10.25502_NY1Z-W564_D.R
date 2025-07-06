# R script for "carob"

# not included because the yield data provided is a rating hence data is out of scope for us until we know what amount the ratings stand for

"
N2Africa farm monitoring - Malawi, 2012 - 2013

N2Africa is to contribute to increasing biological nitrogen fixation and productivity
of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility,
improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"
carob_script <- function(path) {
  
	uri <- "doi:10.25502/NY1Z-W564/D"
	group <- "agronomy"
	ff	<- carobiner::get_data(uri, path, group)
  
  ## data set level data
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication=NA,
		project="N2Africa",
		data_organization = "IITA;ICRAF;WUR",
		carob_contributor="Rachel Mukami",
		carob_date="2023-06-13",
		data_type="variety trial"
		 
	)
	

	d <- data.frame(rep = integer(0), season = character(0), 
		country = character(0), site = character(0), treatment = character(0), 
		longitude = numeric(0), latitude = numeric(0), planting_date = character(0), 
		harvest_date = character(0), trial_id = character(0), crop = character(0), 
		yield = numeric(0), fwy_residue = numeric(0), seed_weight = numeric(0), 
		previous_crop = character(0), OM_type = character(0), N_fertilizer = numeric(0), 
		K_fertilizer = numeric(0), P_fertilizer = numeric(0), Zn_fertilizer = numeric(0), 
		S_fertilizer = numeric(0), yield_part = character(0))
 
 
	carobiner::write_files(meta, d, path=path)

}


	
	# f <- ff[basename(ff) == "a_general.csv"]
	# d <- data.frame(read.csv(f))
	# d$trial_id <- d$farm_id
	# d$adm1 <- carobiner::fix_name(d$district, "title")
	# d$adm2 <- carobiner::fix_name(d$sector_ward, "title")
	# d$adm3 <- carobiner::fix_name(d$vilage, "title")
	# d$site <- carobiner::fix_name(d$action_site,"title")
	# d$latitude <- -d$gps_latitude_dec
	# d$longitude <- d$gps_longitude_dec
	# d$elevation <- d$gps_altitude
	# d <- d[,c("trial_id","season","country","adm1","adm2","adm3","site","latitude","longitude","elevation")]
	
	# f1 <- ff[basename(ff) == "a_technician.csv"]
	# d1 <- data.frame(read.csv(f1))
	# d1$trial_id <- d1$farm_id
	# d1$observation_date <- as.character(as.Date(d1$date,"%m/%d/%Y"))
	# d1 <- d1[,c("trial_id","observation_date")]
	
	# f2 <- ff[basename(ff) == "c_use_of_package_1.csv"]
	# d2 <- data.frame(read.csv(f2))
	# d2$trial_id <- d2$farm_id
	# d2$inoculated <- ifelse(d2$inoculant_used == "Y",TRUE,
													# ifelse(d2$inoculant_used == "N",FALSE,NA))
	
	# d2$OM_type <- carobiner::fix_name(d2$org_fertilizer_type,"title")
	# d2$OM_type <- carobiner::replace_values(d2$OM_type,
																					# c("Dung","Compost Manure","N/a"),
																					# c("Animal Dung","Compost",NA))
	
	# d2$OM_used <- ifelse(d2$OM_type %in% c("Animal Dung","Pit Manure","Compost","Chimato","Manure","Crop Residues"),TRUE,FALSE)
	
	# d2$org_fertilizer_amount_kg <- gsub("kg","",d2$org_fertilizer_amount_kg)
	# d2$org_fertilizer_amount_kg[d2$org_fertilizer_amount_kg == "N/A"] <- NA
	# d2$org_fertilizer_amount_kg <- as.numeric(d2$org_fertilizer_amount_kg)
	
	# # plot from m2 to ha
	# d2$plot_size <- d2$plot_size/10000
	
	# d2$OM_amount <- ifelse(!is.na(d2$org_fertilizer_amount_kg),d2$org_fertilizer_amount_kg/d2$plot_size,NA) # in kg/ha
	
	# d2$fertilizer_type <- carobiner::fix_name(d2$min_fertilizer_type)
	# d2$fertilizer_type <- ifelse(grepl("symp",ignore.case = T,d2$fertilizer_type),"sympal",d2$fertilizer_type)
	# d2$fertilizer_type <- gsub(", ","; ",d2$fertilizer_type)
	# d2$fertilizer_type <- carobiner::replace_values(
		# d2$fertilizer_type,
		# c("23:21:0+4s","D-Compound","D-Compound; Gypsum (25kgeach)","Super-D","None","N/A","NPK; UREA"),
		# c("NPS","D-compound","D-compound; gypsum","D-compound","none",NA,"NPK; urea"))
	
	# d2$min_fertiliser_amount_kg <- gsub("kg","",d2$min_fertiliser_amount_kg)
	# d2$min_fertiliser_amount_kg <- gsub("Kg","",d2$min_fertiliser_amount_kg)
	# d2$min_fertiliser_amount_kg[d2$min_fertiliser_amount_kg == "N/A"] <- NA
	# d2$min_fertiliser_amount_kg <- as.numeric(d2$min_fertiliser_amount_kg)
	# d2$min_fertiliser_amount_kg <- ifelse(!is.na(d2$min_fertiliser_amount_kg),d2$min_fertiliser_amount_kg/d2$plot_size,NA) # in kg/ha
	# d2$min_fertiliser_amount_kg <- ifelse(d2$fertilizer_type == "D compound; gypsum",d2$min_fertiliser_amount_kg/2,d2$min_fertiliser_amount_kg)
	
	# d2$N_fertilizer <- ifelse(d2$fertilizer_type == "NPK; urea",d2$min_fertiliser_amount_kg * 0.46,
						# ifelse(d2$fertilizer_type %in% c("NPK","Compound fertilizer"), d2$min_fertiliser_amount_kg * 0.23,
						 # ifelse(d2$fertilizer_type %in% c("D compound","D compound; gypsum"), d2$min_fertiliser_amount_kg * 0.10,NA)))
	
	# d2$P_fertilizer <- ifelse(d2$fertilizer_type == "TSP",
											# d2$min_fertiliser_amount_kg * 0.46*((2*31)/(2*31+5*16)),
											# ifelse(d2$fertilizer_type == "sympal",
																 # d2$min_fertiliser_amount_kg * 0.23*((2*31)/(2*31+5*16)),
											# ifelse(d2$fertilizer_type %in% c("NPK; urea","NPK","Compound fertilizer"),
																					# d2$min_fertiliser_amount_kg * 0.21*((2*31)/(2*31+5*16)),
											# ifelse(d2$fertilizer_type %in% c("D compound","D compound; gypsum"),
														 # d2$min_fertiliser_amount_kg * 0.20*((2*31)/(2*31+5*16)),NA)))) # takes into account atomic weight of both P and O
	
	# d2$K_fertilizer <- ifelse(d2$fertilizer_type == "sympal",d2$min_fertiliser_amount_kg * 0.15,
														# ifelse(d2$fertilizer_type %in% c("D compound","D compound; gypsum"),d2$min_fertiliser_amount_kg * 0.10,NA))
	
	# d2$crop <- carobiner::fix_name(d2$crop,"lower")
	# d2$crop[d2$crop %in% c("bush bean","beans")] <- "common bean"
	# d2$crop[d2$crop == "groundnuts"] <- "groundnut"
	# d2$crop[d2$crop == "n"] <- "common bean"
	# d2$crop <- ifelse(is.na(d2$crop) & d2$variety == "Nsinjiro","groundnut",
										# ifelse(is.na(d2$crop) & d2$variety == "kholophethe","common bean",
													 # ifelse(is.na(d2$crop) & d2$variety == "IT8216","cowpea",d2$crop)))
	
	# d2$variety1 <- carobiner::fix_name(d2$variety,"upper")
	# d2$variety1[d2$variety1 == "N/A"] <- NA
	# d2$variety1 <- gsub(" AND " ,"; ",d2$variety1)
	# d2$variety1 <- gsub(", " ,"; ",d2$variety1)
	# d2$variety1 <- gsub("/" ,"; ",d2$variety1)
	
	# i <- grepl("TSONGA",ignore.case = T,d2$variety1)
	# d2[i,"crop"] <- "common bean"
	# d2[i,"variety1"] <- "KHALATSONGA"
	# d2$variety1[d2$variety1 %in% c("NSIBJIRO","NISINJIRO","NSINKIRO","MSINJIRO")] <- "NSINJIRO"
	# d2$variety1[d2$variety1 %in% 
								# c("IT82E; 16","IT16","IT32-16E","ITE","IT","IT-16","IT82E16","IT82E-16","IT&ZE-16",
									# "IT&2E-16","IT&E-16","IT8216","IET216")] <- "IT82E16"
	# d2$variety1 <- carobiner::replace_values(
		# d2$variety1,
		# c("MAKWAHA","NSOKO","SUDAN1","SUDAN2","SUDAN3","SUADN1; IT82E16","CG&","CG7","KALIMA; DIMETER","RG1"),
		# c("MAKWACHA","NASOKO","SUDAN 1","SUDAN 2","SUDAN 3","SUDAN 1; IT82E16","CG 7","CG 7","KALIMA; DEMETER","RG 1"))
	
	# d2$variety <- d2$variety1 
	# d2 <- d2[,c( "trial_id","crop","variety","inoculated","OM_used","OM_type","OM_amount",
							 # "fertilizer_type","N_fertilizer", "P_fertilizer","K_fertilizer")]
	
	# f3 <- ff[basename(ff) == "c_use_of_package_3.csv"]
	# d3 <- data.frame(read.csv(f3))
	# d3$trial_id <- d3$farm_id
	# d3$row_spacing <- as.numeric(d3$crop_1_spacing_row_to_row)
	# d3$plant_spacing <- as.numeric(d3$crop_1_spacing_plant_to_plant)
	# d3 <- d3[,c("trial_id","row_spacing","plant_spacing")] # assumption is spacing is in cm
	
	# f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	# d4 <- data.frame(read.csv(f4))
	# d4$trial_id <- d4$farm_id
	# d4 <- d4[,c("trial_id","activity","date_planting_dd","date_planting_mm","date_planting_yyyy")]
	
	# # drop unnecessary rows
	# x <- d4[d4$activity == "Date of planting",]
	# x$date_planting_yyyy[x$trial_id %in% c("FM_MW001_20122013","FM_MW002_20122013",
																				 # "FM_MW115_20122013","FM_MW353_20122013")] <- 2012
	# x$date_planting_yyyy[x$trial_id %in% c("FM_MW005_20122013","FM_MW006_20122013")] <- 2013
	# x$planting_date <- as.character(as.Date(paste(x$date_planting_yyyy,x$date_planting_mm,x$date_planting_dd,sep = "-")))
	# x <- x[,c("trial_id","planting_date")]
	
	# y <- d4[d4$activity == "Date of harvest",]
	# y$date_planting_yyyy[y$trial_id %in% c("FM_MW001_20122013","FM_MW002_20122013","FM_MW012_20122013","FM_MW005_20122013","FM_MW006_20122013")] <- 2013
	# y$harvest_date <- as.character(as.Date(paste(y$date_planting_yyyy,y$date_planting_mm,y$date_planting_dd,sep = "-")))
	# y <- y[,c("trial_id","harvest_date")]
	# d4 <- merge(x,y,by = "trial_id", all = TRUE)
	
	# f6 <- ff[basename(ff) == "f_farmer_assessment.csv"]	
	# d6 <- data.frame(read.csv(f6))
	# d6$trial_id <- d6$farm_id
	# d6$yield <- as.numeric(d6$yield)
	# d6 <- d6[,c("trial_id","yield")] # yield's units are unknown and are rated from 1 through 4
	
	# # combining into one dataset
	# z <- Reduce(function(...) merge(..., all=T), list(d,d1,d2,d3,d4,d6))	 
	# z$is_survey <- TRUE
	# z$on_farm <- FALSE
	# z$dataset_id <- dataset_id
	# z$country <- "Malawi"
	
	# # dropping NA yield
	# z <- z[!is.na(z$yield),]
	
	# # filling for NA coordinates
	# z$latitude <- ifelse(z$site == "Salima" & is.na(z$latitude),-13.762898400000001,
								# ifelse(z$site == "Kasungu" & is.na(z$latitude),-12.992487650000001,
								# ifelse(z$site == "Lilongwe" & is.na(z$latitude),-14.04141045,
								# ifelse(z$site == "Dedza" & is.na(z$latitude),-14.2374479,
								# ifelse(z$site == "Mzimba" & is.na(z$latitude),-11.845810199999999,z$latitude)))))
	
	# z$longitude <- ifelse(z$site == "Salima" & is.na(z$longitude),34.452369976445496,
								 # ifelse(z$site == "Kasungu" & is.na(z$longitude),33.472356695915096,
								 # ifelse(z$site == "Lilongwe" & is.na(z$longitude),33.735370838170624,
								 # ifelse(z$site == "Dedza" & is.na(z$longitude),34.31965143966403,
								 # ifelse(z$site == "Mzimba" & is.na(z$longitude),33.55783191131472,z$longitude))))) 

	# z$longitude <- ifelse(z$adm1 == "Kasungu" & z$latitude == -14.00000 & z$longitude == 33.20000, 33.3,
				 # ifelse(z$adm1 == "Kasungu" & z$latitude == -12.20000 & z$longitude == 33.30000, 33.4,
				 # ifelse(z$adm1 == "Kasungu" & z$latitude == -12.30000 & z$longitude == 33.30000, 33.4,
				 # ifelse(z$adm1 == "Kasungu" & z$latitude == -12.40000 & z$longitude == 33.30000, 33.6,
				 # ifelse(z$adm1 == "Kasungu" & z$latitude == -12.40000 & z$longitude == 33.40000, 33.6,
				 # ifelse(z$adm1 == "Dedza" & z$latitude == -14.20000 & z$longitude == 35.50000, 35.4,
				 # ifelse(z$adm1 == "Dedza" & z$latitude == -13.20000 & z$longitude == 34.90000, 34.3,
				 # ifelse(z$adm1 == "Mzimba" & z$latitude == -12.20000 & z$longitude == 33.30000, 33.4,
				 # ifelse(z$adm1 == "Mchinji" & z$latitude == -14.0 & z$longitude == 33.20000, 33.3,
							# z$longitude)))))))))
	
	# # filling for trial_id's without crop information
	# z$crop[is.na(z$crop)] <- "no crop"
	# z$N_fertilizer[is.na(z$N_fertilizer)] <- 0
	# z$P_fertilizer[is.na(z$P_fertilizer)] <- 0
	# z$K_fertilizer[is.na(z$K_fertilizer)] <- 0
	
	# z <- z[,c("trial_id","season","country","adm1","adm2","adm3","site","longitude","latitude","elevation","observation_date"
						# ,"planting_date","harvest_date","crop","variety","inoculated","OM_used","OM_type","OM_amount","fertilizer_type","N_fertilizer",
						# "P_fertilizer","K_fertilizer","row_spacing","plant_spacing","yield","on_farm","is_survey")]
	
	# carobiner::write_files(meta, z, path, dataset_id, group)
# }
