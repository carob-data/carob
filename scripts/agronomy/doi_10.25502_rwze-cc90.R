# R script for "carob"

# not included because there no yield data are provided. So these data are not in scope for us.

"
N2Africa farm monitoring - Mozambique, 2011 - 2012, II

N2Africa is to contribute to increasing biological nitrogen fixation and productivity
of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility,
improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

carob_script <- function(path) {
  
	uri <- "doi:10.25502/rwze-cc90"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
	 
	## data set level data
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication=NA,
		data_organization = "IITA;ICRAF;WUR",
		carob_contributor="Rachel Mukami",
		carob_date="2022-09-28",
		data_type="survey"
	)
	
	meta$dataset_id <- paste0(meta$dataset_id, "_nodata")
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta)
}


	# # reading the datasets
	# f <- ff[basename(ff) == "a_general.csv"]
	# d <- data.frame(read.csv(f))
	# d$trial_id <- d$farm_id
	# d$adm1 <- carobiner::fix_name(d$district, "title")
	# d$adm2 <- carobiner::fix_name(d$sector_ward, "title")
	# d$adm3 <- carobiner::fix_name(d$vilage, "title")
	# d <- d[,c("trial_id","season","country","adm1","adm2","adm3")]
	
	# f1 <- ff[basename(ff) == "c_use_of_package_1.csv"]
	# d1 <- data.frame(read.csv(f1))
	# d1$trial_id <- d1$farm_id
	# d1 <- d1[,c("trial_id","plot_size")] # assumption is plot size is in m2

	# f2 <- ff[basename(ff) == "c_use_of_package_2.csv"]
	# d2 <- data.frame(read.csv(f2))
	# d2$trial_id <- d2$farm_id
	# d2$inoculated <- ifelse(d2$inoculant_used %in% c("Y","Yes","Biagro"),"yes",	 # BioAgro is a biotechnology company that provides a leguminous inoculant for soybean crops named Nodulest 10.
													# ifelse(d2$inoculant_used %in% c("N","no"),"no",NA))
	# d2$variety <- d2$variety_1 
	# d2$crop <- tolower(d2$crop_1)
	# d2$crop[d2$crop != "groundnut"] <- "soybean"
	
	# d2$fertilizer_type <- ifelse(d2$mineral_fert_amount != 0,d2$mineral_fert_type,NA)
	# d2$plot_size <- ifelse(d2$trial_id %in% d1$trial_id,d1$plot_size,NA)
	# d2$N_fertilizer <- ifelse(d2$mineral_fert_type %in% c("Ureia","Urea","UREA"),((d2$mineral_fert_amount*10000)/d2$plot_size)*0.46,NA) # convert to kg/ha assuming application amount is in kg assuming plot size is in m2
	# d2$P_fertilizer <- ifelse(d2$mineral_fert_type %in% c("Ssp","SSp","P-SSP ( 10,5%)","P-SSP (10,5%)","P-SSP",
																												# "P- SSP	10,5%","SSP (P)","SSP ","P- SSP 10,5%","ssp","SSP 10,5%","SSP"),
																												# ((d2$mineral_fert_amount*10000)/d2$plot_size)*0.145,NA)
	# d2 <- d2[,c( "trial_id","crop","variety","inoculated","fertilizer_type","N_fertilizer", "P_fertilizer")]
	
	# f3 <- ff[basename(ff) == "c_use_of_package_4.csv"]
	# d3 <- data.frame(read.csv(f3))
	# d3$trial_id <- d3$farm_id
	# d3$row_spacing <- d3$crop_1_spacing_row_to_row
	# d3$plant_spacing <- d3$crop_1_spacing_plant_to_plant
	# d3 <- d3[,c("trial_id","row_spacing","plant_spacing")] # assumption is spacing is in cm

	
	# f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	# d4 <- data.frame(read.csv(f4))
	# d4$trial_id <- d4$farm_id
	# d4$planting_date <- as.Date(paste(d4$date_planting_yyyy,d4$date_planting_mm,d4$date_planting_dd,sep = "-"))
	# d4$harvest_date <- as.Date(paste(d4$date_harvest_yyyy,d4$date_harvest_mm ,d4$date_harvest_dd,sep = "-"))
	# d4 <- d4[,c("trial_id","planting_date","harvest_date")]
	
	# f5 <- ff[basename(ff) == "e_harvest.csv"]
	# d5 <- data.frame(read.csv(f5))
	# d5$trial_id <- d5$farm_id
	# d5$seed_weight <- d5$crop_1_weight_grain * 1000 # standardizing to grain weight measured in g assuming grain weight is in kg
	# d5 <- d5[,c("trial_id","seed_weight")]
	
	# f6 <- ff[basename(ff) == "g_farmer_assessment.csv"]	
	# d6 <- data.frame(read.csv(f6))
	# d6$trial_id <- d6$farm_id
	
	# # yield units are unknown 
	# ## RH: that is not correct! 
	# ## there are no numbers. What is provided is a relative performance: "better"
	# ## that is interesting, but out of scope for us 
	
	
	# d6 <- d6[,c("trial_id","yield")] 
	
	# # combining into one dataset
	# z <- carobiner::bindr(d,d1,d2,d3,d4,d5,d6)
	# z$is_survey <- "yes"
	# z$on_farm <- "no"
	# z$longitude <- 35.52956 
	# z$latitude <- -18.66569
	# z$dataset_id <- dataset_id
	# z$country <- "Mozambique"
	# z$crop <- ifelse(z$trial_id %in% d2$trial_id,d2$crop,NA)
 
 
	# z <- z[,c("trial_id","season","country","adm1","adm2","adm3","crop","variety",
						# "planting_date","harvest_date","inoculated","fertilizer_type","N_fertilizer","P_fertilizer",
						# "seed_weight","yield","row_spacing","plant_spacing","on_farm","is_survey","longitude","latitude")]
	
	
	# carobiner::write_files(meta, z, path, dataset_id, group)
	# TRUE
# }
