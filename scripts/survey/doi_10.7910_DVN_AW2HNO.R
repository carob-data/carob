# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Understanding Smallholder Private Irrigation in Abaji, Federal Capital Territory, Nigeria

Understanding the nature of small-scale private irrigation systems in countries like Nigeria is important. This survey provides relevant information from farmers using irrigation to cultivate four major crops, rice, maize, pepper, and okra. The survey focuses on two villages in Abaji Area Council within the Federal Capital Territory because the area has seen the growths of private irrigation recently, but relative to the drier northern Nigeria, these irrigation systems in the more humid areas in Central Nigeria have been understudied. The survey was conducted in late May 2017.

The dataset consist of 178 farmers who irrigated at least one of their plots in 2017 season for growing either rice, maize, pepper or okra. It also includes information on various irrigation activities from the largest irrigated plot for each farm household. The information collected covers plot information, irrigation equipment / sources, inputs uses and outputs, irrigation adoption history, and household characteristics.
"

	uri <- "doi:10.7910/DVN/AW2HNO"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IFPRI",
		publication = "http://ebrary.ifpri.org/cdm/ref/collection/p15738coll2/id/131437",
		project = NA,
		carob_date = "2026-03-26",
		design = "unitOfAnalysis",	
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "002_codebook.xls"]
	f2 <- ff[basename(ff) == "003_dataset.dta"]

	r1 <- carobiner::read.excel(f1)
	r2 <- haven::read_dta(f2) |> carobiner:::unlabel()



	d1 <- data.frame(
		hhid = as.character(r2$nigeria_irrig_17_id),
		plot_area = r2$plotarea_GPS2,
		land_tenure = r2$q_2_1,
		irrigated_rice = grepl("yes", r2$q_2_4),
		irrigated_maize = grepl("yes", r2$q_2_8),
		irrigated_pepper = grepl("yes", r2$q_2_12),
		irrigated_okra = grepl("yes", r2$q_2_16),   
		season_rice = r2$q_2_5,
		season_maize = r2$q_2_9,
		season_pepper = r2$q_2_13,
		season_okra = r2$q_2_17,
		irrigation_number = ifelse(grepl("one", r2$q_3_13), 1L, 
		                  ifelse(grepl("two", r2$q_3_13), 2L, NA)),
		
		latitude = ifelse(is.na(r2$q_3_14a_1)& !is.na(r2$q_3_14a_2), r2$q_3_14a_2,
		                   ifelse(is.na(r2$q_3_14a_1)& !is.na(r2$q_3_14a_3), r2$q_3_14a_3,r2$q_3_14a_1)) ,
		
		longitude = ifelse(is.na(r2$q_3_14b_1)& !is.na(r2$q_3_14b_2), r2$q_3_14b_2,
		                   ifelse(is.na(r2$q_3_14b_1)& !is.na(r2$q_3_14b_3), r2$q_3_14b_3,r2$q_3_14b_1)) ,
		crop_rice = r2$q_4_0_1,
		crop_maize = r2$q_4_0_2,
		crop_pepper = r2$q_4_0_3,
		crop_okra = r2$q_4_0_4,
		fertilizer_time_rice = r2$q_4_90r_1,
		fertilizer_time_maize = r2$q_4_90r_2,
		fertilizer_time_pepper = r2$q_4_90r_3,
		fertilizer_time_okra = r2$q_4_90r_4,
		herbicide_time_rice = as.integer(r2$q_4_91r_1),
		herbicide_time_maize = as.integer(r2$q_4_91r_2),
		herbicide_time_pepper = as.integer(r2$q_4_91r_3),
		herbicide_time_okra = as.integer(r2$q_4_91r_4),
		
		insecticide_time_rice = as.integer(r2$q_4_92r_1),
		insecticide_time_maize = as.integer(r2$q_4_92r_2),
		insecticide_time_pepper = as.integer(r2$q_4_92r_3),
		insecticide_time_okra = as.integer(r2$q_4_92r_4),
		
		fertilizerNPK_amount_rice = ifelse(grepl("50kg", r2$q_4_95rb_1), r2$q_4_95ra_1*50, r2$q_4_95ra_1) ,
		fertilizerNPK_amount_maize = ifelse(grepl("50kg", r2$q_4_95rb_2), r2$q_4_95ra_2*50, r2$q_4_95ra_2),
		fertilizerNPK_amount_pepper = ifelse(grepl("50kg", r2$q_4_95rb_3), r2$q_4_95ra_3*50, r2$q_4_95ra_3),
		fertilizerNPK_amount_okra = ifelse(grepl("50kg", r2$q_4_95rb_4), r2$q_4_95ra_4*50, r2$q_4_95ra_4),
		fertilizerurea_amount_rice = ifelse(grepl("50kg", r2$q_4_94rb_1), r2$q_4_94ra_1*50, r2$q_4_94ra_1),
		fertilizerurea_amount_maize =ifelse(grepl("50kg", r2$q_4_94rb_2), r2$q_4_94ra_2*50, r2$q_4_94ra_2),
		fertilizerurea_amount_pepper = ifelse(grepl("50kg", r2$q_4_94rb_3), r2$q_4_94ra_3*50, r2$q_4_94ra_3),
		fertilizerurea_amount_okra = ifelse(grepl("50kg", r2$q_4_94rb_4), r2$q_4_94ra_4*50, r2$q_4_94ra_4),
		
		herbicide_price_rice = r2$q_4_97r_1,
		herbicide_price_maize = r2$q_4_97r_2,
		herbicide_price_pepper = r2$q_4_97r_3,
		herbicide_price_okra = r2$q_4_97r_4,
		yield_rice = r2$q_5_4ra_1,
		yield_maize = r2$q_5_4ra_2,
		yield_pepper = r2$q_5_4ra_3,
		yield_okra = r2$q_5_4ra_4,
		convert_rice = gsub("k", "", substr(r2$q_5_4rb_1, 1, 3)),
		convert_maize = gsub("k", "", substr(r2$q_5_4rb_2, 1, 3)),
		convert_pepper = gsub("k", "", substr(r2$q_5_4rb_3, 1, 3)),
		convert_okra =  gsub("k", "", substr(r2$q_5_4rb_4, 1, 3)),
		previous_crop_residue_perc_rice = r2$q_5_10r_1,
		previous_crop_residue_perc_maize = r2$q_5_10r_2,
		previous_crop_residue_perc_pepper = r2$q_5_10r_3,
		previous_crop_residue_perc_okra = r2$q_5_10r_4,
		crop_price_rice = r2$q_5_11r_1,
		crop_price_maize = r2$q_5_11r_2,
		crop_price_pepper = r2$q_5_11r_3,
		crop_price_okra = r2$q_5_11r_4,
		location = carobiner::fix_name(r2$q_6_2b, "title") ,
		farmer_age = r2$q_7_1,
		farmer_gender = r2$q_7_2,
		farmer_education_level = r2$q_7_3
		
	)
	
	
	d <- reshape(d1, varying = list(c("irrigated_rice", "irrigated_maize", "irrigated_pepper", "irrigated_okra"),
	                               c("season_rice", "season_maize", "season_pepper", "season_okra"), 
	                               c("crop_rice", "crop_maize", "crop_pepper", "crop_okra"),
	                               c("fertilizer_time_rice", "fertilizer_time_maize", "fertilizer_time_pepper", "fertilizer_time_okra"),
	                               c("herbicide_time_rice", "herbicide_time_maize", "herbicide_time_pepper", "herbicide_time_okra"),
	                               c("insecticide_time_rice", "insecticide_time_maize", "insecticide_time_pepper", "insecticide_time_okra"),
	                               c("fertilizerNPK_amount_rice", "fertilizerNPK_amount_maize", "fertilizerNPK_amount_pepper", "fertilizerNPK_amount_okra"),
	                               c("fertilizerurea_amount_rice", "fertilizerurea_amount_maize", "fertilizerurea_amount_pepper", "fertilizerurea_amount_okra"),
	                               c("herbicide_price_rice", "herbicide_price_maize", "herbicide_price_pepper", "herbicide_price_okra"),
	                               c("yield_rice", "yield_maize", "yield_pepper", "yield_okra"),
	                               c("convert_rice", "convert_maize", "convert_pepper", "convert_okra"),
	                               c("previous_crop_residue_perc_rice", "previous_crop_residue_perc_maize", "previous_crop_residue_perc_pepper", "previous_crop_residue_perc_okra"),
	                               c("crop_price_rice", "crop_price_maize", "crop_price_pepper", "crop_price_okra")),
	            v.names = c("irrigated", "season", "crop", "fertilizer_times", "herbicide_times", "insecticide_times", 
	                        "fertilizer_amount1", "fertilizer_amount2", "herbicide_price", "yield","convf", "previous_crop_residue_perc", "crop_price"),
	            direction = "long"
	            )
	d[d== ""] <- NA
	d <- d[!is.na(d$crop),]
   d$fertilizer_type <- ifelse(!is.na(d$fertilizer_amount1) & is.na(d$fertilizer_amount2), "NPK",
                        ifelse(!is.na(d$fertilizer_amount2) & is.na(d$fertilizer_amount1), "urea", 
                        ifelse(!is.na(d$fertilizer_amount1) & !is.na(d$fertilizer_amount2), "NPK;urea", "none"))) 
	
   d$fertilizer_amount <- (rowSums(d[, c("fertilizer_amount1", "fertilizer_amount2")], na.rm = TRUE))/d$plot_area ## kg/ha
   d$convf <- as.numeric(gsub("oth", "", d$convf))
   d$crop_price <- d$crop_price/(d$yield*d$convf) ## 
   d$yield <- d$yield*d$convf/d$plot_area ## kg/ha 
	
	d$id <- d$time <- d$fertilizer_amount1 <- d$fertilizer_amount2 <- d$convf <- NULL
   
	### fixing Lon and lat coordinates
	i <- grepl("Bauchi", d$location) & is.na(d$longitude)
	d$longitude[i] <- 6.79414
   d$latitude[i] <- 8.656800
   i <- grepl("Katsina", d$location) & is.na(d$longitude)
   d$longitude[i] <- 6.80592
   d$latitude[i] <- 8.711180
   
	### Fixing season names
	
	P <- carobiner::fix_name(d$season)
	P <- gsub("dry season only", "dry", P)
	P <- gsub("rainy season only", "wet", P)
	P <- gsub("both seasons", NA, P)
	d$season <- P 
	
	d$country <- "Nigeria"
	d$currency = "NGN"
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$planting_date <- as.character(NA)
	d$geo_from_source <- TRUE
	d$yield_part <- "none"
	d$trial_id <- paste(d$location, d$hhid, sep="-")
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	## drop two rows with missing longitude, latitude and location
	d <- d[!is.na(d$longitude),] 
	
carobiner::write_files(path, meta, d)
}


