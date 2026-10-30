# R script for "carob"

carob_script <- function(path) {
	
"Availability of multiple-stress tolerant maize is critical for improvement in maize production in West and Central Africa (WCA). A study was carried out to (i) assess a set of inbred lines for combining ability under stressed and optimal conditions, (ii) determine the performance of the testcrosses under different conditions, and (iii) identify outstanding hybrids across the conditions. Two hundred and five testcrosses were planted with five hybrid checks under Striga-infested, low soil nitrogen, drought and optimal conditions between 2015 and 2016 in Nigeria. The grain yield inheritance under optimal condition was largely regulated by additive gene effect whereas non-additive gene effects largely regulated grain yield under the three stresses. Four of the inbreds had significant positive general combining ability effects each under low N and drought, and three under Striga infestation for grain yield. The inbreds could be vital sources of beneficial alleles for development and improvement of tropical yellow maize hybrids and populations. Hybrids TZEI 443 x ENT 13 and TZEI 462 x TZEI 10 were high yielding and stable; they out-performed the three early maturing released hybrids in WCA. The new hybrids should be extensively assessed and released in the sub-region to improve food security.
"
	
	uri <-  "doi:10.25502/K7Q1-BW26/D"
	group <- "maize_trials" 
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		publication= "doi: 10.1038/s41598-019-50345-3",
		data_organization = "IITA",
		carob_contributor="Cedric Ngakou",
		data_type="experiment",
		project=NA,
		carob_date="2023-10-03"	  
	)
	
	bn <- basename(ff)
	r <- read.csv(ff[bn=="BA15188 ACROSS.csv"])  
	
	d <- r[,c("ID","LOC","Rep","Entry","Pedigree","YIELD","PLST","POLLEN","DYSK","ASI","PLHT","EHT","PASP","PHARV","EHARV","EROT","FWT","CO1","CO2","RAT1","RAT2")]#
	 
	colnames(d) <- c("ID","location","rep","variety_code","variety","yield","pl_st","dy_poll","dy_sk","asi","pl_ht","e_ht","p_asp","p_harv","e_harv","e_rot","fwtun","str_co1","str_co2","str_rat1","str_rat2")#,
	
	d$country <- "Nigeria"
	d$crop <- "maize" 
	
	d$trial_id <- paste(d$ID,d$location,sep = "-")
	d$yield_part <- "grain" 
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$borer_trial <- FALSE
	d$striga_infected <- FALSE
	d$ID <- NULL
	d$striga_trial <- FALSE

	d$treatment <- d$treatment
	d$treatment[i] <- "striga"
	i <- grepl("STR", d$treatment)
	d$striga_trial[i] <- TRUE


message("Cedric: what are DT, OPT, LN, HN?")

	#add season 
	d$planting_date <- "2015"
	d$season <- "2015"
	j <- grepl("16", d$location)
	d$season[j] <- "2016"
	d$planting_date[j] <- "2016"
	j <- grepl("KDW16DT" ,d$location) 
	d$season[j] <- "2015-2016"
	d$planting_date[j] <- "2015"
	j <- grepl("IK15DT" ,d$location) | grepl("MI15DT",d$location)
	d$season[j] <- "2014-2015"
	d$planting_date[j] <- "2014"
# fix location 
 
	d$location[grepl("MI15DT", d$location)] <- "Minjibir"
	d$location[grepl("^IK1", d$location)] <- "Ikenne"
	d$location[d$location == "KDW16DT"] <- "Kadawa"	
	d$location[grepl("^MK1" ,d$location)] <- "Mokwa"
	
	d$location[grepl("^IF1", d$location)] <- "Ile-Ife"  
	d$location[grepl("^AB1", d$location)] <- "Abuja"
	### add coordinates
	geo <- data.frame(location=c("Minjibir","Ikenne","Kadawa","Mokwa","Ile-Ife","Abuja"),
						  latitude=c(12.177, 6.901, 11.633, 9.29, 6.7604,9.064),
						  longitude=c( 8.659, 3.698,  8.434, 5.06, 6.207, 7.489))
	
	
	d <- merge(d, geo, by="location")

	# fill whitespace in str_co1, str_rat1, str_rat2
	d$str_co1[d$str_co1==""] <- NA
	d$str_rat1[d$str_rat1==""] <- NA
	d$str_rat2[d$str_rat2==""] <- NA
	#data type
	d$variety_code <- as.character(d$variety_code)
	

	carobiner::write_files(meta, d, path=path)
	
}


