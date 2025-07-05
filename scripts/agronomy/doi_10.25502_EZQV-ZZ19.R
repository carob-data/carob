# R script for "carob"


carob_script <- function(path){
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
          
	uri <- "doi:10.25502/EZQV-ZZ19"
	group <- "agronomy"
	ff <- carobiner::get_data(uri,path,group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication = "doi:10.1016/j.agee.2017.08.015",
		carob_contributor = "Andrew Sila",
		carob_date="2023-07-17",
		data_type = "on-farm experiment",
		data_organization="IITA;WUR",
		response_vars= "yield",
		treatment_vars="inoculated;P_fertilizer"
	)

	
	# read the experiment data table
	f0 <- ff[basename(ff) == "experiment.csv"]
	r2 <- read.csv(f0)
	
	f1 <- ff[basename(ff) == "general.csv"]
	r1 <- read.csv(f1)
	
	
	f5 <- ff[basename(ff) == "production.csv"]
	r3 <- read.csv(f5)
	

  #assemble everything for crop 1 

	colnames(r2)[colnames(r2) %in% c("width_of_harvested_plot_crop_1_plot_1", "pod_weight_unshelled_grain_groundnut_crop_1_plot_2.kg","grain_weight_shelled_grain_crop_1_plot_2.kg")] <- c("width_of_harvested_plot_crop_1_plot_1.m", "pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_2.kg","grain_weight_kg_shelled_grain_crop_1_plot_2.kg")
	
	t <- c("name_treatment_X", "description_treatment_X","width_of_harvested_plot_crop_1_plot_X.m","depth_of_harvested_plot_perpen_dicular_to_rows_crop_1_plot_X.m","number_of_rows_in_plot_crop_1_plot_X","grain_weight_kg_shelled_grain_crop_1_plot_X.kg","pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_X.kg","above_ground_biomass_weight_husks_stover_res_crop_1_plot_X.kg") 
	x <- c("treatment","description","width","length","row_count","seed_weight","pod_weight","fwy_residue")	

	lst <- list()
	for (i in 1:8) {
	  inms <- gsub("X", i, t)
	  ri <- r2[, inms]
	  colnames(ri) <- x
	  lst[[i]] <- ri
	}
	
	dd <- do.call(rbind, lst)
	dd$trial_id <- r2$farm_id
	dd$crop <- carobiner::replace_values(r2$experimental_treatments_crop_1, c("","Groundnut","Soya bean","soya bean","Cowpea","Maize","SOYABEANS","SOYBEAN","COWPEA","SOYA BEAN","G per NUT"),c(NA,"groundnut","soybean","soybean","cowpea","maize","soybean","soybean","cowpea","soybean","groundnut"))
	dd$variety <- carobiner::fix_name(r2$experimental_treatments_variety_crop_1)
	dd$trial_id <- r2$farm_id
 
	
	# assembling everything for crop 2
	t <- c("name_treatment_X", "description_treatment_X","width_of_harvested_plot_crop_2_plot_X.m","depth_of_harvested_plot_perpen_dicular_to_rows_crop_2_plot_X.m","number_of_rows_in_plot_crop_2_plot_X","grain_weight_kg_shelled_grain_crop_2_plot_X.kg","pod_weight_kg_unshelled_grain_groundnut_crop_2_plot_X.kg","above_ground_biomass_weight_husks_stover_res_crop_2_plot_X.kg")
	x <- c("treatment","description","width","length","row_count","seed_weight","pod_weight","fwy_residue")
	
	lst <- list()
	for (i in 1:8) {
	  inms <- gsub("X", i, t)
	  ri <- r2[, inms]
	  colnames(ri) <- x
	  lst[[i]] <- ri
	}
	
	ddd <- do.call(rbind, lst)
	ddd$trial_id <- r2$farm_id
	ddd$crop <- carobiner::replace_values(r2$experimental_treatments_crop_2,c("","Groundnut","Soya bean","Farmer local variety","Maize","soya bean","Cowpea","SOYBEAN","MAIZE","G per NUT","SOYABEANS"),c(NA,"groundnut","soybean","groundnut","maize","soybean","cowpea","soybean","maize","groundnut","soybean"))

	ddd$variety <- carobiner::fix_name(r2$experimental_treatments_variety_crop_2)
	ddd$trial_id <- r2$farm_id

	
	d0 <- carobiner::bindr(dd,ddd)
	
  #filling in the inoculated 
	d0$treatment <- gsub("\\s*([+-])\\s*", "\\1", d0$treatment) # removing spaces between + or - 
	d0$description <- gsub("\\s*([+-])\\s*I\\s*", "\\1I", d0$description)# removing spaces between + or - 
	d0$inoculated <- ifelse(grepl("\\+i", d0$treatment), TRUE, FALSE)
	
	
	#filling in the varieties
	i <- grepl("samnut \\d{2}", d0$treatment) | grepl("evdt 2009", d0$treatment)| grepl("tgx\\d{4}-\\d[a-zA-Z]", d0$treatment)|grepl("tgx\\d{4}", d0$treatment)|grepl("^it \\S+", d0$treatment)
	t <- is.na(d0$variety)
	d0$variety[i & t] <- gsub(".*(samnut \\d{2}|evdt 2009|tgx\\d{4}-\\d[a-zA-Z]|tgx\\d{4}|^it \\S+).*", "\\1", d0$treatment[i & t])
	
	
	#fill in the crops
	d0$crop[grepl("[+-]i", d0$treatment)|grepl("[+-]I", d0$description) & is.na(d0$crop)] <- "soybean"
	d0$crop[(grepl("^it\\s*\\S*|cowpea|^uam\\s*\\S*", d0$treatment) | grepl("^it\\s*\\S*", d0$description) | grepl("(?i)^uam", d0$description)) & is.na(d0$crop)] <- "cowpea"
	d0$crop[grepl("\\s*samnut \\d{2}", d0$treatment) & is.na(d0$crop)] <- "groundnut"
	d0$crop[(grepl("evdt\\s*\\S+|cereal|maize|evdt", d0$treatment)) & is.na(d0$crop)] <- "maize"
	

	# efyrouwa: what should be used to calculate yield?, seed_weight or pod_weight?, 
	##  I used seed_weight, in cases there's no seed_weight, I used pod_weight

    ##RH: as long as you specify that in yield_part!
		
	d0$length[d0$length == 0.75] <- 10 #to change that one entry with 0.75 as the length
	d0$yield <- 10000 / (d0$width*d0$length) * ifelse(is.na(d0$seed_weight), d0$pod_weight , d0$seed_weight)
	
	d0$fwy_residue <- 10000 / as.numeric(d0$width * d0$width) * d0$fwy_residue
	d0$fertilizer_type[grepl("\\+p|\\+ p", d0$treatment, perl = TRUE)] <- "SSP"
	d0$SSP_amt_per_plot[grepl("SSP",d0$fertilizer_type)] <- 2
	
	#calculating the fertilizer different
	d0$P_fertilizer <- (0.0874* d0$SSP_amt_per_plot / (as.numeric(d0$width * d0$length) / 10000)) #p in SSP taken as 8.74
	d0$P_fertilizer[is.na(d0$P_fertilizer)] <- 0
	d0$N_fertilizer <- 0
	d0$K_fertilizer <- 0
	d0 <- d0[!duplicated(d0), ]
	d0$treatment <- carobiner::fix_name(d0$treatment)
	d0 <- d0[, c("trial_id","treatment","fertilizer_type","inoculated","N_fertilizer","P_fertilizer","K_fertilizer","crop","yield", "fwy_residue")]
  
	#the next dataset 
	d1 <- carobiner::change_names(r1,c("district","sector_ward","village"),c("adm1","adm2","location"))
	d1$country <- carobiner::fix_name(d1$country, "title")
	d1$adm1[ d1$adm1 %in% c("Kaduna State", "Kaduna", "KADUNA", "KADUNA per  ZANGON AYA") ] <- "Kaduna"
	d1$adm1 <- carobiner::replace_values(d1$adm1,c("Kano State","Niger State"), c("Kano","Niger"))
	d1$is_survey <- TRUE
	d1$on_farm <- FALSE
	d1$trial_id <- r1$farm_id
	d1$date <- as.character(as.Date(paste(r1$date_hhsurvey_dd.days, r1$date_hhsurvey_mm.months, r1$date_hhsurvey_yyyy.years, sep = "-"), format = "%d-%B-%Y"))
	d1$harvest_date <- as.character(as.Date(paste(r1$date_harvest_dd_technician_1, r1$date_harvest_mm_technician_1, r1$date_harvest_yyyy_technician_1, sep = "-"), format = "%d-%B-%Y"))
	d1$latitude <- r1$gps_latitude_hh.decimal_degrees
	d1$longitude <- r1$gps_longitude_hh.decimal_degrees
	
	
	d1 <- d1[,c("trial_id","country","latitude","longitude","adm1","adm2","location","date","harvest_date","is_survey","on_farm")]	
	
	d <- merge(d1, d0, by = "trial_id", all = TRUE)
	d$yield_part <- "seed"
	
	##efyrouwa: How can we incorporate r3 with d??
	
	# all scripts should end like this

	d <- d[!is.na(d$crop), ]
	d <- d[!is.na(d$yield), ]
	d$planting_date <- as.character(NA)
	
	i <- d$location == "Danmaliki"
	d$geo_from_source <- TRUE
	d$longitude[i] <- 8.82
	d$latitude[i] <- 11.642
	d$geo_from_source[i] <- FALSE
	d$irrigated <- NA
	
	carobiner::write_files(meta, d, path=path)
}

