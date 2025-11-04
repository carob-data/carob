# R script for "carob"


carob_script <- function(path){

"N2Africa was aimed at increasing biological nitrogen fixation and productivity of grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings which was aimed at increasing soil fertility.The trails were conducted in 11 african countries"

	uri <- "doi:10.25502/a7ex-ea51/d"
	group <- "agronomy"
	ff <- carobiner::get_data(uri,path,group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication = "doi:10.1016/j.agee.2017.08.015",
		carob_contributor = "Effie Ochieng'",
		carob_date="2022-08-05",
		data_type = "on-farm experiment",
		data_organization="IITA;ICRAF;WUR",
		response_vars = "yield",
		treatment_vars = "inoculated;P_fertilizer"			
	)

	f <- ff[basename(ff) == "data.csv"]
	d <- data.frame(read.csv2(f, sep = ","))
	
	f1 <- ff[basename(ff) == "general.csv"]
	d1 <- data.frame(read.csv2(f1, sep = ","))

	d1$trial_id <- d1$experiment_id
	d1$country <- d1$country
	d1$first_name <- trimws(sapply(strsplit(as.character(d1$action_site), "/"), `[`, 1))
	d1$last_name <- trimws(sapply(strsplit(as.character(d1$action_site), "/"), `[`, 2))


	d1$adm2 <- ifelse(d1$first_name %in% c("Bugesera","Kamonyi","Kayonza","Burera","Musenyi"), d1$first_name,
				ifelse(d1$last_name %in% "Musanze", d1$last_name, NA))
	
	d1$adm3 <- ifelse(d1$first_name %in% c("Rwaza", "Cyabingo", "Cyuve", "Nyamata", "Mareba", "Musenyi", "Nyamirama","Muko","cyuve", "Musambira"), d1$first_name,
				ifelse(d1$last_name %in% c("Musenyi", "Mareba", "Musambira", "Nyarubaka", "Nyarubaka", "Nyamiyaga", "Nyamirama", "Rukara", "Rukara", "Kinoni"),d1$last_name,NA))
	
	d1$adm3 <- carobiner::fix_name(d1$adm3, "title")

	crds <- data.frame(adm3=c('Cyuve', 'Cyabingo', 'Kinoni', 'Mareba', 'Muko', 'Musambira', 'Musenyi', 'Nyamata', 'Nyamirama', 'Nyamiyaga', 'Nyarubaka', 'Rukara', 'Rwaza'), 
	                   longitude=c(29.65572, 29.69453, 29.73912, 30.06256, 30.60027, 29.8395, 30.0297, 30.09802, 30.52925, 29.76667, 29.83537, 30.4725, 29.66713), 
	                   latitude=c(-1.47401, -1.58553, -1.46114, -2.25791, -2.05674, -2.02396, -2.18864, -2.14395, -1.95002, -2.43333, -2.08663, -1.8005, -1.55886),
	                   geo_from_source = FALSE)

	d1 <- d1[, c("trial_id", "country","adm2","adm3")]
	d1 <- merge(d1, crds, by="adm3", all.x=TRUE)
	
	#fill in crop
	d$variety <- carobiner::fix_name(d$variety)
	v <- carobiner::replace_values(d$variety, c("Peka6", "SB8","SB24"),
									c("Peka 6", "SB 8","SB 24"))
	d$crop <- ""
	d$crop <- ifelse(is.na(d$variety), d$sub_treatment_inoc, "")
	d$crop[d$variety %in% c("Peka 6", "Peka6", "Sc. Saga", "Sc. Square", "Sc. Squille", "SB8", "SB24")] <- "soybean"
	d$crop[d$variety %in% c("RWA 1668", "Gasirida", "RWR 1668")] <- "common bean"
	v <- d$crop ==""
	d$crop[v] <- d$main_treatment[v]
	v <- carobiner::replace_values(d$crop,c("Weed Fallow","Maize"),c("Weed fallow","maize"))
	v <- gsub("Maize & Sorghum","maize; sorghum", v)
	d$crop <- v
	d <- d[!(d$crop %in% c("maize","maize; sorghum","Weed fallow")),]

	#process data sets separately identifying the variables of interest
	d$trial_id <- d$experiment_id
	d$rep <- d$replication_no
	d$on_farm <- TRUE
	d$planting_date <- as.character(as.Date(paste(d$planting_date_yyyy, d$planting_date_mm, d$planting_date_dd, sep = "-")))
	d$harvest_date <- as.character(as.Date(paste(d$date_harvest_yyyy, d$date_harvest_mm, d$date_harvest_dd, sep = "-")))
	d$treatment <- paste(d$main_treatment, d$sub_treatment_inoc, d$sub_treatment_fert, sep = "-")

	d$seed_weight <- as.numeric(d$dry_weight_100_seeds)*10 # to get 1000 seed weight

	
	#getting the biomass total 
	d$above_ground_dry_biomass <- as.numeric(d$above_ground_dry_biomass)/1000 #  convert to kg
	d$area_biomass_sampling <- as.numeric(d$area_biomass_sampling)
	d$dmy_total <- 10000 * d$above_ground_dry_biomass / d$area_biomass_sampling  # to get kg/ha
	i <- d$area_biomass_sampling == 0
	d$dmy_total[i] <- NA

	
	# to get kg/ha
	#getting residue yield according to the reference
	d$plot_area_harvest <- as.numeric(d$plot_area_harvest)
	d$dry_weight_sub_husks <- as.numeric(d$dry_weight_sub_husks)/1000 # convert to kg
	d$fwy_residue <- (10000/d$plot_area_harvest) *d$dry_weight_sub_husks
	
	#getting the yield
	d$dry_weight_sub_grains_sep_husks <- as.numeric(d$dry_weight_sub_grains_sep_husks)/1000
	d$yield <- (10000/d$plot_area_harvest) * d$dry_weight_sub_grains_sep_husks

	#RH removing fiels with zero area. Please check if this is necessary.
	message("    Effie Ochieng', please check")
	i <- d$plot_area_harvest == 0
	d$fwy_residue[i] <- NA
	d$yield[i] <- NA


	# adding the fertilizer information
	d$N_fertilizer <- 0
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	v <- carobiner::fix_name(d$sub_treatment_inoc)
	d$P_fertilizer[v %in% c("DAP", "TSP", "TSP/KCL")] <- 30
	d$K_fertilizer[v == "TSP/KCL"] <- 30
	
	d <- d[, c("trial_id","on_farm","treatment","crop", "planting_date","harvest_date","N_fertilizer","P_fertilizer","K_fertilizer","yield","seed_weight","fwy_residue","dmy_total")]
	
	# combining the processed data sets to one
	e <- merge(d1, d, by = "trial_id")
	
	
	e$yield_part <- "seed"
	
	e$is_survey <- FALSE
	e$irrigated <- NA
	
	# all scripts should end like this
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, e, path=path)
}



