# R script for "carob"


carob_script <- function(path){
  
"Title: N2Africa demo - Nigeria, 2014
Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
  
  uri <- "doi:10.25502/91q9-f709"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
    project="N2Africa",
    publication= NA,
    data_organization = "IITA;ICRAF;WUR",
    carob_contributor="Rachel Mukami",
    carob_date="2023-08-22",
    data_type="on-farm experiment",
	response_vars = "yield",
	treatment_vars = "inoculant;P_fertilizer"
  )
  
  f <- ff[basename(ff) == "general.csv"]
  f1 <- ff[basename(ff) == "experiment.csv"]
  f2 <- ff[basename(ff) == "production.csv"]
  
  d <- data.frame(read.csv(f)) # general
  
  d1 <- data.frame(read.csv(f1)) # experiment
  
  d2 <- data.frame(read.csv(f2)) # production
  
  # from general
  
  d$trial_id <- d$farm_id
  
  d$country <- "Nigeria"
  d$adm1 <- carobiner::fix_name(d$district,"title")
  d$adm1[grepl("kano",ignore.case = T,d$adm1)] <- "Kano"
  d$adm1[grepl("kaduna",ignore.case = T,d$adm1)] <- "Kaduna"
  d$adm1[grepl("niger",ignore.case = T,d$adm1)] <- "Niger"
  
  d$adm2 <- carobiner::fix_name(d$sector_ward,"title")
  d$adm2[grepl("tudun",ignore.case = T,d$adm2)] <- "Tundun Wada"
  d$adm2[grepl("bunkure",ignore.case = T,d$adm2)] <- "Bunkure"
  d$adm2[grepl("lere",ignore.case = T,d$adm2)] <- "Lere"
  d$adm2[grepl("soba",ignore.case = T,d$adm2)] <- "Soba"
  d$adm2[grepl("zangon",ignore.case = T,d$adm2)] <- "ZangonKa"
  d$adm2[grepl("rimau",ignore.case = T,d$adm2)] <- "Kajuru"
  
  d$adm3 <- carobiner::fix_name(d$village,"title")

  d$elevation <- d$gps_altitude_hh.m
  d$latitude <- d$gps_latitude_hh.decimal_degrees
  d$longitude <- d$gps_longitude_hh.decimal_degrees
  
  r <- d[,c("trial_id","country","adm1","adm2","adm3","latitude","longitude","elevation")]
  
  
  # experiment
  
  d1$trial_id <- d1$farm_id
  
  d1$inoculation_used_in_n2africa_field <- carobiner::fix_name(d1$inoculation_used_in_n2africa_field,"upper")
  d1$inoculated <- ifelse(d1$inoculation_used_in_n2africa_field == "Y",TRUE,
                          ifelse(d1$inoculation_used_in_n2africa_field == "N",FALSE,d1$inoculation_used_in_n2africa_field))
                          
  d1$inoculant <- carobiner::fix_name(d1$experimental_treatments_inoculant_type,"title")
  d1$inoculated <- ifelse(!is.na(d1$inoculant),TRUE,FALSE)
  
  d1$crop <- carobiner::fix_name(d1$experimental_treatments_crop_1,"lower")
  d1$variety <- carobiner::fix_name(d1$experimental_treatments_variety_crop_1,"title")
  
  d1$crop[grepl("soya",ignore.case = T,d1$crop)] <- "soybean"
  d1$crop[grepl("288",ignore.case = T,d1$crop)] <- "cowpea"
  d1$crop[grepl("tgx",ignore.case = T,d1$variety)] <- "soybean"
  d1$crop[grepl("evdt",ignore.case = T,d1$variety)] <- "cowpea"
  d1$crop[grepl("nut",ignore.case = T,d1$variety)] <- "groundnut"
  
  d1$crop[grepl("bean",ignore.case = T,d1$experiment_id) & is.na(d1$crop)] <- "soybean"
  d1$crop[grepl("cow",ignore.case = T,d1$experiment_id)  & is.na(d1$crop)] <- "cowpea"
  d1$crop[grepl("ground",ignore.case = T,d1$experiment_id)  & is.na(d1$crop)] <- "groundnut"
  d1$crop[grepl("maize",ignore.case = T,d1$crop)] <- "cowpea"
  d1$crop[grepl("evdt",ignore.case = T,d1$name_treatment_1) & is.na(d1$crop)] <- "cowpea"
  
  # converting from wide to long based on treatments and their characteristics
  
  vars <- c("description", "grain_weight","pod_weight","above_ground_biomass","fert_1_kg_plot","manure_kg_plot")
  i <- grepl(paste(vars, collapse = "|"), names(d1))
  gh <- names(d1)[i] # more information on treatments 1 - 8 properties
  ghh <- gh[c(1:32,57:length(gh))] # subsetting treatment details for crop1 only
  
  des <- grepl("description",ghh)
  grn <- grepl("grain_weight", ghh)
  pod <- grepl("pod_weight", ghh)
  mass <- grepl("biomass", ghh)
  fert <- grepl("fert_1_kg_plot", ghh)
  manu <- grepl("manure_kg_plot", ghh)
  
  treatments <- names(d1)[5:12]
  
  # converting from wide to long
  
  b3 <- reshape(d1,
                direction = "long",
                varying = list(c(treatments),
                               ghh[des],
                               ghh[grn],
                               ghh[pod],
                               ghh[mass],
                               ghh[fert],
                               ghh[manu]),
                v.names = c("treatment","treatment_description", "grain_weight_shelledcrop1_kg/plot","pod_weight_unshelledcrop1_kg/plot", "above_ground_biomass_husksstover_kg/plot", "fert_amtkg/plot","manure_amtkg/plot"),
                idvar = "trial_id",
                timevar = "treatment_number")
  
  rownames(b3) <- NULL
  
  b3$fertilizer_type <- carobiner::fix_name(b3$experimental_treatments_fertilizer_1)
  b3$N_fertilizer <- 0
  b3$P_fertilizer <- 0 # information from protocol says 30kg/ha of P was applied
  b3$P_fertilizer[b3$fertilizer_type == "SSP"] <- 30
  b3$K_fertilizer <- 0
  
  b3$OM_type <- carobiner::fix_name(b3$experimental_treatments_type_of_manure,"title")
  b3$OM_type[b3$OM_type %in% c("Animal Dung", "Farm Yard Manure")] <- "farmyard manure"
  b3$OM_type[b3$OM_type %in% c("Organic Matter")] <- "compost"
  b3$OM_used <- !is.na(b3$OM_type)
  
  b3$plot_width <- 10
  b3$plot_length <- 10
  b3$plot_size_ha <- (b3$plot_width * b3$plot_length)/10000 # convert plot size from m squared to ha
  
  b3$OM_amount <- b3$`manure_amtkg/plot`/b3$plot_size_ha
  
  b3$OM_used[b3$OM_amount > 0 & !is.na(b3$OM_amount)] <- TRUE
  
  b3$yield <- b3$`grain_weight_shelledcrop1_kg/plot`/b3$plot_size_ha
  b3$fwy_residue <- b3$`above_ground_biomass_husksstover_kg/plot`/b3$plot_size_ha
  
  b3$row_spacing <- b3$experimental_treatments_density_1_row_spacing.m
  b3$plant_spacing <- b3$experimental_treatments_density_1_plant_spacing.m
  
  b3$treatment <- carobiner::fix_name(b3$treatment)
  
  # subset to variables of interest
  r1 <- b3[,c("trial_id","crop","variety","treatment","inoculated","inoculant","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer",
              "OM_used","OM_type","OM_amount","row_spacing","plant_spacing","fwy_residue","yield")]

  # drop duplicates
  i <- duplicated(r1) 
  r1 <- r1[!i,]
  
  # production
  
  d2$trial_id <- d2$farm_id
  
  # Extracting planting and harvest dates
  d2$date_of_planting_mm <- carobiner::fix_name(d2$date_of_planting_mm,"title")
  d2$date_of_planting_mm[d2$date_of_planting_mm == 6] <- "June"
  
  d2$date_of_final_harvest_mm <- carobiner::fix_name(d2$date_of_final_harvest_mm,"title")
  d2$date_of_final_harvest_mm[d2$date_of_final_harvest_mm == 11] <- "November"
  
  d2$planting_date <- as.character(as.Date(paste(2014,d2$date_of_planting_mm,d2$date_of_planting_dd,sep = "-"),"%Y-%B-%d"))
  d2$harvest_date <- as.character(as.Date(paste(2014,d2$date_of_final_harvest_mm,d2$date_of_final_harvest_dd,sep = "-"),"%Y-%B-%d"))
  
  # subset variables
  r2 <- d2[,c("trial_id","planting_date","harvest_date")]

  # merge dataframes
  rr <- merge(r,r2,by = "trial_id",all = TRUE)

  z <- merge(r1,rr,by="trial_id",all.x = TRUE)
  z$fertilizer_type[z$fertilizer_type == "2kg"] <- "unknown"
  
  # dropping inputs without treatment, residue yield and yield information
  i <- complete.cases(z[,c("treatment","fwy_residue","yield"),])
  z <- z[i,]
  
  z$yield_part <- "seed"
  z$on_farm <- TRUE
  z$is_survey <- TRUE

  # Adding the processed coordinates
  
  # coords based on adm3
  adm3 <- data.frame(
		country = "Nigeria",
        adm3 = c("Yakasai", "Sabo", "Maigodo", "Tagwaye", "Dadinkowa", 
                 "Ragada", "Gurjiya", "Bunkure", "Soba", "Kwakwa", "Awasha", "Yinda", 
                 "Jammaje", "Marmara", "Yaryasa", "Madakiya", "Rimau", "Kasuwan Magani", 
                 "Sabon Birni", "Saminaka", "Saminaka A", "Kayarda"),
        lat = c(11.99465,6.45577, 10.80363, 10.84889, 9.83863, 10.67227, 12.7195, 11.66716, 
                10.87005, 9.51403, 9.89619, 10.89253, 11.24638, 12.46139, 11.28695, 
                 9.68875, 10.4342, 10.38211, 13.4847, 10.41227, 10.41227, 10.54177),
        lon = c(8.52722, 3.16887, 8.57661, 8.61458, 12.44113, 
                8.65033, 8.60241, 8.59687, 7.96443, 8.61505, 6.80598, 10.19747, 
                8.35526, 8.10005, 8.31093, 8.30329, 7.75881, 7.71662, 6.26409, 
                8.68748, 8.68748, 8.592)
	)
	
  z <- merge(z,adm3,by= c("country","adm3"),all.x = TRUE)
  
	isna <- is.na(z$latitude) | is.na(z$longitude)
	z$latitude <- ifelse(isna, z$lat, z$latitude)
	z$longitude <- ifelse(isna, z$lon, z$longitude)
	z$lon <- z$lat <- NULL
  
  # coordinates based on adm2
  adm2 <- data.frame(
		country = "Nigeria",
        adm2 = c("Soba", "Shiroro", "Bunkure", "Doguwa", "Bichi","Tundun Wada", "Kajuru", "Lere", "Paikoro", "Igabi"), 
        lat = c(10.87005,10.11543, 11.66716, 10.87963, 12.23391, 9.25496, 10.27704, 10.38584,9.4681, 10.781),
        lon = c(7.96443, 6.68307, 8.59687, 8.66389,8.27943, 7.00526, 7.85783, 8.57286, 6.85565, 7.504)
	)
  
	z <- merge(z,adm2,by= c("country","adm2"),all.x = TRUE)
  
	isna <- is.na(z$latitude) | is.na(z$longitude)
	z$latitude <- ifelse(isna,z$lat,z$latitude)
	z$longitude <- ifelse(isna,z$lon, z$longitude)
	z$lon <- z$lat <- NULL
  
  # coordinates based on adm1
  adm1 <- data.frame(country = c("Nigeria", "Nigeria", "Nigeria"), 
                     adm1 = c("Kano", "Kaduna", "Niger"),
                     lat = c(12.00012,10.52641, 18),
                     lon = c(8.51672, 7.43879, 9),
                     geo_from_source = FALSE)
  
  z <- merge(z,adm1,by= c("country","adm1"),all.x = TRUE)
  
  isna <- is.na(z$latitude) | is.na(z$longitude)
  z$latitude <- ifelse(isna,z$lat,z$latitude)
  z$longitude <- ifelse(isna,z$lon, z$longitude)
	z$lon <- z$lat <- NULL
  z$irrigated <- NA
  
	z$yield_moisture <- as.numeric(NA) #needs to be checked

  carobiner::write_files(meta, z, path=path)
}
