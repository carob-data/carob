# R script for "carob"


carob_script <- function(path){
  
"Title: N2Africa demo - Ghana, 2014
 
Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
  
  uri <- "doi:10.25502/QMSB-P921"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
   
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication= NA,
		data_organization = "IITA;WUR",
		carob_contributor="Rachel Mukami",
		carob_date="2023-08-15",
		data_type="on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;inoculated",
		response_vars = "yield"
	
	)
  
  f <- ff[basename(ff) == "general.csv"]
  f1 <- ff[basename(ff) == "experiment.csv"]
  f2 <- ff[basename(ff) == "production.csv"]
  
  d <- data.frame(read.csv(f)) # general
  
  d1 <- data.frame(read.csv(f1)) # experiment

  d2 <- data.frame(read.csv(f2)) # production
  
  # from general
  
  d$trial_id <- d$farm_id
  
  d$country <- "Ghana"
  d$adm1 <- as.character(NA)
  d$adm2 <- carobiner::fix_name(d$district,"title")
  d$adm2[d$adm2 %in% c("Nadowli- Kaleo","Nadowli-Kaleo")] <- "Nadowli"
  d$adm2[d$adm2 %in% c("Wa- West","Wa-West")] <- "Wa West"
  d$adm2[d$adm2 == "Savelugu Per Nanton"] <- "Savelugu Nanton"
  d$adm2 <- gsub(" Municipal","",d$adm2) 
  d$adm3 <- carobiner::fix_name(d$village,"title")

  d$latitude <- d$gps_latitude_field
  d$longitude <- d$gps_longitude_field
  d$elevation <- d$gps_altitude_hh.m
  
  r <- d[,c("trial_id","country","adm1","adm2","adm3","latitude","longitude","elevation")]
  r$geo_from_source <- TRUE
  
  
  # EGB: 
  # # Fix coordinates from those already assumed to be correct in the dataset
  # # At this point is just taking the coordinates from one of the other sites
  # # but worth another look
  r$longitude[r$adm3 %in% c("Dorimon", "Piisie") & r$latitude < 2] <- 2.7
  r$latitude[r$adm3 %in% c("Dorimon", "Piisie") & r$latitude < 2] <- 10
  r$longitude[r$adm3 == "Kojokpere" & r$latitude < 2] <- 2.2
  r$latitude[r$adm3 == "Kojokpere" & r$latitude < 2] <- 10.2
  r$longitude[r$adm3 == "Nyoli" & r$latitude < 2] <- 2.3
  r$latitude[r$adm3 == "Nyoli" & r$latitude < 2] <- 9.5
  r$longitude[r$adm3 == "Kangba" & r$latitude < 2] <- 2.6
  r$latitude[r$adm3 == "Kangba" & r$latitude < 2] <- 9.9
  r$longitude[r$adm3 == "Tampieni" & r$latitude < 2] <- 2.6
  r$latitude[r$adm3 == "Tampieni" & r$latitude < 2] <- 10
  r$longitude[r$adm3 == "Natinga" & r$latitude < 2] <- 0.5
  r$latitude[r$adm3 == "Natinga" & r$latitude < 2] <- 10.8
  r$longitude[r$adm3 == "Natinga" & r$latitude < 2] <- 0.5
  r$latitude[r$adm3 == "Natinga" & r$latitude < 2] <- 10.8
  r$longitude[r$adm3 == "Pion"] <- 0
  r$latitude[r$adm3 == "Pion"] <- 9.6
  r$longitude[r$adm3 == "Nafkolga"] <- -0.3
  r$latitude[r$adm3 == "Nafkolga"] <- 10.9
  r$longitude[r$adm3 == "Tansia"] <- -0.3
  r$latitude[r$adm3 == "Tansia"] <- 10.9
  # # Additionally, 3 records are flipped (lat is long; long is lat)
  r$longitude[r$adm2 == "Yendi" & r$adm3 == "Gbung"] <- 0.0
  r$latitude[r$adm2 == "Yendi" & r$adm3 == "Gbung"] <- 9.6
  r$longitude[r$adm2 == "Yendi" & r$adm3 == "Limpua"] <- 0.1
  r$latitude[r$adm2 == "Yendi" & r$adm3 == "Limpua"] <- 9.6
  r$longitude[r$adm2 == "Yendi" & r$adm3 == "Chirifoyili"] <- 0.1
  r$latitude[r$adm2 == "Yendi" & r$adm3 == "Chirifoyili"] <- 9.7
  # # Additionally, some of the longitude values seem to be reversed
  r$longitude[r$longitude > 0 & !is.na(r$longitude)] <- r$longitude[r$longitude > 0 & !is.na(r$longitude)]*-1
  
  
  # experiment
  
  d1$trial_id <- d1$farm_id
  
  d1$inoculant <- carobiner::fix_name(d1$experimental_treatments_inoculant_type, "title")
  d1$inoculated <- d1$inoculation_used_in_n2africa_field == "Y" | is.na(d1$inoculant)
  
  d1$crop <- carobiner::fix_name(d1$experimental_treatments_crop_1,"lower")
  d1$crop[d1$crop == "soya bean"] <- "soybean"
  
  d1$crop[grepl("bean",ignore.case = T,d1$experiment_id)] <- "soybean"
  d1$crop[grepl("cow",ignore.case = T,d1$experiment_id)] <- "cowpea"
  d1$crop[grepl("ground",ignore.case = T,d1$experiment_id)] <- "groundnut"
  
  d1$variety <- carobiner::fix_name(d1$experimental_treatments_variety_crop_1,"title")
  d1$variety[grepl("song",ignore.case = T,d1$variety)] <- "Songotra"
  
  # converting from wide to long based on treatments and their characteristics
  
  vars <- c("description","width", "depth", "grain_weight","pod_weight","above_ground_biomass","fert_1_kg_plot","manure_kg_plot")
  i <- grepl(paste(vars, collapse = "|"), names(d1))
  gh <- names(d1)[i] # more information on treatments 1 - 8 properties
  ghh <- gh[c(1:48, 89:length(gh))] # subsetting treatment details for crop1 only
  
  des <- grepl("description",ghh)
  wdt <- grepl("width", ghh)
  len <- grepl("depth", ghh)
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
                               ghh[wdt],
                               ghh[len],
                               ghh[grn],
                               ghh[pod],
                               ghh[mass],
                               ghh[fert],
                               ghh[manu]),
                v.names = c("treatment", "treatment_description", "plot_width", "plot_length", "grain_weight_shelledcrop1_kg/plot", "pod_weight_unshelledcrop1_kg/plot", "above_ground_biomass_husksstover_kg/plot", "fert_amtkg/plot", "manure_amtkg/plot"),
                idvar = "trial_id",
                timevar = "treatment_number")
  
  rownames(b3) <- NULL
  
  b3$fertilizer_type <- carobiner::fix_name(b3$experimental_treatments_fertilizer_1)
  b3$N_fertilizer <- 0
  b3$P_fertilizer <- 0
  b3$K_fertilizer <- 0
 
  b3$OM_type <- carobiner::fix_name(b3$experimental_treatments_type_of_manure,"title")
  b3$OM_type[tolower(b3$OM_type) == "fertisol"] <- "compost"
  b3$OM_type[b3$OM_type != "compost"] <- NA
  b3$OM_used <- ifelse(is.na(b3$OM_type),FALSE,TRUE)
  
  b3$plot_size_ha <- (b3$plot_width * b3$plot_length)/10000 # convert plot size from m squared to ha
  
  b3$OM_amount <- b3$`manure_amtkg/plot`/b3$plot_size_ha
  
  b3$OM_used[b3$OM_amount > 0 & !is.na(b3$OM_amount)] <- TRUE
  
  b3$yield <- b3$`grain_weight_shelledcrop1_kg/plot`/b3$plot_size_ha
  b3$fwy_residue <- b3$`above_ground_biomass_husksstover_kg/plot`/b3$plot_size_ha
  
  b3$row_spacing <- b3$experimental_treatments_density_1_row_spacing.m
  b3$plant_spacing <- b3$experimental_treatments_density_1_plant_spacing.m
  
  b3$treatment <- carobiner::fix_name(b3$treatment)
  b3$treatment[grepl("song",b3$treatment)] <- "songotra"
  b3$treatment[b3$treatment == "zaayura"] <- "zayura"
  b3$treatment[b3$treatment == "paditura"] <- "padituya"
  b3$treatment[grepl(paste(c("variety","seed"),collapse = "|"),b3$treatment)] <- "farmer's variety"
  b3$treatment[grepl("practice",b3$treatment)] <- "farmer's practice"
  
  b3$treatment[b3$treatment %in% c("fertisol + phosphorus only","fertisol+p fertiliser","p fertiliser + fertisol")] <- "fertisol + p fertiliser"
  b3$treatment[b3$treatment %in% c("i + p","inculant + p fertiliser","inoculant + p fertilizer","p fertiliser + inoculant","p+ inoculant")] <- "inoculant + p fertiliser"
  b3$treatment[b3$treatment %in% c("i only","inocualnt only")] <- "inoculant only"
  b3$treatment[b3$treatment == "fertisol"] <- "fertisol only"
  b3$treatment[b3$treatment %in% c("p fertiliser","p only","phosphorus only")] <- "p fertiliser only"
  b3$treatment[b3$treatment == "fertisol + inoculant"] <- "inoculant + fertisol"
  
  
  b3$fertilizer_type[grepl("fertil",ignore.case = T,b3$treatment)] <- "TSP"
  b3$inoculated[grepl("inocu",ignore.case = T,b3$treatment)] <- TRUE
  
  # subset to variables of interest
  
  r1 <- b3[,c("trial_id","crop","variety","treatment","inoculated","inoculant","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer",
                    "OM_used","OM_type","OM_amount","row_spacing","plant_spacing","fwy_residue","yield")]
  
  # production
  
  d2$trial_id <- d2$farm_id
  d2$date_of_planting_yyyy <- 2014
  d2$date_of_final_harvest_yyyy <- 2014
  
  d2$planting_date <- as.character(as.Date(paste(d2$date_of_planting_yyyy,d2$date_of_planting_mm,d2$date_of_planting_dd,sep = "-"),"%Y-%B-%d"))
  d2$harvest_date <- as.character(as.Date(paste(d2$date_of_final_harvest_yyyy,d2$date_of_final_harvest_mm,d2$date_of_final_harvest_dd,sep = "-"),"%Y-%B-%d"))
  
  # subset variables
  r2 <- d2[,c("trial_id","planting_date","harvest_date")]

  # merge dataframes
  rr <- merge(r,r2,by = "trial_id",all = TRUE)

  z <- merge(r1,rr,by="trial_id",all.x = TRUE)
  z$fertilizer_type[!is.na(z$fertilizer_type)] <- "TSP"
  z$yield_part <- "seed"
  z$is_survey <- TRUE
  
  
  # dropping inputs without treatment, residue yield and yield information
  
  i <- complete.cases(z[,c("treatment", "yield"),])
  z <- z[i,]
 
  # final data set
  
  z$on_farm <- TRUE
  z$irrigated <- NA
  
  carobiner::write_files(meta, z, path=path)
}



  # EGB:
  # #  Removing --- See L63
  # # Adding the processed coordinates
  # 
  # # coords based on adm3
  # adm3 <- data.frame(country = "Ghana", 
  #                    adm3 = c("Apurimzua", "Atuba", "Azumsapeliga", "Azupupung", 
  #                          "Baadabogu", "Baadaboo", "Bazua", "Bazunde", "Beka", "Bihinaayili", 
  #                           "Booduori", "Botingli", "Boya Natinga", "Bulungu", "Bunglung", 
  #                     "Challam", "Damdu", "Kangba-Jedo", "Dorimon", "Piisie", "Gamaga", 
  #                     "Gbeongo", "Gbung", "Googo Natinga", "Gumbo", "Gumbo Nagure", 
  #                     "Gumyoko", "Guzongo", "Kangba", "Kambontooni", "Tampieni", "Koduziegu", 
  #                     "Kojokpere", "Kpantori", "Kpatia", "Kuboku", "Kubuko", "Kulogtingre", 
  #                     "Kushegu", "Lamboya", "Lamboya-Gozicse", "Limpua", "Chebaa", 
  #                     "Zamboge", "Balienia", "Kaleo Puli", "Toure", "Fian", "Issa", 
  #                     "Nyugluu", "Tabiesi", "Wogu", "Nafkolga", "Nalogu", "Natinga", 
  #                     "Nyolgo", "Nyoli", "Domugjile", "Ombo", "Pase", "Sakpari", "Salpiiga", 
  #                     "Sambolongo", "Sankpiem", "Sapeliga", "Savelugu", "T - Natinga", 
  #                     "Tambiigu", "Tansia", "Tetaku", "Tilli", "Varempere", "Varempare", 
  #                     "Yalugu #1", "Yemo", "Binilobdo", "Chirifoyili", "Gbemba", "Gundogu", 
  #                     "Kulkpanga", "Pion", "Sakpegu", "Yizegu", "Yong", "Zaago2", "Zambogu", 
  #                     "Zeego", "Zuoyanga #1", "Zuoyanga #2"), 
  #                    lat = c(NA, NA,NA, NA, NA, NA, 10.99234, 10.9027, 7.23761, NA, NA, NA, NA, NA,9.59576, NA, 9.62618, NA, 
  #                            10.03436, NA, NA, NA, NA, NA, 10.84746, NA, 10.98779, 11.06855, NA, NA, NA, NA, NA, NA, 9.52346, 
  #                            10.90625, NA, NA, NA, 10.92047, NA, NA, NA, NA, NA, NA, NA, 10.39255, 10.39158,NA, NA, 10.12611, 
  #                            NA, NA, 10.81667, NA, 9.74328, NA, 10.18313, 10.03735, 10.94717, NA, NA, NA, 11.01774, 9.62441, 
  #                            NA, 11.04479,NA, NA, NA, NA, NA, NA, 9.66881, NA, NA, NA, NA, NA, 9.63998,NA, NA, 9.34033, NA, 
  #                            NA, NA, NA, NA), 
  #                    lon = c(NA, NA, NA, 
  #                             NA, NA, NA, -0.36334, -0.52334, 0.5309, NA, NA, NA, NA, NA, -0.79683, 
  #                             NA, -0.78115, NA, -2.68827, NA, NA, NA, NA, NA, -0.47232, NA, 
  #                             -0.34812, -0.37938, NA, NA, NA, NA, NA, NA, -0.02967, -0.48492, 
  #                             NA, NA, NA, -0.51791, NA, NA, NA, NA, NA, NA, NA, -2.46857, -2.33412, 
  #                             NA, NA, 0.15167, NA, NA, -0.08333, NA, -2.49473, NA, -2.51148, 
  #                             -2.7109, -0.35663, NA, NA, NA, -0.30687, -0.8253, NA, -0.3415, 
  #                             NA, NA, NA, NA, NA, NA, -0.50991, NA, NA, NA, NA, NA, -0.06531, 
  #                             NA, NA, -0.81915, NA, NA, NA, NA, NA))
  # 
  # z <- merge(z,adm3,by= c("country","adm3"),all.x = TRUE)
  # 
  # isna <- is.na(z$latitude) | is.na(z$longitude)
  # z$latitude <- ifelse(isna, z$lat, z$latitude)
  # z$longitude <- ifelse(isna,z$lon, z$longitude)
  # z$lat <- z$lon <- NULL
  # 
  # # coordinates based on adm2
  # 
  # adm2 <- data.frame(country = c("Ghana", "Ghana", "Ghana", "Ghana", 
  #                                "Ghana", "Ghana"), 
  #                    adm2 = c("Binduri", "Bawku West", "Nadowli","Savelugu Nanton", "Yendi", "Wa West"), 
  #                    lat = c(10.97214,10.91667, 10.28835, 9.62441, 9.44272, 10.06069), 
  #                    lon = c(-0.30837,-0.51667, -2.60889, -0.8253, -0.00991, -2.50192))
  # 
  # z <- merge(z,adm2,by= c("country","adm2"),all.x = TRUE)
  # 
  # isna <- is.na(z$latitude) | is.na(z$longitude)
  # z$latitude <- ifelse(isna, z$lat, z$latitude)
  # z$longitude <- ifelse(isna,z$lon, z$longitude)
  # z$lat <- z$lon <- NULL
  
