# R script for "carob"



carob_script <- function(path) {
  
"Wheat productivity in eastern Indo-Gangetic plain is sub-optimal that can largely be attributed to delayed wheat sowing and use of late sown varieties. Ideal time of sowing wheat in eastern India is first fortnight of November but in general, it gets delayed by 15-25 days. This delay puts farmers in a situation where they tend to prefer late sown varieties whose potential yields are low. By 2020, more than half wheat farmers in Bihar more than one third in eastern Uttar Pradesh were using late sown wheat varieties. To validate the effect of timely sowing and comparative performance of long and short duration varieties, multi-location on-farm trials were conducted continuously over five years starting from 2016-17. Krishi Vigyan Kendras, district-level extension centre of national agriculture research and extension system were involved in the process of evidence generation. Ten districts so ten centres were selected in a way that all agro-climatic zones of this area is covered. There were five treatments of sowing windows – 01 to 10 November, 11 – 20 November, 21 to 30 November, 01 – 15 December and 16 – 31 December. Varietal performance was compared in T3, T4 and T5 as short duration varieties can’t be sown before 20 November. There is asymmetry in distribution of samples within treatments and over years. That happened as trial was in farmer’s participatory mode and numbers were dependent completely on willingness of farmers to participate. Altogether, the trial was conducted at 3735 sites and we captured 60 variables including yield and yield attributing traits. (2022-10-28)"

  uri <- "hdl:11529/10548817" 
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
    project=NA,
    publication=NA,
    data_organization = "CIMMYT;IRRI",
    data_type="experiment",
    carob_contributor ="Effie Ochieng'",
    carob_date="2023-09-18",
	treatment_vars = "sowing_date;variety",
	response_vars="yield"
  )

  f <- ff[basename(ff) == "CSISA_KVK_Wheat_DoS_Trial_Data.csv"]
  r <- read.csv(f, fileEncoding = "latin1") 

  ## use a subset 
  from <- c("Village","District","State","Block","PreviousCrop","Variety","SowingDate","HarvestDate","BiomassYield","SowingSchedule","Latitude", "Longitude")
  
  d <- carobiner::change_names(r[,from], from, c("location","adm2","adm1","site","previous_crop","variety","planting_date","harvest_date","dmy_total","treatment", "latitude", "longitude"))
  d$planting_date <- as.character(format(as.Date(d$planting_date, format = "%Y.%m.%d"), format = "%Y-%m-%d"))
  d$harvest_date <- as.character(format(as.Date(d$harvest_date, format = "%Y.%m.%d"), format = "%Y-%m-%d"))
  d$dmy_total <- (d$dmy_total)*1000 #to convert to kg/ha
  d$longitude <- gsub(" ", "", d$longitude)
  d$longitude <- as.numeric(d$longitude)
  d$country <- "India"
  d$crop <- "wheat"
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$is_experiment <- TRUE
  d$irrigated <- TRUE 
  # to remove the characters bringing about encoding issues
## can't you read the file as UTF8?
#  d$location <- iconv(d$location, to = "UTF-8", sub = " ")
  
  #identifying the rows with no spatial info
 #h <- d[c(269, 270, 271, 325, 329, 330, 515, 561, 707, 751, 766, 803, 813, 823, 846, 852, 853, 854, 892, 899, 900, 917, 918, 933, 935, 940, 941, 1061, 1080, 1084, 1149, 1152, 1161, 1170, 1191, 1261, 1272, 1538, 1545, 1604, 1699, 1781, 1815, 1816, 1887, 1896, 2071, 2115, 2222, 2523, 2524, 2525, 2552, 2566, 2567, 2568, 2582, 2583, 2598, 2677, 2679, 2703, 2755, 2758, 2768, 2771, 2788, 2791, 2793, 2864, 2866, 2906, 2908, 2920, 2932, 2946, 2954, 2983, 2985, 3004, 3025, 3073, 3253),]
  
  # u <- unique(h[complete.cases(h$location), c("country","location")])
  # g <- carobiner::geocode(country = u$country,  location = u$location, service = "Geonames", username = "efyrouwa")


     # standardizing for fertilizer info
  g <- r$GradeNPK
  g <- gsub(" PM", "", g)
  g <- gsub("12.32.16", "12:32:16", g)
  g <- gsub("20.20.0.13", "20:20:13", g)
  g <- gsub("10.26.26", "10:26:26", g)

  # convert kg/acre to kg/ha
  d$BasalNPK_kg_ha <- r$BasalNPK * 0.404686 
  
  #  h <- r[c(1406,1523,1425,1540,184,185,299,388,390,393,397,399,453,454,490,497,498,502,601,954,957),] 
  #  These rows have peculiar NPK variations, should be looked into
 
	
  d$N_fertilizer1 <- ifelse(g == "12:32:16", 0.12 * d$BasalNPK_kg_ha,
                     ifelse(g == "20:20:13", 0.20 * d$BasalNPK_kg_ha,
                     ifelse(g == "18:46:00" , 0.18 * d$BasalNPK_kg_ha,
                     ifelse(g == "10:26:26" , 0.10 * d$BasalNPK_kg_ha,
                     ifelse(g == "14:35:14" , 0.14 * d$BasalNPK_kg_ha,0)))))

  
  d$P_fertilizer1 <- ifelse(g == "12:32:16", 0.32 * d$BasalNPK_kg_ha,
                    ifelse(g == "20:20:13", 0.20 * d$BasalNPK_kg_ha,
                    ifelse(g == "18:46:00" , 0.46 * d$BasalNPK_kg_ha,
                    ifelse(g == "10:26:26" , 0.26 * d$BasalNPK_kg_ha,
                    ifelse(g == "14:35:14" , 0.35 * d$BasalNPK_kg_ha,0)))))
  
  
  d$K_fertilizer <- ifelse(g == "12:32:16", 0.16 * d$BasalNPK_kg_ha,
                    ifelse(g == "20:20:13", 0.13 * d$BasalNPK_kg_ha,
                    ifelse(g == "10:26:26" , 0.26 * d$BasalNPK_kg_ha,
                    ifelse(g == "14:35:14" , 0.14 * d$BasalNPK_kg_ha,0))))

  d$N_splits <- 3
  # convert kg/acre to kg/ha 
  d$BasalDAP_kg_ha <- r$BasalDAP * 0.404686 
  
  # DAP in carob has 18% N and 20% P
  d$N_fertilizer2 <- ifelse(!is.na(d$BasalDAP_kg_ha),d$BasalDAP_kg_ha * 0.18,0)
  d$P_fertilizer2 <- ifelse(!is.na(d$BasalDAP_kg_ha),d$BasalDAP_kg_ha * 0.20,0)
  
  #MOP in  carob has 49.8% K
  d$MOP_kg_ha <- r$BasalMOP * 0.404686 #kg/acre to kg/ha 
  d$P_fertilizer3 <- ifelse(!is.na(d$BasalDAP_kg_ha),d$BasalDAP_kg_ha * 0.20,0)
  
  d$Zn_fertilizer <- ifelse(!is.na(r$BasalZn),r$BasalZn * 0.404686, 0) #to convert from kg/acre to kg/ha
  d$N_fertilizer <- d$N_fertilizer1 + d$N_fertilizer2 
  d$P_fertilizer <- d$P_fertilizer1 + d$P_fertilizer2 + d$P_fertilizer3
  d$yield <- r$GrainYield * 1000 #ton/acre to kg/ha
  d$yield_part <- "grain"
  d$seed_weight <- r$TestWeight
  d$trial_id <- paste(1:nrow(d), d$site, sep = "_")
  # d$tiller_count <- r$TillersCount are these  important variables? 
  # d$grain_count <- r$GrainsCount   
   
  d <- d[, c("trial_id","country", "adm1","adm2","site","latitude","longitude","crop","variety","planting_date","harvest_date","treatment","N_fertilizer","P_fertilizer","K_fertilizer","Zn_fertilizer","yield_part","yield","seed_weight","on_farm","is_survey","irrigated")]
  
  # EGB:
  # Adding approximate coordinates
  # del <- unique(d[is.na(d$latitude) | is.na(d$longitude), c("country", "adm1", "adm2", "site")])
  # gi <- carobiner::geocode(country = "India", adm1 = del$adm1, location = del$site)

  d$geo_from_source <- TRUE
  d$geo_from_source[is.na(d$longitude)] <- FALSE

  gi <- data.frame(country = c("India", "India", "India", "India", "India", "India", "India", "India", "India", "India", "India", "India"),
                    adm1 = c("Bihar", "UP", "Bihar", "UP", "UP", "Bihar", "Bihar", "Bihar", "Bihar", "Bihar", "UP", "UP"),
                    location = c("Raxaul", "Kaptanganj", "Singheshwar", "fazilnagar", "Dudhi", "Halsi", "Piro", "Bandra", "Barauni", "Chakia", "Fazilnagar", "Hata"),
                    longitude = c(84.8051, 83.6992, 86.8157, 84.0544, 83.0141, 86.0554,
                            84.3541, 85.6226, 86.0284, 84.9933, 84.0544, 83.6678),
                    latitude = c(26.9556, 26.9257, 26.0258, 26.6824, 24.1148, 25.0332,
                            25.3383, 26.0493, 25.4315, 26.4704, 26.6824, 26.8307))
  del <- merge(d[is.na(d$latitude) | is.na(d$longitude), c("trial_id", "country", "adm2", "site")],
               gi, by.x = c("country", "site"), by.y = c("country", "location"), all.x = TRUE)
  del$longitude[del$site == "Gayghat"] <- 84.138
  del$latitude[del$site == "Gayghat"] <- 25.632
  del$longitude[del$site == "Ghailadh"] <- 86.687
  del$latitude[del$site == "Ghailadh"] <- 25.977
  del$longitude[del$site == "Gharani"] <- 84.573
  del$latitude[del$site == "Gharani"] <- 25.410
  del$longitude[del$site == "Gori-Bazar"] <- 83.224
  del$latitude[del$site == "Gori-Bazar"] <- 26.193
  del$longitude[del$site == "Kasaya"] <- 83.619
  del$latitude[del$site == "Kasaya"] <- 26.707
  del$longitude[del$site == "Padrouna"] <- 83.884
  del$latitude[del$site == "Padrouna"] <- 26.739
  del$longitude[del$site == "Ramgharwa"] <- 84.773
  del$latitude[del$site == "Ramgharwa"] <- 26.875
  del$longitude[del$site == "Savrhi"] <- 78.790
  del$latitude[del$site == "Savrhi"] <- 28.853
  del$longitude[del$site == "Sevarhi"] <- 84.217
  del$latitude[del$site == "Sevarhi"] <- 26.722
  del$longitude[del$site == "Surya garha"] <- 86.216
  del$latitude[del$site == "Surya garha"] <- 25.254
  del$longitude[del$site == "Tamkuhiraj"] <- 84.164
  del$latitude[del$site == "Tamkuhiraj"] <- 26.689
  del$longitude[del$site == "tamkuhiraj"] <- 84.164
  del$latitude[del$site == "tamkuhiraj"] <- 26.689
  for (tid in del$trial_id) {
    el <- del[del$trial_id == tid,]
    d[d$trial_id == tid, "longitude"] <- el$longitude
    d[d$trial_id == tid, "latitude"] <- el$latitude
  }
  
  
  carobiner::write_files(meta, d, path=path)
   
}



