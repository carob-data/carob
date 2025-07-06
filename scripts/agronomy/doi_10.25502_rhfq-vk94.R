# R script for "carob"

carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
  
  uri <- "doi:10.25502/rhfq-vk94"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
    publication= NA, 
    data_organization = "IITA;ICRAF;WUR",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-07-20",
    data_type="experiment",
    project=NA,
	response_vars = "yield",
	treatment_vars = "P_fertilizer;inoculated"
  )
  
  f <- ff[basename(ff) == "data_table.csv"] 
  # read the dataset
  r <- read.csv(f)
  
  d <- r[, c("country", "id", "lga_district_woreda", "sector_ward", "gps_field_device_latitude.decimal_degrees", "gps_latitude_field.decimal_degrees", "gps_field_device_longitude.decimal_degrees", "gps_field_device_longitude.decimal_degrees", "crop_1_previous_season", "inoculation_n2africa_field","pack_species","pack_variety","pack_mineral_fertilizer_type","farm_size.ha","farm_size_unit")] 
  
  colnames(d) <- c("country", "trial_id", "location", "site", "latitude1", "latitude2", "longitude1", "longitude2","previous_crop", "inoculated","crop","variety","fertilizer_type","farm_size","farm_size_unit")
  
  # Fix long and lat columns 
  i <- is.na(d$latitude1)
  d$latitude1[i] <- d$latitude2[i]
  i <- is.na(d$longitude1)
  d$longitude1[i] <- d$longitude2[i]
  # keep the relevant column of long and lat
  d$latitude <- d$latitude1
  d$longitude <- d$longitude1
  d$geo_from_source <- TRUE
  
  oldnms <- c("id", "lga_district_woreda", "country", "row_spacing_crop_1_plot_X.cm" , "plant_spacing_crop_1_plot_X.cm", "grain_weight_crop_1_plot_X.kg", "pod_weight_groundnut_crop_1_plot_X.kg", "width_of_harvested_plot_crop_1_plot_X.m", "no_plants_hole_crop_1_plot_X.nr", "date_of_planting_X.date","no_plants_plot_crop_1_plot_X","area_field_X")  
  
  newnms <- c("trial_id", "location", "country", "row_spacing", "plant_spacing", "yield1", "yield2", "width_size_plot", "number_plant_hole","planting_date","number_plant","area_field")
  
  r$no_plants_plot_crop_1_plot_2 <- NA ##
  
  lst <- list()
  i <- c(1,2)  
  for (j in i) {
    inms <- gsub("X", j, oldnms)
    ri <- r[, inms] 
    colnames(ri) <- newnms
    lst[[j]] <- ri
  }	
  
  # append all the treatment data
  d1 <- do.call(rbind, lst)
  
  # fix	yield column 
  i <- is.na(d1$yield1)
  d1$yield1[i] <- d1$yield2[i]
  
  d1$row_spacing[d1$row_spacing >150] <- NA
  
  # Estimation of the number_of_rows
  # can we used this ? to compute the number of rows ?
  #d1$number_row <- d1$number_plant/((d1$plant_spacing +1)*d1$number_plant_hole)
  # calculate the area 
  #d1$area <- d1$width_size_plot * d1$number_row * d1$row_spacing / 100 # in m2
 # d1$yield <- ifelse(d1$area_field!=0, d1$yield1 / d1$area_field,d1$yield1) #kg/ha
  
  # merge d and d1
  d <- merge(d,d1,by=c("trial_id", "location", "country"),all.x = T)
  
  # fix crop name 
  P <- carobiner::fix_name(d$crop,"lower")
  P <- gsub("soya_bean","soybean",P)
  P <- gsub("bush_bean","common bean",P)
  P <- gsub("climbing_bean","common bean",P)
  d$crop <- P
  
  d$yield <- ifelse((d$crop!="common bean"|d$crop!="cowpea") & d$farm_size_unit=="hectare", d$yield1 / d$farm_size,
                    ifelse((d$crop!="common bean"|d$crop!="cowpea") & d$farm_size_unit=="acre",d$yield1 / d$farm_size*0.4046,
                           ifelse((d$crop!="common bean"|d$crop!="cowpea") & d$farm_size_unit=="meter_squared",d$yield1 / (d$farm_size/10000),d$yield1))) #kg/ha
  # fix crop yield unit in Tanzania base on information from protocol 
  i <- grepl("cowpea",d$crop)
  d$yield[i] <- d$yield1[i]/0.01
  
  i <- grepl("common bean",d$crop)
  d$yield[i] <- d$yield1[i]/0.01
  
  d <- d[, c("country", "trial_id", "location", "site", "longitude", "latitude", "planting_date"
             , "crop", "previous_crop", "variety", "inoculated","row_spacing", "plant_spacing","yield","fertilizer_type")]
  # Add columns
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$inoculated[d$inoculated=="y"] <- TRUE
  d$inoculated[d$inoculated=="n"] <- FALSE
  
  # Fix long and lat coordinate 

  geo <- data.frame(site = c("Milungui", "Mkalama", "Majawanga", "Nyameni", "Mangae","Gimi", "Manza", "Baga", "Mogohigwa", "Msingisi", "Lubungo","Ihenje", "Mhenda", "Tabuhotel / Chigela", "Yimirshika", "Mbulatawiwi", "Kigulunde", "Makuyuni", "Kiruweni", "Kwesine","Boheloi", "Ulaya Mbuyuni", "Kigir", "Gabi", "Longuo","Yoghoi", "Kurba gayi", "Nyamabuye/Buhozi/Kisoro", "Kabura","Ajingi", "Kallah", "Rauya", "Teli", "Yamarkumi", "Grim","Tong", "Vina dam", "Tum", "Yamarkumi", "Lotima", "Wanga","Hogoro", "Sagara", "Gare", "Mwangoi", NA, "Lwandai","Briyel", "Msufini", "Kufana", "Mawanjeni", "Migambo","Azare", "Dule", "Dole", "Dibamba", "Kayamda", "Dusu","Mshizii", "Kauru east", "Maina hari", "Grim", "Savelugu","Mbulatawiwi", "Makuyu", "Babban saura", "Kwipipa", "Idon","Hema", NA, "Kwemashai", "Lapai", "Tilla", "Kyeeri","Magani", "Majulai", "Lambu", "Marama", "vina dam", "Kagara","Maina-hari", "Buda", "Mokwa", "Kwaya Bura", NA, "Bobi","Vitonga", "Maina Baba", "Kaprengu", "Mavumo", "maina baba","Miringa", "Miringa", "Wuciciri", "Kibedya", "Kasuwan magani","Guwal", "Kimashuku", "Nankokoli", "Gaidam", "Bunglung","Manolo", "Suguta", NA, "Peta", "Gusi", "Kayamda", "Hambalawei","Kaburasazi/Buhozi/Busanza", "Shanono", "Beri", "Kwami","marama", "Tum", "Adok B", "Sombo", "Yawi", "Ibwaga","Mponde", "Kuya", "gaidam", "Malete", "Kabura", "Mogohigwa","Bihinaayili", "Takpo", "Ngwa", "Kauru", "Kyamukombe","Jauro Garga", "teli", "Kauru East", "Jauro Garga", "Ihenje","Yizegu", "gaidam", "Nankokoli, Nankokoli", "Wazata","Kyomu", NA, "Gashina", "wuyo", "Yidna", "Jumar galadima","Fuma", "Sabon gari", "Kondoa - Mabwerebwere", "Maina hari","Nzukuku", "Yarkanya", "Sakwa", "Yimirshika", "Digawa","Gusi", "guwal", "Langa", "Ketawa", "Wuyo", "Busal,kutung,tegeres","Dusu", "Rabba", "Tanagar", "Danja", "Ghuma", "Kasuwan Magani","Botingli", "Kayarda", "Handei", NA, "Tsamiyar kara","Chikun", "Labozhi", "Nyamabuye/Buhozi/Busanza", "Wuyo","Duma", "Tudun saibu", "Ugogoni", "Yoyo", "Tegina", "Kibale","Ibumba", "gashina", "Azare", NA, "Kawo", "Maina-baba", NA, "Guwal", "Goligoli", "Ikara", "Lubimba", "Maina-hari","Saulawa", "Acinanga", "yamarkumi", "Vina dam", "gaidam","sakwa", "Kauru west", "Tong", "Kanye", NA, "Makarfi","Buba Kayamda", "Nyamabuye/Buhozi/Busanza/kisoro", "Rimau", 
           "Mziasaa", "Sakwa", "Tabuhotel/ Chigela", "Kapchebut"),
  lon = c(38.3653,36.8549, 36.8242, 36.894, 37.2409, 7.9416, 37.5153, 38.4144, 36.9011, 36.8679, 37.4949, 36.9386, 36.9268, 36.9355,12.2414, 12.1002, 38.3497, 37.5596, 37.5899, 38.3993, 38.3638, 36.905, 12.2179, 8.0996, 37.3161, 38.2739, 11.8304,29.6347, 12.2262, 9.0314, 7.9003, 37.5379, 11.7044, 12.1634,12.099, 12.2587, 12.0189, 12.1088, 12.1634, 37.5641,38.4677, 36.4505, 36.5542, 38.3383, 38.3159, -0.8277,38.4233, 11.6124, 37.4778, 7.7118, 37.5815, 38.3432,12.291, 38.3192, 39.3012, 37.46, 11.9764, 12.6067, 38.353,8.2172, 12.1577, 12.0596, -0.8277, 12.1002, 37.3895,7.4726, 36.8672, 7.9453, 12.1927, -0.0035, 38.3207, 6.7167,12.1387, 37.2033, 7.7118, 38.3273, 8.3467, 12.2217, 12.0189,6.2534, 12.1577, 7.6422, 5.0544, 12.1271, 6.7189, 5.909,37.4987, 11.5786, 34.3775, 38.246, 11.5786, 12.1515,12.1515, 7.7712, 36.9242, 7.7118, 11.9253, 37.2549, 33.838,11.7528, -0.7968, 38.2229, 34.2909, 6.8377, 11.9173, 12.0172, 11.9764, 38.2615, 29.6424, 7.9886, 5.7311, 8.3188,12.163, 12.1295, 32.5805, -2.5562, 12.2026, 36.4713,38.4158, 8.1079, 11.7525, 4.5227, 12.2262, 36.9011, -0.8902,-2.6506, 12.2117, 8.2172, 29.8234, 11.5494, 11.7044,8.2172, 11.5494, 36.9386, -0.8902, 11.7528, 33.84, 7.481,37.5165, 8.5724, 11.8596, 11.7462, 6.65, 8.6353, 12.2676,6.6724, 37.0288, 12.1577, 12.1295, 8.8065, 10.2842, 12.2414,8.9029, 12.0108, 11.9253, -0.8902, 8.7509, 11.7462, 34.3775,12.1275, 5.0289, 8.6921, 8.747, 12.2597, 7.7118, -0.7885,8.6015, 38.2966, 0.291, 8.7267, 7.0707, 5.4159, 29.6347,11.7462, 5.3304, 7.9314, 36.4211, 33.7242, 6.1851, 29.6268,29.9857, 11.8596, 10.1918, 8.1641, 8.858, 11.5786, 8.8713,11.9253, 33.7242, 8.2332, 31.2635, 12.1577, 8.1275, 32.3893,12.1634, 12.0189, 11.7525, 10.2842, 8.1254, 12.2587,7.9184, 36.8825, 7.8782, 11.9764, 29.6347, 7.7405, 38.4426, 12.1893, 36.9355, 34.416), 
  lat = c(-4.7561, -6.0872,-6.1119, -7.0521, -7.0631, 11.3182, -7.0107, -4.7846,-6.0976, -6.2031, -6.8323, -6.1607, -7.1751, -6.1415,10.5273, 10.4334, -4.8454, -3.4008, -3.304, -4.8041,-4.7969, -7.0444, 10.5674, 9.3498, -3.3274, -4.8102,10.4486, -1.1633, 10.5809, 11.9736, 10.4401, -3.2916,10.4454, 10.6585, 10.6014, 10.5429, 10.5118, 10.6069, 10.6585, -3.4216, -4.7585, -5.9546, -6.2472, -4.7951,-4.5978, 9.6299, -4.6333, 10.394, -6.2847, 10.3331, -3.3354,-4.754, 10.5261, -4.5801, -6.0433, -6.3075, 10.5713,10.9499, -4.82, 10.5912, 10.6785, 10.5646, 9.6299, 10.4334,-6.3154, 10.5103, -6.2321, 10.2126, 10.5249, 9.4466,-4.8022, 9.0667, 10.5487, -3.1917, 10.3331, -4.6056,11.9999, 10.4286, 10.5118, 10.1844, 10.6785, 10.3591,9.2957, 10.5417, 10.0842, 10.2852, -6.966, 10.3422, 1.3356,-4.6618, 10.3422, 10.732, 10.732, 10.9875, -6.1026, 10.3331,10.5391, -3.3403, 1.1085, 10.4503, 9.5953, -4.6215, 1.3177,9.493, 10.3484, 10.3993, 10.5713, -4.6685, -1.1785, 12.0484,10.2842, 12.022, 10.4599, 10.6161, 2.1293, 10.2588, 10.6019,-6.2229, -4.9155, 11.4355, 10.4532, 8.6656, 10.5809,-6.0976, 9.5828, 10.2128, 10.5321, 10.5912, -1.0046,10.321, 10.4454, 10.5912, 10.321, -6.1607, 9.5828, 10.4503,1.1087, 11.0639, -3.4739, 10.3618, 10.5227, 10.3692,9.3833, 11.9376, 10.5287, 12.1623, -6.7976, 10.6785,10.6161, 12.1308, 12.1108, 10.5273, 12.2564, 10.4122, 10.5391, 9.5828, 12.1007, 10.3692, 1.3356, 10.5018, 9.2101,11.9751, 12.0484, 10.5458, 10.3331, 9.6118, 10.5374,-4.8059, 10.1375, 12.04, 10.242, 9.1817, -1.1633, 10.3692,12.5273, 11.0332, -6.198, 1.144, 10.0674, -1.2174, -1.2503,10.5227, 11.6729, 11.3111, 12.1533, 10.3422, 7.3778,10.5391, 1.144, 11.1945, -0.5775, 10.6785, 11.3263, 1.9757,10.6585, 10.5118, 10.4532, 12.1108, 10.5429, 10.5429,9.5345, -6.2292, 11.3547, 10.5713, -1.1633, 10.4297,-4.751, 10.4914, -6.1415, 1.3587))
  d <- merge(d, geo, bx="site", all.y =TRUE)
  
  d$geo_from_source <- FALSE
  d$geo_from_source[!is.na(d$longitude)] <- TRUE
  
  d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
  d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
  d$lat <- d$lon <- NULL
  # fix long and lat Error 
  d$site[d$site=="Tabuhotel/ Chigela"] <- "Tabuhotel / Chigela"
  
  d$latitude[d$site=="Kibedya"] <- -6.1026115
  d$longitude[d$site=="Kibedya"] <- 36.9242074
  d$latitude[d$site=="Mhenda"] <- -7.1751048
  d$longitude[d$site=="Mhenda"] <- 36.9268124
  d$latitude[d$site=="Tabuhotel / Chigela"] <- -6.1414786
  d$longitude[d$site=="Tabuhotel / Chigela"] <- 36.9355071
  d$latitude[d$site=="Vitonga"] <- -6.9660025
  d$longitude[d$site=="Vitonga"] <- 37.4987411
  d$latitude[d$location=="Shiroro"] <- 9.956072
  d$longitude[d$location=="Shiroro"] <- 6.83326
  d$latitude[d$location=="savelugu"] <- 9.6260647
  d$longitude[d$location=="savelugu"] <- -0.8258943
  d$latitude[d$site=="Nyameni"] <- -7.0521061
  d$longitude[d$site=="Nyameni"] <- 36.8940233
  d$latitude[d$location=="Gboko"] <- 7.3778482
  d$longitude[d$location=="Gboko"] <- 8.8713224
    d$latitude[d$location=="Ikara"] <- 11.176471
    d$longitude[d$location=="Ikara"] <- 8.2231659
    d$latitude[d$location=="Paikoro"] <- 9.4930391
    d$longitude[d$location=="Paikoro"] <- 6.8376786
    d$latitude[d$location=="Lere"] <- 10.386966
    d$longitude[d$location=="Lere"] <- 8.572617
  
    # drop NA in location or site
  d <- d[!(is.na(d$location)| is.na(d$site)),]
  
  #add fertilizer
  # Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare;
  # DAP content: 18% of N  and 46% P205 
  d$N_fertilizer <- 0
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  d$P_fertilizer[grepl("NPK",d$fertilizer_type)] <- 30
  d$P_fertilizer[grepl("TSP",d$fertilizer_type)] <- 30
  d$P_fertilizer[grepl("DAP",d$fertilizer_type)] <- 30*0.46/2.29 
  d$P_fertilizer[grepl("SSP",d$fertilizer_type)] <- 30
  d$K_fertilizer[grepl("NPK",d$fertilizer_type)] <- 20
  d$N_fertilizer[grepl("DAP",d$fertilizer_type)] <- 30*0.18
  
  #fix country name
  dd <- carobiner::fix_name(d$country,"title")
  d$country <- dd
  
  
  #fix previous crop name
  P1 <- carobiner::fix_name(d$previous_crop, "lower")
  P1 <- gsub("green_gram", "mung bean",P1)
  P1 <- gsub("pigeon_pea", "pigeon pea" ,P1)
  P1 <- gsub("soyabean", "soybean",P1)
  P1 <- gsub("sweet_potato"," sweetpotato",P1)
  P1 <- gsub("soya_bean"," soybean",P1)
  P1 <- gsub("other", "unknown",P1)
  P1 <- gsub("bush_bean", "common bean",P1)
  P1 <- gsub( "irish_potato", "potato",P1)
  P1 <- gsub( "fallow", "none",P1)
  d$previous_crop <- P1
  # fix fertilize type
  d$fertilizer_type[d$fertilizer_type=="None"] <- "none"
  d$fertilizer_type[d$fertilizer_type=="other"] <- "none"
  #fix crop yield limit by crop
  d$k <- d$yield
  d$yield[d$crop=="common bean" & d$k>9000] <- NA
  d$yield[d$crop=="cowpea" & d$k>6500] <- NA
  d$yield[d$crop=="groundnut" & d$k> 8500] <- NA
  d$yield[d$crop=="soybean" & d$k>15000] <- NA
  d <- subset(d,select = -k)
  # fix data
  for (i in 1:12) {
    d$planting_date <- gsub(month.abb[i],i,d$planting_date)
  }
  
  d$planting_date <- as.character( as.Date(d$planting_date,'%d-%m-%y'))
  d$planting_date[d$planting_date=="2026-04-16"] <- "2016-04-16"
  d$planting_date[d$planting_date=="2026-03-24"] <- "2016-03-24"
   
  # data type
  d$inoculated <- as.logical(d$inoculated)
  
  d$yield_part <- "seed"
  d$yield_part[grepl("groundnut",d$crop)] <- "pod"
  
  d$row_spacing[d$row_spacing == 0] <- NA
  d$plant_spacing[d$plant_spacing == 0] <- NA
  
  d <- unique(d)
  carobiner::write_files(meta, d, path=path)
}


