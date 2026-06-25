# R script for "carob"


carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and 
  productivity of grain legumes among African smallholder farmers which will 
  contribute to enhancing soil fertility, improving household nutrition and increasing
  income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable,
  long-term partnerships to enable African smallholder farmers to benefit from symbiotic 
  N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers
  adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development
  will be the legacy of the project.The project is implemented in five core 
  countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other 
  countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"
  
  uri <- "doi:10.25502/r6sb-2f47"
  group <- "fertilizer"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
    project="N2Africa",
    publication=NA,
    data_organization = "IITA",
    data_type="survey", # or, e.g. "on-farm experiment", "survey", "compilation"
    carob_contributor="Effie Ochieng'",
	carob_effort = NA,
	carob_date="2023-09-30"
  )
  
  
  
  #read the data
  f <- ff[basename(ff) == "data_table.csv"]
  r <- read.csv(f)
  
  from1 <- c("lga_district_woreda","sector_ward","package_legume","package_variety","inputs_used_on_plot_master_group_plot_id_repeat","date_hhsurvey_1.date","crop1_grain_weigth_plot_kg_master_group_yield_repeat","crop1_pod_weigth_plot_kg_master_group_yield_repeat","width_plot_m_master_group_plot_char_repeat","length_plot_m_master_group_plot_char_repeat", "other_crops_previous_season")
  d <- carobiner::change_names(r[,from1], from1, 
                               c("adm1","location","crop","variety","treatment","date","yield1","yield2","width","length", "crop_rotation"))
  d$country <- carobiner::fix_name(r$country, case = "title") 
  d$adm1 <- carobiner::fix_name(d$adm1, case = "title")
  d$date <- format(as.Date(d$date, format = "%d-%b-%y", locale = "C"), "%Y-%m-%d")
  d$trial_id <- paste(r$id,r$farm_id, sep = "_")
  
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$yield_part <- "seed"
  d$inoculated <- grepl("inoculant",d$treatment)
  d$variety <- r$package_variety
  d$location <- r$sector_ward
  d$crop <- carobiner::replace_values(d$crop,c("soya_bean","faba_bean","bush_bean","climbing_bean"), c("soybean","faba bean","common bean","common bean"))
  # # EGB: Try to include crop rotation since information is available
  rotations <- data.frame(stringr::str_split_fixed(d$crop_rotation, " ", 5))
  rotations[rotations==""] <- NA
  for (col in 1:ncol(rotations)) {
    # # EGB: Removing fallow, other, vegetables, and khat from rotation
    rotations[,col] <- ifelse(rotations[,col] %in% c("other", "fallow", "khat", "vegetables yam cowpea groundnut soyabean"), NA, rotations[,col])
    rotations[,col] <- carobiner::replace_values(rotations[,col],c("irish_potato","climbing_bean","sweet_potato","bush_bean", "soyabean","pigeon_pea","faba_bean","bambara_bean"), c("potato","common bean","sweetpotato","common bean", "soybean","pigeon pea", "faba bean","bambara groundnut"), must_have = FALSE)
  }
  d$crop_rotation <- gsub('^;|;$', '', gsub("NA", "",paste(rotations[,1], rotations[,2], sep = ";")))
  d$crop_rotation <- ifelse(d$crop_rotation == "", NA, d$crop_rotation)

  # efyrouwa: some widths and lengths are either too small or too large,
  ##looking at the protocol I decided on the following cutoff points 
  d$width[d$width < 6 | d$width > 20] <- NA
  d$length[d$length < 10| d$length > 20] <- NA
  
  # calculating the yield, yield 2 was the unshelled yield
  d$yield <- 10000 / (d$length * d$width) * ifelse(is.na(d$yield1), d$yield2, d$yield1)   # Combine columns and calculate yield kg/ha
  d$fertilizer_type <- "none"
  d$fertilizer_type[grep("DAP",d$treatment)] <- "DAP"
  d$fertilizer_type[grep("NPK",d$treatment)] <- "NPK"
  d$fertilizer_type[grep("TSP",d$treatment)] <- "TSP"
  d$fertilizer_type[grep("SSP",d$treatment)] <- "SSP"
  d$fertilizer_type[grep("other_mineral_fertilizer",d$treatment)] <- "unknown"
  d$OM_used <- grepl("organic_fertilizer",d$treatment)
  d$treatment2 <- r$inputs_used_on_plot_master_group_plot_id_repeat# This also looks like it would pass for the treatments???
  d$N_fertilizer <- 0
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  # filling in the lats and lon 
  ## efyrouwa: still 277 lats and lon info to be filled, 
  ## u <- unique(d[complete.cases(d$location), c("country","location")])
  ## g <- carobiner::geocode(country = u$country,  location = u$location, service = "nominatim")
  ## run g$put to get the information making the data frame below
  
  
  g <-  data.frame(country = c("Uganda", "Ethiopia", "Uganda", "Uganda",  "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda",
		"Ethiopia", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda","Uganda",
		"Uganda", "Uganda", "Uganda", "Uganda", "Uganda",  "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda",
		"Uganda", "Uganda", "Uganda", "Uganda","Nigeria", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda","Uganda", "Uganda", "Uganda",
		"Ethiopia", "Uganda", "Uganda", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Ghana", "Nigeria",
		"Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria","Tanzania", "Tanzania", "Tanzania",
		"Tanzania", "Ghana","Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Uganda", "Tanzania", "Tanzania", "Tanzania",
		"Tanzania", "Tanzania", "Ghana", "Nigeria", "Ghana", "Ghana", "Tanzania", "Nigeria", "Tanzania", "Tanzania", "Nigeria", "Nigeria",
		"Tanzania", "Tanzania", "Tanzania", "Nigeria", "Tanzania", "Ghana", "Tanzania", "Tanzania", "Tanzania", "Nigeria", "Ghana", "Tanzania",
		"Nigeria", "Tanzania", "Nigeria", "Nigeria", "Tanzania", "Ghana", "Tanzania", "Uganda", "Tanzania", "Nigeria", "Nigeria", "Ethiopia",
		"Tanzania", "Tanzania", "Ghana", "Nigeria", "Nigeria", "Tanzania", "Nigeria", "Ghana", "Uganda", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Ghana", "Nigeria", "Nigeria", "Ghana", "Ghana", "Nigeria", "Ghana", "Ghana", "Nigeria", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda"), 
        location = c("Kosire, Kidongole","Tsion", "Tangwen", "Kajamaka,Kidongole", "Bufukhula/Bulambuli", "Kituba/Kakures", "Alido", "Katekwana", "Kakures/Kakures", 
        "Odedei", "Kino", "Gamudede", "Odero", "Abedober", "Nakazungu","Miroi", "Bunanganda/Bunanganda", "Alidi A", "Bufukhula/Nabongo", 
        "Bulambuli", "Bumatoola", "Khabutoola", "Kajere", "Katekwan","Kamutur", "Chemwet", "Odongowe", "Bumurumu", "Bugarama/Buhara/Buhara/Kabale", 
        "Gisingo", "Omerein -Odeidei", "Bunambutye/Bumasari/Bubuya","Kashaki A/Buhara/Buhara/Kabale", "Aduu", "Kanyamutamu,Kidongole", 
        "Kocus", "Alyet", "Kajamaka Kidongole", "Kawo,Kidongole","Lulyo, kapenguria", "Yarkasuwa", "Kamakoma", "Kanyipa/Kanyipa", 
        "Bumufuni II", "Kakures/kakures", "Gubongoi", "Apopong","Makogoti", "Kigugwe", "Chenker", "Akakaat", "Kitobo A", 
        "Patigi", "Madakiya", "Aboki", "Amua", "Nasarawa", "Kaima","Wazata", "Kukuo", "Korinya", "Ilesha", "Malete", "EJIDONGARI", 
        "Ilota", "Oke-oyi", "Rimau", "Nariya", "ILOTA", "Mariga","Majawanga", "Milungui", "Mshizii", "Ihenje", "Siriyiri", 
         "Nyameni", "Kwemashai", "Tabuhotel", "Mangae", "Kibedya","Hambalawei", "Nyamushungwa/Bugarama/Kaharo", "Mlanga", "Msingisi", 
        "Baga", "Dule", "Ulaya Mbuyuni", "Yemo", "Soba", "Nayoko no1","Saka", "Msufini", "Zongo", "Kimashuku", "Ijaka", "Nassarawa", 
        "Hema", "Makuyu", "Tubugwe Kibaoni", "Vitonga", "Kaji", "Kiruweni","Tibali", "Mogohigwa", "Rauya", "Matongoro", "jauro Garga", 
        "Karaga", "Sagara B", "Kigir", "Mkalama", "Tangaramta", "Lere","Mawanjeni", "Charia", "Sejeli", "Omuruyenje/Bugarama/Kaharo", 
        "Ulaya mbuyuni", "yimirshika", "maina Baba", "Fate", "Matala", "Makuyuni", "Corner", "gaidam", "Tum", "Kyomu", "kabura", 
        "Tumahi", "Rugarambiro/Bugarama/Kaharo", "Sakdiya", "Dunkur Gusi","Yawi", "Briyel", "Nzukuku", "0", "peta", "mbulatawiwi", 
        "Ase", "Hill-Top", "Yamarkumi", "As3", "Zangbalun", "Tong","Kigarama/Rwanyena/Rubaya", "Kasinde/Mwendo/Kitumba", "Kigarama", 
        "Nyarushanje, Muramba, Rutenga", "Kayorero/Katenga/Kaharo", "Kabirago/Rwanyena/Rubaya", "Nyakahita/Kaharo/Kaharo"), 
         longitude = c(33.9858, 38.7324, 34.3937, 33.9581, 34.2634, 33.912, 32.6536, 34.0296, 33.9167, 33.9084, 37.437, 34.3924, 33.4241, 32.999, 34.3572, 
         34.15, 34.3178, 32.6108, 34.3014, 34.2761, 34.2121, 34.2675, 34.356, 34.0065, 34.2167, 34.3848, 32.9932, 34.2648, 30.0398, 
         34.3594, 33.9156, 34.3524, 29.9923, 32.9577, 33.9758, 34.2131, 32.9363, 33.9581, 34.0165, 34.435, 7.9345, 29.8175, 34.1333, 
         34.2269, 33.9167, 34.3948, 33.7027, 34.3633, 29.984, 37.2667, 34.2116, 34.389, 6.0833, 8.2956, 9.1667, 8.8545, 8.1845, 
         3.9481, 7.481, 0.1586, 8.9222, 4.7415, 4.5227, 4.6491, 4.7333, 4.99, 7.7405, 8.6733, 4.7333, 5.867, 36.8414, 38.3653, 38.353, 
		36.9386, -2.5927, 36.894, 38.3207, 36.9355, 37.2409, 36.9242,  38.2615, 30.0606, 36.4164, 36.8679, 38.4144, 38.3192, 36.913, 
		-0.1689, 8.0299, -0.7652, -0.2038, 36.9045, 8.617, 37.2549,36.5844, 8.1845, 11.9546, 37.1884, 36.6291, 37.4987, 10.1792, 
		37.5899, -0.8439, 36.9011, 37.5397, 36.4682, 11.5494, -0.4317,  36.5314, 12.2179, 34.7439, 12.132, 8.5724, 37.5815, -2.5746, 
		36.3233, 30.0606, 36.913, 12.2414, 11.5786, 38.2371, 37.5598, 38.3316, -0.0931, 11.7525, 12.1088, 37.5165, 12.2262, -0.7978, 
		30.0606, 11.7705, 11.9989, 12.2185, 11.6124, 12.1445, -1.0841,11.9173, 12.1002, -2.3338, -1.0887, 12.1634, 121.054, -0.9893, 
		12.2587, 29.9553, 29.9589, 30.2875, 29.8314, 30.1348, 29.9369,30.0498), 
        latitude = c(1.2395, 9.0654, 1.3174, 1.2456, 1.3621, 1.2753, 1.7925, 1.2077, 1.3, 1.3094, 12.5876, 1.3094, 2.5406, 2.4174, 1.2425, 1.2667, 0.8611, 2.29, 1.3431, 1.3495, 0.8535, 
        0.9043, 1.2459, 1.2121, 1.3667, 1.3548, 2.4213, 0.8625, -1.3689,1.2495, 1.3149, 1.4184, -1.2783, 2.4314, 1.2577, 1.3262, 
        1.7248, 1.2456, 1.2498, 1.3703, 11.9175, -0.9909, 1.3179,0.8141, 1.3, 1.3108, 2.2795, 1.2405, -1.2449, 12.3167, 1.4296, 
        1.311, 9.35, 9.6853, 12.6, 7.1128, 8.5474, 9.6225, 11.0639,8.8494, 6.9154, 7.6246, 8.6656, 8.8643, 8.4167, 8.2548, 10.4297, 
        11.5657, 8.4167, 10.6074, -6.1127, -4.7561, -4.82, -6.1607,10.0427, -7.0521, -4.8022, -6.1415, -7.0631, -6.1026, -4.6685, 
        -1.2354, -6.2198, -6.2031, -4.7846, -4.5801, -7.0356, 5.6367,10.958, 10.3771, 5.7304, -7.788, 9.1343, -3.3403, -6.2484, 
        8.5474, 10.4324, -6.0417, -6.3566, -6.966, 11.6739, -3.304,9.6664, -6.0976, -3.3241, -5.8027, 10.321, 9.9268, -6.2417, 
        10.5674, -4.214, 10.4903, 10.3618, -3.3354, 10.1107, -6.0326, -1.2354, -7.0356, 10.5273, 10.3422, 5.6444, -3.3348, -5.0204, 
        10.8251, 10.4532, 10.6069, -3.4739, 10.5809, 9.5673, -1.2354, 10.4336, 10.3987, 10.5893, 10.394, 10.625, 7.8574, 10.3484, 
        10.4334, 7.347, 10.8942, 10.6585, 14.6968, 9.5583, 10.5429,-1.3973, -1.2605, 0.6047, -0.949, -1.2438, -1.3594, -1.2233))
  
  d <- merge(d,g , by =c("country","location"), all = TRUE) 
  
  # fill in lat and lon that were in the original data set
  for (i in 1:nrow(r)) {
    if (!is.na(r$gps_field_device_latitude.decimal_degrees[i]) && is.na(d$latitude[i]) &&
        !is.na(r$gps_field_device_longitude.decimal_degrees[i]) && is.na(d$longitude[i])) {
      d$latitude[i] <- r$gps_field_device_latitude.decimal_degrees[i]
      d$longitude[i] <- r$gps_field_device_longitude.decimal_degrees[i]
    }
  }
  
  d <- d[,c("trial_id","on_farm","is_survey","country","adm1","location","latitude","longitude","crop","variety","treatment","inoculated","fertilizer_type","OM_used","N_fertilizer","P_fertilizer","K_fertilizer","date","yield","yield_part")]
  
  cat("efyrouwa: lat and lon for 277 locations to be filled \n    some points are not accurate\n    NPK rates to be filled\n    find plot lengths and widths\n")

  carobiner::write_files(meta, d, path=path)
}


