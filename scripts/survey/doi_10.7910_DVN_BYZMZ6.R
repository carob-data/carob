# R script for "carob"
# license: GPL (>=3)

## ISSUES
#Crop production information is missing.
#The total amount of fertilizer applied is missing.


carob_script <- function(path) {

"
Papua New Guinea Rural Household Survey, 2023

The Papua New Guinea Rural Household Survey (2023) collected detailed household-level data on agricultural production, food and non-food consumption and expenditure, and livelihood strategies across 14 provinces, covering communities in the highlands, lowlands, and islands of Papua New Guinea (PNG). 

The survey was designed using a purposive sampling strategy based on defined agro-ecological zones, which allows for the analysis of key factors influencing rural households and communities. It is important to note that the survey is not nationally representative; however, given the careful random selection of survey areas, we expect that generalizable relationships between variables affecting socio-economic and other development outcomes in rural PNG communities will be consistently observed across representative samples and in this survey. These factors include those that contribute to more resilient local food systems, diversified employment opportunities, and improved household wellbeing.

The survey encompasses 2,699 households in 270 communities, spanning five agroecological zones.It features detailed modules on a wide range of topics relevant to rural livelihoods, agricultural production, and household wellbeing, including:    Household Characteristics — Demographics, education, migration, and other household composition details.   Agricultural Production — Crop production, use of household labor, and other farming-related activities.   Household Assets — Ownership of production equipment, consumer durables, livestock, and housing quality.   Off-farm Activities — Income from wage employment, business activities, and transfers or gifts.   Consumption and Expenditure — Non-food and food expenditures, food consumption patterns, and dietary diversity.   Economic Shocks and Food Insecurity — Perceptions of poverty, recent experiences of food insecurity, and access to health and nutrition extension services.   Child and Mother Anthropometric Measurements — Height and weight of the child and biological mother.   Dietary Quality Questionnaire — Individual-level data from a 24-hour recall of specific food groups consumed.
"

	uri <- "doi:10.7910/DVN/BYZMZ6"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group, recursive=TRUE)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		design = "" ,
		data_type = NA,
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-19",
		carob_completion = 50,	
		carob_effort = 4
	)
	
 	f <- ff[basename(ff) == "com_l_l.dta"]
	f1 <- ff[basename(ff) == "hh_1_0.dta"]
	f2 <- ff[basename(ff) == "hh_1_1.dta"]
	f3 <- ff[basename(ff) == "hh_1_1_La.dta"]
	f4 <- ff[basename(ff) == "hh_1_1_Lb.dta"]
	f9 <- ff[basename(ff) == "hh_1_4.dta"]
	f10 <- ff[basename(ff) == "hh_2_1.dta"]
	f11 <- ff[basename(ff) == "hh_2_1_L.dta"]
	f12 <- ff[basename(ff) == "hh_2_2.dta"]
	f13 <- ff[basename(ff) == "hh_2_3a.dta"]
	f16 <- ff[basename(ff) == "hh_2_4_L.dta"]
	f23 <- ff[basename(ff) == "hh_3_2_L.dta"]
	f27 <- ff[basename(ff) == "hh_4_1_L.dta"]
	
	
	r <- carobiner::read.dta(f)
	r1 <- carobiner::read.dta(f1)
	r2 <- carobiner::read.dta(f2)
	r3 <- carobiner::read.dta(f3)
	r4 <- carobiner::read.dta(f4)
	r9 <- carobiner::read.dta(f9)
	r10 <- carobiner::read.dta(f10)
	r11 <- carobiner::read.dta(f11)
	r12 <- carobiner::read.dta(f12)
	r13 <- carobiner::read.dta(f13)
	r16 <- carobiner::read.dta(f16)
	r23 <- carobiner::read.dta(f23)
	r27 <- carobiner::read.dta(f27)
	
	
####	
	d1 <- data.frame(
	 elevation = r$l2altitude,
	 latitude = round(r$l2latitude, 4),
	 longitude = round(r$l2longitude, 4),
	 com_id = r$community,
	 id = r$today,
	 geo_from_source = TRUE
	)

	d2 <- data.frame(
	  adm1 = trimws(gsub(" Province", "", r1$province)),
	  adm2 =  trimws(gsub(" District", "", r1$district)),
	  hhid = r1$hhid,
	  com_id = r1$community,
	  id = r1$today
	)
	
#### merge d1 and d2
	d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all.y = TRUE)
	d$record_id <- as.integer(1:nrow(d))
	d$id <- NULL
	
	d3 <- data.frame(
	  hh_child_12 = r3$child_u12,
	  hh_child_5 = r3$child_u5,
	  hh_adult_men = r3$adult_male,
	  hh_adult_women = r3$adult_female,
	  age = r3$sec11_4a,
	  sex = r3$sec11_3_gender,
	  #relationship = r3$sec11_2_relat2hd,
	  hhid = r3$hhid,
	  com_id = r3$community
	) 
	
	#### merge d and d3
	d <- merge(d, d3, by= intersect(names(d), names(d3)), all = TRUE)
	d <- d[!duplicated(d$record_id),]
	d$record_id <- NULL
	
	d4 <- data.frame(
	  hhid = r4$hhid,
	  age = r4$sec11__age,
	  civil_status = r4$sec11_7,
	  occupation = r4$sec11_8,
	  com_id = r4$community
	)
	
	d4 <- d4[!is.na(d4$civil_status),]
	#### merge d and d4
	d <- merge(d, d4, by= intersect(names(d), names(d4)), all = TRUE)
	d <- d[!is.na(d$adm1),]
	
	d5 <- data.frame(
	  language = r9$sec14_1,
	  hhid = r9$hhid,
	  com_id = r9$community
	)
	cc_v = c("Tok pisin" =1, "Hiri Mota" =2, "Unserdeutsh"=3, "English"= 4, "local"= 5, "kuanua"=6, "others"= 777)
	code_to_name <- setNames(names(cc_v), as.character(cc_v))
	d5$language <- sapply(strsplit(d5$language, "\\s+"), function(x) {
	  paste(code_to_name[x], collapse = "; ")
	})
	
	#### merge d and d5
	d <- merge(d, d5, by= intersect(names(d), names(d5)), all = TRUE)
	d$record_id <- as.integer(1:nrow(d))
	
	d6 <- data.frame(
	  hhid = r11$hhid,
	  field_id = as.character(r11$garden_num),
	  #field_size = r11$sec21_5a, # unspecified 
	  hh_owner = grepl("own", r11$sec21_5c),
	  crop = substr(r11$sec21_6, 1, 2),
	  herbicide_used = grepl("yes", tolower(r11$sec21_10a)),
	  ferti = r11$sec21_10b,
	  com_id = r11$community
	)
	crop <- c("yam"= 11, "sweetpotato"= 12, "taro"= 13, "banana"= 14, "cassava"= 15, "potato"=16, "rice"= 17, "maize"=18, "banana"= 21, "coconut"= 22, "jackfruit"= 23, "papaya"=24, "mango"=25, "guava"=26, "common bean"=31, "cabbage"= 32, "pumpkin"= 33, "groundnut"=41, "galip"=42, "areca nut"=43, "NA"= 44, "cocoa"=51, "sage"= 52, "coffee"= 53, "tea"=56)
	code_to_name <- setNames(names(crop), as.character(crop))
	d6$crop <- sapply(strsplit(d6$crop, "\\s+"), function(x) {
	  paste(code_to_name[x], collapse = "; ")
	})
	fert <- c("lime"= 106, "NPK(17-17-17)"= 201, "NPK(20-10-10)"= 202, "NPK(25-5-5)"= 203, "urea" = 204, "urea"= 205, "DAP" = 206, "TSP"= 207, "KCL" = 208)
	code_to_name <- setNames(names(fert), as.character(fert))
	d6$fertilizer_type <- gsub("NA;|;NA|     |   | ", "", sapply(strsplit(d6$ferti, "\\s+"), function(x) {
	  paste(code_to_name[x], collapse = "; ")
	}))
	d6$fertilizer_type <- gsub(";NA|NA", "", d6$fertilizer_type)
	d6$fertilizer_type <- ifelse(grepl("NPK", d6$fertilizer_type), "NPK", d6$fertilizer_type)
	d6$fertilizer_type <- gsub("urea;urea", "urea", d6$fertilizer_type)
	
	herb <- c("glyphosate" = 401,  "dimethoate"= 303) 
	code_to_name <- setNames(names(herb), as.character(herb))
	d6$herbicide_product <- gsub("NA;|;NA|     |   | ", "", trimws(sapply(strsplit(d6$ferti, "\\s+"), function(x) {
	  paste(code_to_name[x], collapse = "; ")
	})))
	
	d6[d6==""| d6=="NA"] <- NA
	d6$herbicide_product <- ifelse(is.na(d6$herbicide_product) & grepl("TRUE", d6$herbicide_used), "unknown", gsub(";NA|NA;", "", d6$herbicide_product))
	d6$ferti <- NULL
	
	#### merge d and d6
	d <- merge(d, d6, by= intersect(names(d), names(d6)), all = TRUE)
	d <- d[!duplicated(d$record_id),]
	
	d7 <- data.frame(
	  farm_labour_hh = r12$sec22_9*12,
	  labor_price = r12$sec22_10, # Labor cost per person
	  com_id = r12$community,
	  hhid = r12$hhid
	)
	
	#### merge d and d7
	d <- merge(d, d7, by= intersect(names(d), names(d7)), all = TRUE)
	
	
	d8 <- data.frame(
	  hhid = r23$hhid,
	  animal = r23$sec32_2,
	  animal_price = r23$sec32_3, ## selling price of one animal 
	  com_id = r23$community
	)
	ani_names <- c("pig"= 1, "pig"= 2, "pig"= 3, "chicken"= 4, "chicken"= 5, "chicken"= 6, "cattle"= 7, "goat"= 8, "sheep"= 9, "fish"= 10, "cossowary"= 11, "Deer" = 12, "duck" =13)
	code_to_name <- setNames(names(ani_names), as.character(ani_names))
	d8$animal <- gsub("character\\(0\\)", NA, trimws(sapply(d8$animal, function(x) {
	  code_to_name[x]})))
	
	#### merge d and d8
	d <- merge(d, d8, by= intersect(names(d), names(d8)), all = TRUE)
	d <- d[!duplicated(d$record_id),]
	
	d9 <- data.frame(
	  hhid = r27$hhid,
	  hh_income_source = r27$sec41__inc_name,
	  com_id = r27$community
	)
	
	#### merge d and d9
	d <- merge(d, d9, by= intersect(names(d), names(d9)), all = TRUE)
	d <- d[!duplicated(d$record_id),]
	
	d$adm2 <- gsub("/", "-", d$adm2)
	d$adm2[d$adm2== "Ambunti-Drekikier"] <- "Ambunti-Dreikikir"
	d$adm2[d$adm2== "Kainanatu"] <- "Kainantu"
	d$adm2[d$adm2== "Tambul Nebilyer"] <- "Tambul-Nebilyer"
	d$adm2[d$adm2== "Popondetta"] <- "Sohe"
	
	### Fixing long and lat 
	i <- grepl("Abau", d$adm2) & grepl(paste(148.1775, 148.1818, 148.169, 148.1817, sep ="|"), d$longitude)
	d$longitude[i] <- NA
	d$latitude[i] <- NA
	
	i <- grepl("Alotau", d$adm2) & grepl(paste(150.874, 150.4558, sep ="|"), d$longitude)
	d$longitude[i] <- NA		
	d$latitude[i] <- NA
	
	i <- grepl("Kerema", d$adm2) & grepl(paste(145.7723, 145.7745, 145.765, 145.7745, 145.7722, 145.774,  145.7652, sep ="|"), d$longitude) 
	d$longitude[i] <- NA	
	d$latitude[i] <- NA
	
	
	i <- grepl("North Bougainville", d$adm2) & grepl("154.6707", d$longitude)
	d$longitude[i] <- NA
	d$latitude[i] <- NA
	
	i <- grepl("South Fly", d$adm2) & grepl(paste(143.2, 143.2102, sep ="|"), d$longitude) 
	d$longitude[i] <-  NA
	d$latitude[i] <- NA
	
#  CN: great
#u <- unique(d[, c("adm2", "longitude", "latitude")])
# better way to estimate lon/lat since most locations are clustered within adm2?
#terra::plet(u, "adm2", cex=3)  |> lines(geodata::gadm("PNG", level=2, path))
#a <- aggregate(u[, c("longitude", "latitude")], u["adm2"], mean, na.rm=T)
	
	geo <- data.frame(
	  adm2 = c("Central Bougainville", "North Bougainville", "South Bougainville", "Abau", "Kerowagi", "Kokopo", "Sohe", "Ambunti-Dreikikir", "Kainantu", "Kerema", "Anglimp-South Waghi", "Madang", "Alotau", "Menyamya", "North Fly", "South Fly", "Mul-Baiyer", "Tambul-Nebilyer"),
	  lon = c(148.0871, 151.7734, 142.7366, 144.6196, 155.4895, 145.8807, 145.8695, 144.8775, 152.3150, 145.6913, 146.2538, 144.1884, 154.6885, 141.1886, 141.3323, 155.5946, 142.2237, 144.0158),
	  lat = c(-10.125861, -5.5411, -3.773819, -5.884722, -6.198702, -6.263997, -7.979943, -5.933903, -4.394262, -5.212156, -7.334717, -5.728377, -5.274877, -5.723983, -5.4395, -6.661557, -8.981245, -5.898895),
	  geo_from = FALSE,
	  geo_uncertainty = c(70231, 263638, 76737, 104025, 23991, 30904, 126554, 116454 ,37448, 96638 , 39047, 41829, 126387, 55258, 113929, 184813, 36441, 44991),
	  geo_source = c("GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2")
	)
	
	d <- merge(d, geo, by= "adm2", all.x = TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$geo_from_source[is.na(d$geo_from_source)] <- d$geo_from[is.na(d$geo_from_source)]
	
	d$lon <- d$lat <- d$geo_from <- NULL 
	
	d$is_survey <- TRUE
	d$on_farm  <- FALSE
	d$trial_id <- paste(d$adm1, d$com_id)
	d$yield <- NA_real_
	d$planting_date <- NA_character_
	d$yield_moisture <- NA_real_
	d$yield_part <- "none"
	d$country <- "Papua New Guinea"
	d$irrigated <- NA
	d$yield_isfresh <- TRUE
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	d$com_id <- NULL
	
	carobiner::write_files(path, meta, d)
}


