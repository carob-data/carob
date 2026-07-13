# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"N2Africa impact survey - Mozambique, 2013

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/r6mn-jn95"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

		meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = "",
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-07-09",
		carob_completion = 100,	
		carob_effort = 5
	)

	f1 <- ff[basename(ff) == "a_general_1.csv"]
	f2 <- ff[basename(ff) == "a_general_2.csv"]
	f3 <- ff[basename(ff) == "a_general_4.csv"]
	f4 <- ff[basename(ff) == "c_land_holding_management.csv"]
	f5 <- ff[basename(ff) == "c_land_holding_management_2.csv"]
	f6 <- ff[basename(ff) == "d_crop_production_use.csv"]
	f7 <- ff[basename(ff) == "e_changes_legume_haulm_process.csv"]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)
	r6 <- read.csv(f6)
	r7 <- read.csv(f7)
	
	d1 <- data.frame(
	  hhid = as.integer(r1$id),
	  field_id = r1$farm_id,
	  country = r1$country,
	  adm1 = r1$sector_state,
	  adm2 = r1$action_site,
	  location = r1$village,
	  longitude = r1$gps_longitude_dec,
	  latitude = r1$gps_latitude_dec,
	  elevation = r1$gps_altitude
	)

	# Create a single date column (YYYY-MM-DD)
	d1$date <- as.Date(paste(r1$date_interview_yyyy, r1$date_interview_mm, r1$date_interview_dd, sep = "-"), format = "%Y-%m-%d")
	d1$date <- as.character(d1$date)
	
	d2 <- data.frame(
	  field_id  = r2$farm_id,
	  sex      = trimws(tolower(r2$sex_farmer)),
	  age      = r2$age_farmer,
	  is_head  = r2$farmer_head_hh,
	  hh_size  = r2$total_number_hh,
	  hh_child_18 = r2$children_0_16,
	  hh_adult_women = suppressWarnings(as.numeric(r2$females_17_35) + as.numeric(r2$females_36_65)),
	  hh_elder_women  = as.numeric(r2$females_over_65),
	  hh_adult_men = suppressWarnings(as.numeric(r2$males_17_35) + as.numeric(r2$males_36_65)),
	  hh_elder_men  = as.numeric(r2$males_over_65)
  	)
	
	d2$is_head <- ifelse(d2$is_head == "Y",TRUE,FALSE)
	
	d3 <- data.frame(
	  field_id   = r3$farm_id,
	  animal = trimws(tolower(r3$livestock_type)),
	  heads = r3$livestock_number
	)
	
	d3$animal[d3$animal=="goats"] <- "goat"
	d3$animal[d3$animal=="chickens"] <- "chicken"
	d3$animal[d3$animal=="pigeons"] <- "pigeon"
	d3$animal[d3$animal=="ducks"] <- "duck"
	d3$animal[d3$animal=="pigs"] <- "pig"
	d3$animal[d3$animal=="rabit"] <- "rabbit"
	
	d4 <- data.frame(
	  field_id = r4$farm_id,
	  field_size = r4$farm_size_ha,
	  inoculant = r4$inoculant_type
	  )

	d4$crop_rotation <- apply(r4[, c("crop_rotation_1_season_1", "crop_rotation_1_season_2","crop_rotation_1_season_3","crop_rotation_1_season_4","crop_rotation_2_season_1","crop_rotation_2_season_2","crop_rotation_2_season_3","crop_rotation_2_season_4")],1,function(x) paste(na.omit(x[x != ""]), collapse = ";"))
	d4$crop_rotation[d4$crop_rotation == ""] <- NA
	d4$crop_rotation <- trimws(tolower(d4$crop_rotation))
	d4$inoculated <- ifelse(!is.na(r4$inoculant_obtained) & trimws(r4$inoculant_obtained) != "",TRUE,FALSE)
	d4$fertilizer_type <- apply(r4[, c("p_based_fert_type", "other_min_fert_type")],1,function(x) paste(na.omit(x[x != ""]), collapse = ";"))
	d4$fertilizer_used <- ifelse(r4$p_based_fert_obtained == "Y"|r4$other_min_fert_obtained == "Y",TRUE,FALSE)
	
	#fix fertilizer_type values
	d4$fertilizer_type <- gsub("^npk$", "NPK", d4$fertilizer_type, ignore.case = TRUE)
	d4$fertilizer_type <- gsub("^ssp$", "SSP", d4$fertilizer_type, ignore.case = TRUE)
	d4$fertilizer_type <- gsub("UREIA|Ureia", "urea", d4$fertilizer_type)
	d4$fertilizer_type <- gsub("superphosphato simple", "SSP", d4$fertilizer_type, ignore.case = TRUE)
	d4$fertilizer_type <- gsub("superphosphato", "SSP", d4$fertilizer_type, ignore.case = TRUE)
	d4$fertilizer_type <- gsub("super phosphorus", "SSP", d4$fertilizer_type, ignore.case = TRUE)
	d4$fertilizer_type <- gsub(",", ";", d4$fertilizer_type)
	d4$fertilizer_type <- gsub("/", ";", d4$fertilizer_type)
	d4$fertilizer_type <- gsub(";\\s*", ";", d4$fertilizer_type)
	d4$fertilizer_type[d4$fertilizer_type == "Urea;SSP"] <- "urea;SSP"
	d4$fertilizer_type[d4$fertilizer_type == "SSP;CAL"] <- "SSP;lime"
	d4$fertilizer_type[d4$fertilizer_type == "NP K;inoculants"] <- "NPK"
	d4$fertilizer_type[d4$fertilizer_type == "Phosphorium"] <- "unknown"
	d4$fertilizer_type[d4$fertilizer_type == "Phosphorus"] <- "unknown"
	d4$fertilizer_type[d4$fertilizer_type == "P"] <- "unknown"
	d4$fertilizer_type[d4$fertilizer_type %in% c ("","Cypremethrine")] <- NA
	
	# Convert all separators to ;
	d4$crop_rotation <- gsub("\\+", ";", d4$crop_rotation)
	d4$crop_rotation <- gsub("/", ";", d4$crop_rotation)
	d4$crop_rotation <- gsub(",", ";", d4$crop_rotation)
	d4$crop_rotation <- gsub("\\s-\\s", ";", d4$crop_rotation)
	d4$crop_rotation <- gsub("\\band\\b", ";", d4$crop_rotation, ignore.case = TRUE)
	
	d4$crop_rotation <- trimws(d4$crop_rotation)
	# Standardize crop names
	d4$crop_rotation <- gsub("commun bean|commun beans|common beans|common  bean","common bean", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("soyabean|soybena|sybean|soybeans|maize soybean","soybean", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("soybeancowpea","soybean;cowpea", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("csoybean","soybean", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("cow pea|maize cowpea","cowpea", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("pigeonpea|pegeon pea","pigeon pea", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("goundnut|grundnut","groundnut", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("sesam|sesamee","sesame", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("sunflouwer","sunflower", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("shorgum","sorghum", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("potatoe","potato", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("tobaco|snuff","tobacco", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("manihot","cassava", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("milho", "maize", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("mize","maize", d4$crop_rotation, ignore.case = TRUE)
	d4$crop_rotation <- gsub("butter beans","lima bean", d4$crop_rotation, ignore.case = TRUE)
	# Remove placeholder values
	d4$crop_rotation <- gsub("\\bn\\b", "", d4$crop_rotation)
	# Remove repeated separators
	d4$crop_rotation <- gsub(";+", ";", d4$crop_rotation)
	d4$crop_rotation <- gsub("^;|;$", "", d4$crop_rotation)
	
	d4$crop_rotation <- sapply(strsplit(d4$crop_rotation, ";"), function(x){
	  
	  x <- trimws(x)
	  x <- x[x != ""]
	  x <- unique(x)
	  paste(x, collapse=";")
	})
	
	d4$crop_rotation[d4$crop_rotation=="maize;beans;cassava"] <- "maize;common bean;cassava"
	d4$crop_rotation[d4$crop_rotation %in% c("maize;soybean;bean","maize;bean;common bean;soybean")] <- "maize;soybean;common bean"
	d4$crop_rotation[d4$crop_rotation %in% c("maize;beans","maize;bean")] <- "maize;common bean"
	
	d5 <- data.frame(
	  field_id = r5$farm_id,
	  field_size = r5$size_ha,
	  crop = trimws(tolower(r5$crops_grown)),
	  variety = r5$varieties,
	  fertilizer_type = r5$min_fert_type,
	  fertilizer_amount = r5$min_fert_amount,
	  OM_used = ifelse(r5$organic_input_applied == "Y",TRUE,FALSE),
	  inoculated = ifelse(r5$inoculant_applied == "Y",TRUE,FALSE),
	  yield = r5$harvest_amount
	  )

	#fix crop names
	d5$crop <- gsub("\\+", ";", d5$crop)
	d5$crop <- gsub("/", ";", d5$crop)
	d5$crop <- gsub(",", ";", d5$crop)
	d5$crop <- gsub("\\band\\b", ";", d5$crop, ignore.case = TRUE)
	# Remove percentages (e.g. 90%, 21.05%)
	d5$crop <- gsub("[0-9.]+\\s*%", "", d5$crop)
	d5$crop <- gsub("\\s*;\\s*", ";", d5$crop)
	d5$crop <- trimws(d5$crop)
	# Common spelling corrections
	d5$crop <- gsub("commun bean|coomon bean|commonbean|common bens|commom beans|commun bea",
	                "common bean", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("soyabean|soybeans", "soybean", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("cow pea", "cowpea", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("ground nut", "groundnut", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("gruoundnut", "groundnut", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("pigeonpea", "pigeon pea", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("shorgum|shorghum|sogrhum", "sorghum", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("sesam", "sesame", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("sunflour|sunflouwer", "sunflower", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("coton", "cotton", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("tobaco", "tobacco", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("potatoe", "potato", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("tomatoes", "tomato", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("butter beans", "lima bean", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("butter bean", "lima bean", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("garden cabbage|spring cabbage", "cabbage", d5$crop, ignore.case = TRUE)
	d5$crop <- gsub("gergelim", "sesame", d5$crop, ignore.case = TRUE)
	# Remove duplicate separators and leading/trailing semicolons
	d5$crop <- gsub(";+", ";", d5$crop)
	d5$crop <- gsub("^;|;$", "", d5$crop)
	d5$crop <- trimws(d5$crop)
	
	d6 <- data.frame(
		field_id = r6$farm_id,
		crop = r6$crop,
		yield = r6$total_production_farm
	)
  
	#fix crop names
	d6$crop <- tolower(trimws(d6$crop))
	d6$crop[d6$crop %in% c("commun bean", "commun beans","commonbean", "common  bean","common bens", "common")] <- "common bean"
	d6$crop[d6$crop == "soyabean"] <- "soybean"
	d6$crop[d6$crop == "soybeans"] <- "soybean"
	d6$crop[d6$crop == "cow pea"] <- "cowpea"
	d6$crop[d6$crop %in% c("ground nut", "ground nut ")] <- "groundnut"
	d6$crop[d6$crop %in% c("pigeonpea", "pigoen pea", "pegeon pea")] <- "pigeon pea"
	d6$crop[d6$crop == "sogghum"] <- "sorghum"
	d6$crop[d6$crop %in% c("sunflour", "sunflouwer")] <- "sunflower"
	d6$crop[d6$crop == "sesam"] <- "sesame"
	d6$crop[d6$crop == "sesame "] <- "sesame"
	d6$crop[d6$crop == "gergelim"] <- "sesame"
	d6$crop[d6$crop == "potatoe"] <- "potato"
	d6$crop[d6$crop == "tomatoes"] <- "tomato"
	d6$crop[d6$crop == "tobaco"] <- "tobacco"
	d6$crop[d6$crop %in% c("butter bean", "butter beans","butterbean", "butter")] <- "lima bean"
	d6$crop[d6$crop %in% c("garden cabbage", "spring cabbage")] <- "cabbage"
	d6$crop[d6$crop == "amendoim"] <- "groundnut"
	d6$crop[d6$crop == "manihot"] <- "cassava"

	d7 <- data.frame(
		field_id = r7$farm_id,
		previous_crop_residue_management = r7$haulm_use_now
		)

 d <- d6
 d <- merge(d, d1, by = "field_id", all.x = TRUE)
 d <- merge(d, d2, by = "field_id", all.x = TRUE)
 d <- merge(d, d3, by = "field_id", all.x = TRUE)
 d <- merge(d, d4, by = "field_id", all.x = TRUE)
 d <- merge(d, d5, by = c("field_id","field_size","fertilizer_type","inoculated"), all.x = TRUE)
 d <- merge(d, d7, by = "field_id", all.x = TRUE)
 
 d$crop.y <- d$yield.y <- NULL
 names(d)[names(d) == "crop.x"] <- "crop"
 names(d)[names(d) == "yield.x"] <- "yield"
 
  d$trial_id <- as.character(as.integer(as.factor(1)))
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
  d$planting_date <- as.character(as.Date(NA))
	d$harvest_date  <- as.character(as.Date(NA))
	
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- as.numeric (NA)
  d$fertilizer_type[d$fertilizer_type %in% c ("ssp;urea","SSP;UREA","Urea;ssp")] <- "SSP;urea"
  d$fertilizer_type[d$fertilizer_type=="SSP;Urea;NPK"] <- "SSP;urea;NPK"
  d$fertilizer_type[d$fertilizer_type=="NPK;Urea"] <- "NPK;urea"
  
  d$crop[d$crop=="common beans"] <- "common bean"
  d$crop[d$crop=="boer bean"] <- "faba bean"            #assuming boar was a typing error for broad
  d$crop[d$crop=="snuff"] <- "tobacco"
  # Create variable
  d$yield_part <- NA
  
  d$yield_part[d$crop %in% c("maize", "sorghum", "rice")] <- "grain"
  
  d$yield_part[d$crop %in% c("common bean","soybean","faba bean","groundnut","sesame","cowpea","pigeon pea","sunflower","lima bean","cotton")] <- "seed"
  d$yield_part[d$crop == "cassava"] <- "roots"
  d$yield_part[d$crop == "potato"] <- "tubers"
  d$yield_part[d$crop %in% c("tomato")] <- "fruit"
  d$yield_part[d$crop %in% c("lettuce", "cabbage", "tobacco")] <- "leaves"
  d$yield_part[d$crop %in% c("onion", "garlic")] <- "none"
	d$yield_isfresh <- NA
	
	#add missing longitude & latitude
	d$longitude[d$location=="Namurequele"] <- "36.7861"
	d$latitude[d$location=="Namurequele"] <- "-15.3492"
	d$longitude[d$location %in% c("Chimbuwa","Chimbua","Nhamukunga","Chicilua")] <- "33.4"
	d$latitude[d$location %in% c("Chimbuwa","Chimbua","Nhamukunga","Chicilua")] <- "-19.5"
	d$longitude[d$location %in% c("Dhimbontory","Chiquiso","Chikuzo","Chiquizo")] <- "33.9"
	d$latitude[d$location %in% c("Dhimbontory","Chiquiso","Chikuzo","Chiquizo")] <- "-19.6"
	d$longitude[d$location %in% c("Chindeque","chideque")] <- "34.4"
	d$latitude[d$location %in% c("Chindeque","chideque")] <- "-14.7"
	d$longitude[d$location %in% c("chilombo","amba amba")] <- "34.1"
	d$latitude[d$location %in% c("chilombo","amba amba")] <- "-14.5"
	d$longitude[d$location %in% c("NITA","Nita")] <- "39.7"
	d$latitude[d$location %in% c("NITA","Nita")] <- "-15.9"
	d$longitude[d$location %in% c("murimo","murrimu","Murrimo","Muripa")] <- "36.7947"
	d$latitude[d$location %in% c("murimo","murrimu","Murrimo","Muripa")] <- "-15.3508"
	d$longitude[d$location=="Muripa"] <- "36.8225"
	d$latitude[d$location=="Muripa"] <- "-15.3492"
	d$longitude[d$location=="Nampawa"] <- "39.4667"
	d$latitude[d$location=="Nampawa"] <- "-15.6667"
	d$longitude[d$location %in% c("Mahuwa","Muiane","Muiwane","Kokwe","Muhuwa")] <- "39.2"
	d$latitude[d$location %in% c("Mahuwa","Muiane","Muiwane","Kokwe","Muhuwa")] <- "-15.6"
	d$longitude[d$location %in% c("PISSI-1","Pissi-1","Pissi - 1","Pissi","Pini","pissi-1","Pissone","Metovola","pedeniva")] <- "36.82"
	d$latitude[d$location %in% c("PISSI-1","Pissi-1","Pissi - 1","Pissi","Pini","pissi-1","Pissone","Metovola","pedeniva")] <- "-15.1289"
	d$longitude[d$location %in% c("Sodoma","Sopoma")] <- "36.8"
	d$latitude[d$location %in% c("Sodoma","Sopoma")] <- "-15.3"
	d$longitude[d$location %in% c("Sede Nova","sede nova")] <- "36.9911"
	d$latitude[d$location %in% c("Sede Nova","sede nova")] <- "-15.4878"
	d$longitude[d$adm2 == "Angonia"] <- "34.4"
	d$latitude[d$adm2 == "Angonia"] <- "-14.8"
	d$longitude[d$adm2 == "Gondola"] <- "33.4"
	d$latitude[d$adm2 == "Gondola"] <- "-19.3"
	d$longitude[d$location %in% c("Ruace","ruace","Ruace - sede")] <- "36.675"
	d$latitude[d$location %in% c("Ruace","ruace","Ruace - sede")] <- "-15.2217"
	d$longitude[d$location %in% c("Sodoma","Sopoma")] <- "36.8"
	d$latitude[d$location %in% c("Sodoma","Sopoma")] <- "-15.3"
	d$longitude[d$location == "Mangigi"] <- "36.9508"
	d$latitude[d$location == "Mangigi"] <- "-15.5219"
	d$longitude[d$location == "Mangigi"] <- "36.9508"
	d$latitude[d$location == "Mangigi"] <- "-15.5219"
	d$longitude[d$adm2 == "Tsangano"] <- "34.4"
	d$latitude[d$adm2 == "Tsangano"] <- "-14.8"
	d$longitude[d$location == "Mecutamala"] <- "39.5"
	d$latitude[d$location == "Mecutamala"] <- "-15.7"
	
	i <- d$location == "Chikuizo"
	d$latitude[i]  <- -19.3
	d$longitude[i] <- 33.3
	
	d$latitude[d$adm1 == "nampula" & d$adm2 == "mogovolas" & d$location == "vorra"] <- -15.7
	
#remove empty character values
	vars <- c("adm1", "adm2", "location", "previous_crop_residue_management")
	
	for (v in vars) {
	  d[[v]] <- trimws(d[[v]])
	  d[[v]][d[[v]] == ""] <- NA
	}
	
	d$adm1 <- trimws(d$adm1)
	d$longitude <- as.numeric(d$longitude)
	d$latitude <- as.numeric(d$latitude)
	d$elevation[d$elevation > 5000] <- NA
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}

