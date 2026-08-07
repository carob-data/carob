# R script for "carob"

## ISSUES
##P_fertilizer data is in integers meaning it maybe a code for certain amount. This needs to be checked.

# # EGB: N2Africa data typically applies 30 kg/ha of P and about 60 kg/ha of nitrogen fertilizer.
# # In this dataset it is assumed these are the amounts, as per most of the protocols for the country and year.

##Yield and biomass data is recorded as per area but its not clear what area we are working with.We assumed area is 200 m2 but this needs to be re-looked at once we get more feedback from Joost. 
# # EGB: Although some values seem to be very low, most of the plot_are reported seem to be in m2
# # as the kg/ha seem to make sense. This could/need to be confirmed

carob_script <- function(path) {

"Description
N2Africa farm monitoring - Malawi, 2010 - 2011   

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success,  
N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/8YQ1-DM57/D"
	group <- "fertilizer"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication=NA,
		carob_contributor="Effie Ochieng'",
		carob_date="2023-07-27",
		data_type="survey",
		data_organization="IITA",
		project="N2Africa",
		carob_modified_by = "Eduardo Garcia Bendito",
		carob_last_modified = "2024-03-07"
	)

	f0 <- ff[basename(ff) == "a_general.csv"]
	#f1 <- ff[basename(ff) == "b_info_site_1.csv"]
	f2 <- ff[basename(ff) == "b_info_site_2.csv"]
	f3 <- ff[basename(ff) == "c_use_of_package_1.csv"]
	f4 <- ff[basename(ff) == "c_use_of_package_2.csv"]
	#f5 <- ff[basename(ff) == "c_use_of_package_3.csv"]
	#f6 <- ff[basename(ff) == "c_use_of_package_4.csv"]
	#f7 <- ff[basename(ff) == "c_use_of_package_5.csv"]
	f8 <- ff[basename(ff) == "e_harvest.csv"]
	#f9 <- ff[basename(ff) == "f_treatments.csv"]
	f10 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	
	r0 <- read.csv(f0)
	#r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	#r5 <- read.csv(f5)
	#r6 <- read.csv(f6)
	#r7 <- read.csv(f7)
	r8 <- read.csv(f8)
	#r9 <- read.csv(f9)
	r10 <- read.csv(f10)
	
	d <- data.frame( trial_id = r4$farm_id, plot_id = r4$plot_no, adm1 = "Central Region")
	d <- merge(d, r0[,c("farm_id", "country", "district", "vilage")], by.x = "trial_id", by.y = "farm_id")
	d$adm2 <- paste0(carobiner::fix_name(tolower(d$district), case = "title"), " District")
	d$site <- carobiner::fix_name(tolower(d$vilage), case = "title")
	d <- merge(d, r0[,c("farm_id", "gps_altitude")], by.x = "trial_id", by.y = "farm_id")
	colnames(d)[colnames(d) == "gps_altitude"] <- "elevation"
	d <- merge(d, r2[,c("farm_id", "main_crop_last_season")], by.x = "trial_id", by.y = "farm_id")
	colnames(d)[colnames(d) == "main_crop_last_season"] <- "previous_crop"
	d$previous_crop <- tolower(d$previous_crop)
	d$previous_crop[d$previous_crop == "sweet potatoes"] <- "sweetpotato"
	d$previous_crop[d$previous_crop == "fallow"] <- "no crop"
	d$previous_crop[d$previous_crop == "groundnuts"] <- "groundnut"

	d <- merge(d, r3[,c("farm_id", "type_of_package", "plot_size")], by.x = "trial_id", by.y = "farm_id")
	d$crop <- tolower(trimws(gsub("\\+.*","",d$type_of_package)))
	d$crop[d$crop == "groundnuts"] <- "groundnut"
	d$crop[d$crop %in% c("s0ya", "soya")] <- "soybean"
	d$crop[d$crop %in% c("beans", "bean")] <- "common bean"
	d$crop[d$crop == ""] <- NA

	d$intercrops <- NA
	d$intercrops[grep("aize", d$type_of_package)] <- "maize"
	d$inoculated <- grepl("nocul", d$type_of_package)
	d$type_of_package <- NULL
	d <- merge(d, r4[,c("farm_id", "plot_no", "variety_1", "mineral_fert_amount")], by.x = c("trial_id", "plot_id"), by.y = c("farm_id", "plot_no"))
	colnames(d)[colnames(d) == "variety_1"] <- "variety"
	d$variety <- tolower(d$variety)
	d$variety[d$variety == ""] <- NA

	d$fertilizer_type <- NA
	d$fertilizer_type[d$mineral_fert_amount == 1] <- "TSP"
	d$fertilizer_type[d$mineral_fert_amount == 2] <- "TSP;urea"
	d$N_fertilizer <- 0
	d$N_fertilizer[d$mineral_fert_amount == 2] <- as.numeric(60*0.46)
	d$N_splits <- NA
	d$N_splits[d$mineral_fert_amount == 2] <- 2L
	d$P_fertilizer <- 0
	d$P_fertilizer[d$mineral_fert_amount != 0] <- 30L
	d$K_fertilizer <- 0
	d$mineral_fert_amount <- NULL
	
	d <- merge(d, r8[,c("farm_id", "plot_no", "crop_1_area_harvested", "crop_1_weight_grain")], by.x = c("trial_id", "plot_id"), by.y = c("farm_id", "plot_no"))
	d$yield <- (d$crop_1_weight_grain/d$crop_1_area_harvested) * 10000 # kg/m2 -> kg/ha
	d$yield_part <- "grain"
	d <- merge(d, r10[,c("farm_id", "date_planting_dd", "date_planting_mm", "date_planting_yyyy", "date_harvest_dd", "date_harvest_mm", "date_harvest_yyyy")], by.x = "trial_id", by.y = "farm_id")
	d$planting_date <- paste0(d$date_planting_yyyy, "-", sprintf("%02d", d$date_planting_mm), "-", sprintf("%02d", d$date_planting_dd))
	d$planting_date[d$date_planting_dd == 0] <- gsub("00", "01", d$planting_date[d$date_planting_dd == 0])
	d$harvest_date <- paste0(d$date_harvest_yyyy, "-", sprintf("%02d", d$date_harvest_mm), "-", sprintf("%02d", d$date_harvest_dd))
	d$harvest_date[d$date_harvest_dd == 0] <- gsub("00", "01", d$harvest_date[d$date_harvest_dd == 0])
	d$harvest_date[d$harvest_date == "0-01-01"] <- "2011-05-01"
	d$trial_id <- paste0(d$trial_id, "-", d$plot_id)
	d[,grep("date_", colnames(d))] <- NULL
	d$plot_size <- NULL
	d$crop_1_area_harvested <- NULL
	d$crop_1_weight_grain <- NULL
	d$vilage <- NULL
	d$district <- NULL
	d$plot_id <- NULL
	d$country <- "Malawi"
	
	geo <- data.frame(
	    location = c("Thamolatha", "Mkumbi", "Chipulumba", "Sinoya", "Kasungeni Kapanda",
			"Kakunga", "Kavala", "Kalumpha-Mkwinya", "Kachiteya", "Kathobwa",
			"Katobwa", "Kambatata", "Funwell", "Peni", "Pitala", "Chilola",
			"Kabampanje", "Nthondoni", "Amon", "Kapeta", "Chipembere", "Khote",
			"Thukwi", "Mnusu", "Kapuzira", "Mphunga", "Kaseka", "Thengoliweta",
			"Sangwa", "S4", "Chinguluwe Centre", "S3"),
	    longitude = c(NA, 33.156, NA, NA, 33.251, NA, 33.766, NA, NA, 33.166, 33.166, NA, NA,
			33.483, 33.094, NA, NA, 33.133, 33.866, 34.297, 33.1765, NA, NA, NA, NA,
			34.2834, 33.45, NA, 32.974, NA, NA, NA), 
	    latitude = c(NA, -13.534, NA, NA, -13.35, NA, -13.2, NA, NA, -13.55, -13.55, NA, NA,
			-13.183, -13.394, NA, NA, -13.4, -13.366, -12.886, -13.1014, NA, NA, NA,
			NA, -13.6523, -12.666, NA, -13.719, NA, NA, NA))
	
	d <- merge(d, geo, by.x = "site", by.y = "location")
	
	d$longitude[is.na(d$longitude) & d$adm2 == "Mchinji District"] <- 33.055
	d$latitude[is.na(d$latitude) & d$adm2 == "Mchinji District"] <- -13.963
	d$longitude[is.na(d$longitude) & d$adm2 == "Salima District"] <- 34.440
	d$latitude[is.na(d$latitude) & d$adm2 == "Salima District"] <- -13.730
	
	d <- d[!is.na(d$crop),]

	carobiner::write_files(meta, d, path=path)
}

# # EGB:
# # Geocoding
# geo <- carobiner::geocode(country = "Malawi", location = unique(d$site))
# geo$df$lon[geo$df$location == "Mkumbi"] <- 33.156
# geo$df$lat[geo$df$location == "Mkumbi"] <- -13.534
# geo$df$lon[geo$df$location == "Kasungeni Kapanda"] <- 33.251
# geo$df$lat[geo$df$location == "Kasungeni Kapanda"] <- -13.350
# geo$df$lon[geo$df$location == "Kavala"] <- 33.766
# geo$df$lat[geo$df$location == "Kavala"] <- -13.2
# geo$df$lon[geo$df$location == "Kalumpha"] <- 34.1
# geo$df$lat[geo$df$location == "Kalumpha"] <- -11.6
# geo$df$lon[geo$df$location %in% c("Kathobwa", "Katobwa")] <- 33.166
# geo$df$lat[geo$df$location %in% c("Kathobwa", "Katobwa")] <- -13.55
# geo$df$lon[geo$df$location == "Peni"] <- 33.483
# geo$df$lat[geo$df$location == "Peni"] <- -13.183
# geo$df$lon[geo$df$location == "Pitala"] <- 33.094
# geo$df$lat[geo$df$location == "Pitala"] <- -13.394
# geo$df$lon[geo$df$location == "Nthondoni"] <- 33.133
# geo$df$lat[geo$df$location == "Nthondoni"] <- -13.4
# geo$df$lon[geo$df$location == "Amon"] <- 33.866
# geo$df$lat[geo$df$location == "Amon"] <- -13.366
# geo$df$lon[geo$df$location == "Kapeta"] <- 34.297
# geo$df$lat[geo$df$location == "Kapeta"] <- -12.886
# geo$df$lon[geo$df$location == "Kaseka"] <- 33.45
# geo$df$lat[geo$df$location == "Kaseka"] <- -12.666
# geo$df$lon[geo$df$location == "Sangwa"] <- 32.974
# geo$df$lat[geo$df$location == "Sangwa"] <- -13.719
# geo$df$lon[geo$df$location %in% c("S4", "S3")] <- NA
# geo$df$lat[geo$df$location %in% c("S4", "S3")] <- NA
# # geo$df$lon[is.na(geo$df$lon)] <- 32.685
# # geo$df$lat[is.na(geo$df$lat)] <- -13.780
