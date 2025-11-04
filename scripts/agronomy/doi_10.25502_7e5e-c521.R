# R script for "carob"


carob_script <- function(path) {
	
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project."
	
	uri <- "doi:10.25502/7e5e-c521"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication= NA, 
		data_organization = "IITA;ICRAF;WUR",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-07-14",
		data_type="experiment",
		project="N2Africa",
		response_vars= "yield",
		treatment_vars="N_fertilizer;P_fertilizer;K_fertilizer"
	)
	
	
	f <- ff[basename(ff) == "data_table.csv"] 
	# read the dataset
	r <- read.csv(f)

	# process file(s)
 
	d <- r[, c("country", "id", "lga_district_woreda", "sector_ward", "gps_field_device_latitude.decimal_degrees", "gps_latitude_field.decimal_degrees", "gps_field_device_longitude.decimal_degrees", "gps_field_device_longitude.decimal_degrees", "crop_1_previous_season", "inoculation_n2africa_field", "pack_species", "pack_variety","pack_mineral_fertilizer_type")] 
	
	colnames(d) <- c("country", "trial_id", "location", "site", "latitude1", "latitude2", "longitude1", "longitude2","previous_crop", "inoculated","crop", "variety", "fertilizer_type")
	
	# Fix long and lat columns 
	i <- is.na(d$latitude1)
	d$latitude1[i] <- d$latitude2[i]
	i <- is.na(d$longitude1)
	d$longitude1[i] <- d$longitude2[i]
	# keep the relevant column of long and lat
	d$latitude <- d$latitude1
	d$longitude <- d$longitude1
	
	oldnms <- c("id", "lga_district_woreda", "country", "row_spacing_crop_1_plot_X.cm" , "plant_spacing_crop_1_plot_X.cm", "grain_weight_crop_1_plot_X.kg", "pod_weight_groundnut_crop_1_plot_X.kg", "width_of_harvested_plot_crop_1_plot_X.m", "no_plants_hole_crop_1_plot_X.nr", "number_of_rows_crop_1_plot_X.nr","date_of_planting_X.date")	
	
	newnms <- c("trial_id", "location", "country", "row_spacing", "plant_spacing", "yield1", "yield2", "width_size_plot", "number_plant", "number_row","planting_date")
	
	names(r) <- gsub("number_of_rows_crop_1_plot_.nr", "number_of_rows_crop_1_plot_3.nr", names(r))
	
	r$date_of_planting_3.date <- NA # missing variable
	
	lst <- list()
	i <- c(1,3)	# c(1, 3)	because number_of_rows_crop1_plot_2.nr	is missing Therefore we can can't have plot area and then yield

## RH is this is wrong see areas etc 
## unit_area_field_2	amount_harvested_most_important_crop_field_2	unit_amount_field_2

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
	# calculate the area 
	d1$area <- d1$width_size_plot * d1$number_row * d1$row_spacing / 100 # in m2
	d1$yield <- 10000 * d1$yield1 / d1$area #kg/ha
	#d1$yield[d1$yield > 6000] <- NA
	# merge d and d1
	d <- merge(d,d1,by=c("trial_id", "location", "country"),all.x = T)
	
	#d[c('planting_date', 'time')] <- stringr::str_split_fixed(d$planting_date, " ", 2)
	#d[c('harvest_date', 'time')] <- stringr::str_split_fixed(d$harvest_date, " ", 2)
	
	d <- d[, c("country", "trial_id", "location", "site", "longitude", "latitude", "planting_date"
						 , "crop", "previous_crop", "variety", "inoculated","row_spacing", "plant_spacing", "yield","fertilizer_type")]
	# Add columns
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$inoculated[d$inoculated=="y"] <- TRUE
	d$inoculated[d$inoculated=="n"] <- FALSE
	
	# Fix long and lat coordinate 
	geo <- data.frame(
		location = c("HAWUL",NA, "Kibuku", "HAWUL", "Kabale", "Pallisa", "HAWUL", "Bugiri","HAWUL", "BIU", "HAWUL", "Kisoro", "Pallisa", "Kisoro", "Apac","HAWUL", "Lushoto", "HAWUL", "KWAYA KUSAR", "Lushoto", "Lushoto","KWAYA KUSAR", "BAYO", "BAYO", "Bugiri", "HAWUL", "HAWUL","HAWUL", "Paikoro", "Kisoro", NA, "KWAYA KUSAR", "BAYO","BAYO", "HAWUL", "KWAYA KUSAR", "BAYO", "BIU", "Bayo", "Lushoto","Lushoto", "Rakai", "BIU", "HAWUL", "HAWUL", "HAWUL", "BAYO","Lushoto", NA, "BIU", "KWAYA KUSAR", "HAWUL", "Pawe", "Pallisa","Pallisa", "Biu", "Apac", "BIU", "Kisoro", "Lushoto", "Kabale","BAYO", "BIU", "BIU", "KWAYA KUSAR", "KWAYA KUSAR", "HAWUL","BIU", "HAWUL", "BIU", "Kisoro", "Apac", "Lushoto", "Kibuku","Lushoto", "Lushoto", "Kisoro", "Pallisa", NA, "Bugiri","binduri", "Lapai", "Bugiri", "Gwer West", "Kisoro", "Paikoro","HAWUL", "Paikoro", "Kisoro", "Kisoro", "Kisoro", "Rakai","Lushoto", NA, "Kibuku", "Kibuku", NA, NA),
		lon = c(12.2117,32.3925, 33.8697, 12.0596, 29.9828, 34.1646, 12.0189, 33.7225,12.1927, 12.1295, 12.1275, 29.6132, 33.7744, 29.6132, 32.5302,12.2597, 38.2229, 11.9764, 11.9253, 38.246, 38.2739, 11.8543,11.7044, 11.7525, 33.6667, 12.1002, 12.1893, 12.2587, 6.8377,29.6167, 8.617, 12.0108, 11.5786, 11.7462, 12.2217, 11.8304,11.5494, 12.1577, 11.7044, 38.353, 38.2615, 31.2857, 12.2026,12.2676, 11.9764, 12.1271, 11.5786, 38.3207, 32.7258, 12.0318,11.8596, 12.1387, 36.3873, 33.7659, 33.7934, 12.1577, 32.471,12.1634, 29.6268, 38.3159, 29.9251, 11.6124, 12.2179, 12.1577,11.9173, 11.8304, 12.291, 12.1295, 12.0596, 12.2262, 29.6132,32.4523, 38.448, 33.7242, 38.3192, 38.3273, 29.6167, 33.7833,5.7048, 33.6661, -0.3062, 6.6754, 33.6317, 8.2063, 29.6446,6.6723, 12.2414, 6.65, 29.7487, 29.6132, 29.6268, 31.2531, 38.353, 32.423, 33.8505, 33.84, 6.1112, 12.0172),
		lat = c(10.5321,2.4063, 1.1328, 10.5646, -1.2416, 1.0794, 10.5118, 0.635,10.5249, 10.6161, 10.5018, -1.1917, 1.278, -1.1917, 2.0019,10.5458, -4.6215, 10.5713, 10.5391, -4.6618, -4.8102, 10.4857,10.4454, 10.4532, 0.4167, 10.4334, 10.4914, 10.5429, 9.493, -1.2001, 9.1343, 10.4122, 10.3422, 10.3692, 10.4286, 10.4486,10.321, 10.6785, 10.4454, -4.82, -4.6685, -0.7326, 10.6019,10.5287, 10.5713, 10.5417, 10.3422, -4.8022, 2.3404, 10.8436,10.5227, 10.5487, 11.2597, 1.2879, 1.2922, 10.6785, 2.0636,10.6585, -1.2174, -4.5978, -1.1963, 10.394, 10.5674, 10.6785,10.3484, 10.4486, 10.5261, 10.6161, 10.5646, 10.5809, -1.1917,2.0449, -4.4953, 1.144, -4.5801, -4.6056, -1.2001, 1.3167, 8.1348, 0.4362, 10.9695, 8.7811, 0.4593, 7.621, -1.2358,9.3437, 10.5273, 9.3833, -1.2346, -1.1917, -1.2174, -0.5578,-4.82, 2.4199, 1.102, 1.1087, 8.3288, 10.3993))
	
	d <- merge(d,geo,bx="location", all.y =TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$lat <- d$lon <- NULL
	 # fix long and lat Error 
	d$latitude[d$site=="Manolo"] <- -4.6214543
	d$longitude[d$site=="Manolo"] <- 38.2229416
	d$latitude[d$site=="Yagba"] <- 8.1347528
	d$longitude[d$site=="Yagba"] <- 5.7048151
	d$latitude[d$site=="Zongo"] <- 9.1343447
	d$longitude[d$site=="Zongo"] <- 8.6170085
	d$latitude[d$site=="Gusi"] <- 10.3993
	d$longitude[d$site=="Gusi"] <- 12.0172
	d$latitude[d$site=="Ayaka"] <- 7.54557
	d$longitude[d$site=="Ayaka"] <- 8.80838
	d$latitude[d$site=="Taki"] <- 8.3287812
	d$longitude[d$site=="Taki"] <- 6.1111875
	d$latitude[d$site=="Agan"] <- 8.23333
	d$longitude[d$site=="Agan"] <- 4.3
	d$latitude[d$site=="Pukica"] <- 2.3403573
	d$longitude[d$site=="Pukica"] <- 32.7258227
	d$latitude[d$site=="Aceno"] <- 2.4198729
	d$longitude[d$site=="Aceno"] <- 32.4230282
	d$latitude[d$site=="Odenyo"] <- 2.1460009
	d$longitude[d$site=="Odenyo"] <- 32.4943264
	d$latitude[d$site=="Odyenyo"] <- 2.1460009
	d$longitude[d$site=="Odyenyo"] <- 32.4943264
	
	d$geo_from_source <- FALSE

	# drop rows containing non-existent locations	
	d <- d[!(d$site=="Vigi"| d$site=="KYAGHAEU" | d$site=="Tartio" | d$site=="Auk East"),]
	d <- d[!(is.na(d$longitude)& is.na(d$site)),]

#add fertilizer
	# fix fertilize type
	d$fertilizer_type[d$fertilizer_type=="None"] <- "none"
	d$fertilizer_type[d$fertilizer_type=="other"] <- NA
	d$fertilizer_type[d$fertilizer_type=="Urea"] <- "urea"

## RH: this is wrong 
## 
#type_of_mineral_fertilizer_field_1	amount_of_mineral_fertilizer_field_1 unit_amount_fertilizer_field_1
#type_of_mineral_fertilizer_field_3	amount_of_mineral_fertilizer_field_3 unit_amount_fertilizer_field_3
#note that the unit can be kg or bags

	# Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare;
	# DAP content: 18% of N	and 46% P205 
	#Urea was applied at a rate of 60 kg N/ha in Kenya and Rwanda trials and we assume it was the same in others Trials
	d$N_fertilizer <- 0
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	
	i <- grepl("NPK", d$fertilizer_type)
	d$N_fertilizer[i] <- 30
	d$P_fertilizer[i] <- 30
	d$K_fertilizer[i] <- 20
	
	i <- grepl("DAP", d$fertilizer_type)
	d$N_fertilizer[i] <- d$N_fertilizer[i] + 30*0.18
	d$P_fertilizer[i] <- d$P_fertilizer[i] + 30*0.46/2.29 

	i <- grepl("SSP", d$fertilizer_type)
	d$P_fertilizer[i] <- d$P_fertilizer[i] + 30

	i <- grepl("SSP", d$fertilizer_type)
	d$N_fertilizer[i] <- d$N_fertilizer[i] + 60

	d$N_fertilizer[is.na(d$fertilizer_type)] <- NA
	d$P_fertilizer[is.na(d$fertilizer_type)] <- NA
	d$K_fertilizer[is.na(d$fertilizer_type)] <- NA

	
	#fix country name
	dd <- carobiner::fix_name(d$country,"title")
	d$country <- dd
	# fix crop name 
	P <- carobiner::fix_name(d$crop,"lower")
	P <- gsub("soya_bean","soybean",P)
	P <- gsub("bush_bean","common bean",P)
	P <- gsub("climbing_bean","common bean",P)
	P <- gsub("faba_bean","faba bean",P)
	d$crop <- P
	
	#fix previous crop name
	P1 <- carobiner::fix_name(d$previous_crop,"lower")
	P1 <- gsub("irish_potato" ,"potato",P1)
	P1 <- gsub("soyabean","soybean",P1)
	P1 <- gsub("climbing_bean","common bean",P1)
	P1 <- gsub("sweet_potato","sweetpotato",P1)
	P1 <- gsub("soya_bean","soybean",P1)
	P1 <- gsub("other", "unknown", P1)
	P1 <- gsub("bush_bean","common bean",P1)
	d$previous_crop <- P1

	#fix crop yield limit by crop
	d$yield[d$crop=="common bean" & d$yield>9000] <- NA
	d$yield[d$crop=="cowpea" & d$yield>6500] <- NA
	d$yield[d$crop=="groundnut" & d$yield> 8500] <- NA
	d$yield[d$crop=="soybean" & d$yield>15000] <- NA
	# data type
	d$inoculated <- as.logical(d$inoculated)
	#for (i in 1:12) {
	#	d$planting_date <- gsub(month.abb[i], i, d$planting_date)
	#}
	d$planting_date <- as.character( as.Date(d$planting_date,'%d-%b-%y'))

	d$planting_date[is.na(d$planting_date)] <- "2015"
	d$yield_part <- "seed"
	d$yield_part[grepl("groundnut",d$crop)] <- "pod"
	
	d$row_spacing[d$row_spacing == 0] <- NA
	d$location[d$location == "KWAYA KUSAR"] <- "Kwaya Kusar"
	d$location[d$location == "HAWUL"] <- "Hawul"
		
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}

