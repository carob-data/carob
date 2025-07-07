# R script for "carob"


carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings."
  
	uri <- "doi:10.25502/V1CA-7Y60"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",	
		publication=NA, 
		data_organization = "IITA;ICRAF;WUR", 
		carob_contributor="Cedric Ngakou", 
		carob_date="2023-06-01",
		data_type="experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;Zn_fertilizer;S_fertilizer;inoculated",		
		response_vars = "yield"
	)
	
	
	f <- ff[basename(ff) == "data_table.csv"] 
	
	# read the dataset
	r <- read.csv(f)
	
	d <- r[, c("country", "id", "lga_district_woreda", "sector_ward", "gps_homestead_device_latitude.decimal_degrees", "gps_latitude_field.decimal_degrees", "gps_homestead_device_longitude.decimal_degrees", "gps_field_device_longitude.decimal_degrees", "date_of_planting_whole_n2africa_field.date", "date_of_final_harvest_whole_n2a_field.date", "legume_planted_in_the_n2africa_trial", "crop_1_previous_season", "inoculation_n2africa_field")] 
 
	colnames(d) <- c("country", "trial_id", "location", "site", "latitude1", "latitude2", "longitude1", "longitude2", "planting_date", "harvest_date", "crop", "previous_crop", "inoculated")
	
 # Fix long and lat columns 
	i <- is.na(d$latitude1)
		d$latitude1[i] <- d$latitude2[i]
		i <- is.na(d$longitude1)
		d$longitude1[i] <- d$longitude2[i]
 
# keep the relevant column of long and lat
	d$latitude <- d$latitude1
	d$longitude <- d$longitude1
 
	oldnms <- c("id", "lga_district_woreda", "country", "treatmentX", "row_spacing_crop_1_plot_X.cm" , "plant_spacing_crop_1_plot_X.cm", "grain_weight_crop_1_plot_X.kg", "pod_weight_groundnut_crop_1_plot_X.kg", "above_ground_biomass_weight_crop_1_plot_X.kg", "width_of_harvested_plot_crop_1_plot_X.m", "no_plants_hole_crop_1_plot_X.nr", "number_of_rows_crop_1_plot_X.nr")
	
 
	newnms <- c("trial_id", "location", "country", "treatment", "row_spacing", "plant_spacing", "yield1", "yield2", "fwy_residue", "width_size_plot", "number_plant", "number_row")

	names(r) <- gsub("number_of_rows_crop_1_plot_.nr", "number_of_rows_crop_1_plot_3.nr", names(r))
 
	#extract relevant columns for treatment1
	lst <- list()
		for (i in 1:12) {
		inms <- gsub("X", i, oldnms)
		ri <- r[, inms] 
		colnames(ri) <- newnms
		lst[[i]] <- ri
	}	
		
	# append all the treatment data
	dd <- do.call(rbind, lst)
	#remove rows with no treatment
	dd <- dd[complete.cases(dd$treatment), ] 
	
	# fix	yield column 
	i <- is.na(dd$yield1)
	dd$yield1[i] <- dd$yield2[i]
	
# calculate the yield value using area of different plot 
## RH: "-1" is wrong
## RH: dd$area <- ((dd$width_size_plot*(dd$number_row-1)*dd$row_spacing)/100)/10000 # in ha

# yield is in kg, width in m, row spacing in cm

# there are some crazy yields
	dd$width_size_plot[dd$width_size_plot < 4 | dd$width_size_plot > 12] <- NA
	dd$number_row[dd$number_row < 4 | dd$number_row > 24] <- NA
	dd$row_spacing[dd$row_spacing < 40 | dd$row_spacing > 100] <- NA


	dd$area <- dd$width_size_plot * dd$number_row * dd$row_spacing / 100 # in m2
	dd$yield <- 10000 * dd$yield1 / dd$area #kg/ha
	dd$yield[dd$yield > 6000] <- NA
	
	dd$fwy_residue <- dd$fwy_residue / dd$area

	#plant density
	dd$plant_spacing[dd$plant_spacing > 30] <- NA

# RH not ok:	
# number of plan per ha
#	dd$plant_density <- (((dd$width_size_plot/dd$plant_spacing)+1)*dd$number_row)/dd$area 

	# spacing is in cm, one_row_plot_length in m
	# length of plot if it was 1 ha and had 1 row
	one_row <- 10000 / (dd$row_spacing/100)
	dd$plant_density <- one_row / (dd$plant_spacing/100)

	
	
	 # variety column
	variety1 <- c("SAMNUT 24", "TGX1951-3F", "FTGX1955-4F", "SAMNUT 22", "TGX 1951-3F", "Maksoy 3N", "Lyamungu 90", "Nabe 12C", "Local", "Tumaini", "TGX 1955-4F", "Pendo", "Songotra", "Jenguma", "Samnut23", "Jenguma", "TGX1955-4F")
	dd$variety <- NA
	for (i in 1:length(variety1)){
		j <- grepl(variety1[i], dd$treatment)
		dd$variety[j] <- variety1[i]
		}
 

# merge d and dd
	d	 <- merge(d, dd, by=c("country", "location", "trial_id"))

	d <- d[, c("country", "trial_id", "location", "site", "longitude", "latitude", "planting_date"
			, "harvest_date", "crop", "previous_crop", "variety", "inoculated", "treatment", "row_spacing", "plant_spacing", "yield", "fwy_residue", "plant_density")]

# Add columns
		 
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	# Add fertilizer
	
	d$N_fertilizer	 <- 0	 # 
	d$K_fertilizer <- 	ifelse(grepl("K", d$treatment), 20, 0)
	d$P_fertilizer <- 	ifelse(grepl("P", d$treatment) & (!grepl("Pendo", d$treatment)), 30, 0)
	d$Zn_fertilizer <- 	ifelse(grepl("Zn", d$treatment), 2.5, 0)
	d$S_fertilizer <- ifelse(grepl("+ S", d$treatment) | grepl("\\+S", d$treatment) | grepl("MgS", d$treatment), 12, 0)

 	d$OM_used <- grepl("OM", d$treatment) | grepl("Manure", d$treatment)
 #fix country name 
	d$country <- carobiner::fix_name(d$country, "title")	
	# fix location
	d$location <- carobiner::fix_name(d$location, case="title")
	d$site <- carobiner::fix_name(d$site, case="title")
 
# fix inoculated	column
	e <- carobiner::fix_name(d$inoculated)	
	e <- gsub("y", TRUE, e)
	e <- gsub("n", FALSE, e)
	d$inoculated <- e 
	
	# longitude and latitude
# Cedric, could we have used "site"?
	geo <- data.frame(location=c('Tolon', 'Karaga', 'Wa-West', 'Ajingi', 'Bugiri', 'Biu', 'Kassena Nankana', 'Bawku Municipal', 'Nadowli', 'Bagwai', 'Bayo', 'Chikun', 'Gwarzo', 'Hawul', 'Igabi', 'Kajuru', 'Kwaya Kusar', 'Lapai', 'Paikoro', 'Shiroro', 'Gairo', 'Kilosa', 'Kongwa', 'Mvomero', 'Bukedea', 'Kapchorwa', 'Kibuku', 'Kisoro', 'Kole', 'Kumi', 'Pallisa', 'Shanono', 'Apac', 'Kanungu', 'Oyam'), 
	lon = c(-1.0665974, -0.4316607, -2.6832633, 9.03665, 33.76176, 12.1909612, -1.1132999, -0.2333, -2.664189, 8.1357, 11.6827, 7.0706742, 8.8295, 12.2, 7.776152, 7.680287, 11.848337, 6.716667, 6.837686, 6.83326, 36.86907, 38.648945, 37.91068, 37.44778, 34.04511, 34.45081, 33.79363, 29.69267, 32.80096, 33.96053, 34.16643, 7.988561, 32.5, 29.9, 32.4),
	lat = c(9.432468, 9.92678, 9.898577, 11.9678, 0.5637032, 10.61461, 10.9589, 11.05, 10.372021, 12.1577, 10.4524, 10.242006, 12.358697, 10.5, 10.83151, 10.331663, 10.562206, 9.066667, 9.4930391, 9.956072, -6.140271, -10.835097, -7.253275, -6.3042153, 1.3440293, 1.3967784, 1.043475, -1.2822138, 2.4283032, 1.495144, 1.0778522, 12.048429, 2, -1, 2.4))

	d <- merge(d, geo, by="location", all.x =TRUE)
# Cedric, I think we should not overwrite existing lon/lat data (as you did)
	d$latitude[d$latitude==0] <- NA

	d$geo_from_source <- FALSE
	d$geo_from_source[!is.na(d$latitude)] <- TRUE

	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$lat <- d$lon <- NULL

	i <- d$country == "Tanzania" & d$latitude > 0
	d$latitude[i] <- -d$latitude[i]
	
	# fix crops names
	b <- carobiner::fix_name(d$crop, "lower")	
	b <- gsub("soya_bean", "soybean", b)
	b <- gsub("bush_bean", "common bean", b)
	b <- gsub("climbing_bean", "common bean", b)
	d$crop <- b 
	# fix previous crops names
	b <- carobiner::fix_name(d$previous_crop)	
	b <- gsub("millet", "pearl millet", b)
	b <- gsub("fallow", "none", b)
	b <- gsub("soyabean", "soybean", b)
	b <- gsub("other", "unknown", b)
	b <- gsub("sweet_potato", "sweetpotato", b)
	b <- gsub("bambara_bean", "bambara groundnut", b)
	b <- gsub("bush_bean", "common bean", b)
	b <- gsub("irish_potato", "potato", b)
	b <- gsub("pigeon_pea", "pigeon pea", b)
	b <- gsub("climbing_bean", "common bean", b)
	b <- gsub("green_gram", "mung bean", b)
	d$previous_crop <- b 
 ###RH ? b2 <- carobiner::fix_name(d$planting_date) 
 ## b2 <- gsub("2025-07-08", , b2)
 ## d$planting_date <- b2
## d$planting_date[d$planting_date == "2025-07-08"] <- "2015-07-08"
# change the date format 
##RH ?	d$planting_date <- format(dmy(d$planting_date), '%d/%m/%Y')
##RH ?	d$harvest_date <- format(dmy(d$harvest_date), '%d/%m/%Y')

## this works in English locales (that is, not on e.g. a French computer)
##	d$planting_date <- as.Date(d$planting_date, '%d-%b-%y')
##	d$harvest_date <- as.Date(d$harvest_date, '%d-%b-%y')

##	therefore
	for (i in 1:12) {
		d$planting_date <- gsub(month.abb[i], i, d$planting_date)
		d$harvest_date <- gsub(month.abb[i], i, d$harvest_date)
	}
	d$planting_date <- as.character(as.Date(d$planting_date, '%d-%m-%y'))
	d$harvest_date <- as.character(as.Date(d$harvest_date, '%d-%m-%y'))
	
	#fix start date column
##??	b2 <- carobiner::fix_name(d$planting_date) 
##	b2 <- gsub("08/07/2025", "08/07/2016", b2) # end date is 2016-11-05 and the crop is soybean
	d$planting_date[d$planting_date=="2025-07-08"] <- "2016-07-08"
	d$planting_date[d$planting_date=="2021-10-01"] <- "2016-10-01"
	d$planting_date[d$planting_date=="2018-07-07"] <- "2015-07-07"
	
	d$harvest_date[d$harvest_date=="2005-11-26"] <- "2015-11-26"
	d$harvest_date[d$harvest_date=="2019-10-27"] <- "2016-10-27"

	d$harvest_date[substr(d$harvest_date, 1, 4) == "1999"] <- NA
	d$harvest_date[substr(d$harvest_date, 1, 4) == "2005"] <- NA
	d$planting_date[substr(d$planting_date, 1, 4) == "1999"] <- NA

	i <- which(as.Date(d$planting_date) > as.Date(d$harvest_date))
	d$planting_date[i] <- NA
	d$harvest_date[i] <- NA
	i <- which(as.numeric(as.Date(d$harvest_date) - as.Date(d$planting_date)) > 300)
	d$planting_date[i] <- NA
	d$harvest_date[i] <- NA
	i <- which(as.numeric(as.Date(d$harvest_date) - as.Date(d$planting_date)) > 300)
	d$planting_date[i] <- NA
	d$harvest_date[i] <- NA
	i <- which(as.numeric(as.Date(d$harvest_date) - as.Date(d$planting_date)) < 40)
	d$planting_date[i] <- NA
	d$harvest_date[i] <- NA

	
	#data type
 ## d$planting_date <- format(as.Date(d$planting_date, format = '%d/%m/%Y'), "%Y-%m-%d")
##	d$harvest_date <- format(as.Date(d$harvest_date, format = '%d/%m/%Y'), "%Y-%m-%d")
	d$yield <- as.numeric(d$yield)
	d$inoculated <- as.logical(d$inoculated)

	d <- d[!is.na(d$yield), ]
	d$yield_part <- "seed"
	d$yield_part[d$crop == "groundnut"] <- "pod"
	
	carobiner::write_files(meta, d, path=path)
	
}
	


