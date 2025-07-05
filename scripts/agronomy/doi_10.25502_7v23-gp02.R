# R script for "carob"


carob_script <- function(path) {
	
"N2Africa is to contribute to increasing biological nitrogen fixation  and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project."

	uri <- "doi:10.25502/7v23-gp02"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
 
	 ## dataset level data 
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication=NA,
		data_organization = "IITA;WUR",
		carob_contributor="Eduardo Garcia Bendito",
		carob_date="2023-08-21",
		data_type="experiment",
		response_vars= "yield",
		treatment_vars="N_fertilizer;P_fertilizer;K_fertilizer"		
	)
	
 
	#read the data file
	f1 <- ff[basename(ff) == "general.csv"]
	# f2 <- ff[basename(ff) == "production.csv"] 
	f3 <- ff[basename(ff) == "experiment.csv"] 
	# read the dataset
	d1 <- data.frame(read.csv(f1))
	# d2 <- data.frame(read.csv(f2))
	d3 <- data.frame(read.csv(f3))
	
	# Subset d1
	## use carobiner::change_names instead
	colnames(d1)[c(3,4,5,6,8,9)] <- c("trial_id", "obs_day", "obs_month", "obs_year", "adm1", "adm2")
	colnames(d1)[c(59,60,61)] <- c("harvest_date_day", "harvest_date_month", "harvest_date_year")
	d1 <- d1[,c("trial_id", "obs_day", "obs_month", "obs_year", "adm1", "adm2","harvest_date_day", "harvest_date_month", "harvest_date_year")]
	d1$country <- "Tanzania"
	d1$adm1 <- trimws(tools::toTitleCase(tolower(d1$adm1)))
	d1$adm2 <- trimws(tools::toTitleCase(tolower(d1$adm2)))
	d1$adm1[grep("Moshi", d1$adm1, value = F)] <- "Moshi Rural"
	d1$adm2[grep("Mwika", d1$adm2, value = F)] <- "Mwika South"
	# # Subset d2
	# d2 <- d2[,c(3:8,13:16,49:52,65:81,133:138)]
	# colnames(d2) <- c("trial_id", "area_field1", "area_field2", "area_field2", "area_field3", "area_field4", "area_field_unit",
	#									 "main_crop_field1", "main_crop_field2", "main_crop_field3", "main_crop_field4",
	#									 "variety_field1", "variety_field2", "variety_field3", "variety_field4",
	#									 "amount_mineral_fertilizer_field1", "amount_mineral_fertilizer_field2", "amount_mineral_fertilizer_field3", "amount_mineral_fertilizer_field4","amount_fertilizer_unit",
	#									 "fertilizer_type_field1", "fertilizer_type_field2", "fertilizer_type_field3", "fertilizer_type_field4",
	#									 "OM_used_field1", "OM_used_field2", "OM_used_field3", "OM_used_field4",
	#									 "innocuated_field1", "innocuated_field2", "innocuated_field3", "innocuated_field4",
	#									 "50pct_flowering_day", "50pct_flowering_month", "50pct_maturity_day", "50pct_maturity_month")
	# Subset d3
	d3 <- d3[,c(3,5:12,86:297)]
	d3 <- cbind(d3[,c(1:7)],
		d3[,c(grep(paste0("plot_",1:6, collapse = "|"), colnames(d3[10:ncol(d3)]), value = T))],
		d3[,c(grep("experimental_treatments_", colnames(d3[10:ncol(d3)]), value = T))])
	# reshape d3
	rr <- reshape(d3,
		direction='long', 
		varying=list(names(d3)[8:ncol(d3)]),
		v.names = "value",
		idvar = "farm_id",
		timevar = "var",
		times = colnames(d3)[8:ncol(d3)])
	rownames(rr) <- 1:nrow(rr)
	
	d <- NULL
	for(i in 1:6){
		rrr <- reshape(rr[rr$var %in% grep(paste0(paste0("plot_",i), "|experimental_treatments_"), rr$var, value = T),
		c("farm_id", paste0("name_treatment_",i), "var", "value")],
		idvar = c("farm_id", paste0("name_treatment_",i)), timevar = "var",
		direction='wide')
		rrr$plot <- i
		colnames(rrr) <- gsub("value.", "", colnames(rrr))
		colnames(rrr) <- gsub(paste0("_plot_",i), "", colnames(rrr))
		colnames(rrr)[1:2] <- c("trial_id", "treatment")
		colnames(rrr) <- gsub("\\..*", "", colnames(rrr))
		colnames(rrr)[c(3,4,6,7,8)] <- c("plot_width", "plot_length", "yield", "residue", "dmy_total")
		d <- rbind(d, rrr)
	}
	
	# Merge site info and agronomy info
	d <- merge(d, d1, by = "trial_id")
	
	# Standardization
	d$trial_id <- paste(d$trial_id, d$plot, sep = "_")
	d$country <- "Tanzania"
	d$date <- paste(as.integer(d$obs_year),
				ifelse(d$obs_month == 'December', as.integer(12), as.integer(11)),
				sprintf('%02d', d$obs_day),
				sep = "-")
	## EGB:
	## This is not included because how can harvest date be after the survey?
	## RH: how can they not know the harvest date if they harvested?	
	## replace missing with reasonable guess (average)
	d$harvest_date_year[is.na(d$harvest_date_year)] <- 2015
	d$harvest_date_month[d$harvest_date_month == ""] <- "Feb"
	d$harvest_date_day[is.na(d$harvest_date_day)] <- 15
	d$harvest_date <- paste(d$harvest_date_year, 
						substr(d$harvest_date_month, 1, 3),
						sprintf('%02d', d$harvest_date_day), sep = "-")

	d$harvest_date <- as.character(as.Date(d$harvest_date , "%Y-%b-%d"))
	
	# is the all we have?
	d$planting_date <- "2014"
	
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	# d$treatment <- 
	d$crop <- "common bean"
	d$variety <- 'Lyamungo 90'
	
	# Fertilizer part
	d$fertilizer_type <- 'none'
	d$fertilizer_type[grepl('\\+', d$treatment)] <- "PKS"
	d$fertilizer_type[grep('pk', d$treatment)] <- "PKS"
	d$fertilizer_type[grep('npk', d$treatment)] <- "NPK"
	d$fertilizer_type[grepl('mpal', d$treatment)] <- "sympal"
	
	plot_size <- as.numeric(d$plot_width) * as.numeric(d$plot_length)

	d$fert_1_kg_plot <- as.numeric(d$fert_1_kg_plot)
	d$fert_2_kg_plot <- as.numeric(d$fert_2_kg_plot)
	d$fert_3_kg_plot <- as.numeric(d$fert_3_kg_plot)
	fert_kg_plot <- rowSums(d[, c("fert_1_kg_plot","fert_2_kg_plot", "fert_3_kg_plot")], na.rm=TRUE)

#	a <- cbind(fert_kg_plot, d[, c("fertilizer_type")])
#	a <- a[order(a[,2]), ]
# RH note that there are some cases where the amount is zero 
# but fertilizer type is not. 
#     fert_kg_plot         
#[1,] "0"          "none"  
#[2,] "2.5"        "NPK"   
#[3,] "0"          "NPK"   
#[4,] "0.87"       "PKS"   
#[5,] "0"          "PKS"   
#[6,] "2"          "sympal"
#[7,] "0"          "sympal"
## which ones are correct? 

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- 0

	i <- d$fertilizer_type == "NPK" # Assumed to be NPK (10:18:24)
	d$N_fertilizer[i] <- (fert_kg_plot[i] / plot_size[i]) * 10000 * 0.1 
	d$P_fertilizer[i] <- (fert_kg_plot[i] / plot_size[i]) * 10000 * 0.18 
	d$K_fertilizer[i] <- (fert_kg_plot[i] / plot_size[i]) * 10000 * 0.24

	i <- d$fertilizer_type == "PKS" # Assumed to be PK (18:24)
	d$N_fertilizer[i] <- 0
	d$P_fertilizer[i] <- (fert_kg_plot[i] / plot_size[i]) * 10000 * 0.18 
	d$K_fertilizer[i] <- (fert_kg_plot[i] / plot_size[i]) * 10000 * 0.24

	i <- d$fertilizer_type == "sympal"
	d$N_fertilizer[i] = 0 # Sympal has 0 % N
	d$P_fertilizer[i] <- (fert_kg_plot[i] / plot_size[i]) * 10000 * 0.23
	d$K_fertilizer[i] <- (fert_kg_plot[i] / plot_size[i]) * 10000 * 0.15
		
	d$OM_used <- NA
	d$OM_used[grepl('\\+', d$treatment)] <- TRUE
	d$OM_type <- ifelse(d$OM_used == TRUE, "farmyard manure", NA)
	d$OM_amount <- ifelse(d$OM_used == TRUE,
	((as.numeric(d$manure_kg_plot) / plot_size) * 10000),
	NA)
	# Yield
	d$yield <- (as.numeric(d$yield) / plot_size) * 10000 # kg/ha
	d$yield_part <- "seed"
	d$fwy_residue <- (as.numeric(d$residue) / plot_size) * 10000 # kg/ha
	d$dmy_total <- (as.numeric(d$dmy_total) / plot_size) * 10000 # kg/ha
	
	# Other
	d$irrigated <- FALSE
	d$row_spacing <- as.numeric(d$experimental_treatments_density_1_row_spacing)
	d$plant_spacing <- as.numeric(d$experimental_treatments_density_1_plant_spacing)
	d$plant_density <- (as.numeric(d$plot_width)/(as.numeric(d$row_spacing)/100)) * (as.numeric(d$plot_length)/(as.numeric(d$plant_spacing)/100)) # plants/plot
	d$plant_density <- (as.numeric(d$plant_density) / plot_size * 10000) # plants/ha
	
	# # EGB:
	# # There is info on herbicide
	
	# Add geo
	s <- data.frame(country = c("Tanzania", "Tanzania", "Tanzania","Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania"),
		adm1 = c("Lushoto", "Lushoto", "Lushoto", "Lushoto", "Lushoto", "Lushoto", "Lushoto", "Moshi Rural", "Moshi Rural", "Moshi Rural"),
		adm2 = c("Lushoto", "Kwemashai", "Shume", "Migambo", "Manoro", "Mwangoi", "Dule m", "Makuyuni", "Marangu East", "Mwika South"),
		latitude = c(-4.545, -4.806, -4.7, -4.545, -4.545, -4.603, -4.545, -3.402, -3.283, -3.283),
		longitude = c(38.439, 38.328, 38.216, 38.439, 38.439, 38.313, 38.439, 37.57, 37.516, 37.583),
		geo_from_source = FALSE)
	
	d <- merge(d,s, by = c("country", "adm1", "adm2"))
	
	# Subset final
	d <- d[,c('trial_id','treatment','country','adm1','adm2','longitude','latitude','geo_from_source',
				'date','on_farm','is_survey','crop','variety',
				'fertilizer_type','N_fertilizer','P_fertilizer','K_fertilizer','OM_used','OM_type','OM_amount', 'planting_date',
				'yield', 'yield_part', 'fwy_residue', 'dmy_total',
				'irrigated', 'row_spacing', 'plant_spacing', 'plant_density')]
	
	
	d <- d[!is.na(d$yield), ]
	d <- d[!is.na(d$N_fertilizer), ]
	
	carobiner::write_files (meta, d, path=path)
	
}

# # EGB: Georeferencing
# s <- unique(d1[,c("country", "adm1", "adm2")])
# s$latitude <- NA
# s$longitude <- NA
# s$latitude[grep("Lushoto", s$adm1)] <- -4.545 # https://www.geonames.org/155568/lushoto.html
# s$longitude[grep("Lushoto", s$adm1)] <- 38.439 # https://www.geonames.org/155568/lushoto.html
# s$latitude[grepl("Moshi", s$adm1)] <- -3.362 # https://www.geonames.org/7840035/moshi-rural-district.html
# s$longitude[grepl("Moshi", s$adm1)] <- 37.459 # https://www.geonames.org/7840035/moshi-rural-district.html
# s$latitude[grep("Mwika South", s$adm2)] <- -3.283 # https://www.geonames.org/152120/mwika.html
# s$longitude[grep("Mwika South", s$adm2)] <- 37.583 # https://www.geonames.org/152120/mwika.html
# s$latitude[grep("Marangu East", s$adm2)] <- -3.283 # https://www.geonames.org/154777/marangu.html
# s$longitude[grep("Marangu East", s$adm2)] <- 37.516 # https://www.geonames.org/154777/marangu.html
# s$latitude[grep("Makuyuni", s$adm2)] <- -3.402 # https://www.geonames.org/11004800/makuyuni.html
# s$longitude[grep("Makuyuni", s$adm2)] <- 37.57 # https://www.geonames.org/11004800/makuyuni.html
# s$latitude[grep("Dule M", s$adm2)] <- -4.563 # https://www.geonames.org/11006908/dule-m.html
# s$longitude[grep("Dule M", s$adm2)] <- 38.309 # https://www.geonames.org/11006908/dule-m.html
# s$latitude[grep("Mwangoi", s$adm2)] <- -4.603 # https://www.geonames.org/11007048/mwangoi.html
# s$longitude[grep("Mwangoi", s$adm2)] <- 38.313 # https://www.geonames.org/11007048/mwangoi.html
# s$latitude[grep("Shume", s$adm2)] <- -4.7 # https://www.geonames.org/149973/shume.html
# s$longitude[grep("Shume", s$adm2)] <- 38.216 # https://www.geonames.org/149973/shume.html
# s$latitude[grep("Kwemashai", s$adm2)] <- -4.806 # https://www.geonames.org/11006961/kwemashai.html
# s$longitude[grep("Kwemashai", s$adm2)] <- 38.328 # https://www.geonames.org/11006961/kwemashai.html
# s <- dput(s)
