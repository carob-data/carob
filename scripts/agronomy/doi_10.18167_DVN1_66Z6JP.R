# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
N2O and CH4 dataset from a long-term cassava-based conservation agriculture experiment in Cambodia

These are the raw data of the paper 'Impacts of long-term cassava-based conservation agriculture systems on soil greenhouse gas emissions in Cambodia' authored by Vira Leng, Laurent Thuriès, Vang Seng, Florent Tivet, Phearum Mark, Chhay Ngin, Try Yorn, Titouan Filloux, Pascal Lienhard, Johan Six, Lyda Hok, Stéphane Boulakia, Manuel Reyes, P. V. Vara Prasad, Rémi Cardinael.

More specifically, the data comprise nitrous oxide (N2O) and methane (CH4) emissions from 2 years of field measurement at the long-term experimental site at Bos Khnor Research station in Cambodia, comparing conventional cassava-based cropping systems to different no-till systems (including or not cover crops and rotation with maize). It also includes weather data, as well as measured driving variables of GHG emissions, such as water-filled pore space (WFPS) and soil mineral nitrogen at 0-20 and 20-40 cm.
"

	uri <- "doi:10.18167/DVN1/66Z6JP"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD;MAFF;ETHZ; RUA; KSU", # MAFF: Ministry of Agriculture, Forestry and Fisheries #ETHZ: Ecole polytechnique fédérale de Zurich 
		publication = "doi:10.1016/j.agee.2026.110363",
		project = NA,
		carob_date = "2026-04-21",
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;N_fertilizer;intercrops;crop_rotation",
		response_vars = "yield;soil_N2O;soil_CH4", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "251207_Leng et al., dataset for GHG emissions paper 2022-2024. revised.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="Legend_ReadMe")
	r2 <- carobiner::read.excel(f1, sheet="Soil data 2022-24 at 0-20 cm")
	r3 <- carobiner::read.excel(f1, sheet="Soil data 2022-24 at 20-40 cm")
	r4 <- carobiner::read.excel(f1, sheet="GHG emissions 2022-24")
	r5 <- carobiner::read.excel(f1, sheet="Crop Pro. & Cum GHG emissions")
	r6 <- carobiner::read.excel(f1, sheet="Cover crops before Mz 2022-24")
	r7 <- carobiner::read.excel(f1, sheet="Climate data_Jan 22 - Apr 24")


	d1 <- data.frame(
		crop = tolower(r2$Main_crop) ,
		planting_date = substr(r2$Cropping_season, 1, 4),
		treatment = r2$Treatment_ID,
		rep = as.integer(r2$Replicate),
		soil_NO3 = r2$NO3_N_mg_kg_soil,
		soil_NH4 = r2$NH4_N_mg_kg_soil,
		soil_N = r2$Soil_minN_mg_kg_soil,
		depth_top = 20,
		depth_bottom = 0
		
	)
	
	#####
	d2 <- data.frame(
	   planting_date = substr(r3$Cropping_season, 1, 4),
	   crop = tolower(r3$Main_crop),
	   treatment = r3$Treatment_ID,
	   rep = as.integer(r3$Replicate),
	   soil_NO3 = r3$`NO3_N_mg_kg_ soil`,
	   soil_NH4 = r3$NH4_N_mg_kg_soil,
	   soil_N = r3$Soil_minN_mg_kg_soil,
	   depth_top = 40,
	   depth_bottom = 20
	)
	
	d <- carobiner::bindr(d1, d2)
	####
	
	d3 <- data.frame(
	   planting_date = substr(r4$Cropping_season, 1, 4),
	   crop = tolower(r4$Main_crop),
	   treatment = r4$Treatment_ID,
	   rep = as.integer(r4$Replicate),
	   soil_N2O = r4$g_N2O_N_day_ha,
	   soil_CH4 = r4$g_CH4_C_day_ha
	)
	
	### merge d and d3
	agg <- aggregate(.~ crop + planting_date+ treatment+rep , d3, function(x) mean(x))
	d <- merge(d, agg, by = intersect(names(d), names(d3)), all = TRUE)
	#####
	d4 <- data.frame(
	   planting_date = substr(r5$Cropping_season, 1, 4),
	   crop =  tolower(ifelse(grepl("Avg/plot",  r5$Main_crop), NA,  r5$Main_crop)),
	   treatment = r5$`Treatment ID`,
	   rep = as.integer(r5$Replicate),
	   N_fertilizer = r5$Cum_N_input_kg_ha_yr,
	   yield = r5$Yield_scaled_CH4_kg_ha*r5$Cum_CH4_g_ha,
	   land_prep_method = ifelse(grepl("CTM", r5$`Treatment ID`), "conventional", "none") ,
	   
	   crop_rotation = ifelse(grepl("NTR-Cs", r5$`Treatment ID`), "cassava;maize", 
	                   ifelse(grepl("NTR-Mz", r5$`Treatment ID`), "maize;cassava", "none")),
	   
	   intercrops = ifelse(grepl("NTR1-Mz|NTR2-Mz", r5$`Treatment ID`) , "millet;sunn hemp;cowpea",
	                ifelse(grepl("NTR1-Cs", r5$`Treatment ID`), "millet;sunn hemp", "none")),
	   location = "Bos Khnor Research station",
	   country = "Cambodia",
	   longitude = 105.3180 ,
	   latitude = 12.18248,
	   trial_id = "1", 
	   on_farm = TRUE, 
	   is_survey = FALSE, 
	   yield_part = ifelse(grepl("Maize", r5$Main_crop), "grain", "tubers"), 
	   yield_moisture = as.numeric(NA),
	   irrigated = NA, 
	   geo_from_source = FALSE
	)
	
	 	### merge d and d4
	d <- merge(d, d4[!is.na(d4$crop),], by = intersect(names(d), names(d4)), all = TRUE)
	
	### remove negative values
	d <- d[which(d$yield > 0), ]
	#d <- d[which(d$soil_NH4 > 0), ]
	#d <- d[which(d$soil_N > 0), ]
	#d <- d[which(d$soil_NO3 > 0), ]
	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	####  weather data 
	
	dw <- data.frame(
	   date = as.character(r7$Date),
	   temp = r7$`Temperature (AVG °C)`,
	   tmin = r7$`Temperature (MIN °C)`,
	   tmax = r7$`Temperature (MAX °C)`,
	   prec = r7$`Precipitation (SUM mm)`,
	   srad = r7$`Global Radiation (MAX W/m²)`,
	   rhum = r7$`Relative Humidity (AVG % RH)`,
	   rhmn =  r7$`Relative Humidity (MIN % RH)`,
	   rhmx = r7$`Relative Humidity (MAX % RH)`,
	   wspd = r7$`Wind Speed (AVG km/h)`/3.6, #m/s
	   wspdmx = r7$`Wind Speed (MAX km/h)`/3.6,
	   location = "Bos Khnor Research station",
	   country = "Cambodia",
	   longitude = 105.3180 ,
	   latitude = 12.18248,
	   geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d, wth = dw)
}


