# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Central Luzon Loop Survey (2015-2016)

The objective of the study is to monitor the changes in rice farming in the major rice producing area of the Philippines - the Central Luzon region, which is called as the 'rice bowl of the Philippines.
"

	uri <- "doi:10.7910/DVN/01ZTPA"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = NA,
		project = "Loop Survey",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-18",
		carob_completion = 90,	
		carob_effort = 5
	)
	


	f1 <- ff[basename(ff) == "Central Luzon Loop 2015-206.xlsx"]
	#f2 <- ff[basename(ff) == "Central Luzon Coding Manual.xlsx"]

	###
	r1 <- carobiner::read.excel(f1, sheet="HHInfo", skip = 5)
	r2 <- carobiner::read.excel(f1, sheet="Farm Char", skip = 5)
	r3 <- unique(suppressWarnings(carobiner::read.excel(f1, sheet="Land Prep", skip =7)))
	r4 <-  suppressWarnings(carobiner::read.excel(f1, sheet="Crop Estab", skip = 5))
	r5 <- carobiner::read.excel(f1, sheet="Crop Care", skip= 6)
	r6 <- carobiner::read.excel(f1, sheet="Fertilizer", skip = 8)
	r7 <- carobiner::read.excel(f1, sheet="HarvThreshPHarv", skip= 7)
	r8 <- carobiner::read.excel(f1, sheet="Production", skip =7)
	r9 <- carobiner::read.excel(f1, sheet="Seeds", skip =5)

#### process
	
	d1 <- data.frame(
	  date = r1$YEAR,
	  season = r1$SEASON,
	  hhid = as.character(r1$HHCODE),
	  hh_size = r1$`HOUSEHOLD SIZE`,
	  #hh_member = r1$`HOUSEHOLD MEMBER`,
	  sex = c("M"= "Male", "F"= "Female")[r1$SEX],
	  age = r1$AGE,
	  civil_status = r1$`CIVIL STATUS`,
	  occupation = r1$`PRIMARY OCCUPATION`
	)
	
	d1 <- d1[!duplicated(d1$hhid),]
	
	d2 <- data.frame(
	  date = r2$YEAR,
	  season = r2$SEASON,
	  hhid = as.character(r2$HHCODE),
	  plot_id = as.character(r2$PARNO),
	  cropland = round(as.numeric(gsub(".0.3", 0.3, r2$FSIZE)), 4),
	  variety = r2$VARIETY,
	  #irrigation_type = r2$IRRTYPE,
	  irrigated = !grepl("Rainfed", r2$IRRTYPE),
	  cropland_owned = ifelse(grepl("Own",r2$TENURE), as.numeric(gsub(".0.3", 0.3, r2$FSIZE)), NA),
	  cropland_rentedout = ifelse(grepl("Lent out|Rented out",r2$TENURE), as.numeric(gsub(".0.3", 0.3, r2$FSIZE)), NA),
	  soil_texture = tolower(r2$`SOIL TYPE`),
	  plant_spacing = r2$`PLANT SPACING`
	)
	
	## Fixing soil type 
	
	P <- carobiner::fix_name(d2$soil_texture)
	P <- gsub("^sandy$", "sand", P)
	P <- gsub("sandy loam \\(galas\\)", "sandy loam", P)
	P <- gsub("^clayee$", "clay", P)
	P <- gsub("clay loam \\(lagkitin, galas\\)|lagkitin galas", "clay loam", P)
	P <- gsub("sandy \\(buhaghag\\)", "sand", P)
	P <- gsub("clay loam \\(mestisong galas\\)", "clay loam", P)
	P <- gsub("clayee \\(malagkit\\)", "clay", P)
	P <- gsub("no idea", NA, P)
	d2$soil_texture <- P
	#### merge d1 and d2
	
	d <- merge(d1, d2, by= c("hhid", "date", "season"), all  = TRUE)
	
	d3 <- data.frame(
	  date = r3$YEAR,
	  season = r3$SEASON,
	  hhid = as.character(r3$HHCODE),
	  plot_id = as.character(r3$PARNO),
	  cropland = round(as.numeric(r3$FSIZE), 4),
	  land_prep_method = tolower(r3$ACTIVITY),
	  land_prep_implement = r3$POWSOURCE,
	  sex = c("M"= "Male", "F"= "Female")[r3$SEX],
	  farm_labour = r3$DAYFAM,
	  farm_labour_exchange = r3$DAYEXC,
	  farm_labour_hired = r3$DAYHIR,
	  land_prep_cost = as.numeric(gsub("566\\.\\.67|PL", NA, r3$WAGE)),
	  equipment_cost = r3$RENTAL
	)
	
	#### fix land_prep
	
	P <- carobiner::fix_name(d3$land_prep_method)
	P <- gsub("cleaning and repair of dikes|pre lp|dukit", "unknown", P)
	P <- gsub("rotavating", "rotovating", P)
	P <- gsub("plowing", "ploughing", P)
	d3$land_prep_method <- P 
	
	d3$land_prep_implement[grepl("tractor|Tractor", d3$land_prep_implement)] <- "4 wheel tractor"
	d3$land_prep_implement[grepl("Power|power tiller", d3$land_prep_implement)] <- "2 wheel tractor"
	d3$land_prep_implement[grepl("Grasscutter|grasscutter|Cow|Carabao", d3$land_prep_implement)] <- "animal"
	
	#### merge d and d3
	
	d <- merge(d3, d, by= c("hhid", "date", "season", "plot_id", "cropland", "sex"), all.x  = TRUE)
	
####	
	d4 <- data.frame(
	  date = r4$YEAR,
	  season = r4$SEASON,
	  hhid = as.character(r4$HHCODE),
	  plot_id = as.character(r4$PARNO),
	  cropland = round(r4$FSIZE, 4),
	  planting_method = r4$ACTIVITY,
	  planting_date = r4$DATE,
	  sex = c("M"= "Male", "F"= "Female")[r4$SEX],
	  planting_cost = as.numeric(gsub("PL", NA, r4$WAGE))
	)
	
	d4 <- d4[grepl("Transplanting|Replanting|Direct-seeding", d4$planting_method),]
	d4 <- d4[!is.na(d4$planting_date),]
	d4$planting_method <- gsub("Direct-seeding", "direct seeding", d4$planting_method)
	d4$planting_method <- tolower(gsub("Replanting", "transplanting", d4$planting_method))
	
	### Fixing planting date 
	d4$planting_date <- gsub("03 Sept. 2015", "2015-09-03", d4$planting_date)
	d4$planting_date <- gsub("27 Aug. 2015", "2015-08-27", d4$planting_date)
	d4$planting_date <- gsub("7/16-22/2015", NA, d4$planting_date)
	
	i <- !grepl("-", d4$planting_date)
	d4$planting_date[i] <- as.character(as.Date(as.numeric(d4$planting_date[i]), origin = "1899-12-30"))
	
	#### merge d and d4
	d <- merge(d, d4, by= c("hhid", "date", "season", "plot_id", "cropland", "sex"), all  = TRUE)
	
	#####
	d5 <- data.frame(
	  date = r5$YEAR,
	  season = r5$SEASON,
	  hhid = as.character(r5$HHCODE),
	  plot_id = as.character(r5$PARNO),
	  cropland = round(r5$FSIZE, 4),
	  
	  herbicide_used = ifelse(grepl("Herbicide application|HERBICIDE APPLICATION", r5$ACTIVTY1), TRUE, FALSE) ,
	  herbicide_amount = as.numeric(ifelse(grepl("Herbicide application|HERBICIDE APPLICATION", r5$ACTIVTY1), r5$QTY, NA)) ,
	  herbicide_cost = ifelse(grepl("Herbicide application|HERBICIDE APPLICATION", r5$ACTIVTY1), r5$COST, NA) ,
	  herbicide_price = as.numeric(ifelse(grepl("Herbicide application|HERBICIDE APPLICATION", r5$ACTIVTY1), r5$PRICE, NA)) ,
	  
	  fungicide_used = ifelse(grepl("Fungicide application", r5$ACTIVTY1), TRUE, FALSE) ,
	  fungicide_amount = as.numeric(ifelse(grepl("Fungicide application", r5$ACTIVTY1),r5$QTY, NA)) ,
	  fungicide_cost = as.numeric(ifelse(grepl("Fungicide application", r5$ACTIVTY1),r5$COST, NA)) ,
	  fungicide_price = as.numeric(ifelse(grepl("Fungicide application", r5$ACTIVTY1),r5$PRICE, NA)) ,
	  
	  insecticide_used = ifelse(grepl("Insecticide application", r5$ACTIVTY1), TRUE, FALSE) ,
	  insecticide_amount = gsub("2 pack|1 pack", NA, ifelse(grepl("Insecticide application", r5$ACTIVTY1), r5$QTY, NA)) ,
	  insecticide_cost = ifelse(grepl("Insecticide application", r5$ACTIVTY1), r5$COST, NA) ,
	  insecticide_price = ifelse(grepl("Insecticide application", r5$ACTIVTY1), r5$PRICE, NA) ,
	  
	  pesticide_used = ifelse(grepl("Pesticide application", r5$ACTIVTY1), TRUE, FALSE) ,
	  pesticide_amount = ifelse(grepl("Pesticide application", r5$ACTIVTY1), r5$QTY, NA) ,
	  pesticide_cost = ifelse(grepl("Pesticide application", r5$ACTIVTY1), r5$COST, NA) ,
	  pesticide_price = ifelse(grepl("Pesticide application", r5$ACTIVTY1), r5$PRICE, NA) ,
	  
	  weeding_done = ifelse(grepl("Weeding", r5$ACTIVTY1), TRUE, FALSE) ,
	  DAP = as.integer(gsub("SB",NA,  r5$`DBT/DAT`)),
	  weeding_labour_cost = as.numeric(gsub("PL|Pl", NA, r5$WAGE))
	)
	
	### restructure d5 dataset
	comNm <- names(d5)[!grepl("herbicide|insecticide|fungicide|pesticide", names(d5))]
	dcom <- unique(d5[, comNm])
	dherb <- unique(d5[!is.na(d5$herbicide_amount), c(comNm, "herbicide_used", "herbicide_amount", "herbicide_cost", "herbicide_price")])
	dfung <- unique(d5[!is.na(d5$fungicide_amount), c(comNm, "fungicide_used", "fungicide_amount", "fungicide_cost", "fungicide_price")])
	##
	dinsect <- unique(d5[!is.na(d5$insecticide_amount), c(comNm, "insecticide_used", "insecticide_amount", "insecticide_cost", "insecticide_price")])
	dinsect <- aggregate(lapply(dinsect[c("insecticide_amount", "insecticide_cost", "insecticide_price")], as.numeric), dinsect[comNm], mean, na.rm = TRUE)
  ###
	dpest <- unique(d5[!is.na(d5$pesticide_amount), c(comNm, "pesticide_used", "pesticide_amount", "pesticide_cost", "pesticide_price")])
	dpest <- aggregate(lapply(dpest[c("pesticide_amount", "pesticide_cost", "pesticide_price")], as.numeric), dpest[comNm], mean, na.rm = TRUE)
	
	d5f <- Reduce(function(x, y) merge(x, y, by = comNm, all.x = TRUE), list(dcom, dherb, dfung, dpest, dinsect))# dfung, dinsect, dpest
	
	####  merge d and d5f
	
	d <- merge(d, d5f[!duplicated(d5f[c("hhid", "date", "season", "plot_id", "cropland")]),], by= c("hhid", "date", "season", "plot_id", "cropland"), all.x  = TRUE)
	
	#####
	d6 <- data.frame(
	  date = r6$YEAR,
	  season = r6$SEASON,
	  hhid = as.character(r6$HHCODE),
	  plot_id = as.character(r6$PARNO),
	  cropland = round(r6$FSIZE, 4),
	  DAP = as.integer(gsub("SB", NA, r6$DBTDAT)),
	  N_fertilizer = r6$N,
	  P_fertilizer = r6$P,
	  K_fertilizer = r6$K,
	  fertilizer_price = r6$PRICE,
	  fertilizer_amount = r6$QTYKG,
	  fertilizer_cost = r6$FERCOST,
	  sex = gsub("M", "Male", r6$SEX)
	)
	
	d6$sex[is.na(d6$sex)] <- "Female"
	d6 <- aggregate(lapply(d6[c("N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_amount", "fertilizer_cost")], as.numeric), d6[, c("hhid", "date", "season", "plot_id", "cropland", "sex", "DAP")], mean, na.rm = TRUE)
	####  merge d and d6
	d <- merge(d, d6, by= c("hhid", "date", "season", "plot_id", "cropland", "sex", "DAP"), all.x  = TRUE)
	
	###
	d7 <- data.frame(
	  date = r7$YEAR,
	  season = r7$SEASON,
	  hhid = as.character(r7$HHCODE),
	  plot_id = as.character(r7$PARNO),
	  cropland = round(r7$FSIZE, 4),
	  activy = r7$ACTIVITY,
	  harvest_date = r7$DATE,
	  sex =  c("M"= "Male", "F"= "Female")[r7$SEX],
	  harvest_cost = as.numeric(gsub("PL", NA, r7$WAGE))
	)
	
	d7 <- d7[grepl("Harvesting", d7$activy),]
	d7$activy <- NULL
	d7$harvest_date <- gsub("17 N0v 2015", "2015-11-17", d7$harvest_date)
	d7$harvest_date <- gsub("10 Nov. 2015", "2015-11-10", d7$harvest_date)
	d7$harvest_date <- gsub("28 Nov. 2015", "2015-11-28", d7$harvest_date)
	d7$harvest_date <- gsub("25 Nov. 2015", "2015-11-25", d7$harvest_date)
	d7$harvest_date <- gsub("15 Nov. 2015", "2015-11-15", d7$harvest_date)
	d7$harvest_date <- gsub("03 Dec. 2015", "2015-12-03", d7$harvest_date)
	d7$harvest_date <- gsub("22 Oct. 2015", "2015-10-22", d7$harvest_date)
	d7$harvest_date <- gsub("29 Oct. 2015", "2015-10-29", d7$harvest_date)
	d7$harvest_date <- gsub("15 Oct. 2015", "2015-10-15", d7$harvest_date)
	d7$harvest_date <- gsub("25 Oct. 2015", "2015-10-25", d7$harvest_date)
	
	i <- !grepl("-", d7$harvest_date)
	d7$harvest_date[i] <- as.character(as.Date(as.numeric(d7$harvest_date[i]), origin = "1899-12-30"))
	
	####  merge d and d6
	d <- merge(d, d7, by= c("hhid", "date", "season", "plot_id", "cropland", "sex"), all  = TRUE)
	
	##
	d8 <- data.frame(
	  date = r8$YEAR,
	  season = r8$SEASON,
	  hhid = as.character(r8$HHCODE),
	  plot_id = as.character(r8$PARNO),
	  cropland = round(r8$FSIZE, 4),
	  variety = r8$VARIETY,
	  yield = r8$PRODNKG/r8$FSIZE,# kg/ha
	  yield_marketable = r8$SOLDKG/r8$FSIZE, ## kg/ha
	  crop_price = r8$PRICE
	)
	
	####  merge d and d8
	d <- merge(d, d8, by= c("hhid", "date", "season", "plot_id", "cropland", "variety"), all  = TRUE)
	
	####
	d9 <- data.frame(
	  date = r9$YEAR,
	  season = r9$SEASON,
	  hhid = as.character(r9$HHCODE),
	  plot_id = as.character(r9$PARNO),
	  cropland = r9$FSIZE,
	  seed_rate = r9$QTYSEED/r9$FSIZE,
	  seed_cost = r9$SEEDVAL,
	  seed_source = r9$SEEDSRC
	)
	
	####  merge d and d9
	d <- merge(d, d9, by= c("hhid", "date", "season", "plot_id", "cropland"), all  = TRUE)
	
	d$season <- c("wet", "dry")[d$season]
	d$planting_date[is.na(d$planting_date)] <- d$date[is.na(d$planting_date)]
	d$date <- NULL
	
	d$crop <- "rice"
	d$is_survey <- TRUE
	d$on_farm <- FALSE
	d$trial_id <- "1"
	d$yield_moisture <- NA
	d$yield_part <- "none"
	d$country <- "Philippines"
	d$location <- "Central Luzon"
	d$currency <- "PHP"
	d$yield_isfresh <- TRUE
	d$geo_from_source <- FALSE
	d$latitude <- 15.91459
	d$longitude <- 121.0841
	
	### 
	d$harvest_date[(as.Date(d$harvest_date)-as.Date(d$planting_date)) < 20] <- NA
	d$harvest_date[(as.Date(d$harvest_date)-as.Date(d$planting_date))> 366] <- NA
	### remove duplicate
	d <- unique(d)
	
	
	carobiner::write_files(path, meta, d)
}


