# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Long term Biochar Trials in Kenya

In this project, we report findings from the 15 years of  meta-replicated trials, which are assessing the effect of biochar addition on maize and soybean rotations in smallholder farmers’ fields at three sites in two sub-humid regions of Kenya. Specific objectives were to analyse the effects of biochar input on: i) the yield of maize and soybean without and with inorganic fertiliser, ii) yield reliability, i.e. random variation among seasons, and iii) soil C and N stocks, extractable phosphorus (P) and potassium (K) content, acidity, water-holding capacity and bulk density.
"

	uri <- "doi:10.25502/38kc-eg87/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; SLU",
		publication = "doi:10.1007/s13593-022-00793-5;doi:10.1016/j.fcr.2019.02.015",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;biochar",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-28",
		carob_completion = 70,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "bc-data-from-lr016-to-sr022.csv"]
	f2 <- ff[basename(ff) == "c-stock_lr2022_dr.csv"]
	f3 <- ff[basename(ff) == "c-stock_lr2022_dr_bd.csv"]
	#f4 <- ff[basename(ff) == "data_dictionary.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

#### yield data 
	d1 <- data.frame(
		trial_id = paste(r1$ID,r1$Farm, sep = "_"),
		location = r1$Site,
		plot_id = as.character(r1$Plot_No),
		rep = r1$Rep,
		treatment = r1$Treatment,
		season = ifelse(grepl("SR", r1$Season), "short rains", "long rains")  ,
		crop = tolower(r1$Crop),
		yield = r1$Grain_yield_tha_1* 1000,
		year = gsub("SR|LR", "2", r1$Season),
		crop_rotation = ifelse(grepl("maize", tolower(r1$Crop)), "maize;soybean", "soybean;maize"),
		is_survey = FALSE, 
		on_farm = TRUE, 
		yield_moisture = NA_real_, 
		yield_part = "grain", 
		irrigated = NA, 
		K_fertilizer = as.numeric(NA),
		yield_isfresh = NA
	)
	
	d1 <- d1[!is.na(d1$rep),]
	
	## from publication
	Nfer <- c("Fert" = 60, "Control" = 0, "Biochar" = 0, "Fert+Biochar"= 60)
	Pfer <- c("Fert" = 67, "Control" = 0, "Biochar" = 0, "Fert+Biochar"= 67)
 
	### from publication and from the Authors ( Biochar = 100Mg /ha)
	bc <- c("Fert" = 0, "Control" = 0, "Biochar" = 100*1000 , "Fert+Biochar"= 100*1000)# kg/ha
	d1$N_fertilizer <- Nfer[d1$treatment]
	d1$P_fertilizer <- Pfer[d1$treatment]
	d1$biochar <- bc[d1$treatment]
	
	### process carbon stock 
	
	d2 <- data.frame(
	  trial_id = paste(r2$Site, r2$ID, sep = "-"),
	  location = r2$Site,
	  plot_id = r2$Plot_No,
	  treat = r2$Treatment,
	  depth = r2$Sampling_Depth,
	  soil_bd = r2$BD,
	  soil_C_total = r2$perc_C,
	  soil_C_stock = r2$C_Stock_t_C_ha_1*100, # g/m2
	  longitude = 34.403 ,
	  latitude = 0.131, ## from publication
	  geo_from_source = TRUE,
	  is_survey = FALSE,
	  yield = NA,
	  yield_part = "none", 
	  country = "Kenya"
	)
	
	tret <- c("CROP+FERT" = "Fert", "CROP+FERT+CHARC"= "Fert+Biochar", "CROP" = "Control", "CROP+CHARC" = "Biochar")
	tret1 <- c("CROP+FERT" = "CROP+Fert", "CROP+FERT+CHARC"= "CROP+Fert+Biochar", "CROP" = "CROP", "CROP+CHARC" = "CROP+Biochar")
	d2$treatment <- tret1[d2$treat]
	d2$treat <- tret[d2$treat]
	d2$N_fertilizer <- Nfer[d2$treat]
	d2$P_fertilizer <- Pfer[d2$treat]
	d2$biochar <- bc[d2$treat]
  d2$treat <- NULL
  
	# Water holding capacity
	d3 <- data.frame(
	  trial_id = paste(r3$Site, r3$ID, sep = "-"),
	  location = r3$Site,
	  plot_id = r3$Plot_no,
	  treat = r3$Treatment,
	  depth = as.numeric(gsub("-", "", substr(r3$Sampling_Depth, 3, 5))),
	  soil_bd = r3$BD,
	  soil_WHC_sat = ((r3$Sample_fresh_weight_g-r3$Sample_dry_weight_g)/r3$Sample_dry_weight_g)*100,
	  longitude = 34.403 ,
	  latitude = 0.131, ## from publication
	  geo_from_source = TRUE,
	  is_survey = FALSE,
	  yield = NA,
	  yield_part = "none", 
	  country = "Kenya"
	)
	
	tret <- c("CR+F" = "Fert", "CR+F+CH"= "Fert+Biochar", "CN" = "Control", "CN+CH" = "Biochar", "CR"= "Control", "CR+CH"= "Biochar")
	tret1 <- c("CR+F" = "Crop+Fert", "CR+F+CH"= "CROP+ Fert+Biochar", "CN" = "Control", "CN+CH" = "Biochar", "CR"= "CROP", "CR+CH"= "CROP+Biochar")
	d3$treatment <- tret1[d3$treat]
	d3$treat <- tret[d3$treat]
	d3$N_fertilizer <- Nfer[d3$treat]
	d3$P_fertilizer <- Pfer[d3$treat]
	d3$biochar <- bc[d3$treat]
	d3$treat <- NULL
	## Adding long and lat 
	
	geo <- data.frame(
	  location = c("Siaya", "Embu" ),
	  longitude = c(34.405,  37.5),
	  latitude = c(0.133, -0.5),
	  geo_from_source = TRUE, # from publication
	  country = "Kenya",
	  soil_clay = c(61.00, 70.33),
	  soil_sand = c(23.67, 17.67),
	  soil_C_total = c(1.82, 1.90),
	  soil_N_total = c(0.16,  0.11),
	  soil_P = c(8.60, 0.54),
	  soil_P_method = "Olsen",
	  soil_K = c(0.13,  0.12),
	  soil_Ca = c(3.04, 4.41),
	  soil_Mg = c(1.65, 1.50)
	)
	
	d <- merge(d1, geo, by= "location", all.x = TRUE)
	
	### Adding planting and harvest date available
	
	ph  <- data.frame(
	  year =c(rep("2015", 2), "2016", "2017", rep("2015", 2), "2016", "2017"),
	  season = c("long rains", "short rains", rep("long rains", 2), "long rains", "short rains", rep("long rains", 2)),
	  location =  c(rep("Siaya", 4), rep("Embu", 4)),
	  planting_date = c("2015-03-10", "2015-10-14", "2016-03-21", "2017-03-21","2015-04-17", "2015-10-15", "2016-04-10", "2017-04-07"),
	  harvest_date = c("2015-08-14", "2016-02-14", "2016-08-16", "2017-08-04","2015-09-16", "2016-03-05", "2016-09-18", "2017-09-13")
	)
	
	d <- merge(d, ph, by= c("year", "location", "season"), all.x = TRUE)
  i <- is.na(d$planting_date)
  d$planting_date[i] <- d$year[i]
  d$year <- NULL
	
  ### combine with carbon stock and WHC data
  d <- carobiner::bindr(d, d2, d3)
	
	carobiner::write_files(path, meta, d)
}


