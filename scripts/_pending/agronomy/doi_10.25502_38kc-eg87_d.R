# R script for "carob"
# license: GPL (>=3)

## ISSUES

# -The biochar application rate is missing from the raw data. According to the publication, biochar was applied at **0, 1, 5, and 10 Mg/ha

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
		publication = "doi:10.1007/s13593-022-00793-5",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;biochar",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-28",
		carob_completion = 70,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "bc-data-from-lr016-to-sr022.csv"]
	f2 <- ff[basename(ff) == "c-stock_lr2022_dr.csv"]
	f3 <- ff[basename(ff) == "c-stock_lr2022_dr_bd.csv"]
	#f4 <- ff[basename(ff) == "data_dictionary.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	#r4 <- read.csv(f4)

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
	#bc <- c("Fert" = 0, "Control" = 0, "Biochar" = ? , "Fert+Biochar"= ?)
	d1$N_fertilizer <- Nfer[d1$treatment]
	d1$P_fertilizer <- Pfer[d1$treatment]
	
	### process carbon stock
	# Not sure how to merge this with yield data (different site)
	d2 <- data.frame(
	  trial_id = paste(r2$Site, r2$ID, sep = "-"),
	  location = r2$Site,
	  plot_id = r2$Plot_No,
	  treatment = r2$Treatment,
	  depth = r2$Sampling_Depth,
	  soil_bd = r2$BD,
	  soil_C = r2$perc_C,
	  soil_C_stock = r2$C_Stock_t_C_ha_1*100, # g/m2
	  longitude = 34.458 ,
	  latitude = 0.155
	)
	
	tret <- c("CROP+FERT" = "Fert", "CROP+FERT+CHARC"= "Fert+Biochar", "CROP" = "Control", "CROP+CHARC" = "Biochar")
	d2$treatment <- tret[d2$treatment]

	## Adding long and lat 
	
	geo <- data.frame(
	  location = c("Siaya", "Embu" ),
	  longitude = c(34.2488, 37.6259),
	  latitude = c(-0.0546, -0.5922),
	  geo_uncertainty = c(44931, 60722),
	  geo_source ="GADM 4.1, adm1",
	  geo_from_source = FALSE,
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
	
	### Adding planting and harvest date 
	
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
	
	
	carobiner::write_files(path, meta, d)
}


