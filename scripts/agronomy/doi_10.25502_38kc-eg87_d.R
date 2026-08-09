# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. added new variables
#2. NA detected: yield, yield_part


carob_script <- function(path) {

"
Long term Biochar Trials in Kenya

In this project, we report findings from the 15 years of  meta-replicated trials, which are assessing the effect of biochar addition on maize and soybean rotations in smallholder farmers’ fields at three sites in two sub-humid regions of Kenya. Specific objectives were to analyse the effects of biochar input on: i) the yield of maize and soybean without and with inorganic fertiliser, ii) yield reliability, i.e. random variation among seasons, and iii) soil C and N stocks, extractable phosphorus (P) and potassium (K) content, acidity, water-holding capacity and bulk density.
"

	uri <- "doi:10.25502/38kc-eg87/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; Swedish University of Agricultural Sciences",
		publication = NA,
		project = NA,
		design = NA,
		data_type ="experiment",
		treatment_vars = "fertilizer_used;biochar_used",#no fertilizer/biochar rates mentioned
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-08-08",
		carob_completion = 99,	
		carob_effort = 5
	)
	
	f1 <- ff[basename(ff) == "bc-data-from-lr016-to-sr022.csv"]
	f2 <- ff[basename(ff) == "c-stock_lr2022_dr.csv"]
	f3 <- ff[basename(ff) == "c-stock_lr2022_dr_bd.csv"]
	#f4 <- ff[basename(ff) == "data_dictionary.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	#r4 <- read.csv(f4)

	d1 <- data.frame(
	  location = r1$Site,
	  plot_id= r1$Plot_No,
	  rep = r1$Rep,
	  treatment = r1$Treatment,
	  season = r1$Season,
	  crop = tolower(r1$Crop),
	  yield = r1$Grain_yield_tha_1*1000,
	  depth = NA,
	  soil_bd = NA,
	  soil_SOC = NA,
	  soil_SOC_stock = NA
	)

	d2 <- data.frame(
	  location = r2$Site,
	  plot_id = r2$Plot_No,
	  rep = NA,
	  treatment = r2$Treatment,
	  season = NA,
	  crop = NA,
	  yield = NA,
	  depth = r2$Sampling_Depth,
	  soil_bd = r2$BD,
	  soil_SOC = r2$perc_C,
	  soil_SOC_stock = r2$C_Stock_t_C_ha_1
	)
	
	d3 <- data.frame(
	  location = r3$Site,
	  plot_id = r3$Plot_no,
	  rep = NA,
	  treatment = r3$Treatment,
	  season = NA,
	  crop = NA,
	  yield = NA,
	  depth = r3$Sampling_Depth,
	  soil_bd = r3$BD,
	  soil_SOC = NA,
	  soil_SOC_stock = NA
)
	
	#mapping Control;Fert;Biochar;Fert+Biochar
	biochar1 <- c(Control = 0, Fert = 0, Biochar = 1, "Fert+Biochar" = 1)
	fert1    <- c(Control = 0, Fert = 1, Biochar = 0, "Fert+Biochar" = 1)
	
	d1$biochar_used    <- biochar1[d1$treatment]
	d1$fertilizer_used <- fert1[d1$treatment]
	
	#mapping CROP;CROP+FERT;CROP+CHARC;CROP+FERT+CHARC
	biochar2<- c(CROP = 0, "CROP+FERT" = 0, "CROP+CHARC" = 1, "CROP+FERT+CHARC" = 1)
	fert2<- c(CROP = 0, "CROP+FERT" = 1, "CROP+CHARC" = 0, "CROP+FERT+CHARC" = 1)
	
	d2$biochar_used   <- biochar2[d2$treatment]
	d2$fertilizer_used <- fert2[d2$treatment]

	#3rd file has the following trtmnts: CR;CR+F;CR+CH;CR+F+CH;CN;CN+CH, but
	#CN and CN+CH are undefined in the dictionary and don't appear anywhere else, so
	# i'm deliberately mapping to NA rather than guessing
	biochar3<- c(CR = 0, "CR+F" = 0, "CR+CH" = 1, "CR+F+CH" = 1, CN = NA, "CN+CH" = NA)
	fert3<- c(CR = 0, "CR+F" = 1, "CR+CH" = 0, "CR+F+CH" = 1, CN = NA, "CN+CH" = NA)
	
	d3$biochar_used    <- biochar3[d3$treatment]
	d3$fertilizer_used <- fert3[d3$treatment]

	d <- rbind(d1,d2,d3)
	
	d$biochar_used <- d$biochar_used==1
	d$fertilizer_used <- d$fertilizer_used==1
  d$country <- "Kenya"
  
  loc <- data.frame(
    location = c("Embu", "Siaya","Nyabeda"),
    longitude = c(37.6259, 34.2488, 34.4029),
    latitude = c(-0.5922, -0.0546, 0.1276 ),
    geo_uncertainty = c(60722, 44931, NA),
    geo_source = c("GADM 4.1, adm1", "GADM 4.1, adm1", "Google Maps")
  )
  
  d <- merge(d,loc,by="location", all.x = T)
  
  d[d==""] <- NA
	d$trial_id <- paste(d$location,1:nrow(d),sep = "_")
	
	#soil depth
	parts <- strsplit(d$depth, "-")
	
	d$depth_top <- sapply(parts, function(x) {if (is.na(x[1])) NA else if (length(x) == 2) as.numeric(x[1]) else 0})	
	d$depth_bottom <- sapply(parts, function(x) if (length(x) == 2) as.numeric(x[2]) else if (length(x) == 1) as.numeric(x[1]) else NA)
	
	#fixing season
	season_map <- c(LR = "long rains", SR = "short rains")
	d$season <- season_map[substr(d$season, 1, 2)]
	
	d$plot_id <- as.character(d$plot_id)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	d$planting_date <- NA
	d$harvest_date  <- NA
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA) 
	d$yield_part <- ifelse(d$crop=="maize","grain","seed")
	d$yield_moisture <- NA
  d$yield_isfresh <- NA
  
  d <- d[!is.na(d$location),]
  d$depth <- NULL
  
	carobiner::write_files(path, meta, d)
}
