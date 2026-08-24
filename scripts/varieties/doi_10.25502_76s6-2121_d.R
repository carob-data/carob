# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them
### Harvest_date was calculated from days after planting, yet the planting date was not provide
###Therefore missing harvest_date and planting_date
## added a new disease "cercospora leaf spot"

carob_script <- function(path) {

"
Advanced Variety Trials (AVT), Zambia - 2018

Soybean (Glycine max (L.) Merrill.) is one of the most important oil crops of the world which also has tremendous importance as a food legume. The work on soybean aims at providing farmers, both commercial and subsistence, varieties with their preferred attributes to increase yield and income. These include high yield, resistance to deadly diseases, such as soybean rust, and insect pests, early maturity, good seed quality, and resistance to other stresses such as drought and soil acidity.The International Institute of Tropical Agriculture (IITA) is a key player in tropical soybean research and a partner of the Soybean Innovation Lab.
"

	uri <- "doi:10.25502/76s6-2121/d"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "Randomised Complete Block Design",
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;seed_weight;plant_height;flowering_days;maturity_days;disease", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-08-20",
		carob_completion = 80,	
		carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "zambia-observation.csv"]
	f2 <- ff[basename(ff) == "zambia-abbr.csv"]
	#f3 <- ff[basename(ff) == "zambia-meta-data.csv"]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	d1 <- data.frame(
	  location = r1$LOC,
	  trial_id = "1",
	  plot_id = as.character(r1$ID),
	  planting_date = "2018",
	  harvest_date = NA,
	  seed_source = r1$SOURCE,
	  rep = r1$REP_NO,
	  block_id = as.character(r1$BLOCK_NO),
	  variety = as.character(r1$ENTRY_CODE),
	  variety_pedigree = r1$CROSS,
	  plot_area = NA,
	  plant_height = r1$PLHT,
	  maturity_days = r1$DM,
	  flowering_days = r1$DFFL,
	  yield = r1$`YIELD`,
	  podding_days = r1$DF_P,
	  seed_weight = r1$SWT100 * 10,
	  sl = r1$LODGING,
	  dmy_residue = r1$BIOM,
	  yield_moisture = r1$MC, # moisture content for grain yield
	  crop = "soybean",
	  rust1 = r1$RUST_R3,
	  rust2 = r1$RUST_R6,
	  frogeye = r1$FROGEYE,
	  BB = r1$BB,
	  BP = r1$BP_R3,
	  RED = r1$RED_LB,
	  CER = r1$CERPOSPORA._LS,
	  record_id = as.integer(1:nrow(r1))
	)
	
	d1$plant_height[d1$plant_height %in% c(6, 167)] <- NA
	
	cols <- names(d1)[grepl("rust|frogeye|BB|BP|RED|CER|record_id", names(d1))]
	long <- d1[, cols]
	
	cols <- cols[cols!="record_id"]
	long <- reshape(long, varying = cols,  v.names = "disease_severity",  timevar = "disease", direction = "long")
	
	long$disease <- c("rust", "frogeye leaf spot", "bacterial blight", "bacterial pustule",  "red leaf blotch", 
						"cercospora leaf spot")[long$disease]
	long <- long[!is.na(long$disease_severity), ]
	long$disease_severity <- as.character(long$disease_severity)
	long$id <- NULL

	d1 <- d1[, !(names(d1) %in% cols)]


## this is not correct. you get 1-3, 1-4 and 1-5. 
## that suggests to me that they are all 1-5. But it could be that the scale is 1-10, and that very high values 
## are not observed.	
#	severity_scale <- data.frame(
#	  disease = c("rust", "frogeye leaf spot", "cercospora leaf spot",  "bacterial blight",  "bacterial pustule",  "red leaf blotch"),
#	  severity_scale = c(
#	    paste(range(c(r1$RUST_R3, r1$RUST_R6), na.rm = TRUE), collapse = "-"),
#	    paste(range(r1$FROGEYE, na.rm = TRUE), collapse = "-"),
#	    paste(range(r1$CERPOSPORA._LS, na.rm = TRUE), collapse = "-"),
#	    paste(range(r1$BB, na.rm = TRUE), collapse = "-"),
#	    paste(range(r1$BP_R3, na.rm = TRUE), collapse = "-"),
#	    paste(range(r1$RED_LB, na.rm = TRUE), collapse = "-")
#	  )
#	)
#	long <- merge(long, severity_scale, by = "disease", all.x = TRUE)
	
## see carobiner::geocode
###IITA-SARAH and SEEDCO are found in Lusaka district whereas Good Nature Agro is located in Chipata district
##The coordinates where taken from google maps. 

## geo_uncertanity were obatined from adm2 = Chipata and Lusaka districts
## RH:: this uncertainty is much too large if you know where these locations are 
	
	geo <- data.frame(
      location = c("IITA-SARAH", "Good Nature Agro", "SEEDCO"), 
	  longitude = c(32.6450, 28.18173, 28.26166), 
	  latitude = c(-13.6450, -15.1809, -15.455833), 
	  geo_uncertainty = c(1000, 1000, 1000), 
	  geo_source = "Google Maps",
	  geo_from_source = FALSE
	)
	
	d <- merge(d1, geo, by = "location", all.x = TRUE)
	
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA) 
  
	d$country = "Zambia"
	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- NA
	d$yield_part <- "grain"
	d$yield_isfresh <- TRUE
	## d$yield_moisture <- 13 from where?

	carobiner::write_files(path, meta, d, long=long)
}


