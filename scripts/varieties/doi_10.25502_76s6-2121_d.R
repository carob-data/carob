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

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

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
		response_vars = "yield;seed_weight;plant_height;flowering_days;maturity_days;disease;severity_scale", 
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
	  location = as.character(r1$LOC),
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
	  seed_weight = r1$SWT100*10,
	  sl = r1$LODGING,
	  dmy_residue = r1$BIOM,
	  moist = r1$MC, #moisture content for grain yield
	  crop = "soybean"
	)
	
	d1$location <- paste0(
	  toupper(substr(tolower(d1$location), 1, 1)),
	  substr(tolower(d1$location), 2, nchar(d1$location))
	)
	
	d1$plant_height[d1$plant_height %in% c(6, 167)] <- NA
	
	long <- data.frame(
	  disease = c(
	    "rust",
	    "frogeye leaf spot",
	    "cercospora leaf spot",
	    "bacterial blight",
	    "bacterial pustule",
	    "red leaf blotch"
	  ),
	  severity_scale = c(
	    paste(range(c(r1$RUST_R3, r1$RUST_R6), na.rm = TRUE), collapse = "-"),
	    paste(range(r1$FROGEYE, na.rm = TRUE), collapse = "-"),
	    paste(range(r1$CERPOSPORA._LS, na.rm = TRUE), collapse = "-"),
	    paste(range(r1$BB, na.rm = TRUE), collapse = "-"),
	    paste(range(r1$BP_R3, na.rm = TRUE), collapse = "-"),
	    paste(range(r1$RED_LB, na.rm = TRUE), collapse = "-")
	  )
	)
	
	long$trial_id <- "1"
	

## see carobiner::geocode
###IITA-SARAH and SEEDCO are found in Lusaka district whereas Good Nature Agro is located in Chipata district
##The geo-coordinates where taken from google maps. geo_uncertanity were obatined from adm2 = Chipata and Lusaka districts
	
	geo <- data.frame(
	  adm1 = c("Eastern", "Lusaka", "Lusaka"), 
	  adm2 = c("Chipata", "Lusaka", "Lusaka"), 
	  location = c("Good nature agro", "Iita-sarah", "Seedco"), 
	  longitude = c(32.6450, 28.18173, 28.26166), 
	  latitude = c(-13.6450, -15.1809, -15.455833), 
	  geo_uncertainty = c(38917, 19447, 19447), 
	  geo_source = rep("GADM 4.1, adm2", 3),
	  geo_from_source = FALSE
	)
	
	d <- merge(d1, geo, by = "location", all.x = TRUE)
	
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- as.numeric(NA) 
  d$fertilizer_type <- NA

  
  d$country = "Zambia"
  d$on_farm <- TRUE
  d$is_survey <- FALSE 
  d$irrigated <- NA
	d$yield_part <- "grain"
	d$yield_isfresh <- TRUE
	d$yield_moisture <- 13

	carobiner::write_files(path, meta, d, long=long)
}


