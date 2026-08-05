# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Advanced Variety Trials (AVT), Malawi - 2018

Soybean (Glycine max (L.) Merrill.) is one of the most important oil crops of the world which also has tremendous importance as a food legume. The work on soybean aims at providing farmers, both commercial and subsistence, varieties with their preferred attributes to increase yield and income. These include high yield, resistance to deadly diseases, such as soybean rust, and insect pests, early maturity, good seed quality, and resistance to other stresses such as drought and soil acidity.The International Institute of Tropical Agriculture (IITA) is a key player in tropical soybean research and a partner of the Soybean Innovation Lab.
"

	uri <- "doi:10.25502/H8FK-PK81/D"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_pedigree",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-08-03",
		carob_completion = 90,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "data.csv"]
	#f2 <- ff[basename(ff) == "abv.csv"]
	#f3 <- ff[basename(ff) == "dictionary.csv"]

	r <- read.csv(f1)
	#r2 <- read.csv(f2)
	#r3 <- read.csv(f3)


	d <- data.frame(
	  country="Malawi",
	  location="Chitedze Research Station",
	  adm1="Central Region",
	  adm2="Lilongwe",
	  adm3="Chitedze",
	  latitude=-13.9815,
	  longitude=33.6371,
	  geo_uncertainty=100,#approx length of the field 
	  crop="soybean",
	  variety_pedigree=r$CROSS,
	  rep=as.integer(r$REP_NO),
	  plot_id=as.character(r$PLOT_NO),
	  flowering_days=r$DFFL,
	  podding_days=r$DF_P,
	  maturity_days=r$DM,
	  seed_weight=r$SWT100,#100 seed weight
	  yield=r$YIELD,
	  yield_part="seed",
	  yield_moisture=r$MC,
	  yield_isfresh=TRUE,
	  rust_3=r$RUST_R3,
	  rust_6=r$RUST_R6,
	  SMV=r$SMV,#Soyabean Mosaic Virus
	  frogeye=r$FROGEYE,
	  bacterial_pustule=r$BP_R3,
	  bacterial_blight=r$BB,
	  red_leaf_blot=r$RED_LB,
	  pod_shattering_score=r$SHATTERING
	  )
	
	d$trial_id <- paste(d$variety_pedigree,d$plot_id,sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
	d$planting_date <- NA
	d$harvest_date  <- NA
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA) 
  
  ##reshaping disease cols 
  disease_cols <- c("rust_3", "rust_6", "SMV", "frogeye", 
                    "bacterial_pustule", "bacterial_blight", "red_leaf_blot")
  
  d$row_id <- seq_len(nrow(d))
  
  long <- reshape(d, 
                  varying = disease_cols, 
                  v.names = "severity_scale", 
                  timevar = "disease", 
                  times = disease_cols, 
                  idvar = "row_id", 
                  direction = "long")
  
  rownames(long) <- NULL
  long$row_id <- NULL
  
  #cleaning disease names
  disease_lookup <- c(
    rust_3 = "rust",
    rust_6 = "rust",
    SMV = "soybean mosaic virus",
    frogeye = "frogeye leaf spot",
    bacterial_pustule = "bacterial pustule",
    bacterial_blight = "bacterial blight",
    red_leaf_blot = "red leaf blotch"
  )
  
  # lookup: raw column name -> growth stage (NA where not applicable)
  stage_lookup <- c(
    rust_3 = "R3",
    rust_6 = "R6",
    SMV = NA,
    frogeye = NA,
    bacterial_pustule = "R3",  
    bacterial_blight = NA,
    red_leaf_blot = NA
  )
  
  long$growth_stage <- stage_lookup[long$disease]
  long$disease <- disease_lookup[long$disease]
  
  d <- long
  d$severity_scale <- as.character(d$severity_scale)
  
	carobiner::write_files(path, meta, d)
}

