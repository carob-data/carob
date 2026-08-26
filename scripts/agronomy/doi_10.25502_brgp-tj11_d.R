# R script for "carob"
# license: GPL (>=3)

## ISSUES

## Treatment
  # - combinations of whole tubers and minisetts and three tuber-size 
  


carob_script <- function(path) {

"
Seed yam (Dioscorea rotundata Poir.) production from whole tubers versus minisetts

An experiment was conducted to investigate the effects of using whole tubers versus minisetts to produce seed yams. Six treatments were combinations of whole tubers and minisetts and three tuber-size classes of 30 – 59 g, 60 - 89 g, and 90 – 120 g (referred to as 45 g, 75 g, and 105 g, respectively). The eExperiment was conducted as a Randomized Complete Block Design with three replications.
"

	uri <- "doi:10.25502/brgp-tj11/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-24",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "cut-vs-whole-trial-yiifswa_abj_data.csv"]
	f2 <- ff[basename(ff) == "metadata_cut_whole_trials.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

#### process
	
	d <- data.frame(
	  plot_id = as.character(r1$Plot),
	  rep = r1$Rep,
	  location = r1$Loc,
	  planting_date = as.character(r1$YearFull),
	  #seed_size = r1$Size,
	  seed_treatment = paste(r1$SettType,r1$Size, sep = "-"),
	  germination_days = r1$Day50_perc_Sprout, #Duration of 50% Sprout
  	yield = r1$tha*1000,
	  LAI = r1$LAI,
	  virus_severity = as.character(r1$VIRUS),
	  disease = "Anthracnose",
	  disease_severity = as.character(r1$ANTHRAC),
	  root_infection = r1$C_ROOT,
	  #crack_severity = r1$CRACK,
	  insect = r1$INSECT,
	  severity_scale ="1-5",
	  crop = "yam",
	  country = "Nigeria",
	  trial_id = ifelse(grepl("Whole", r1$SettType), "1", "2")
	)
	
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	d$yield_part <- "tubers"
	d$geo_from_source <- FALSE
	d$latitude <- 9.0547
	d$longitude <- 7.4917
	d$geo_source <- "Google Maps" 
	d$irrigated <- NA
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	d$harvest_date <- NA_character_
	

	carobiner::write_files(path, meta, d)
}

