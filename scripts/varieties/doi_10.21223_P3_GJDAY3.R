# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Dataset for: Orange fleshed Observation Trials at Gurue Research Station evaluated in 2017

The Observation Trial was carried out in 2017 season B at Gurue Research Station. The trial had 2667 experimental clones, 2 check clones and progenitors/parents of the experimental clones. Trial design was Westcott. Each clone was planted to a single row plot, 1 m long, replicated two times following a randomized complete block design. The check clones were alternatively planted after every 10 experimental clones. The objective of the  trial was to select clones with orange flesh, tolerant to weevils, virus, high yielding, high plant vigour and high dry matter. The clones meeting the above mentioned characters were advanced to preliminary yield trials. Raw data for commercial root yield, non-commercial root yield and vine yield are presented in the report.
"

	uri <- "doi:10.21223/P3/GJDAY3"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCB",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-26",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	
	f <- ff[basename(ff) == "Data Orange flesh OT2017 Gurue.csv"]
	#f1 <- ff[basename(ff) == "Dictionary_OFSP OT Gurue.xlsx"]


	r <- read.csv(f)
	
### process	
	d <- data.frame(
	  plot_id = as.character(r$plot),
	  rep = r$rep,
	  variety = r$geno,
	  yield_marketable = r$RCTHA*1000,
	  yield = rowSums(r[, c("RCTHA", "NRCTHA")], na.rm = TRUE)*1000,
	  fwy_total = r$FYTHA*1000,
	  planting_date = "2017",
	  crop = "sweetpotato",
	  country = "Mozambique",
	  location = "Gurue Research Station"
	)
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_isfresh <- TRUE
	d$trial_id <- "1"
	d$yield_moisture <- NA_real_
	d$yield_part <- "roots"
	d$geo_from_source <- FALSE
	d$latitude <- -15.4664
	d$longitude <- 36.9785
	d$geo_source <- "Google Maps"
	d$irrigated <- NA
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	d$harvest_date <- NA_character_

	carobiner::write_files(path, meta, d)
}


