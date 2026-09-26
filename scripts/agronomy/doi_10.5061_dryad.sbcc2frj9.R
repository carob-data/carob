# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Optimizing maize yield in West and Central Africa

A study was conducted in four different environments of Ghana. The aim was to optimize maize by developing maize hybrids tolerant of high plant density. The hybrids were evaluated under three plant densities, namely, high (88,888 plants/ha), medium (66,666 plants/ha), and low (53,333 plants/ha). The experimental design was 8 x 6 alpha lattice with split plot. The experiment was replicated two times in each of the four environments. Data on different phenotypic traits were collected either by measuring or counting.
"

	uri <- "doi:10.5061/dryad.sbcc2frj9"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
		data_organization = "MAK; UGHA; IARI",
		publication = "doi:10.1002/pei3.70046",
		project = NA,
		design = "8 x 6 alpha lattice with split plot",
		data_type = "experiment",
		treatment_vars = "plant_density;variety",
		response_vars = "yield", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-09-17",
		carob_completion = 90,	
		carob_effort = 5
	)
	

	f1 <- ff[basename(ff) == "Optimizing_maize_yield_in_West_and_Central_Africa.xlsx"]
	#f2 <- ff[basename(ff) == "README.md"]  #dictionary

	r1a <- carobiner::read.excel(f1, sheet="Harvest & After Harvest")
	r1b <- carobiner::read.excel(f1, sheet="Before Harvest")


	d1 <- data.frame(
	  rep = as.integer(r1a$Rep),
	  block_id = as.character(r1a$Block),
	  variety = r1a$Genotype,
	  location = r1a$Environment,
	  plant_density = ifelse(r1b$PD == "Low", 53333,
	                         ifelse(r1b$PD == "Medium", 66666,
	                                ifelse(r1b$PD == "High", 88888, NA))),
	  seed_weight = r1a$HKW * 10,    
	  yield = as.numeric(r1a$Yield * 1000)
	)
	
	r1b$CHC[r1b$CHC == "n/a"] <- NA
	
  d2 <- data.frame(
    rep = as.integer(r1b$Rep),
    block_id = as.character(r1b$Block),
    location = r1b$Environment,
    variety = r1b$Genotype,
    SPAD = as.numeric(r1b$CHC),
    ear_height = r1b$EH,
    plant_height = r1b$PH,
    slper = r1b$SL,
    rlper = r1b$RL,
    asi = r1b$ASI,
    plant_density = ifelse(r1b$PD == "Low", 53333,
                           ifelse(r1b$PD == "Medium", 66666,
                                  ifelse(r1b$PD == "High", 88888, NA)))
  )  
	
  d <- merge(d1, d2, by = c("rep", "block_id", "location", "variety", "plant_density"), all = TRUE)
  
  d$trial_id <- "1"
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
  d$country <- "Ghana"
	d$location <- c("Fumesua", "Legon_off", "Legon_Mi", "Nyankpala")
	d$latitude <- c(6.7147, 5.660, 5.660, 9.391)
	d$longitude <- c(-1.5397, -0.191, -0.191, -1.008)
	d$geo_from_source <- TRUE  ##The coordinates were obtained from the publication


	d$planting_date <- NA
	d$harvest_date  <- NA


  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$fertilizer_type <- NA
  
  d$crop <- "maize"
  d$yield_part <- "grain"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- NA
  
  	carobiner::write_files(path, meta, d)
}
