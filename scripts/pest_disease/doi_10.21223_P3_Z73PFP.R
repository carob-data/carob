# R script for "carob"
# license: GPL (>=3)

## ISSUES
## The description says the experiment was done on the locations but the dataset has only one location Kachwekano ZARDI
#Plant height for potatoes were out of bound with maximum value 75.5

carob_script <- function(path) {

"
Dataset for: Multi-location confined field trials (ML-CFTs) for dossier development of biotech late blight resistant potato in Uganda

Transgenic potato Vic.1 carries three resistance (R) genes from wild potato relatives that were introduced to confer resistance against potato late blight (LB) disease caused by Phytophthora infestans. Preliminary field trials show that the introduction of these R genes indeed confers extreme resistance to P. infestans. In this regulatory studies, we aim to confirm the effectiveness of the introduction of the R genes in Vic.1 by conducting confined field trials during two seasons at three locations.
"

	uri <- "doi:10.21223/P3/Z73PFP"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP; NARO",
		publication = NA,
		project = "3R potato ML-CFT",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-08-26",
		carob_completion = 70,	
		carob_effort = 4
	)
	
	#f1 <- ff[basename(ff) == "Data Dictionary.xlsx"]
	f2 <- ff[basename(ff) == "Data.xls"]
	
	#r1 <- carobiner::read.excel(f1)
	#r2a <- carobiner::read.excel(f2, sheet="CFT Data")
	r2b <- carobiner::read.excel(f2, sheet="RS 8 9 AGRO ERA-Plant Dev", na="No data")
	r2c <- carobiner::read.excel(f2, sheet="RS9-ERA-NTO")
	#r2d <- carobiner::read.excel(f2, sheet="RS4-LB data")    # no data
	r2e <- carobiner::read.excel(f2, sheet="Harvest data")
	#r2f <- carobiner::read.excel(f2, sheet="Leaf samples")
	#r2g <- carobiner::read.excel(f2, sheet="Tuber samples")

	d1 <- data.frame(
	  plot_id = as.character(r2b$Plot),
	  date = r2b$`Actual date`,
	  variety = r2b$Genotype,
	  disease = "potato late blight",
	  disease_incidence = as.character(r2b$`Incidence of LB (%)`),
	  plant_height = r2b$`Estimated average plant height (cm)`
	)
	d1 <- d1[!(is.na(d1$date) | is.na(d1$variety)), ]
    d1$date <- gsub("/18", "/2018", d1$date)
    d1$variety <- gsub("820046.09999999998", "820046.1", d1$variety)
    d1$variety <- gsub("381381.20000000001", "381381.20", d1$variety)
	
    d1$date <- ifelse(grepl("/", d1$date), as.character(as.Date(d1$date, format="%d/%m/%Y")), 
	           ifelse(grepl("-", d1$date), as.character(as.Date(d1$date, format="%m-%d-%Y")), d1$date))
	i <- !grepl("-", d1$date)
	d1$date[i] <- as.character(as.Date("1899-12-29") + as.integer(d1$date[i]))
	
	obs_lookup <- c(
	  "Incidence of early blight (%)"      = "early blight",
	  "Incidence of Rhizoctonia (%)"       = "Rhizoctonia",
	  "Incidence of bacterial wilt (%)"    = "bacterial wilt",
	  "Incidence of black leg (%)"         = "black leg",
	  "Incidence of virus (%)"             = "virus",
	  "Incidence of aphids (%)"            = "aphids",
	  "Incidence of white flies (%)"       = "white flies",
	  "Incidence of leaf miner flies (%)"  = "leaf miner flies",
	  "Incidence of moth (%)"              = "moth"
	)
	
	d_long <- do.call(rbind, lapply(names(obs_lookup), function(cn) {
	  data.frame(
	    plot_id = as.character(r2c$Plot),
	    pest_species = obs_lookup[[cn]],
	    pest_incidence = as.integer(r2c[[cn]])
	  )}))
	  d_long <- d_long[!is.na(d_long$plot_id), ]
		
## the interest in d3 / r2e would be to get to yield?
	r2e$tuber_fresh_weight <- with(
	  r2e,
	  `Weight of small tubers (g)` +
	    `Weight of medium tubers (g)` +
	    `Weight of large tubers (g)`
	)
	
	d3 <- data.frame(
	  plot_id     = as.character(r2e$Plot),
	  flesh_color = tolower(r2e$`Flesh colour`),
	  fw_tubers = r2e$tuber_fresh_weight,   #g per plot
	  plot_area   = 6.75,  #m2
	  yield       = (r2e$tuber_fresh_weight / 6.75) * 10 
	)|> unique()
	
	d <- merge(d1, d3, by = "plot_id", all.x = TRUE)	    
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
  d$trial_id <- "1"	
	d$country = "Uganda"
	d$location = "Kachwekano ZARDI" #location provided in r2a <- carobiner::read.excel(f2, sheet="CFT Data")
	d$longitude <- 29.942
	d$latitude <- -1.254
	d$geo_source <- "Google maps"   #Kachwekano Zonal Agricultural Research and Development Institute  #actual coordinates were provided
	d$geo_from_source <- FALSE
	
	d$planting_date <-"2017-11-27"	
	d$harvest_date <- "2018-03-20"
		
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
	d$fertilizer_type <- NA
	d$yield_part <- "tubers"
	d$yield_moisture <- NA
	d$crop <- "potato"
	d$yield_isfresh <- TRUE
		
	carobiner::write_files(path, meta, d, long=d_long)	
}

