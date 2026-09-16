# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Stem rot of rice in California: incidence-severity relationship, yield impacts, and fungicide management.

Stem rot, caused by Sclerotium oryzae, is a common and important disease of rice grown in California. This study used 19 stem rot fungicide trials conducted between 2017 and 2024 in the Sacramento Valley of California to determine the relationship between disease incidence and severity. Eighteen of these trials were used to estimate the impact of stem rot severity on grain yield and seven to evaluate the best timing of application for the azoxystrobin containing fungicides Quadris and QuiltXcel. Results show that incidence and severity are related and that incidence up to values of 90% can predict the severity of stem rot up to a severity index value of 2. Yield loss due to stem rot severity was found to be described best by a linear model including yield level as a factor, with a rate of yield loss of 258 kg/ha (95% CI: 193 to 325 kg/ha) for each unit change in stem rot severity index. Relative yield loss per unit increase in severity index was calculated to be 2.63% (95% CI: 1.95 to 3.3%). The azoxystrobin containing fungicides Quadris and QuiltXcel reduced stem rot severity when applied at the boot stage but not at tillering. Application of Quadris at boot or QuiltXcel at heading resulted in significant yield increases.
"


	uri <- "doi:10.5061/dryad.j3tx95xxf"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "UCDANR", # University of California Division of Agriculture and Natural Resources
		publication = "doi:10.1016/j.cropro.2026.107829",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code;fungicide_used",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-15",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "SR_data_DRYAD.csv"]
	#f2 <- ff[basename(ff) == "README.md"]

	r1 <- read.csv(f1, na= c("null", "N/A"))


	d <- data.frame(
		trial_id = as.character(r1$Trial),
		treatment = r1$Treatment_1,
		fungicide_used = !grepl("^1$", r1$Treatment_1),
		rep = r1$Rep,
		yield = as.numeric(r1$Yield_kg),
		year = r1$Year,
		variety_code = c("M-206", NA ,"M-209")[r1$Variety],
		#disease_level = r1$Disease_level,
		disease = "stem rot",
		disease_incidence = as.character(r1$SR_Inc),
		disease_severity = as.character(r1$SR_Sev)
	)
	
	### drop treatment that were Not used in the analysis (#from README file)
	d <- d[!is.na(d1$treatment),]
	
	trt <- c("1"= "untreated", "22"= "Quadris at tillering", "23"= "Quadris at boot", "24"= "Quadris at heading", "25"= "QuiltXcel at tillering", "26"= "QuiltXcel at boot", "27" = "QuiltXcel at heading")
  d$treatment <- trt[d$treatment]
  d$fungicide_product <- ifelse(grepl("Quadris", d$treatment), "azoxystrobin", 
                         ifelse(grepl("QuiltXcel", d$treatment), "azoxystrobin;propiconazole",
                         ifelse(grepl("untreated", d$treatment), "none", "unknown"))) 
	
  ### additional information from publication
  
  inf_from_pub <- data.frame(
    trial_id = c("5", "6", "7", "8", "9", "11", "13"),
    planting_date = c("2018-05-29", "2018-05-17", "2019-06-15", "2019-05-18", "2019-05-17", "2020-06-01", NA),
    fungicide_dates = c("2018-07-03; 2018-08-20", "2019-06-26;2019-08-03","2019-07-24;2019-08-16;2019-08-30","2019-06-24;2019-07-29;2019-08-12","2019-06-23;2019-07-26;2019-08-10","2020-08-06;2020-08-19",NA),
    harvest_date = c("2018-10-25", "2018-10-11", "2019-10-24", NA, "2019-10-01", "2020-10-20", NA)
      )
  
  d <- merge(d, inf_from_pub, by= "trial_id", all.x = TRUE)
  
  d$planting_date <- ifelse(is.na(d$planting_date), d$year, d$planting_date)
  d$year <- NULL
  
  d$crop <- "rice"
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  d$yield_moisture <- 14
  d$yield_part <- "grain"
  d$country <- "United States"
  d$location <- "Sacramento Valley"
  d$geo_from_source <- FALSE
  d$latitude <- 38.0493
  d$longitude <- -121.7953
  d$irrigated <- NA
  
  d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
  
  ### remove one duplicate record
  d <- unique(d)
  
	carobiner::write_files(path, meta, d)
}


