# R script for "carob"
# license: GPL (>=3)

## ISSUES
# there are two columns in "time-of-planting-trials-yiifswa_abj_data.csv" named "Yield_t_ha". read.csv changed the name of the second one to Yield_t_ha.1 [this is the actual yield as the first one corresponds to # of leaves]
# "time-of-planting-trials-yiifswa_abj_data.csv" had fully empty trailing rows that resulted in a few variables being read as empty, so they were removed
# planting_date & harvest_date weren't provided the closest document were TimePlant (recorded as 'early', 'mid', and 'end') and Year (recorded as 1 and 2, for presumably 2015 and 2016)

carob_script <- function(path) {

"
The influence of minisett size and time of planting on the yield of seed yam (Dioscorea rotundata) in Abuja, Nigeria

The data is from an investigation of the influence of planting different minisett sizes at different periods on yield. The trial was carried out on the experimental field at IITA Abuja Station (9.164694 N, 7.345136 E) during the cropping seasons of 2015 and 2016. It was laid out in a Randomized Complete Block Design (RCBD) with three replications with nine treatment combinations: 30 g minisetts planted early (Early); 30 g minisetts planted 21 days after the early planting (Mid); 30 g minisetts planted 21 days after the mid planting (Late). Similarly, 60 g and 90 g minisetts were planted early, mid and late, respectively. The D. rotundata landrace ‘Meccakusa’ was used.
"
	uri <- "doi.org/10.25502/hmrd-ba78/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "Randomised Complete Block Design (RCBD) with three replications with nine treatment combinations",
		data_type = "experiment",
		treatment_vars = "minisett_size;planting_time",
		response_vars = "yield", 
		notes = "see ISSUES",
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-07-24",
		carob_completion = 70,	
		carob_effort = 8
	)
	
	f1 <- ff[basename(ff) == "time-of-planting-trials-yiifswa_abj_data.csv"]
	# f2 <- ff[basename(ff) == "metadata_time_of_planting.csv"]
	
	r1 <- read.csv(f1)
	# r2 <- read.csv(f2)
	
	# removing trailing empty rows
	r1 <- r1[!is.na(r1$Plot), ] 
	
	d <- data.frame(
		plot_id = as.character(r1[["Plot"]]),
		year = ifelse(r1[["Year"]] == 1, 2015, 2016), # see issues
		minisett_size = r1[["SettSize"]], # could not find equivalent in terminag
		planting_time = r1[["TimePlant"]], # could not find equivalent in terminag
		yield = r1[["Yield_t_ha.1"]] * 1000 # converting to kg/ha
	)
	
	d$country <- "Nigeria" 
	d$trial_id <- as.character(as.integer(as.factor( d$year ))) # the only thing seperating the trials is the cropping seasons (2015/2016)
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	d$crop_rotation <- NA
	d$crop <- "yam"
	
	# given in the publication - https://doi.org/10.25502/hmrd-ba78/d
	d$longitude <- 7.34514
	d$latitude <- 9.16469
	
	d$geo_from_source <- TRUE
	
	d$planting_date <- NA
	d$harvest_date  <- NA

### Fertiliser
	d$P_fertilizer <- NA
	d$K_fertilizer <- NA
	d$N_fertilizer <- NA
	d$S_fertilizer <- NA
	d$lime <- NA
	
	d$fertilizer_type <- NA
	
### Yield
	d$yield_part <- "tubers"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	
	d$fwy_storage <- d$yield
	d$dmy_storage <- NA
	d$dmy_total <- NA
	
	carobiner::write_files(path, meta, d)
}

