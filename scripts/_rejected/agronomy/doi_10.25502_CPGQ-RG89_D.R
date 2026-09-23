# R script for "carob"
# license: GPL (>=3)

## ISSUES

### Rejected - missing plot size

# we got this from the authors Ivy (ivyeva6@gmail.com) and Newton(newtonnelsi@gmail.com) :

## Regarding the study, we did not measure the plot sizes because the assessment was conducted under farmers’ practices, based on the space available on their farms, rather than under controlled experimental conditions.
#In most cases, farmers space spinach at approximately 30 cm × 30 cm. However, the recommended spacing is ideally 30 cm × 45 cm, which is generally applied in experimental plots. Since our study was conducted under farmers’ conditions, the spacing was not standardized, and the plot sizes varied depending on the available space.


carob_script <- function(path) {

"
RCI veg seedling trial data.Nairobi, Kenya season 1 2023

WP1 of RCI to improve UPU crop productivity; assessment of tray vegetable seedling benefits
"

	uri <- "doi:10.25502/CPGQ-RG89/D"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seedling_type",
		response_vars = "yield;germination_rate", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-19",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	
	f1 <- ff[basename(ff) == "transplanting-data.csv"]
	f2 <- ff[basename(ff) == "germinability-yield-data.csv"]
	f3 <- ff[basename(ff) == "dicitionary.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

	### process
	
	d1 <- data.frame(
		crop = tolower(r1$Crop),
		seedling_type = ifelse(grepl("HS", r1$Seedling_Type), "healthy seedling", "ground seedling") ,
		seed_density = r1$Total_Transplanted,
		plant_density = r1$Total_Survived 
	)

	d2 <- data.frame(
		crop = tolower(r2$CROP),
		seedling_type = ifelse(grepl("HS", r2$PLOT), "healthy seedling", "ground seedling"),
		germination_rate = r2$GERMINABILITY_PERC,
		yield = r2$YIELD_KG
	)
	
	d <- merge(d1, d2, by= c("crop", "seedling_Type"), all = TRUE)

	### Fixing crop 
	P <- carobiner::fix_name(d$crop) 
	P <- gsub("brocolli", "broccoli", P)
	P <- gsub("capsicum", "bell pepper", P)
	P <- gsub("kales", "kale", P)
	P <- gsub("leeks", "leek", P)
	P <- gsub("okra \\(direct planting\\)", "okra", P)
	d$crop <- P
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$trial_id <- ifelse(d$seedling_Type== "healthy seedling", "1", "2")
	d$yield_moisture <- NA
	d$yield_part <- "none"
	d$country <- "Kenya"
	d$location <- "Nairobi"
	d$geo_from_source <- FALSE
	d$latitude <- -1.2905	
	d$longitude <- 36.8667	
	d$geo_source <- "GADM 4.1, adm1"
	d$geo_uncertainty <- 26553  
	d$irrigated <- NA
	d$planting_date <- "2023"
	d$harvest_date <- NA_character_
	d$yield_isfresh <- NA
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	### drop duplicate rows
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}


