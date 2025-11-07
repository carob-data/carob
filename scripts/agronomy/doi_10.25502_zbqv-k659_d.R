# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Cassava best planting practices set 5

The African Cassava Agronomy Initiative (ACAI) aims at improving cassava root yield and quality, and cassava supply to the processing sector. The project has 6 use cases of which best planting practices (BPP) is one. BPP is focusing on guiding farmers in choosing best-suited planting practices for cassava, with a focus on tillage operations and in close relation with improved weed control recommendations.
"

	uri <- "doi:10.25502/zbqv-k659/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "ACAI",
		carob_date = "2025-11-06",
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method",
		response_vars = "yield;yield_marketable", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "bbp5_harvestdata.csv"]
	#f1 <- ff[basename(ff) == "treatment.csv"]
	#f2 <- ff[basename(ff) == "data_dictionary.csv"]

	r <- read.csv(f)
	#r1 <- read.csv(f2)
	#r2 <- read.csv(f3)

### process
	
	d <- data.frame(
	   record_id = r$ID,
		country = r$Country,
		location = r$Site,
		plot_id = as.character(r$Plot_no),
		rep = r$Rep,
		treatment = r$Plough,
		land_prep_method = ifelse(grepl("No/Ridge", r$Sec_tillage), "ridge tillage", 
		                   ifelse(grepl("Harrow/No", r$Sec_tillage), "harrow", 
		                   ifelse(grepl("Harrow/Ridge", r$Sec_tillage), "harrow;ridge tillage", 
		                   ifelse(grepl("Harrow/Harrow", r$Sec_tillage), "harrow", "none")))),
		plant_density = r$plants_m2*10000,# plant/ha
		fwy_leaves = r$Leaf_mass_fresh_m2*10000,
		fwy_stems = r$Stem_mass_fresh_m2*10000,
		yield_marketable = r$mass_roots_fresh_m2*10000,
		yield =  rowSums(r[, c("mass_roots_fresh_m2", "mass_bad_roots_fresh_m2")])*10000, 
		latitude = r$Lat,
		longitude = r$Long,
		elevation = r$Altitude,
		trial_id = paste(r$Site, r$ID, sep = "-"), 
		planting_date = "2016", 
		on_farm = TRUE, 
		is_survey = FALSE, 
		crop = "cassava", 
		yield_part = "roots", 
		yield_moisture = as.numeric(NA),
		irrigated = NA, 
		geo_from_source = TRUE
	)

	d$treatment <- ifelse(grepl("DP", trimws(d$treatment)), "double plough", 
	                ifelse(grepl("SP", trimws(d$treatment)), "Single plough", "Zero plough"))
	d$land_prep_method <- ifelse(grepl("Zero plough", d$treatment), d$land_prep_method, paste("ploughing", d$land_prep_method, sep = ";")) 

### Fixing long and lat 
	
 P <- carobiner::fix_name(d$latitude)	
 P <- gsub("N 7o 34' 30''", 7.575, P)
 P <- gsub("N 7o 34' 22''", 7.572, P)
 P <- gsub("N 8o 37' 4''", 8.617, P)
 P <- gsub("N 8o 35' 44''", 8.585, P)
 d$latitude <- as.numeric(P)

 P <- carobiner::fix_name(d$longitude)	
 P <- gsub("E 2o 51' 27''", 2.85 , P)
 P <- gsub("E 2o 51' 48''", 2.863, P)
 P <- gsub("E 3o 33' 10''", 3.552, P)
 P <- gsub("E 3o 36' 24''", 3.606, P)
 d$longitude <- as.numeric(P)		

 d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
 
 
carobiner::write_files(path, meta, d)

}


