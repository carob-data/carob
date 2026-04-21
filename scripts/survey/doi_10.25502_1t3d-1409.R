# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Quantification of Food Systems Greenhouse Gas Emissions in Kenya: A Bottom-Up Approach.

The CGIAR Initiative on Low-Emission Food Systems, also known as Mitigate+, works closely with key actors in the target countries so that they are equipped with the knowledge, information, and tools they need to make robust evidence-based decisions as they confront challenges in food system discourse, policy development, and implementation to reduce greenhouse gas emissions.
"


	uri <- "doi.org/10.25502/1t3d-1409"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; CIMMYT",
		publication = NA,
		project = NA,
		carob_date = "2026-04-18",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "soil_CO2;soil_N2O;soil_CH4", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "data-dictionary.csv"]
	f2 <- ff[basename(ff) == "quantification-of-food-systems-greenhouse-gas-emissions-in-kenya.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2, na= "")


	d <- data.frame(
		country = r2$Country,
		adm1 = carobiner::fix_name(r2$State, "title"),
		location = carobiner::fix_name(r2$District, "title"),
		longitude = r2$Longitude,
		latitude = r2$Latitude,
		soil_texture = ifelse(grepl("medium", r2$Type), "medium", 
		               ifelse(grepl("clay", r2$Type), "clay", "sand")) ,
		soil_SOC = r2$Organic_C,
		soil_N = r2$N_Content,
		soil_pH = r2$pH,
		soil_bd = r2$Bulk_density,
		crop = tolower(r2$Crop),
		yield = r2$Yield,
		previous_crop_burnt = grepl("burned", r2$Residue),
		harvest_days = r2$Crop_duration,
		land_prep_method = ifelse(grepl("Conventional", r2$Tillage), "conventional", r2$Tillage),
		OM_used = grepl("Incorporated", r2$Manure),
		OM_type =  gsub("NA;|;NA", "", paste(ifelse(grepl("Farmyard manure", r2$Type2), "farmyard manure", NA), tolower(r2$Type3), sep = ";")),
		OM_amount = rowSums(r2[, c("Amount2", "Amount3")], na.rm = TRUE),
		fertilizer_type = gsub("NA;|;NA", "", paste(r2$Type4, r2$Type5, r2$Type6, r2$Type7, r2$Type8, sep = ";")),
		fertilizer_amount = rowSums(r2[, c("Amount4", "Amount5", "Amount6", "Amount7", "Amount8")], na.rm = TRUE),
		#soil_GHGE = r2$Total_GHGE,
		N_fertilizer = r2$Total_N_applied,
		record_id = r2$ID,
		trial_id = paste(r2$State, r2$Climate, sep = "-"),
		soil_CO2 = r2$Total_CO2,
		soil_CH4 = ifelse(is.na(r2$Total_CH4), r2$Total_CH4.1 , r2$Total_CH4),
		soil_N2O = as.numeric(r2$Total_N2O)
		
	)
	
	### Fixing crop names
	
	P <- carobiner::fix_name(d$crop)
	P <- gsub("cow peas, dry", "cowpea", P)
	P <- gsub("other legume", "legume", P)
	P <- gsub("beans, dry", "common bean", P)
	P <- gsub("chick peas", "chickpea", P)
	P <- gsub("other", "unknown", P)
	d$crop <- P
	
	### Fixing fertilizer type 
	
	P <- carobiner::fix_name(d$fertilizer_type)
	P <- gsub("Urea", "urea", P)
	P <- gsub("Diammonium phosphate", "DAP", P)
	P <- gsub("Calcium ammonium nitrate", "CAN", P)
	P <- gsub("Calcium nitrate", "CN", P)
	P <- gsub("Compound NPK", "NPK", P)
	P <- gsub("NA", NA, P)
	d$fertilizer_type <- P
	d$OM_type <- gsub("NA", NA, d$OM_type)
	
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$planting_date <- as.character(NA)
	d$geo_from_source <- TRUE
	d$yield_part <- "none"
	d$irrigated <- NA
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
	
	

	carobiner::write_files(path, meta, d)
}


