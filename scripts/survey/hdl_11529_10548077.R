# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Identifying high-yield low-emission pathways for the cereal production in South Asia

Household survey was conducted by International Maize and Wheat Improvement Centre (CIMMYT) as part of CGIAR research program on Climate Change Agriculture and Food Security (CCAFS) in Karnal district of Haryana state and Vaishali district of Bihar state in India. The overall aim is to identify high-yield low-emission development pathways in cereal production systems. To achieve this, specific objectives are as follows: (i) to identify various technologies and farm management practices that influence GHG emissions and (ii) to explore household socio-economic factors that determine the adoption of low-emission technologies and management practices at the farm level. The information collected through questionnaire was crop production, socio-economic and demographic conditions, climate risks in agriculture and adaptation and mitigation measures. The data presented was tillage and crop establishment methods, water management, crop type, N rate (kg/ha), yield (Mg/ha), CO2 emission (kgCO2e/ha), N2O (kgCO2e/ha), CH4 (kgCO2e/ha) and total GWP (kgCO2/ha).
"

	uri <- "hdl:11529/10548077"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT; CCAFS", ## CCAFS: Climate Change Agriculture and Food Security
		publication = "doi:10.1007/s11027-017-9752",
		project = NA,
		carob_date = "2026-05-25",
		design = NA,
		data_type = "survey",
		treatment_vars = "land_prep_method;N_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "GHG_emission - Dataset-1.xlsx"]
	f2 <- ff[basename(ff) == "GHG_emission - Metadata.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## process
	
	d <- data.frame(
		country = r1$Country,
		adm1 = carobiner::fix_name(r1$State, "title"),
		adm2 = carobiner::fix_name(r1$District, "title"),
		location = r1$Village,
		latitude = r1$Lat,
		longitude = r1$Lon,
		water_regime = gsub("rainfed, wet season", "rainfed", tolower(r1$Water_Regime)),
		crop = gsub("rice_paddy", "rice", tolower(r1$crop)),
		land_prep_method = r1$tillage_level,
		soil_N2O = r1$`Total_N20 emission (kgCO2)`,
		soil_CO2 = r1$`Total_CO2 emission (kgCO2)`,
		soil_CH4 = r1$`Total_CH4 emission (kgCO2)`,
		soil_GHG = r1$`Total_GHG emission (kgCO2)`,
		N_fertilizer = r1$`total_N(inorg)`,
		N_organic = r1$`total_n(org)`,
		yield = r1$yield,
		fertilizer_type = "DAP;urea",
		OM_used = TRUE,
		OM_type = "farmyard manure",
		trial_id = paste(r1$Water_Regime, r1$ID, sep = "-"), 
		planting_date = as.character(NA), 
		soil_texture = "medium",
		soil_pH = ifelse(grepl("Karnal", d$adm2), 7.3, 7),
		soil_SOC = ifelse(grepl("Karnal", d$adm2), 0.565, 0.7),
		soil_bd =  1.5,
		soil_CEC = ifelse(grepl("Karnal", d$adm2), 21.4945, 20.122),
		on_farm = FALSE, 
		is_survey = TRUE, 
		yield_part = "grain", 
		yield_moisture = as.numeric(NA),
		irrigated = NA, 
		yield_isfresh = TRUE,
		geo_from_source = TRUE
	)

	### Fixing land preparation method
	
	d$land_prep_method <- gsub("Conventional tillage", "conventional", d$land_prep_method)
	d$land_prep_method <- gsub("No tillage", "none", d$land_prep_method)
	
	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	### Fixing Lon and lat error
	
	geo <- data.frame(
	  location = c("Bilandpur", "Bhatha Dasi", "Raja Pakar", "mirpur", "Panapur Camp", "Sounkra",  "Barthal", "Garhi Jattan", "Churni Jagir", "Nanhara", "Bir Narayana", "Mukundpore", "Mohri Jagir", "Sanwat", "Sandhir"),
	  long = c(85.345,  85.363, 85.354, 85.328, 85.0675, 76.873, 76.8705, 77.033, 77.0246, 77.155, 76.8759,  77.181, 76.753, 76.8004, 76.974),
	  lat = c(25.753, 25.753, 25.74, 25.7833, 25.665, 29.796, 29.869, 29.920, 29.806, 30.4404, 29.7598, 28.7408, 30.266, 29.843, 29.843),
	  geo_from = FALSE
	)
	
	d <- merge(d, geo , by= "location", all.x = TRUE)
	d$longitude[!is.na(d$long)] <- d$long[!is.na(d$long)]
	d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
	d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
	d$lat <- d$long <- d$geo_from <- NULL
	
	
	carobiner::write_files(path, meta, d)
}


