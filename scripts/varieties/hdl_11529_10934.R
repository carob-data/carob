# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Grain yield and stability of white early hybrids in the highland valleys of Mexico

^^
"
	uri <- "hdl:11529/10934"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		carob_date = "2026-09-07",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		carob_effort = 2
		
	)
	

	f1 <- ff[basename(ff) == "COMBART2016-37LOC data.csv"]

	r1 <- read.csv(f1)

### process
	
	d <- data.frame(
		plot_id = as.character(r1$PLOT),
		location = c("Batan 1", "Batan 2", "Batan 3", "Batan 4", "Quecholac", "Huejotzingo", "Epitacio Huerta", "Coatlinchan", "San Diego", "Felipe Angeles", "Acatzingo", "Jilotepec", "Ajacuba", "Texcoco", "San Andres", "Teotongo", "San Luis de laPaz", "Jesus Maria", "Santo Domingo", "Jilotepec", "Ixtlahuaca", 
		             "Atlacomulco 1", "Atlacomulco 2", "Calimaya", "Mazapiltepec", "Amealco", "A de Juarez", "Muñoz", "Metepec", "Ixtenco", "Zacapoaxtla", "Temascalcingo", "Comtepec", "Ciudad Hidalgo", "Colon", "Atlangatepec", "Tlachaloya")[r1$ENV],
		rep = r1$REP,
		block_id = as.character(r1$BLOCK),
		variety_pedigree = r1$PEDIGREE1,
		variety = r1$GENO,
		elevation = r1$ALTITUD,
		yield = r1$GY*1000,
		plant_height = r1$PH,
		crop = "maize",
		country = "Mexico",
		trial_id = as.character(r1$ENV),
		planting_date = NA, 
		harvest_date = NA,
		yield_isfresh = NA
	)
	
	### Adding lon and lat coordinate
	
	geo <- data.frame(
	  location = c("Ajacuba", "Metepec", "Calimaya", "Ixtlahuaca", "Jilotepec", "Temascalcingo", "Texcoco", "Epitacio Huerta", "Teotongo", "Acatzingo", "Huejotzingo", "Quecholac", "Zacapoaxtla", "Santo Domingo", "Atlangatepec", "Ixtenco","Batan 1", "Batan 2", "Batan 3", "Batan 4", "A de Juarez", "Amealco", "Atlacomulco 1", "Atlacomulco 2", "Ciudad Hidalgo", "Coatlinchan", "Colon", "Comtepec", "Felipe Angeles", "Jesus Maria", "Mazapiltepec", "Muñoz", "San Andres", "San Diego", "San Luis de laPaz", "Tlachaloya"),
	  longitude = c(-99.0780, -98.3429, -99.6433, -99.8070, -99.5966, -100.0370, -98.8278, -100.2893, -97.5385, -97.7650 , -98.4488, -97.6345, -97.5813, -101.6811, -98.1829 ,-97.9114, rep(-103.3469, 4), -98.0914, -99.5494, -99.877, -99.877, -100.5583, -98.8725, -99.169,  -100.1632, -99.111, -100.2574, -97.699, -98.209, -98.4668, -98.4841, -100.5247, -99.665),
	  latitude = c(20.1198, 20.2578, 19.1655, 19.5899, 20.0191, 19.9213, 19.4716, 20.1473, 17.7486, 19.0261, 19.1671, 18.9482, 19.8544, 23.3496, 19.5451, 19.2576, rep(20.7232, 4), 19.0531, 20.239, 19.797, 19.797, 19.693, 19.449, 19.379, 19.952, 19.4529, 20.623, 19.121, 19.4749, 19.1059, 19.1305, 21.2936, 19.4755),
	  geo_uncertainty = c(13721, 11636, 12203, 17546 , 23883, 19074, 21307, 19145, 5586, 11161, 20147, 12256, 15076, 63582, 9003, 7892, rep(NA, 20)),
	  geo_source = c(rep("GADM 4.1, adm2", 16), rep("Google Maps", 20)),
	  geo_from_source = FALSE
	)
	
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_moisture <- NA
	d$yield_part <- "grain"
	d$irrigated <- NA 
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	
	carobiner::write_files(path, meta, d)
}


