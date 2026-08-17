# R script for "carob"
# license: GPL (>=3)

## ISSUES
### fertilizer amount and OM_amount are given in kg/station
# station size is not defined 

# abstract mentions cowpea, but no cowpea in data

# This is an intercropping experiment, but only "grain_yield" is given, presumably the yield for the "main_crop" which is always a cereal (maize, sorghum, or millet).  So we do not have the yield of the other (legume) intercrop?!


carob_script <- function(path) {

"
Cereal legume intercropping studies in Malawi

Innovations in Technology, Institutional and Extension Approaches towards Sustainable Agriculture and enhanced Food and Nutritional Security in Africa (InnovAfrica), a European Commission funded project validated and upscaled cereal legume intercropping technology in Dedza and Mzimba districts of Malawi involving farmers in between year 2017 and 2020. The results from the intercropping experiment of three cereals (maize, sorghum, and finger millet) and various legume species (pigeonpea, soyabeans, Bambara nut, groundnut, cowpea, and common beans) are presented in this data set.
"

	uri <- "doi:10.5061/dryad.hmgqnk9kr"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "CIMMYT; UNIMA; ILRI", 
		publication = NA,
		project = "InnovAfrica",
		design = NA,
		data_type = "experiment",
		treatment_vars = "intercrops",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-11",
		carob_completion = 100,	
		carob_effort = 2
	)

	f1 <- ff[basename(ff) == "Intercrop.Malawi.xlsx"]
	#f2 <- ff[basename(ff) == "README1.txt"]

	r1 <- carobiner::read.excel(f1, na = "NA")
	#r2 <- read.???(f2)

	d <- data.frame(
		adm2 = carobiner::fix_name(r1$district, "title"),
		season = r1$season,
		trial_id = r1$trial_name,
		hhid = as.character(r1$`Farmer ID`),
		soil_color = r1$soil_colour,
		soil_type = r1$soil_type,
		sex = r1$sex,
		age = r1$age,
		hh_size = r1$`No. of person per household`,
		labour = r1$persons_working,
		#r1$`experience_crop_farming (yrs)`,
		CA_years = r1$`experience_conservation (yrs)`,
		rain = r1$`rainfall (mm)`,
		#crop_type = r1$crop_type,
		crop = tolower(r1$crops),
		#intercrops = tolower(r1$cropping_system_type),
		#r1$cropping_systems,
		#r1$technology,  CA = cons ag, CONV = conventional
		variety = ifelse(is.na(r1$cereal_varieties) & !is.na(r1$legume_variety), r1$legume_variety, r1$cereal_varieties) ,
		row_spacing = r1$`row_spacing (cm)`,
		plant_spacing = r1$`with in row spacing (cm)`,
		land_prep_method = gsub("Manual hoe tillage", "hoeing", r1$tillage_method),
		land_prep_implement = "manual" ,
		residue_prevcrop_used = r1$residues_applied =="Yes",
		residue_type = tolower(r1$residue_type),
		planting_date = as.character(r1$seeding_date),
		harvest_date = as.character(r1$harvest_date),
		OM_type = tolower(r1$`basal fertilizer_type`),
		fertilizer_type = tolower(r1$`top_dressing fertilizer_type`),
		yield = r1$`grain_yield (kg/ha)`,
		fwy_total = r1$`total_biomass (kg/ha)`
		## OM_amount = as.numeric(gsub("500g/station", 500,  r1$`basal fertilizer (bokash)_rate`))/1000, # kg/station
		##N_fertilizer = as.numeric(gsub("3g/station", 3, r1$`topdress Urea application_rate`))*0.46/1000, # kg/station
	)
	d <- d[(!is.na(d$crop)) & (!is.na(d$yield)), ]
	
	crop <- gsub(" ", "", d$crop)
	crop <- gsub("g_nuts|gnuts", "groundnut", gsub("p_peas|pigeonpea", "pigeon pea", d$crop))
	crop <- gsub("miilet", "millet", crop)
	crop <- gsub("bambara", "bambara groundnut", crop)
	crop <- gsub("beans", "common bean", crop)
	crop <- gsub("soya", "soybean", crop)

	crop <- strsplit(crop, "_")
	crop <- data.frame(do.call(rbind, lapply(crop, \(x) x[1:2])))

	d$crop <- crop$X1
	d$intercropped <- !is.na(crop$X2)
	d$intercrops <- crop$X2
	d$intercrops[is.na(d$intercrops)] <- "none"
    d$OM_type[d$OM_type == "bokash manure"] <- "compost" # presumably bokashi comost
	d$residue_type[d$residue_type == "no"] <- "none"
  
	###### adding geo coordinate
	geo <- data.frame(
	  adm2 = c("Mzimba","Dedza" ),
	  longitude = c(33.5899, 34.3234),
	  latitude = c(-11.8988, -14.3820	),
	  geo_uncertainty = c(2347, 4005),
	  geo_source = c("GADM 4.1, adm2"),
	  geo_from_source = FALSE
	) 
	
	d <- merge(d, geo, by = "adm2", all.x = TRUE)
	d$harvest_date <- ifelse( grepl("2018-19", d$season) & grepl("2018-12", d$planting_date), gsub("2018", "2019", d$harvest_date), d$harvest_date)
	d$planting_date <- ifelse(is.na(d$planting_date), substr(d$season, 1, 4), d$planting_date)
	d$season <- NULL
	d$fwy_total[which(d$fwy_total< 0)] <- NA
	
	d$is_survey <- FALSE 
	d$on_farm <- TRUE
	d$yield_moisture <- NA
	d$yield_part <- "grain"
	d$country <- "Malawi" 
	d$irrigated <- NA 
	d$yield_isfresh <- TRUE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}


