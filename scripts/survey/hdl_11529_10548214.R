# R script for "carob"


carob_script <- function(path) {

"Crop cut survey in 2015 conducted by EIAR and CIMMYT. Replicated crop cuts of 16m2 in farmers fields along with additional data on nutrient use and variety, and soil sample. There are two linked files for early and mid-season data. (2015)"

	uri <- "hdl:11529/10548214"
	group <- "survey"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "EIAR;CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type="crop-cuts",
		response_vars = "none",
		treatment_vars = "none", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-05-23",
		carob_effort = NA
	)
	
	f1 <- ff[basename(ff) == "ET_BAko agronomy data.xlsx"]
	r1 <- carobiner::read.excel(f1,sheet ="Raw_Data")
	f2 <- ff[basename(ff) == "ET_BAko agronomy data_2015.xls"]
	r2 <-carobiner::read.excel(f2, sheet ="Raw_Data")

	d1 <- data.frame(
		latitude=r1$Latitude,
		longitude=r1$Longitude,
		yield = r1$`Average yield kg/ha or (Q1+Q2)/2`,
		variety_type=r1$`Type of Variety`,
		land_prep_method=tolower(r1$`Land Preparation Method`),
		elevation=r1$Altitude,
		crop_rotation=tolower(r1$`Crop Rotation with`),
		intercrops=r1$`Intercropping with legume`,
		previous_crop=tolower(r1$`Previous/precursor crop`),
		soil_pH=r1$pH,
		soil_Ca=r1$`Ca (mg kg-1)`,
		soil_K=r1$`K (mg kg-1)`,
		soil_Na=r1$`Na (mg kg-1)`,
		soil_Mg=r1$`Mg (mg kg-1)`,
		soil_B=r1$`Boron (mg kg-1)`,
		soil_Mn=r1$`Mn (mg kg-1)`,
		soil_Fe=r1$`Fe (mg kg-1)`,
		soil_Zn=r1$`Zn (mg kg-1)`, 
		soil_Al=r1$`Al (mg kg-1)`,
		soil_C=r1$`Carbon (%)`,
		fertilizer_type=r1$`Type of Inorganic Fertilizer`,
		location = r1$`Name of the Village`,
		planting_date = as.character(as.Date(r1$`Planting Date`)),	
		weed_cover = r1$`Average % weed cover`,
		fertilizer_amount = r1$`Amount of Inorganic Fertilizer (kg)`
	)

	d2 <- data.frame(
		latitude=r2$Latitude,
		longitude=r2$Longitude,
		yield = r2$Average.yield.kg.ha.or..Q1.Q2..2,
		variety_type=r2$Type.of.Variety,
		land_prep_method=tolower(r2$Land.Preparation.Method),
		elevation=r2$Altitude,
		crop_rotation=tolower(r2$Crop.Rotation.with),
		intercrops=r2$Intercropping.with.legume,
		previous_crop=tolower(r2$Previous.precursor.crop),
		soil_pH=r2$pH,
		soil_Ca=r2$Ca..mg.kg.1.,
		soil_K=r2$K..mg.kg.1.,
		soil_Na=r2$Na..mg.kg.1.,
		soil_Mg=r2$Mg..mg.kg.1.,
		soil_B=r2$Boron..mg.kg.1.,
		soil_Mn=r2$Mn..mg.kg.1.,
		soil_Fe=r2$Fe..mg.kg.1.,
		soil_Zn=r2$Zn..mg.kg.1., 
		soil_Al=r2$Al..mg.kg.1.,
		soil_C=r2$Carbon....,
		fertilizer_type=r2$Type.of.Inorganic.Fertilizer,
		location = r2$Name.of.the.Village,
		planting_date = as.character(as.Date( r2$Planting.Date  )),
		weed_cover = r1$`Average % weed cover`,
		fertilizer_amount = r1$`Amount of Inorganic Fertilizer (kg)`
	)
  
	d <- carobiner::bindr(d1, d2)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- as.logical(NA)
	
	d$country <- "Ethiopia" 
	d$adm1 <- "Oromia"
	d$adm2 <- "West Showa"
	d$geo_from_source <- TRUE
	d$crop <- "maize"
	d$yield_part <- "grain"
	#d$trial_id <- "1"
	harvest_date = "2015"
	
	d$fertilizer_type <- gsub("\r\n|\\.| ", "", tolower(trimws(d$fertilizer_type)))
	d$fertilizer_type <- gsub(",|&|-|and", ";", d$fertilizer_type)
	d$fertilizer_type <- gsub("ure$", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("dap", "DAP", d$fertilizer_type)
	d$fertilizer_type <- gsub("nps|nsp|sps", "NPS", d$fertilizer_type)
	
	d$crop_rotation <- gsub(", ", ";", d$crop_rotation)
	d$crop_rotation <- gsub("beans", "common bean", d$crop_rotation)
	d$crop_rotation <- gsub("other", "unknown", d$crop_rotation)

	d$previous_crop <- gsub("beans", "common bean", d$previous_crop)
	d$previous_crop <- gsub("other", "unknown", d$previous_crop)

	d$intercrops <- gsub("Haricot bean", "common bean", d$intercrops)
	
	#survey
	d$trial_id <- as.character(1:nrow(d))
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}

