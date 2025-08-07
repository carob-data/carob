# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
TAMASA Ethiopia. Yield, soil and agronomy data from farmers’ maize fields collected by CSA,  2015 season

This files ontains yiedl and soils data coillected by CSA (Central Statistical Agency)  from four  zones in Oromia (East Showa, Wollega, West and South West Showa and Jimma) and one Zone in Amhara (West Gojjam). A total of 38 primary sampling units were covered and making a total of 383 sampling points. Unreplicated crop cuts were made on farmers maize fields and yield measured. Soil samples were also collected at 0-20 and 20-50cm but are not all analysed yet.
"


	uri <- "hdl:11529/11014"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "survey",
		treatment_vars = NA
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2025-08-05",
		notes = NA,
		design = NA
	)
	
	f <- ff[basename(ff) == "TAMASA_ET_CC_2015_CSAF.xlsx"]
	r <- carobiner::read.excel(f,sheet = "Revised_data")

	d <- data.frame(
	  country = "Ethiopia",
	  crop= "maize",
	  variety_type = r$`Type of variety`,
	  variety = r$`Name of variety`,
	  OM_used = r$`Fertilizer type/organic`,
	  OM_amount = r$`amount of organic fertilizer`,
	  fertilizer_used = r$`Fertilizer type/inorganic`,
	  fertilizer_amount = r$`amount of Inorganic fertilizer`,
	  plant_density = r$`Number of crop stands /16m2`*625,
	  cob_density = r$`Number of cobs/16m2`*625,
	  fw_yield = r$`Field weight of remaining grain (kg/16m2)`*625,
	  yield = r$`Moisture adjusted grain  yield (kg /ha)`,
	  yield_moisture = 14,
	  yield_part = "grain",
      crop_cut = TRUE,
	  yield = r$`Moisture adjusted grain  yield (kg /ha)`,
	  plot_area = r$`Field area (ha)`,
	  soil_SOC = r$`Carbon (%)`,
	  soil_pH = r$pH,
	  soil_Al = r$`Al (mg/kg)`,
	  soil_Ca = r$`Ca  (mg/kg)`,
	  soil_S = r$`S  (mg/kg)`,
	  soil_Mn = r$`Mn  (mg/kg)`,
	  soil_P_total = r$`P  (mg/kg)`,
	  soil_Zn = r$`Zn  (mg/kg)`,
	  soil_K = r$`K  (mg/kg)`,
	  soil_Na = r$`Na  (mg/kg)`,
	  soil_Fe = r$`Fe  (mg/kg)`,
	  soil_B = r$`Boron  (mg/kg)`,
	  soil_N = r$`Nitrogen (%)`*10000,
	  depth_top = 0,
	  depth_bottom = 20
	)

	  
	d$trial_id <- as.character(as.integer(as.factor(1)))
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$borer_trial <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	d$geo_from_source <- FALSE

	d$planting_date <- "2015"
	d$harvest_date  <- "2016"
	
	d$variety <- gsub("\r\n", "", d$variety)
	d$variety <- gsub("BH- 660|Bh-660","BH-660",d$variety)
	d$variety <- trimws(d$variety)

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- as.numeric(NA)
	d$fertilizer_type <- as.character(NA)
	d$fertilizer_amount[d$fertilizer_amount == "."] <- NA 
	d$fertilizer_amount <- as.numeric(d$fertilizer_amount)
	d$fertilizer_amount[d$fertilizer_amount > 1000] <- NA
  
   
	carobiner::write_files(path, meta, d)
}

