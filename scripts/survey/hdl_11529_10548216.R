# R script for "carob"

# NOTE 
# Central  Statistical Authority  (CSA)

carob_script <- function(path) {
   
"Crop cut data collected by CSA in 2015 from 382 fields in West Showa, South West Showa, East and West Wollega, Jimma, Iliababur, East Showa, West and East Gojjam. Name of enumerator, name of farmer, gender, field coordinates (longitude, latitude, altitude), type of maize variety, name of variety, fertilizer use, fertilizer types, quantity (organic/inorganic), GPS location of the quadrant, number of crop stand, number of cobs, field weight of sub-samples, grain weight of sub-sample, weight of remaining cobs, field area (2016)"

   uri <- "hdl:11529/10548216"
   group <- "survey"
   
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1, 
      data_organization = "CIMMYT", 
      publication ="doi:10.5897/AJAR2019.14338", 
      project = "TAMASA", 
      data_type="crop-cuts", 
      response_vars = "none",
      treatment_vars = "none", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-07-04",
      carob_effort = NA
   )
   f <- ff[basename(ff) == "ET_Baseline_CSA_2015.xlsx"] 	
   r <- carobiner::read.excel(f, sheet = "Revised_data", na=c("", "."))
   d <- data.frame(
      country = "Ethiopia",
      latitude=r$latitude,
      longitude=r$longitude,
	  geo_from_source = TRUE,
      elevation=r$altitude,
      crop = "maize",
      yield_part = "grain",
      farmer_gender=r$Gender,
      variety_type=r$`Type of variety`,
      variety=r$`Name of variety`,
      yield= r$`Moisture adjusted grain  yield (kg /ha)`,
      plot_area= 16, #16 m2
      OM_used= r$`Fertilizer type/organic`,
      fertilizer_amount = r$`amount of Inorganic fertilizer`,
	  plant_density  = 10000 * r$`Number of crop stands /16m2` / 16,
	  cob_density = 10000 * r$`Number of cobs/16m2` / 16,

      ### Soil data
      soil_SOC = r$`Carbon (%)`,
      soil_pH  =r$pH,
      soil_Al =r$`Al (mg/kg)`,
      soil_Ca =r$`Ca  (mg/kg)`, 
      soil_EC =r$`EC.S (dS/m)`,
      soil_S =r$`S  (mg/kg)`,
      soil_Mn=r$`Mn  (mg/kg)`,
      soil_P_total =r$`P  (mg/kg)`,
      soil_Zn=r$`Zn  (mg/kg)`, 
      soil_K=r$`K  (mg/kg)`, 
      soil_Mg=r$`Mg  (mg/kg)`, 
      soil_Na=r$`Na  (mg/kg)`, 
      soil_Fe=r$`Fe  (mg/kg)`, 
      soil_B=r$`Boron  (mg/kg)`,
      soil_N = (r$`Nitrogen (%)`) * 10000 # from % to [mg/kg]
   )
	#OM_amount= r$`amount of organic fertilizer`,
	#adjust with r$`Unit for Organic fertilizer`
  
   d$variety_type <- gsub("Improved_variety", "improved", d$variety_type)
   d$variety_type <- gsub("local_variety", "local", d$variety_type)
   
   d$planting_date <- as.character(NA)
   d$on_farm <- TRUE
   d$is_survey <- TRUE
   d$irrigated <- as.logical(NA)
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	# in a survey each record is a separate "trial"
   d$trial_id <- as.character(1:nrow(d))
   
   ## taking out some of the crazy ones. Others still very high.
   d$fertilizer_amount[d$fertilizer_amount > 2000] <- NA
   
   carobiner::write_files(path, meta, d)
   
}


