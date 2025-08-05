# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Raw data has no specific geo locations
# Raw data has no information on fertilizer types and rates for N, P and K

carob_script <- function(path) {

"This files ontains yiedl and soils data coillected by CSA (Central Statistical Agency) from four zones in Oromia (East Showa, Wollega, West and South West Showa and Jimma) and one Zone in Amhara (West Gojjam). A total of 38 primary sampling units were covered and making a total of 383 sampling points. Unreplicated crop cuts were made on farmers maize fields and yield measured. Soil samples were also collected at 0-20 and 20-50cm but are not all analysed yet."



	uri <- "hdl:11529/11014"
	group <- "agronomy"


	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "fertilizer_used;fertilizer_amount;OM_used;OM_amount",
		response_vars = "yield", 
		completion = 0,
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
	  dm_yield = r$`Moisture adjusted grain  yield (kg /ha)`,
	  yield_part = "grain",
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

## Raw data states that the trial coverage included Oromia and Amhara regions 
	#Jimma, Wollega, West Showa, East Showa, Gojjam  Districts 
	#but there was no specification as to which location(s) was the data collected from

	#adm1 = carobiner::fix_name(r$Region, "title"), #Oromia & Amhara
	#adm2 = carobiner::fix_name(r$District, "title"), #East & West Showa, Jimma, Wollega, Illubabor under Oromia region
                                                    #East & West Gojjam under Amhara region
	
	  #d$longitude[d$adm2=="East Showa"] <- 39.1667
	  #d$longitude[d$adm2=="West Showa"] <- 38.5
	  #d$longitude[d$adm2=="Jimma"] <- 36.8344
	  #d$longitude[d$adm2=="Wollega"] <- 36.5506
	  #d$longitude[d$adm2=="Illubabor"] <- 35.7559
	  #d$longitude[d$adm2=="East Gojjam"] <- 37.81
	  #d$longitude[d$adm2=="West Gojjam"] <- 37.3
	  #d$latitude[d$adm2=="East Showa"] <- 8.9167
	  #d$latitude[d$adm2=="West Showa"] <- 9.0500
	  #d$latitude[d$adm2=="Jimma"] <- 7.6736
	  #d$latitude[d$adm2=="Wollega"] <- 9.0820
	  #d$latitude[d$adm2=="Illubabor"] <- 8.2752
	  #d$latitude[d$adm2=="East Gojjam"] <- 10.33
	  #d$latitude[d$adm2=="West Gojjam"] <- 10
	  
	d$trial_id <- as.character(as.integer(as.factor(1)))
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$borer_trial <- FALSE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	d$geo_from_source <- FALSE

	d$planting_date <- as.character(2015)
	d$harvest_date  <- as.character(2016)
	
	d$variety <- gsub("Limu\r\n","Limu",d$variety)
	d$variety <- gsub("Kenya \r\n","kenya",d$variety)
	d$variety <- gsub("BH- 660\r\n\r\n|Bh-660","BH-660",d$variety)
	d$variety <- gsub("BH-661\r\n","BH 661",d$variety)

  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- as.numeric(NA)
  d$fertilizer_type <- as.character(NA)
  d$fertilizer_amount <- as.numeric(d$fertilizer_amount)
  d$fertilizer_amount[d$fertilizer_amount > 1000] <- NA
  d$soil_Al[d$soil_Al > 200] <- NA
  d$soil_Na[d$soil_Na > 200] <- NA
   
	carobiner::write_files(path, meta, d)
}

