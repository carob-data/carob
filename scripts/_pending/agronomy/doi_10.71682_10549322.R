# R script for "carob"
# license: GPL (>=3)

## ISSUES
#  Missing fertilizer application details 


carob_script <- function(path) {

"
Grain yield, biomass, and nutritional values of various cropping systems tested on-station with/without fertilizer application on two soils in Zimbabwe between 2020 and 2023

This database contains data on grain, biomass, protein, and calorie yields from on-station trials in Zimbabwe, conducted at the Domboshava Training Centre (DTC) (17.62°S, 31.17°E) and the University of Zimbabwe farm (UZ) (17.73°S, 31.020°E), characterized by different soil types.

The trials tested maize monocropping, maize-legume rotations, and intercropping with various layouts (traditional intercropping and double-row strip cropping), with and without fertilizer application. Two contrasting legumes, cowpea and pigeon pea, were included in the cropping systems. The experiment was established during the 2019/2020 growing season, but data were collected from the 2020/21 to 2022/23 seasons to allow the rotations to develop. It was carried out under Conservation Agriculture and rainfed conditions.

 The dataset is divided into three parts: 1) Field measurement, including treatments, grain yields, and crop biomass; 2) Calculations at the cropping system level (total biomass, calories, and proteins); 3) Processed data used to evaluate the stability of each cropping system, as shown in a radar plot presented in the related paper.
"


	uri <- "doi:10.71682/10549322"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication =NA,
		project = NA,
		carob_date = "2025-08-11",
		design = NA,
		data_type = "experiment",
		treatment_vars = "fertilizer_used;crop_rotation;intercrops",
		response_vars = "yield;dmy_total", 
		carob_contributor = "Cedric Ngakou",
   	completion = 50,	
		notes = "The data on radar_plot_data and system_data sheets are not suitable for carob"
	)
	

	f <- ff[basename(ff) == "Masamba et al_contrasting legumes and layouts_data.xlsx"]

	
	r <- carobiner::read.excel(f, sheet="yield_data")
	#r1 <- carobiner::read.excel(f1, sheet="conversion_factors")
	#r2 <- carobiner::read.excel(f1, sheet="system_data")
	#r3  <- carobiner::read.excel(f1, sheet="radar_plot_data")

	d <- data.frame(
	   planting_date = as.character(r$year),
		location = ifelse(grepl("DTC", r$site), "Domboshava Training Centre", "University of Zimbabwe"),
		latitude= ifelse(grepl("DTC", r$site), -18.03333, -18.21667) , 
		longitude= ifelse(grepl("DTC", r$site), 31.28333, 31.020) ,
		country= "Zimbabwe",
		plot_id = r$plot,
		rep= as.integer(gsub("DTC.|UZ.", "", r$rep)),
		crop_rotation= ifelse(grepl("MZ-CP", r$cropping.system), "maize;cowpea",
		                ifelse(grepl("MZ-PP", r$cropping.system), "maize;pigeon pea", "none")),
		intercrops= ifelse(grepl("MZPP1|MZPP2", r$cropping.system), "pigeon pea",
		           ifelse(grepl("MZCP2", r$cropping.system), "cowpea", "none")),
		intercrop_type= ifelse(grepl("MZPP1", r$cropping.system), "mixed",
		                        ifelse(grepl("MZCP2|MZPP2", r$cropping.system), "strip", "none")),
		crop = tolower(r$crop),
		treatment= r$fertilizer,
		fertilizer_used= ifelse(grepl("Fert-", r$fertilizer), FALSE, TRUE),
		dmy_total= r$biomass.kg.ha,
		yield= r$grain.kg.ha,
		yield_moisture= 12.5,
		geo_from_source=TRUE,
		on_farm= FALSE,
		is_survey= FALSE,
		yield_part="grain",
		irrigated= NA,
		trial_id= ifelse(grepl("DTC", r$site), "1", "2")
		
	)
	
	d$crop <- gsub("pigeonpea", "pigeon pea", d$crop) 
	d$intercrops <- ifelse(grepl("cowpea|pigeon pea", d$crop) & grepl("cowpea|pigeon pea", d$intercrops), "maize", d$intercrops)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) # this need to be fixed 
	
	carobiner::write_files(path, meta, d)
}


