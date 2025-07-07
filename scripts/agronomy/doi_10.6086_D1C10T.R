# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Data from: Water-conscious management strategies reduce per-yield irrigation and soil emissions of CO2, N2O, and NO in high-temperature forage cropping systems.

Agriculture produces large emissions of carbon dioxide (CO2), nitrous oxide (N2O), and nitric oxide (NO), especially in high-temperature agroecosystems, where management approaches for reducing these emissions are needed. A promising management solution to increase water infiltration and reduce trace gas emissions is subsurface drip irrigation, a method which increases rhizosphere access to water and nitrogenous fertilizers. In a multi-year field study, we compared per-yield irrigation and soil emissions for flood- and drip-irrigated field plots in southern California during two seasons and between two forage crops differing in fertilizer requirements: alfalfa (Medicago sativa L.) and sudangrass (Sorghum bicolor ssp. Sudanese). We monitored soil climate and emission responses to irrigation using a custom array of automated chambers connected to trace gas analyzers that measured gas fluxes continuously every 30 minutes. We found that, compared to flood-irrigated fields, drip irrigation in alfalfa increased yield by 7%, decreased irrigation demand by 11%, and decreased CO2 emissions by 59%, N2O by 14%, and NO by 27%. Drip irrigation in sudangrass increased yield by 6%, decreased irrigation by 68%, increased CO2 emissions by 3%, and decreased both N2O and NO emissions by 62%. In both crops, differences between irrigation types were strongest in the summer when flooded soil produced the largest pulses of N2O and NO relative to small drip-irrigated pulses. As agriculture continues to intensify in warmer climates, implementation of subsurface drip irrigation can help reduce agroecosystem contributions to climate change and air pollution while increasing crop yields.
"

	uri <- "doi:10.6086/D1C10T"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=NA,
		data_organization = "UCR; CSUEB; UIA", ##University of California, Riverside; California State University, East Bay; University of Iowa
		publication = "doi:10.1016/j.agee.2022.107944",
		project = NA,
		carob_date = "2025-07-06",
		design =NA, 
		data_type = "experiment",
		treatment_vars = "irrigation_method; N_fertilizer",
		response_vars = "fw_yield; fwy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)


	f1 <- ff[basename(ff) == "drecHarvest.csv"]
	f2 <- ff[basename(ff) == "drecIrrigationFertilizationSchedule.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	
	d1 <- data.frame(
		crop = tolower(r1$Crop),
		season= ifelse(grepl("Early", r1$Season), "early wet", "late wet"),
		grow= r1$GrowPeriod,
		planting_date = as.character(r1$Year),
		harvest_date = r1$HarvestDate,
		irrigation_method= tolower(r1$Irrigation),
		fw_yield= r1$TonPerAcre*2.471*1000,
		fwy_total= (r1$TotalWeightTons/r1$FieldAcres)*2.471*1000 ,
		plot_area= r1$FieldAcres*2.471 
	)

	d2 <- data.frame(
		crop = tolower(r2$Crop),
		planting_date = as.character(r2$Year),
		season= ifelse(grepl("Early", r2$Season), "early wet", "late wet"),
		grow= r2$GrowPeriod,
		plot_area= r2$FieldAcres*2.471,
		irrigation_method= tolower(r2$Irrigation),
		irrigation_dates= r2$IrrigationTimeStart,
		fertilizer_amount= r2$FertilizerApplied,
		fertilizer_type= ifelse(grepl("NH3", r2$FertilizerType), "AN", 
		                 ifelse(grepl("UN32", r2$FertilizerType), "urea;AN", "none")),
		N_fertilizer= ifelse(grepl("NH3", r2$FertilizerType), r2$FertilizerApplied*0.34 + 100, 
		              ifelse(grepl("UN32", r2$FertilizerType), r2$FertilizerApplied*0.32+ 100, r2$FertilizerApplied)),
		P_fertilizer= 0, 
		K_fertilizer= 0,
		country= "United States",
		location= "Holtville",
		adm1 = "California",
		adm2 = "Imperial",
		longitude= -119.3333 ,
		latitude= 40.01183, 
		geo_from_source= TRUE,
		irrigated= TRUE,
		on_farm= TRUE,
		is_survey= FALSE,
		trial_id= ifelse(grepl("Sudangrass", r2$Crop), "1", "2") , 
		yield_part= "grain"		
	)
	
	d <- merge(d2, d1, by=c("crop", "planting_date","plot_area", "irrigation_method", "season", "grow"), all.x = TRUE)
	d$grow <- NULL
	d$crop <- gsub("alfalfa", "lucerne",  d$crop) 
	d$irrigation_dates <- as.character(format(as.Date(d$irrigation_dates, format = "%m/%d/%Y %H:%M"), "%Y-%m-%d"))
	d$harvest_date <- as.character(format(as.Date(d$harvest_date, format = "%m/%d/%Y"), "%Y-%m-%d"))
	
	### Adding CO2 and N20 emission from paper
	
	ems <- data.frame(
	   crop= c(rep("sudangrass", 4), rep("lucerne", 4)),
	   season= c(rep("early wet", 2), rep("late wet", 2), rep("early wet", 2), rep("late wet", 2)),
	   irrigation_method= c(rep(c("flood", "drip"), time= 4)),
	   soil_CO2= c(9.24, 11.09, 14.63, 13.05, 4.22, 3.63, 10.37, 3.66),
	   soil_N2O= c(5.07, 3.44, 5.73, 1.48, 3.44, 3.94, 3.21, 0.92),
	   soil_NO= c(1.31, 1.36, 1.67, 0.41, 0.19, 0.21, 0.5, 0.43)
	)
	
	d <- merge(d, ems, by=c("crop", "season", "irrigation_method"), all.x = TRUE)
	
	d$crop <- gsub("sudangrass", "sudan grass", d$crop)
	
	carobiner::write_files(path, meta, d)
}


