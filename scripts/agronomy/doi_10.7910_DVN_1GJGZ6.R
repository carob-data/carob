# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Assessing Benefits of Innovative Approaches Addressing the Pigeon Pea and Common Bean Productivity Within Maize-Based Cropping System and Variable Weather

The dataset contains seven datasheets with different types of data  i.e., cropping system physiological characteristics, radiation intercepting, gross margins, soil nutrient characterization, seasonal physical conditions of the soil, soil water infiltration and seasonal weather conditions. The data will be helpful in assessing how the different plant spacing patterns affects plant physiological and soil physical chemical characteristics under maize legume intercropping systems.

The cropping system physiological characteristic datasheet contains data on maize leaf chlorophyll measurements using SPAD and soil moisture and temperature reading using time domain reflectometry. Radiation interception dataset contains data on photosythentically active radiation (PAR) fraction intercepted by plant canopy under different plant spacing arrangements. Besides is data on LAI under varying plant arrangements. Both reading were measured using AccuPAR LP-80 ceptometer during periods of limited cloud cover. Gross margin datasheet contains data on yields of the different plant (maize, beans and pigeon pea) components (grain, stover, maize toppings, haulms, stalks and husks). The different components were converted into gross revenue using the farm gate prices of each parameter. Besides, BNF data is also included to indicate the amount of N fixed from two experimental fields located in low altitude-low rainfall and medium altitude- low rainfall eco-zones. Soil nutrient characterization datasheet contains data on soil nutrient content at 0-20 cm depth which was sampled at block level during planting. Seasonal physical conditions of soil datasheet has data on daily soil physical conditions i.e., soil moisture, temperatures and bulk electrical conductivity during the cropping season. This data was measured at a meteorological station located in the low altitude-low rainfall eco-zone of Babati. Soil water infiltration data indicates the rate of water infiltrating under two suctions i.e., -2 cm sec2 and -6 cm sec2 using mini-disc infiltrometer at field level. Seasonal weather condition indicates daily weather data collected in three agro-ecological conditions of Babati during the 2019 long rain season.
"

   uri <- "doi:10.7910/DVN/1GJGZ6"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ABC",
		publication = NA,
		project = NA,
		carob_date = "2025-08-15",
		design = "unitOfAnalysis: Plot level experimental data; universe: Experimental trials located in farmer fields in three ecologies (Sabilo, Riroda and Gallapo villages) in Babati District of Northern Tanzania.; samplingProcedure: The experimental trials were designed using randomized complete block designs with three replicates and in five farmer fields. All measurements were conducted at random points at either plot or field level.",
		data_type = "experiment",
		treatment_vars = "intercrops",
		response_vars = "yield;fwy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 75,	
		notes = "Seasonal Soil Conditions  and Soil Water Infiltration Trial have not been processed, as thier contents are not suitable for this Carob version."
	)
	

	f1 <- ff[basename(ff) == "03_Radiation_Interception_Trial_1_Data_2019.xlsx"]
	f2 <- ff[basename(ff) == "04_GrossMargins_Trial_1_Data-2019.xlsx"]
	f3 <- ff[basename(ff) == "06_Soil_Characterization_Trial_1_Data_2019.xlsx"]
	f4 <- ff[basename(ff) == "08_Seasonal_Weather_2019_Trial_1_Data_2019.xlsx"]
	#f5 <- ff[basename(ff) == "07_Seasonal_Soil_Conditions_Trial_1_Data_2019.xlsx"]
	#f6 <- ff[basename(ff) == "05_Soil_Water_Infiltration_Trial_1_Data_2019.xlsx"]
	
	
	r1 <- carobiner::read.excel(f1, fix_names = TRUE)
	r2 <- carobiner::read.excel(f2, fix_names = TRUE)
	r3 <- carobiner::read.excel(f3, fix_names = TRUE)
	r4 <- carobiner::read.excel(f4, fix_names = TRUE)
	#r5 <- carobiner::read.excel(f4, fix_names = TRUE)
	#r6 <- carobiner::read.excel(f6, fix_names = TRUE)

## Process 
	
	### Process r1
	d1 <- data.frame(
	   trial_id= r1$Farmer.Name,
	   location= tolower(r1$Ecozone),
	   rep= as.integer(r1$Reps),
	   treatment= r1$Trt_Descr,
	   trt= as.numeric(r1$Trt),
	   LAI= r1$Leaf.Area.Index.LAI,
	   growth_stage= ifelse(grepl("V6", r1$Growth.stage), "maize had six fully expanded leaves", "Post maize harvesting") 
	)
	
### process r2 	
	d2 <- data.frame(
	   trial_id= r2$Farmer.Name,
	   location= tolower(r2$Ecozone),
	   adm1= "Manyara",
	   adm2= "Babati",
	   country= "Tanzania",
	   rep= as.integer(gsub("F.P", NA, r2$Rep)),
	   trt= as.numeric(gsub("F.P", NA, r2$Trt)),
	   treatment= r2$Trt_Descr,
	   maize_yield= r2$Mz.Grain_Yld.t.ha,
	   maize_biomass=ifelse(!is.na(r2$Mz.Toppings_Yld.t.ha), rowSums(r2[, c("Mz.Stover_Yld.t.ha", "Mz.Toppings_Yld.t.ha")]), r2$Mz.Stover_Yld.t.ha), #not sure
	   bean_yield= r2$Bn.Grain.Yld.t.ha,
	   bean_biomass= r2$Bn.Haulm.Yld.t.ha,
	   pigeon_yield= r2$PP.Grain_Yld.t.ha,
	   pigeon_biomass= r2$PP.Stalks_Yld.t.ha,
	   #r3$PPHusks_Yld.bags , ## giving in bags unit
	   N_fixation= r2$BNF.kg.ha,
	   N_fertilizer= 50,
	   P_fertilizer= 20,
	   K_fertilizer= 0,
	   price= r2$Gross_Rev,
	   planting_date= "2019",
	   geo_from_source= FALSE,
	   on_farm= TRUE,
	   is_survey= FALSE,
	   yield_part= "grain",
	   irrigated= NA,
	   yield_moisture= as.numeric(NA)
	)
	
	dd <- merge(d1, d2, by= c("trial_id", "location", "rep", "treatment", "trt"), all.x = TRUE)
		
	d <- reshape(dd, varying = list(c("maize_yield","bean_yield", "pigeon_yield"), c("maize_biomass","bean_biomass", "pigeon_biomass")), v.names = c("yield","fwy_total"), 
				  timevar ="crop",
				  times = c(1,2,3),
				  direction = "long")
		
	d <- d[!is.na(d$yield),]
	d$yield <- d$yield*1000 ## kg/ha
	d$fwy_total <- d$fwy_total*1000 ## kg/ha
	d$crop_price <- d$price/d$yield ## 
	d$currency <- "TZS" ## (Not sure as it is missing from the raw data.) 
	d$variety <- c("Meru 513", "Jesica", "Karatu")[d$crop]
	d$crop <- c("maize", "common bean", "pigeon pea")[d$crop]

	d$intercrops <- ifelse(grepl("Continuous maize", d$treatment) & grepl("maize", d$crop), "none",
					  ifelse(grepl("Mbili-Mbili", d$treatment) & grepl("maize", d$crop), "pigeon pea;common bean",
					  ifelse(grepl("Meru 513|Maize 2 plants per hill|Maize topped|Maize no topping", d$treatment) & grepl("maize", d$crop), "pigeon pea", 
					  ifelse(grepl("Mbili-Mbili", d$treatment) & grepl("pigeon pea", d$crop), "maize;common bean",
					  ifelse(grepl("Meru 513|Maize 2 plants per hill|Maize topped|Maize no topping", d$treatment) & grepl("pigeon pea", d$crop), "maize", 
					  ifelse(grepl("Doubled-up Legume", d$treatment) & grepl("pigeon pea", d$crop), "common bean",
					  ifelse(grepl("Doubled-up Legume", d$treatment) & grepl("common bean", d$crop), "pigeon pea",
					  ifelse(grepl("Mbili-Mbili", d$treatment) & grepl("common bean", d$crop), "pigeon pea;maize", "none"))))))))

	d$intercrop_type <- ifelse(grepl("Mbili-Mbili", d$treatment), "strip", 
						  ifelse(grepl("none", d$intercrops), "monocrop", "mixed"))
	d$id <- d$trt <- d$price <- NULL

	### Process soil data

	d3 <- data.frame(
	   trial_id= r3$Farmer.Name,
	   location= tolower(r3$Ecozone),
	   rep= as.integer(r3$Reps),
	   depth_top = 0,
	   depth_bottom = 20,
	   soil_pH= r3$pH,
	   soil_EC= r3$X.EC.Salts.uS.cm/1000, #mS/cm
	   soil_N= r3$pctN,
	   soil_C= r3$pctC,
	   soil_P= r3$X.Phosphorus.Olsen.ppm,
	   soil_K= r3$Potassium.ppm,
	   soil_Ca= r3$Calcium.ppm,
	   soil_Mg= r3$Magnesium.ppm,
	   soil_Mn= r3$Manganese.ppm,
	   soil_S= r3$Sulphur.ppm,
	   soil_Cu= r3$Copper.ppm,
	   soil_B= r3$Boron.ppm,
	   soil_Zn= r3$Zinc.ppm,
	   soil_Fe= r3$Iron.ppm,
	   soil_Na= r3$X.Sodium.ppm,
	   soil_CEC= r3$X.C.E.C.meq.100g
	)

	soilmeta <- data.frame(
	   variable = "soil_P",
	   method = "Olsen"
	)

	### merge d3 with d
	d <- merge(d, d3, by= c("trial_id", "location", "rep"), all.x = TRUE)

	### Adding geo coordinate 

	geo <- data.frame(
	   location= c("sabilo", "gallapo", "riroda"),
	   longitude= c(35.4790, 35.855, 35.645),
	   latitude= c(-4.347, -4.2840, -4.3001)
	) 

	d <- merge(d, geo, by= "location", all.x = TRUE)

 #### process weather data 
	W <- data.frame(
		location= tolower(r4$Ecozone),
		adm2= "Babati",
		adm1= "Manyara",
		country= "Tanzania",
		geo_from_source= FALSE,
		date= as.character(as.Date(carobiner::eng_months_to_nr(paste0("2019", "-", r4$Measurement.Date)), format ="%Y-%d-%m")),
		srad= r4$Solar.Rad.wat.m2,
		rhum= r4$Relative.Humidity.pct,
		temp= r4$Temperature.C,
		prec= r4$Rainfall.mm,
		wdir= r4$Wind.Direction.Deg,
		wgst= r4$Wind.Gust.km.h/3.6, ## m/s
		wspd= r4$Wind.Speed.km.h/3.6, ## m/s
		dewp= r4$Dew.Point.C,
		longitude= ifelse(grepl("Gallapo", r4$Ecozone), 35.855, 
					ifelse(grepl("Sabilo", r4$Ecozone), 35.4790, 35.745)),
		
	   latitude= ifelse(grepl("Gallapo", r4$Ecozone), -4.2840, 
						  ifelse(grepl("Sabilo", r4$Ecozone), -4.347, -4.207))
	)

	carobiner::write_files(path, meta, d, wth = W, var_meta = soilmeta)
}

