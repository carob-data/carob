# R script for "carob"


carob_script <- function(path) {

"The dataset contains six datasheets with different types of data i.e., cropping system physiological characteristics, gross margins, soil nutrient characterization, seasonal physical conditions of the soil, soil water infiltration and seasonal weather conditions. The data will be helpful in assessing how the different Climate Smart Agricultural practices (CSA) affect crop physiological characteristics, yield, and soil physical attributes.

11 The cropping system physiological characteristic datasheet contains data on maize leaf chlorophyll measurements using SPAD and soil moisture and temperature reading using time domain reflectometry.

2. The gross margin datasheet contains data on yields of the different plant (maize, beans and pigeon pea) components (grain, stover, maize toppings, haulms, stalks and husks). The different components were converted into gross revenue using the farm gate prices of each parameter.

3. The soil nutrient characterization datasheet contains data on soil nutrient content at 0-20 cm depth which was sampled at block level during planting.

4. The seasonal physical conditions of soil datasheet has data on daily soil physical conditions i.e., soil moisture, temperatures and bulk electrical conductivity during the cropping season. This data was measured at a meteorological station located in the low altitude-low rainfall eco-zone of Babati.

5. The soil water infiltration datasheet indicates the rate of water infiltrating under two suctions i.e., -2 cm sec2 and -6 cm sec2 using mini-disc infiltrometer at field level.

6. The seasonal weather condition indicates daily weather data collected in three agro-ecological conditions of Babati during the 2019 long rain season."


   uri <- "doi:10.7910/DVN/M0XCES"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
      project="Africa Rising", 
      publication= NA, 
      data_organization = "ABC", 
      carob_contributor="Cedric Ngakou", 
      treatment_vars="variety;intercrops",
      response_vars= "yield;fwy_residue",
      carob_date="2024-05-14", 
      data_type="experiment"
   )
   
   ### yield data 
	r1 <- carobiner::read.excel(ff[basename(ff)=="03_Gross Margins_Trial_2_Data_2019.xlsx"], fix_names=TRUE)

	r1 <- r1[, c("Ecozone", "Trial.type", "Rep", "Trt", "Trt_Descr", "Mz.Grain_Yld.t.ha", "Mz.Stover_Yld.t.ha", "Bn.Grain.Yld.t.ha", "Bn.Haulm.Yld.t.ha", "PP.Grain_Yld.t.ha", "PP.Stalks_Yld.t.ha")]

	r1 <- carobiner::change_names(r1, names(r1), c("location", "trial", "rep", "Trt", "treatment", "yield_M", "fwy_residue_M", "yield_B", "fwy_residue_B", "yield_P", "fwy_residue_P"))

	# these are the treatments, not the trials d0$trial_id <- paste(d0$trial, d0$Trt, sep ="_")
	## create a long data set 	
	d1 <- lapply(c("_M", "_B", "_P"), \(i) {
			new_name <- gsub(i, "", names(r1), fixed=TRUE) 
			d <- carobiner::change_names(r1, names(r1), new_name)
			d$crop <- i
			d[, c("crop", "location", "rep", "treatment", "yield", "fwy_residue")]
		}
	) 
	d1 <- do.call(rbind, d1)
	
	d1$yield <- d1$yield*1000 # in kg/ha
	d1$fwy_residue <- d1$fwy_residue*1000 # in kg/ha
	d1$rep <- as.integer(d1$rep)
   
	d1$crop <- gsub("_M", "maize", d1$crop )
	d1$crop <- gsub("_B", "common bean", d1$crop)
	d1$crop <- gsub("_P", "pigeon pea", d1$crop)

	d1$country <- "Tanzania"
	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$irrigated <- FALSE
	d1$inoculated <- FALSE
	d1$yield_part <- "grain"  
	d1$planting_date <- "2019"
	d1$trial_id <- "1"

   ## get from data description 
	
	d1$intercrops[d1$crop=="maize"] <- "common bean; pigeon pea" 
	d1$intercrops[d1$crop=="common bean"] <- "maize; pigeon pea"
	d1$intercrops[d1$crop=="pigeon pea"] <- "common bean; maize"
   
	d1$variety <- ifelse(d1$crop=="maize", "Meru 513", 
                  ifelse(d1$crop=="pigeon pea", "Karatu",
                  ifelse(d1$crop=="common bean", "Jesica", NA))) 
   
   d1$N_fertilizer <- 50
   d1$P_fertilizer <- 20 
   d1$K_fertilizer <- 0

   ## Add longitude and latitude    
	geo <- data.frame(
		location=c("Gallapo", "Sabilo" ), 
		longitude=c(35.853, 35.477), 
		latitude=c(-4.283, -4.346)
	)
	d1 <- merge(d1, geo, by="location", all.x = TRUE)
   d1$geo_from_source <- FALSE

# merge soil data and yield data
	r2 <- carobiner::read.excel(ff[basename(ff)=="05_Soil_Characterization_Trial_2_Data_2019.xlsx"], fix_names=TRUE)
	d2 <- data.frame(
		location = r2$Ecozone, 
		rep = as.integer(r2$Reps),
		soil_pH = r2$pH, 
		soil_EC = r2$X.EC.Salts.uS.cm, 
		soil_P = r2$X.Phosphorus.Olsen.ppm, 
		soil_K = r2$Potassium.ppm, 
		soil_Ca = r2$Calcium.ppm, 
		soil_Mg = r2$Magnesium.ppm, 
		soil_S = r2$Sulphur.ppm, 
		soil_Cu = r2$Copper.ppm, 
		soil_B = r2$Boron.ppm, 
		soil_Zn = r2$Zinc.ppm, 
		soil_Fe = r2$Iron.ppm, 
		soil_Na = r2$X.Sodium.ppm, 
		soil_CEC = r2$X.C.E.C.meq.100g
	)
	d <- merge(d1, d2, by=c("location", "rep"), all.x = TRUE)  
   
	## removing duplicate records (12 rows) (occur probably during the data collection process)
	d <- unique(d)

	w <- carobiner::read.excel(ff[basename(ff)=="07_Seasonal_Weather_2019_Trial_2_Data_2019.xlsx"], fix_names=TRUE)
	dw <- data.frame(
		country = "Tanzania",
		date = paste0(w$Measurement.Date, "-2019"),
		location = w$Ecozone,
		temp = w$Temperature.C,
		dewp = w$Dew.Point.C,
		prec = w$Rainfall.mm,
		rhum = w$Relative.Humidity.pct, 
		wspd = w$Wind.Speed.km.h / 3.6, #km/h -> m/s
		wdir = w$Wind.Direction.Deg,
		wgst = w$Wind.Gust.km.h,
		srad = w$Solar.Rad.wat.m2
	)
	dw$date <- as.Date(carobiner::eng_months_to_nr(dw$date), format="%d-%m-%Y") |> as.character()
	
	## dw needs lon/lat as well
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d, wth=dw)   
}


