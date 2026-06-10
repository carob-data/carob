# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Some files have not been processed because the fertilizer rate information is missing.


carob_script <- function(path) {

"
Ethiopia Performance Trials 2016

Performance trials (N=52) in two zones (West Shewa and Jimma) in Ethiopia. Trials comprise four nutrient management treatments, namely control with zero fertilizer; and three fertilizer recommendations to achieve the same target yield based on regional fertilizer recommendation, a Nutrient Expert (IPNI software) based recommendation and a soil-test NE based recommendation.  Trials were conducted on-farm with four plots per farm. Observations include biomass and grain yields, as well as pre-sowing pH, nitrogen and phosphorus levels. Some N & K data are missing.
"


	uri <- "hdl:11529/10548390"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		carob_date = "2026-06-08",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 80,	
		notes = "Some files have not been processed because the fertilizer rate information is missing."
	)
	

	f1 <- ff[basename(ff) == "TAMASA_ET_PT_2016F.xlsx"]
	f2 <- ff[basename(ff) == "TAMASA_NE_PT_Data_Ethiopia_2016 (Blurred).xlsx"]

	
	r1b <- carobiner::read.excel(f1, sheet="Plot layout")
	r1 <- carobiner::read.excel(f1, sheet="Variables")
	#r2 <- carobiner::read.excel(f1, sheet="Raw Data")
	r3 <- carobiner::read.excel(f1, sheet="Revised Data")
	r4 <- carobiner::read.excel(f1, sheet="Soil and fertilizer data", fix_names = TRUE)
	r5 <- carobiner::read.excel(f2, sheet="Variable list & description ")
	r6 <- carobiner::read.excel(f2, sheet="Data (NE database) ")
	#r7 <- carobiner::read.excel(f2, sheet="Data(source)-Omonada")
	#r8 <- carobiner::read.excel(f2, sheet="Data(source)-Kersa")
	#r9 <- carobiner::read.excel(f2, sheet="Data(source)-Bako RS")
	#r10 <- carobiner::read.excel(f2, sheet="Data(source)-Bako NRS")
	#r11 <- carobiner::read.excel(f2, sheet="Data(source)-Gobusayo RS")
	#r12 <- carobiner::read.excel(f2, sheet="Data(source)-Gobusayo NRS")
	#r13 <- carobiner::read.excel(f2, sheet="Data(source)-NOT Area 2")
	#r14 <- carobiner::read.excel(f2, sheet="Data(source)-IFA_NOT")
	#r15 <- carobiner::read.excel(f2, sheet="Data(source)-Bako_2016")
	#r16 <- carobiner::read.excel(f2, sheet="Data(source)-Jimma_2016")

	d11 <- data.frame(
		adm1 = carobiner::fix_name(r3$Region, "title"),
		adm2 = carobiner::fix_name(r3$Zone, "title"),
		adm3 = carobiner::fix_name(r3$Districts, "title"),
		location = carobiner::fix_name(r3$Location, "title"),
		elevation = r3$Altitude,
		treatment = r3$Treatments,
		#shelling_percentage = r3$`Shelling factor`*100,
		yield_moisture = r3$`Grain MC(%)`,
		yield = r3$`Kernel yield       (kg/ha)`,
		fwy_total = r3$`Total Biomass Weight (kg)`*100, # kg/ha
		plot_area = 100, # m2
		soil_pH = r3$pH,
		soil_N_total = as.numeric(gsub(".", NA, r3$`TN               (%)`))*10000, # from % to ppm
		soil_P = r3$`Av. P (ppm)`,
		country = "Ethiopia",
		crop = "maize",
		planting_date = "2016",
		trial_id = paste(r3$`Crop Stand count`, r3$Districts, sep = "-"), 
		on_farm = TRUE, 
		is_survey = FALSE, 
		yield_part = "grain", 
		irrigated = NA
	)
	
	cols <- c("elevation" , "soil_pH", "soil_N_total", "soil_P")
	
	for(v in cols) {
	  i <- cumsum(!is.na(d11[[v]]))
	  d11[[v]] <- d11[[v]][match(i, i)]
	}
	d11$soil_pH <- round(as.numeric(gsub("^.$", NA, d11$soil_pH)), 3)
	d11$soil_N_total <- round(as.numeric(gsub("^.$", NA, d11$soil_N_total)), 3)
	
	rr <- reshape(r4, varying = c("Control", "NE.Recomm", "Regional.Recom", "Soil.Test.Based.Recom"), v.names = "NPK",
	              timevar = "Treatments",
	              times = c("Control", "NE Recommendation", "Regional Recommendation", "Soil Test Based Recommendation"),
	               direction = "long")
	rownames(rr) <- NULL
	
	d12 <- data.frame(
	  location = carobiner::fix_name(rr$Peasant.Association, "title"),
	  soil_pH = rr$pH.1.2.5.water,
	  soil_N_total = rr$TN..pct,
	  soil_P = rr$Av.P.ppm,
	  NPK = rr$NPK,
	  treatment = rr$Treatments
	  
	)

	d1 <- merge(d11, d12, by= intersect(names(d11), names(d12)), all.x = TRUE)
	tmp <- do.call(rbind, strsplit(d1$NPK, split = "-"))
	d1$N_fertilizer  <- as.numeric(tmp[, 1])
	d1$P_fertilizer  <- as.numeric(tmp[, 2])
	d1$K_fertilizer  <- as.numeric(tmp[, 3])
	d1$NPK <- NULL
	### 

	d2 <- data.frame(
	  planting_date = r6$Date,
	  country = r6$Country,
	  adm1 = carobiner::fix_name(r6$Region, "title"),
	  adm2 = carobiner::fix_name(r6$`Province/State`, "title"),
	  adm3 = carobiner::fix_name(r6$`Municipality/District`, "title"),
	  location = carobiner::fix_name(r6$`Location/Site name`, "title"),
	  crop = tolower(r6$Crop),
	  variety_type = r6$`Variety type`,
	  latitude = r6$Latitude,
	  longitude = r6$Longitude,
	  soil_pH = r6$Soil_pH,
	  previous_crop_residue_perc = as.numeric(gsub("Removed all", NA, r6$Residue_tc)),
	  land_prep_method = tolower(r6$Tillage),
	  soil_texture = r6$soil_texture,
	  soil_color = r6$soil_color,
	  soil_P = r6$P_soil_value,
	  N_fertilizer_ST = r6$ST_N_NPK,
	  P_fertilizer_ST = r6$ST_P_NPK,
	  K_fertilizer_ST = r6$ST_K_NPK,
	  yield_ST = r6$GY_ST*1000,
	  N_fertilizer_FP = r6$FP_N_NPK,
	  K_fertilizer_FP = r6$FP_K_NPK,
	  P_fertilizer_FP = r6$FP_P_NPK,
	  yield_FP = r6$GY_FP*1000,
	  N_fertilizer_SR = r6$SR_N_NPK,
	  P_fertilizer_SR = r6$SR_P_NPK,
	  K_fertilizer_SR = r6$SR_K_NPK,
	  yield_SR = r6$GY_SR*1000,
	  N_fertilizer_cnt = 0,
	  P_fertilizer_cnt = 0,
	  K_fertilizer_cnt = 0,
	  yield_cnt = r6$GY_control*1000,
	  geo_from_source = TRUE,
	  trial_id = paste(r6$Data_no., r6$`User name`, sep = "-"), 
	  on_farm = TRUE, 
	  is_survey = FALSE, 
	  yield_part = "grain", 
	  irrigated = NA
	  
	  
	)
	
	dd <- reshape(d2, varying = list(c("N_fertilizer_ST", "N_fertilizer_FP", "N_fertilizer_SR", "N_fertilizer_cnt"), c("P_fertilizer_ST", "P_fertilizer_FP", "P_fertilizer_SR", "P_fertilizer_cnt"),
	                                 c("K_fertilizer_ST", "K_fertilizer_FP", "K_fertilizer_SR", "K_fertilizer_cnt"),
	                                 c("yield_ST", "yield_FP", "yield_SR", "yield_cnt")),
	              v.names = c("N_fertilizer", "P_fertilizer", "K_fertilizer", "yield"),
	              timevar = "treatment",
	              times = c("Soil Test Recommendation", "Farmer's pratices", "State recommendation", "control"),
	              direction = "long")
	
	rownames(dd) <- NULL
	dd$id <- NULL
	
	
	d <- carobiner::bindr(d1, dd)
	
	d$soil_texture <- gsub("Clayey|Cleyey", "clay", d$soil_texture)
	d$soil_texture <- gsub("Loamy", "loam", d$soil_texture)
	
	## Adding missing long and lat coordinate
	
	geo <- data.frame(
	  adm3 = c("Ilu Galen", "Kersa", "Tiro Afeta", "Bako Tibe","Sekoru", "Gobu Sayo" ,"Omo Nada" ),
	  lon = c(37.0223, 38.984, 37.330, 37.1542, 37.423, 36.9683,  37.254),
	  lat = c(8.7477, 7.530, 7.918, 9.131, 7.918, 9.2080, 7.502),
	  geo_from = FALSE
	)
	
	d <- merge(d, geo, by= "adm3", all.x = TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$geo_from_source[is.na(d$geo_from_source)] <- d$geo_from[is.na(d$geo_from_source)]
  
	d$lon <- d$lat <- d$geo_from <- NULL
	
	### drop rows with missing fertilizer
	d <- d[!is.na(d$N_fertilizer),]
	
		carobiner::write_files(path, meta, d)
}

