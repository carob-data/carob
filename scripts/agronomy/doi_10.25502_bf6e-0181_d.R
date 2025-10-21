# R script for "carob"
# license: GPL (>=3)

## ISSUES
### The data description is not clear for some variables ( For example, the file 1-varietyfertilizer_effect_data.csv contains r5$FW_StorageRoot.1 and r5$FW_StorageRoot.2, but there is no explanation in the description.)

carob_script <- function(path) {

"
Datasets from fertilized improved and local varieties of cassava grown in the highlands of South Kivu, DR Congo

The use of mineral fertilizer and organic inputs with an improved and local variety of cassava allows (i) to identify nutrient limitations to cassava production, (ii) to investigate the effects of variety and combined application of mineral and organic inputs on cassava growth and yield and (iii) to evaluate the profitability of the improved variety and fertilizer use in cassava production. Data on growth, yield and yield components of an improved and local variety of cassava, economic analysis, soil and weather, collected during two growing cycles of cassava in farmer’s fields in the highlands of the Democratic Republic of Congo (DR Congo) are presented. The data complement the recently published paper “Increased cassava growth and yields through improved variety use and fertilizer application in the highlands of South Kivu, Democratic Republic of Congo” (Munyahali et al., 2023) [1]. Data on plant height and diameter were collected throughout the growing period of the crop while the data on the storage root, stem, tradable storage root, non-tradable storage root and harvest index were determined at 12 months after planting (MAP). An economic analysis was performed using a simplified financial analysis whereby additional benefits were calculated relative to the respective control treatments; the total costs included the purchasing price of fertilizers and the additional net benefits represented the revenue from the increased storage root yield due to fertilizer application. The value cost ratio (VCR) was calculated as the additional net benefits over the cost of fertilizer purchase.
"

	uri <- "doi:10.25502/bf6e-0181/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1016/j.fcr.2023.109056",
		project = NA,
		carob_date = "2025-10-20",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;K_fertilizer;P_fertilizer;Ca_fertilizer;Mg_fertilizer;S_fertilizer;Zn_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 50,
		notes = "VCR files were not processed because they were deemed less relevant for data reuse"
	)
	

	f1 <- ff[basename(ff) == "1-nutrient-response_data.csv"]
	f2 <- ff[basename(ff) == "2-data_soil-characteristic_field-experiment.csv"]
	f3 <- ff[basename(ff) == "3-price_nutrient-response.csv"]
	f4 <- ff[basename(ff) == "4-rainfall-_temperature_data.csv"]
	#f5 <- ff[basename(ff) == "1-varietyfertilizer_effect_data.csv"]
	#f6 <- ff[basename(ff) == "3-vcr_fertilizer.csv"]
	#f7 <- ff[basename(ff) == "3-vcr_nutrient-response_2.csv"]
	#f8 <- ff[basename(ff) == "3-vcr_nutrient_response.csv"]
	#f9 <- ff[basename(ff) == "3-vcr_variety.csv"]
	#f10 <- ff[basename(ff) == "datasets_fertilized_cassava_varieties_data_dictionary.csv"]

	r1 <- read.csv(f1, na= "")
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	#r5 <- read.csv(f5)
	#r6 <- read.csv(f6)
	#r7 <- read.csv(f7)
	#r8 <- read.csv(f8)
	#r9 <- read.csv(f9)
	#r10 <- read.csv(f10)

### process yield data
	d1 <- data.frame(
		adm2 = r1$Site ,
		location = r1$Village,
		season = r1$Season,
		rep = r1$Replicate,
		treatment = r1$Fertilizer,
		variety = r1$Variety,
		plant_height = r1$H12MAP,
		fwy_roots =  r1$FW_StorageRoot*1000,
		yield = r1$FW_StorageRoot*1000,
		harvest_index = r1$Harvest_Index_HI,
		fwy_stems = r1$FW_Stem*1000,
		OM_used = grepl("FYM", r1$Fertilizer),
		
		##from publication
		land_prep_method = "hoeing",
		crop = "cassava",
		country = "Democratic Republic of the Congo",
		yield_part = "roots",
		on_farm = TRUE,
		is_survey = FALSE,
		irrigated = NA,
		yield_moisture = as.numeric(NA),
		trial_id = paste(r1$Village, r1$ID, sep = "-")
	)
	
	## removing empty rows 
  d1 <- d1[!is.na(d1$yield),]
	
### process soil data

	d2 <- data.frame(
		location = r2$Village,
		adm2 = r2$Site,
		season = r2$Season,
		rep = r2$Replicate,
		soil_pH = r2$pH._CaCl2_2H2O,
		soil_C_total = r2$C,
		soil_N = r2$N,
		soil_P = r2$P,
		soil_K = r2$K*39*10, ## from cmol/kg to mg/kg
		soil_Mg = r2$Mg*24.31*10,
		soil_Ca= r2$Ca*40*10,
		soil_Mn = r2$Mn,
		soil_Na = r2$Na*23*10,
		soil_CEC = r2$CEC,
		soil_clay = r2$Clay,
		soil_sand = r2$Sand,
		soil_silt = r2$Silt
	)

	### process fertilizer data 
	d3 <- data.frame(
		location = r3$Village ,
		adm2 = r3$Site,
		season = r3$Season,
		rep = r3$Replicate,
		treatment = r3$Fertilizer,
		N_fertilizer = r3$Qt_N,
		P_fertilizer = r3$Qt_P,
		K_fertilizer = r3$Qt_K,
		Ca_fertilizer = r3$Qt_Ca,
		Mg_fertilizer = r3$Qt_Mg,
		S_fertilizer = r3$Qt_S,
		Zn_fertilizer = r3$Qt_Zn,
		OM_amount = r3$Qt_FYM,
		fertilizer_type = "urea;TSP;KCl;CaCO3;MgSO4;ZnSO4",
		fertilizer_amount = rowSums(r3[, c("Urea", "TSP", "KCl", "CaCO3", "MgSO4", "ZnSO4")]),
		fertilizer_price = as.character(r3$Total_Applied_fertilizer)
	)
	

  d3$currency <- "USD"
  d3$treatment <- gsub("NPK \\+ \\(Ca, Mg, S, Zn\\)", "NPK+(Ca,Mg,S,Zn)",  d3$treatment)
  d3$treatment <- gsub("NPK \\+FYM", "NPK+FYM",  d3$treatment)
	
	### Adding fertilizer
	d <- merge(d1, d3, by= c("adm2", "location", "rep", "season", "treatment"), all.x  = TRUE)
	
	### merge soil data and yield data 
	d <- merge(d, d2, by= c("adm2", "location", "rep", "season"), all.x  = TRUE)
	
	d$planting_date <- gsub("LR", "", d$season)
	
	d$season <-  NULL
	
	### process weather data
     ### We used "Kalehe" as location because the data does not have a specific location.
	d4 <- data.frame(
		date = paste(r4$Year, gsub("t", "", carobiner::eng_months_to_nr(r4$Month)), r4$Day, sep = "-"),
		location = "Kalehe",
		prec = r4$Precipitation,
		temp = r4$Temperature,
		longitude =  28.904,
		latitude = -2.0969 ,
		country = "Democratic Republic of the Congo",
		geo_from_source = FALSE
	)
d4$date <- as.character(as.Date(d4$date, "%Y-%m-%d"))
	### Adding long and lat coordinate 
	
	geo <-  data.frame(
	   location = c("Kasheke", "Muhongoza", "Munanira", "Cibanda", "Cibandja"),
	   longitude = c(28.855, 28.899, 28.8107, 28.904, 28.904) ,
	   latitude = c(-2.14528, -2.0693, -2.7903, -2.0969, -2.0969) ,
	   geo_from_source = FALSE
	) 
	
	d <- merge(d, geo, by = "location", all.x = TRUE)

## drop duplicate rows
	d <- unique(d)

	carobiner::write_files(path, meta, d, wth = d4)

}


