# R script for "carob"
# license: GPL (>=3)

## ISSUES

  # The total fertilizer amount is given, but the amount of each type of fertilizer is missing.


carob_script <- function(path) {

"
Tamasa APS Ethiopia  2016 Agronomy Panel Survey
"

	uri <- "hdl:11529/10548305"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization= NA,
		publication = "NA",
		project ="Tamasa",
		carob_date = "2025-08-05",
		design = NA,
		data_type = "crop-cuts",
		treatment_vars = "none",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 75,	
		notes = "Two files (TAMASA_ET_APS_ALL, TAMASA_ET_APS_FOCAL) have not been processed as the data seem to be unsuitable for carob."
	)
	

	f1 <- ff[basename(ff) == "ET_BAko agronomy data_2015_02Jun17 (Blurred).xlsx"]
	f2 <- ff[basename(ff) == "ET_Baseline_CSA_2015_02Jun17 (Blurred).xlsx"]
	f3 <- ff[basename(ff) == "ET_Baseline_EIAR_2015 (Blurred).xlsx"]
	f4 <- ff[basename(ff) == "TAMASA_ET_APS_ALL_PLOTS_Meher_2016.xlsx"]
	#f5 <- ff[basename(ff) == "TAMASA_ET_APS_ALL_PLOTS_Belg_2016.xlsx"]
	#f6 <- ff[basename(ff) == "TAMASA_ET_APS_FOCAL_PLOT_2016.xlsx"]

	
	r1 <- carobiner::read.excel(f1, sheet="Raw_Data", fix_names = TRUE, na= ".")
	r2 <- carobiner::read.excel(f2, sheet="Revised_data", fix_names = TRUE, na= c("."))
	r3 <- carobiner::read.excel(f3, sheet="Revised_data", fix_names = TRUE, na=c(".", "n/a"))
	r4 <- suppressWarnings(carobiner::read.excel(f4, sheet = "Prod&family labor_Meher_2016"))
	r41 <- carobiner::read.excel(f4, sheet = "Characterstics_all_plots_2016")
	

	d1 <- data.frame(
	   location= r1$Name.of.the.Village,
		plot_id = as.character(r1$plot.ID),
		latitude = r1$Section_D._gps_latitude,
		longitude = r1$Section_D._gps_longitude,
		elevation = r1$Section_D._gps_altitude,
		planting_date = r1$Planting.Date,
		variety = r1$Type.of.Variety,
		variety_type= r1$Seed.type.Local.vs.Improved,
		previous_crop = tolower(r1$Previous.precursor.crop),
		yield = r1$Average.yield.kg.ha.or.Q1.Q2.2,
		yield_moisture= rowMeans(r1[, c("Moisture.content.99", "Moisture.content.111")]),
		farmer_gender= r1$Sex,
		crop= "maize",
		land_prep_method= tolower(r1$Land.Preparation.Method),
		planting_method= gsub("Open_furrow_cover", "direct seeding", r1$Planting.Method),## not sure
		fertilizer_used= r1$Section_D.inorg_fertilizer,
		fertilizer_type= r1$Type.of.Inorganic.Fertilizer,
		fertilizer_amount= r1$Amount.of.Inorganic.Fertilizer.kg,
		#fertilizer_method= r1$Inorganic.Fertilizer.application.Method,
		house_distance= r1$Farm.Distance.to.Home,
		soil_type= r1$Soil.type,
		soil_color= r1$Soil.colour,
		intercrops= gsub("Haricot bean", "common bean", r1$Intercropping.with.legume),
		crop_rotation= r1$Crop.Rotation.with,
		plant_density= r1$Average.Number.of.crop.stand.16m2*10000, # plant/ha
		previous_crop_residue_perc= rowMeans(r1[, c("pct.crop.residue.cover.Q1", "pct.crop.residue.cover.Q2")]),
		#pest_incidence= r1$Average.pct.Insect.pest,
		disease_incidence= as.character(r1$Average.pct.Disease.Q1),
		weed_cover= r1$Average.pct.weed.cover,
		plot_area= r1$Harvested.Area.4m.4m/10000, #ha 
		trial_id= "Bako",
		soil_SOC= r1$Carbon.pct,
		soil_pH= r1$pH,
		soil_Al= r1$Al.mg.kg.1,
		soil_Ca= r1$Ca.mg.kg.1,
		soil_EC= r1$EC.S.dS.m/100,
		soil_S= r1$S.mg.kg.1,
		soil_Mn= r1$Mn.mg.kg.1,
		soil_P_available= r1$P.mg.kg.1,
		soil_Zn= r1$Zn.mg.kg.1,
		soil_K= r1$K.mg.kg.1,
		soil_Mg= r1$Mg.mg.kg.1,
		soil_Na= r1$Na.mg.kg.1,
		soil_Fe= r1$Fe.mg.kg.1,
		soil_B= r1$Boron.mg.kg.1,
		soil_N= r1$Nitrogen.pct*10000
		
	)

	d2 <- data.frame(
	   farmer_gender= r2$Gender,
	   latitude= r2$latitude,
	   longitude= r2$longitude,
	   elevation= r2$altitude,
	   variety_type= r2$Type.of.variety,
	   variety= r2$Name.of.variety,
	   OM_used= r2$Fertilizer.type.organic,
	   crop="maize",
	   fertilizer_used= r2$Fertilizer.type.inorganic,
	   fertilizer_amount= r2$amount.of.Inorganic.fertilizer,
	   OM_amount= r2$amount.of.organic.fertilizer,
	   plant_density= r2$Number.of.crop.stands.16m2*10000,
	   yield= r2$Moisture.adjusted.grain.yield.kg.ha,
	   trial_id= "CSA",
	   soil_SOC= r2$Carbon.pct,
	   soil_pH= r2$pH,
	   soil_Al= r2$Al.mg.kg,
	   soil_Ca= r2$Ca.mg.kg,
	   soil_EC= r2$EC.S.dS.m/100,
	   soil_S= r2$S.mg.kg,
	   soil_Mn= r2$Mn.mg.kg,
	   soil_P_available=  r2$P.mg.kg,
	   soil_Zn= r2$Zn.mg.kg,
	   soil_Mg= r2$Mg.mg.kg,
	   soil_K= r2$K.mg.kg,
	   soil_Na= r2$Na.mg.kg,
	   soil_Fe= r2$Fe.mg.kg,
	   soil_B= r2$Boron.mg.kg,
	   soil_N= r2$Nitrogen.pct*10000
	)
	
	d3 <- data.frame(
	   farmer_gender= r3$Gender,
	   latitude= r3$Latitude,
	   longitude= r3$Longitude,
	   elevation= r3$Altitude,
	   variety_type= r3$Type.of.variety,
	   variety= r3$Name.of.variety,
	   crop= "maize",
	   OM_used= r3$Fertilizer.type.organic,
	   fertilizer_used= r3$Fertilizer.type.inorganic,
	   fertilizer_amount= r3$amount.of.Inorganic.fertilizer,
	   OM_amount= r3$Org_fert_qty,
	   yield= rowMeans(r3[, c("Quadrant.1.Grain.yield.kg.ha", "Quadrant.2.Grain.yield.kg.ha","Quadrant.3.Grain.yield.kg.ha")]),
	   cob_density= rowMeans(r3[, c("Quadrats.1.num_cobs.16m2", "Quadrats.2.num_cobs.16m2")])*10000,
	   trial_id= "EIRA",
	   soil_SOC= r3$Carbon.pct,
	   soil_pH= r3$pH,
	   soil_Al= r3$Al.mg.kg,
	   soil_Ca= r3$Ca.mg.kg,
	   soil_EC= r3$EC.S.dS.m/100,
	   soil_S= r3$S.mg.kg,
	   soil_Mn= r3$Mn.mg.kg,
	   soil_P_available= r3$P.mg.kg,
	   soil_Zn= r3$Zn.mg.kg,
	   soil_K= r3$K.mg.kg,
	   soil_Mg= r3$Mg.mg.kg,
	   soil_Na= r3$Na.mg.kg,
	   soil_Fe= r3$Fe.mg.kg,
	   soil_B= r3$Boron.mg.kg,
	   soil_N= r3$Nitrogen.pct*10000 # mg/kg
	 
	   
	)
	
	### join d1, d2 and d3
	d <- carobiner::bindr(d1, d2, d3)
	d$crop_rotation <- ifelse(!is.na(d$crop_rotation), paste("maize", d$crop_rotation, sep = ";"), d$crop_rotation)
   d$crop_rotation <- gsub(", ", ";", d$crop_rotation)
   d$crop_rotation <- gsub("Other", "unknown", d$crop_rotation)
	d$planting_date <- as.character(as.Date(d$planting_date))
   d$fertilizer_amount <- as.numeric(d$fertilizer_amount)
	d$OM_amount <- as.numeric(d$OM_amount)
### Process Useful data from TAMASA_ET_APS_ALL_PLOTS_Meher_2016.
	
	d4 <- data.frame(
	   trial_id= r4$hhid,
	   adm1= r4$region,
	   adm2= r4$zone,
	   adm3= r4$woreda,
	   location= r4$kebele,
	   plot_id= as.character(r4$plotid),
	   irrigated= ifelse(grepl("no", r4$c1), FALSE, 
	               ifelse(grepl("yes", r4$c1), TRUE, NA)) ,
	   crop= ifelse(grepl("Other", r4$c3a) & !is.na(r4$c3a_other), r4$c3a_other, r4$c3a),
	   yield= r4$c7a, # in kg
	   previous_crop= r4$c9
	)
	
	d41 <- data.frame(
	   trial_id= r41$hhid,
	   adm1= r41$region,
	   adm2= r41$zone,
	   adm3= r41$woreda,
	   location= r41$kebele,
	   plot_id= as.character(r41$plotid),
	   plot_area= r41$p1,
	   house_distance= r41$p2
	)
	
	d4 <- merge(d4, d41, by=c("trial_id", "adm1", "adm2", "adm3", "location", "plot_id"), all.x = TRUE)
   d4$yield <- d4$yield/d4$plot_area


   d <- carobiner::bindr(d, d4)
   
   ## Keep positive and non-null yield values.
   d <- d[d$yield > 0,]
   
   ### Fixing crop 
   
   P <- carobiner::fix_name(d$crop, "lower")
   P <- gsub("field peas /ater", "pea", P)
   P <- gsub("nigerseed / nug", "niger", P)
   P <- gsub("Chat \\(khat/miraa\\)", "khat", P)
   P <- gsub("sweet potato", "sweetpotato", P)
   P <- gsub("haricot bean", "common bean", P)
   P <- gsub("sugar cane", "sugarcane", P)
   P <- gsub("faba beans", "faba bean", P)
   P <- gsub("durum wheat|bread wheat", "wheat", P)
   P <- gsub("other root/veg", "vegetable", P)
   P <- gsub("tarrow|taro\\(godere\\)|tarro|taro/godere", "taro", P)
   P <- gsub("desi chickpea", "chickpea", P)
   P <- gsub("chat \\(khat/miraa\\)", "khat", P)
   P <- gsub("enset\\+|enser", "enset", P)
   P <- gsub("other perennial crop|missing", NA, P)
   P <- gsub("soya bean", "soybean", P)
   P <- gsub("linseed", "flax", P)
   P <- gsub("mixed teff|white teff|red teff", "teff", P)
   d$crop <- P
   
   ## Fixing previous crop
   P <- carobiner::fix_name(d$previous_crop, "lower")
   P <- gsub("nigerseed / nug", "niger", P)
   P <- gsub("other root/veg|vegetables", "vegetable", P)
   P <- gsub("sweet potato", "sweetpotato", P)
   P <- gsub("field peas /ater", "pea", P)
   P <- gsub("chat \\(khat/miraa\\)", "khat", P)
   P <- gsub("faba beans", "faba bean", P)
   P <- gsub("bread wheat", "wheat", P)
   P <- gsub("beans", "common bean", P)
   P <- gsub("missing", NA, P)
   P <- gsub("other legume","legume", P)
   P <- gsub("other cereal", "cereal", P)
   P <- gsub("linseed", "flax", P)
   P <- gsub("mixed teff|white teff|red teff", "teff", P)
   d$previous_crop <- P
   d$previous_crop <- ifelse(grepl("other|otherfodder", d$previous_crop), "unknown", d$previous_crop)
   
   ### Fixing fertilizer type
   	
   P <- carobiner::fix_name(d$fertilizer_type)
   P <- gsub(",|, | ,", ";", P)
   P <- gsub(" & |&", ";", P)
   P <- gsub("-", ";", P)
   P <- gsub(" and ", ";", P)
   P <- gsub(" .;", ";", P)
   d$fertilizer_type <- P
   d$location <- gsub("\r\n", "", d$location)
   d$variety <- gsub("\r\n| ", "", d$variety)
   
   d$fertilizer_used <- ifelse(grepl("Yes", d$fertilizer_used), TRUE, 
                        ifelse(grepl("No", d$fertilizer_used), FALSE, NA))
   
   d$crop_rotation <- tolower(gsub("Vegetables", "vegetable", d$crop_rotation))
   d$crop_rotation <- gsub("beans", "common bean", d$crop_rotation)
   d$crop_rotation <- gsub("beans", "common bean", d$crop_rotation)
   
   P <- carobiner::fix_name(d$fertilizer_type)
   P <- gsub("Urea|UREA|URE|UREA", "urea", P)
   P <- gsub("NSP", "NPS", P)
   P <- gsub("SPS", "SSP", P)
   P <- gsub(" ;|; ", ";", P)
   d$fertilizer_type <- P
   
   d$country <- "Ethiopia"
   d$on_farm <- FALSE 
   d$is_survey <- TRUE
   d$yield_part <- "none"
   d$geo_from_source <- TRUE
   d$crop_cut <- TRUE
   
   ### Adding fertilizer 
   
   ### The total fertilizer amount is given, but the amount of each type of fertilizer is missing.
   
   d$N_fertilizer <- ifelse(grepl("^DAP$", d$fertilizer_type), 18,
                     ifelse(grepl("NPS;urea|urea;NPS", d$fertilizer_type), 23+46,
                     ifelse(grepl("DAP;urea|urea;DAP", d$fertilizer_type), 18+46,
                     ifelse(grepl("urea;SSP", d$fertilizer_type), 46, NA))))
   
   d$P_fertilizer <- ifelse(grepl("^DAP$", d$fertilizer_type), 20,
                     ifelse(grepl("NPS;urea|urea;NPS", d$fertilizer_type), 21,
                     ifelse(grepl("DAP;urea|urea;DAP", d$fertilizer_type), 20,
                     ifelse(grepl("urea;SSP", d$fertilizer_type), 8.74, NA))))
   
   d$K_fertilizer <- ifelse(grepl("^DAP$", d$fertilizer_type), 10,
                     ifelse(grepl("NPS;urea|urea;NPS", d$fertilizer_type), 0,
                     ifelse(grepl("DAP;urea|urea;DAP", d$fertilizer_type), 10,
                      ifelse(grepl("urea;SSP", d$fertilizer_type), 0, NA))))
   
   d$S_fertilizer <- ifelse(grepl("^DAP$", d$fertilizer_type), 0,
                     ifelse(grepl("NPS;urea|urea;NPS", d$fertilizer_type), 4,
                     ifelse(grepl("DAP;urea|urea;DAP", d$fertilizer_type), 0,
                     ifelse(grepl("urea;SSP", d$fertilizer_type), 12, NA))))
   
   
### Fixing longitude and latitude
   
   i <- grepl("East Wollega", d$adm2) 
   d$latitude[i] <- 9.643
   d$longitude[i] <- 39.4940       
   d$geo_from_source[i] <- FALSE 
   
   i <- grepl("West shewa", d$adm2) 
   d$latitude[i] <- 8.6270
   d$longitude[i] <- 37.4585       
   d$geo_from_source[i] <- FALSE 
   
   i <- grepl("Jimma", d$adm2) 
   d$latitude[i] <- 7.6758
   d$longitude[i] <- 36.8378      
   d$geo_from_source[i] <- FALSE 
   
   i <- grepl("wayu", d$location) 
   d$latitude[i] <-  9.0196
   d$longitude[i] <- 39.118
   d$geo_from_source[i] <- FALSE 
   
   i <- grepl("Tullu sa'a", d$location) 
   d$latitude[i] <-  6.9883 
   d$longitude[i] <- 38.687
   d$geo_from_source[i] <- FALSE 
   
### remove one row with missing value in location, adm1, adm2 and adm3
   d <- d[!is.na(d$latitude), ] 
### removing duplicate records (from the raw data)
   
d <- unique(d)
   
carobiner::write_files(path, meta, d)

}




