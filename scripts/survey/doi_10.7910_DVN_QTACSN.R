# R script for "carob"
# license: GPL (>=3)

## ISSUES
### Absence of a converter to transform local units into standard units.

carob_script <- function(path) {

"
An integrated approach for understanding the factors that facilitate or constrain the adoption of soil carbon enhancing practices in East Africa, Kenya and Ethiopia.

The survey data on soil carbon enhancing practices in Ethiopia is systematically organized in Microsoft Excel tables. The data entails general household characteristics, plot characteristics, crops grown, yield, practices implemented, inputs, livestock ownership, social capital, access to credit, access to extension services.
"

	uri <- "doi:10.7910/DVN/QTACSN"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIAT;HARAU",
		publication = NA,
		project = NA,
		carob_date = "2026-03-11",
		design = "universe: The survey was carried out in two watersheds that is Yiser and Azugashube watershed. Yesir watershed and Azugashube watershed covers an area of 115.8km² with a population density of 158 persons per km² and 88.7km² with a population density of 502.13 persons per km² respectively.

The population in this area practice a mixed system of crop and livestock keeping in which crop and livestock mutually benefit one another. The most common crops in the area includes teff, barley, wheat and horse beans (Bakela). These crops are grown mostly for subsistence purposes. Other important crops include; maize, sorghum, finger millet, enset, pulses and oil crops. Cattle, goats, sheep and poultry forms the major types of livestock kept. In addition to donkey, horses and mules are also common in these areas. The area experience bimodal type of rainfall with the main season (meher) between June and September while the short rainy season (belg) is experienced between February and April. The mean annual precipitation of Yesir is1500mm and Azugashube is 1656mm and with mean temperatures ranging between 20°C and 24°C.

A total of 379 households were sampled, the households were randomly drawn from a list of pastoral area (PA) household rosters. In total 161, and 218 household were sampled in Yiser and Azugashube respectively.",
		
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We only process files with useful information for carob"
	)
	

	f1 <- ff[basename(ff) == "02.Codebooks.xlsx"]
	f2 <- ff[basename(ff) == "Data"]

	
	r1 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_2018_Main File.dta", sep = "/")) |> carobiner:::unlabel()
	r2 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_2018-Crops and Land Info.dta", sep = "/")) |> carobiner:::unlabel()
	#r3 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_2018-Extension.dta", sep = "/")) |> carobiner:::unlabel()
	#r4 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_2018-Fertility.dta", sep = "/")) |> carobiner:::unlabel()
	#r5 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_2018-Livestock.dta", sep = "/")) |> carobiner:::unlabel()
	r6 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_2018-Sale of Crops.dta", sep = "/")) |> carobiner:::unlabel()
	#r7 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_2018-Soil Carbon Practices.dta", sep = "/")) |> carobiner:::unlabel()
	#r8 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_20180-Credit.dta", sep = "/")) |> carobiner:::unlabel()
	r9 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_20180-Demographic Information.dta", sep = "/")) |> carobiner:::unlabel()
	r10 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_20180-Fertlizer Usage.dta", sep = "/")) |> carobiner:::unlabel()
	#r11 <- haven::read_dta(paste(f2, "ETHIOPIA_SOIL_CARBON_ENHANCEMENT_PROJECT_20180-Group Membership.dta", sep = "/")) |> carobiner:::unlabel()

	### process files
	
	d1 <- data.frame(
	  adm2 = r1$Zone_0,
	  location = r1$SOC_sites,
	  farmer_age = r1$hh_age,
	  farmer_gender = r1$gender,
	  fertilizer_used = grepl("Yes", r1$fert_use),
	  OM_used = grepl("Yes", r1$manure_use),
	  OM_amount = r1$qty_manure,
	  #unit = r1$manure_unit,
	  land_prep_method = paste(r1$mnt_prac1, r1$mnt_prac2, r1$mnt_prac3, r1$mnt_prac4, r1$mnt_prac5, r1$mnt_prac6, r1$mnt_prac7, r1$mnt_prac8,r1$mnt_prac9, sep = ";"),
	  trial_id = r1$KEY
	)
	
	### 
	d2 <- data.frame(
	  crop = tolower(r2$crops_id),
	  plot_id = as.character(r2$plot_id7),
	  farmland= r2$landsizeHa,
	  yield = r2$QuantityHarvested,
	  irrigated = ifelse(r2$irr_c1 %in% r2$plot_id7|r2$irr_c2 %in% r2$plot_id7|r2$irr_c3 %in% r2$plot_id7|r2$irr_c4 %in% r2$plot_id7|r2$irr_c5 %in% r2$plot_id7|r2$irr_c6 %in% r2$plot_id7|r2$irr_c7 %in% r2$plot_id7, TRUE, FALSE),
	  residue_prevcrop = r2$proportion1,
	  soil_texture = gsub("sandy", "sand", tolower(r2$soil)),
	  land_tenure = r2$tenure,
	  trial_id = r2$PARENTKEY,
	  record_id = as.integer(1:nrow(r2))
	)
	
### merge d1 and d2
	
	d <- merge(d1, d2, intersect(names(d1), names(d2)), all = TRUE)
	
	####
	d3 <- data.frame(
	  #lot_id = r6$j,
	  crop = tolower(r6$c_id),
	  yield_marketable = ifelse(grepl("Quintal", r6$unit_c), r6$c_sold*100, r6$c_sold) ,
	  crop_price = r6$c_price,
	  trial_id = r6$PARENT_KEY
	  
	)
	
	d3 <- d3[d3$yield_marketable>0,]
	
	## merge d and d4
	d <- merge(d, d3, intersect(names(d), names(d3)), all = TRUE)
	
	###############
d4 <- data.frame(
	  farmer_age = r9$age,
	  farmer_gender = r9$gender3,
	  famer_marital_status = r9$marital,
	  farmer_education_level = r9$education,
	  trial_id = r9$PARENT_KEY
	)
	
	## merge d and d4
	d <- merge(d, d4, intersect(names(d), names(d4)), all.x = TRUE)

############
	d5 <- data.frame(
	  fertilizer_type = r10$fertilizer_id,
	  fertilizer_amount = ifelse(grepl("Quintal", r10$unit_fert), r10$fert_quantity*100, r10$fert_quantity) ,
	  fertilizer_price = as.character(ifelse(grepl("Quintal", r10$unit_fert), r10$fert_price/100, r10$fert_price)),
	  trial_id = r10$PARENT_KEY
	)
	
	## merge d and d5
	d <- merge(d, d5, intersect(names(d), names(d5)), all = TRUE)
	
	d <- d[!duplicated(d$record_id),] 
	### remove one row with NA in record_id
	d <- d[!is.na(d$record_id),]
	
	### 
	d <- d[which(d$farmland>0),]
	d$yield <- d$yield/d$farmland
	d$yield_marketable <- d$farmland
	
	
	#### Lon and lat coordinate 
	i <- grepl("Azugashube", d$location)
	d$longitude[i] <- 37.6687
	d$latitude[i] <- 7.29053
	 
	i <- grepl("Yiser", d$location)  
	d$longitude[i] <- 38.7435
	 d$latitude[i] <- 9.03765
	
	### fixing crop names 
	P <- carobiner::fix_name(d$crop)
	P <- gsub("pepers|berebere", "pepper", P)
	P <- gsub("bananas", "banana", P)
	P <- gsub("barley \\(gebis\\)", "barley", P)
	P <- gsub("beet root \\(key sir\\)", "beetroot", P)
	P <- gsub("cabbage \\(tikil gomen\\)", "cabbage", P)
	P <- gsub("chick peas \\(shimbra\\)", "chickpea", P)
	P <- gsub("eucalyptus", "unknown", P)
	P <- gsub("field peas \\(ater\\)", "pea", P)
	P <- gsub("fruit tree", "unknown", P)
	P <- gsub("garlic \\(nech shinkurt\\)", "garlic", P)
	P <- gsub("girar", "chickpea", P)
	P <- gsub("haricot bean \\(boloke\\)", "common bean", P)
	P <- gsub("horse beans \\(bakela\\)", "common bean", P)
	P <- gsub("inseed \\(telba\\)", "flax", P)
	P <- gsub("local cabbage \\(gomen\\)", "cabbage", P)
	P <- gsub("maize \\(bekolo\\)", "maize", P)
	P <- gsub("millet \\(dagussa\\)", "millet", P)
	P <- gsub("onions \\(key shinkurt\\)", "onion", P)
	P <- gsub("other fruits", "unknown", P)
	P <- gsub("paper", "pepper", P)
	P <- gsub("potatoes", "potato", P)
	P <- gsub("sinar", "oats", P)
	P <- gsub("sorghum \\(mashila\\)", "sorghum", P)
	P <- gsub("spinach \\(quosta\\)", "spinach", P)
	P <- gsub("sunflower \\(suf\\)", "sunflower", P)
	P <- gsub("susbania", "sesbania", P)
	P <- gsub("wanza", "unknown", P)
	P <- gsub("lflax", "flax", P)
	P <- gsub("wheat \\(durrah, sinde \\)", "wheat", P)
	d$crop <- P
	
	### Fixing land prep method
	d$land_prep_method <- ifelse(!grepl("strips|tillage|Ridging|trip", d$land_prep_method), NA, d$land_prep_method )
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub("Agroforestry trees", "unknown", P)
	P <- gsub("Allay cropping \\(e.g. Susbania\\)|Check dam", "unknown", P)
	P <- gsub("Contour tillage", "tillage", P)
	P <- gsub("Crop rotation|Diversion canal|Fayna juu- terrace", "unknown", P)
	P <- gsub("Interception ditches|Other|Hedge", "unknown", P)
	P <- gsub("Manuring", "manual puddling", P)
	P <- gsub("Minimum or zero tillage", "minimum tillage", P)
	P <- gsub("Mulching", "unknown", P)
	P <- gsub("Ridging and ridge tying", "ridge tillage", P)
	P <- gsub("Soil/Stone bund|Terrace", "unknown", P)
	P <- gsub("trip cropping|Grass strips", "strip tillage", P)
	P <- gsub("NA;|;NA", "", P)
	P <- gsub("unknown;|;unknown", "", P)
	P <- gsub("Sstrip", "strip", P)
	d$land_prep_method <- P
	
	### fixing fertilizer type 
	d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("NPSB", "NPS", d$fertilizer_type)
	d$fertilizer_type <- gsub("Lime", "lime", d$fertilizer_type)
	
	######
	d$country <- "Ethiopia"
	d$currency = "ETB"
	d$planting_date <- as.character(NA) 
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE
	d$yield_part <- "none"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	### drop all rows with missing location (adm1, adm2, location)
	d <- d[!is.na(d$longitude),]
	
carobiner::write_files(path, meta, d)

}


