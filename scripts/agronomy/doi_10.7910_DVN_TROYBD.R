# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Fertilizer response and nitrogen use efficiency in African smallholder maize farms

Improving fertilizer recommendations for farmers is essential to increase food security in smallholder landscapes. Currently, blanket recommendations are provided across agro-ecological zones, although fertilizer response and nutrient use efficiency by maize crop are spatially variable. We aimed to identify factors that could help to refine fertilizer recommendation by analyzing the variability in fertilizer response (FR) and the agronomic nitrogen use efficiency (N-AE). A literature search for on-farm studies across Kenya and Sub-Sahara Africa (SSA), excluding Kenya, yielded 71 publications. The variability in FR was studied using a meta-analysis whereas key factors that influence FR and N-AE were studied with linear regression models. On average, the FR was 2, but it varied considerably from 1 to 28.5 (excluding outliers). In SSA, 18% of the plots were non-responsive plots with an FR < 1. The main factors affecting N-AE for Kenya were P-Olsen, silt content, soil pH, clay and rainfall, whereas only soil pH, exchangeable K and texture were important for SSA. However, our study indicates that available data on soil, climate and management factors could explain only a small part (< 33%) of the variation in FR and N-AE. Soil pH, P-Olsen, silt content, and rainfall had significant but low levels of power in explaining variation in FR and N-AE. Our findings indicate that strategies to refine fertilizer recommendation should include information on soil types and soil properties.
"
   
	uri <- "doi:10.7910/DVN/TROYBD"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRAF;UONBI;WUR",
		publication = "doi:10.1007/s10705-018-9958-y",
		project = NA,
		carob_date = "2025-09-03",
		design = NA,
		data_type = "compilation",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
	f1 <- ff[basename(ff) == "Raw data_Fertilizer Response_metadata.csv"]
	#f2 <- ff[basename(ff) == "Raw data_Fertilizer Response_journals_referred.csv"]
	#f3 <- ff[basename(ff) == "Raw data_Fertilizer Response_table1_data.csv"]

	r1 <- read.csv(f1, na= "")
	#r2 <- read.csv(f2)
	#r3 <- read.csv(f3)

	
	d1 <- data.frame(
		year = r1$ApplTrmentYear,
		reference = trimws(r1$Authors),
		country = r1$Country,
		location = r1$SiteName,
		latitude = r1$Lat,
		longitude = r1$Long,
		treatment_crt= r1$Control,
		yield_crt = r1$ContYield_mg.ha,
		yield_trt = r1$TrtmtYield_mg.ha,
		treatment_trt = r1$Treatment,
		N_fertilizer_trt = r1$NRates_kg.ha,
		P_fertilizer_trt = r1$PRates_kg.ha,
		K_fertilizer_trt = r1$KRates_kg.ha,
		OM_amount_trt = r1$Manure_kg.ha,
		N_fertilizer_crt = 0, ## from control treatment
		P_fertilizer_crt = 0,
		K_fertilizer_crt = 0,
		OM_amount_crt = 0,
		crop = "maize",
		rain= r1$Rainfall_mm,
		soil_texture = tolower(r1$Soiltexture),
		soil_type = r1$Soiltype,
		soil_pH = r1$pHw,
		soil_C = r1$TotalCarbon_g.kg/10,
		soil_N = r1$TotalNitrogen_g.kg,
		soil_K = r1$ExchK,
		soil_Ca = r1$ExchCa,
		soil_Mg = r1$ExchMg,
		soil_clay = r1$Clay,
		soil_sand = r1$Sand,
		soil_silt = r1$Silt,
		on_farm = grepl("On-farm", r1$DesignExp),
		is_survey = FALSE, 
		yield_part = "grain", 
		yield_moisture= as.numeric(NA), 
		irrigated = NA, 
		geo_from_source = TRUE,
		trial_id = paste(r1$Authors, r1$Pub_Year, sep = "-")
	
		
	)

   d1$soil_P = r1$AvaP_mg.kg
   i <- is.na(d1$soil_P)
   d1$soil_P[i] = r1$ExtrP_mg.kg[i]
   d1$soil_P_method <- ifelse(i, "Bray1", "Olsen")


	d <- reshape(d1, varying = list(c("treatment_trt", "treatment_crt"), c("yield_trt", "yield_crt"),
                                 c("N_fertilizer_trt", "N_fertilizer_crt"), c("P_fertilizer_trt", "P_fertilizer_crt"),
                                 c("K_fertilizer_trt", "K_fertilizer_crt"), c("OM_amount_trt","OM_amount_crt")), 
              v.names = c("treatment", "yield", "N_fertilizer", "P_fertilizer", "K_fertilizer", "OM_amount"),
              times = c(1, 2), direction = "long")
 
	d$id <- NULL
	d$yield <- d$yield*1000 ## from Mega-gram/ha to kg/ha

 ## Fixing longitude and latitude 
 
	d$location <- ifelse(grepl("Chuka", d$latitude) & is.na(d$location), "Chuka", d$location)
 
	convert_to_decimal <- function(x) {
		if (x == ""|x == "Chuka"| is.na(x)) return(NA_real_)
		x <- gsub("''|\"|’", "'", x)   # unify symbols
		x <- gsub(" ", "", x)          # remove spaces
		
		# Extract direction (N/S/E/W)
		dir <- ""
		if (grepl("[NSEW]$", x)) {
		   dir <- substr(x, nchar(x), nchar(x))
		   x <- substr(x, 1, nchar(x)-1)
		}
		
		# Split into parts (degrees, minutes, seconds)
		parts <- unlist(strsplit(x, "'"))
		parts <- parts[parts != ""]  # remove empties
		
		deg <- as.numeric(parts[1])
		min <- ifelse(length(parts) >= 2, as.numeric(parts[2]), 0)
		sec <- ifelse(length(parts) >= 3, as.numeric(parts[3]), 0)
		
		val <- deg + min/60 + sec/3600
		
		# Apply sign depending on direction
		if (dir %in% c("S", "W")) val <- -val
		
		return(val)
	}
 
	 # get long and lat in decimal
	 d$latitude <- suppressWarnings(sapply(d$latitude, convert_to_decimal))
	 d$longitude <- sapply(d$longitude, convert_to_decimal) 
	 
	 geo <- data.frame(
		 location = c("Trans Nzoia", "West Pokot", "Chuka", "Embu, Kivwe", "Kisii, Nyamonyo", "Kisii, Nyatieko", "Meru", "Kakamega", "Marenyo", "Lihanda", "Ulumbi", "Jona", "Matunda", "Chobosta", "Anin", "Egerton University", "Domboshawa", "Abeokuta", "Kabete ", "Ibadan", "Western Kenya", "Shurugwi ", "Machang'a ", "Mkundi", "Pangawe", "Kingilwira", "Mlali", "Wami Vijana", "Mvomero" , "Bertoua", "Shinyalu", "Magadu", "Mlingano", "Nkundi", "Sasanda", "Lubonde", "Mpangala", "Babungo", "Mfonta", "Makoholi", "Busia"),
		 long = c(34.881, 35.252, 37.653, 37.519, 34.7178, 34.786, 37.646, 34.750, 34.5209, 34.516, 34.549, 34.578, 35.1205, 35.3027, 35.558, 35.933, 31.143, 3.365, 36.7145, 3.946, 34.606, 29.9768, 37.656, 37.3834, 37.795, 37.690, 36.749, 37.471, 37.444, 11.519, 34.751, 37.653, 38.8411, 31.436, 34.977, 38.7009, 37.7621, 10.4346,10.127, 30.7721, 34.1093) ,
		 lat = c(1.148, 1.662, -0.322, -0.508, -0.773, -0.634, 0.0513, 0.282, 0.0631, 0.05146, 0.094, 0.044, 0.8272, 0.552, 0.693, -0.372, -17.609, 7.147, -1.236, 7.378, 0.232, -19.677, -0.778, -6.315, -6.796, -6.768, -6.282, -6.408, -6.3019, 3.925, 0.282, -6.861, -5.1046, -7.851, -5.707, -10.246, -7.0526,  6.0579, 6.3546, -19.8304, 0.4610 ),
		 country = c(rep("Kenya", 16), "Zimbabwe", "Nigeria", "Kenya", "Nigeria", "Kenya", "Zimbabwe", "Kenya", rep("Tanzania", 6), "Cameroon", "Kenya", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", "Tanzania", rep("Cameroon", 2), "Zimbabwe", "Kenya"),
		 geo_from= FALSE
	)
	
	d <- merge(d, geo, by= c("location", "country"), all.x = TRUE)
	
	d$longitude[!is.na(d$long)] <- d$long[!is.na(d$long)]
	d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
	d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
	
	d$location <- trimws(d$location)
	d$treatment <- trimws(d$treatment)
	d$planting_date <- gsub("Yr4|Yr1|Yr2|Yr3|Year", NA, substr(d$year, 1, 4))
	d$lat <- d$long <- d$geo_from <- d$year <- d$time <- NULL
	### Fixing country names
	d$country <- gsub("Benin Republic", "Benin", d$country)
	d$country <- gsub("Co\\ˆ te d’Ivoire", "Côte d'Ivoire", d$country)
	## fixing soil texture
	P <- carobiner::fix_name(d$soil_texture)
	P <- gsub("^clay $", "clay" , P)
	P <- gsub("loam sand", "loamy sand" , P)
	P <- gsub("^sandy$", "sand", P)
	P <- gsub("^loamy$", "loam", P)
	P <- gsub("^sandy loamy$", "sandy loam", P)
	P <- gsub("^loamy sandy$", "loamy sand", P)
	d$soil_texture <- P

	### Fixing fertilizer
	i <- grepl("12o kg N 90 kg P2O5 30 kg K", d$treatment)
	d$N_fertilizer[i] <- 120 
	d$P_fertilizer[i] <- 90/2.29
	d$K_fertilizer[i] <- 30
	i <- grepl("^60 kg N/ha_SR$|^60 kg N/ha_LR$|^60 kg N/ha_SR$", d$treatment)
	d$N_fertilizer[i] <- 60
	i <- grepl("^Manure+30kg N/ha_LR$|^Manure+30kg N/ha_SR$", d$treatment)
	d$N_fertilizer[i] <- 30
	
	 ## 
	i <- grepl("^30 kg P$|^20 kg P$|^40 kg P$", d$treatment)
	d$P_fertilizer[i] <- d$N_fertilizer[i]
	d$N_fertilizer[i] <- 0
	
	i <- grepl("^75 kg N  20 kg P$", d$treatment)
	d$N_fertilizer[i] <- 75 
	d$P_fertilizer[i] <- 20
	d$K_fertilizer[i] <- 0
	d$OM_used <- grepl("manure|Manure|Manure_LR|Manure_SR|FYM|5 tons", d$treatment)
	
	d$P_fertilizer <- ifelse(grepl("P2O5", d$treatment), d$P_fertilizer/2.29, d$P_fertilizer)
	
	### Regarding the treatment, all NA  equal to no fertilizer apply
	d$N_fertilizer[is.na(d$N_fertilizer)] <- 0
	d$P_fertilizer[is.na(d$P_fertilizer)] <- 0
	d$K_fertilizer[is.na(d$K_fertilizer)] <- 0
	d$OM_amount[is.na(d$OM_amount)] <- 0
	
	### Fixing errors in coordinates
	i <- grepl("Kenya", d$country) & grepl("Gatuanyaga", d$location)
	d$longitude[i] <- 37.188
	d$latitude[i] <-   -1.05276 
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Kenya", d$country) & grepl("Ndeiya", d$location)
	d$longitude[i] <- 36.605
	d$latitude[i] <-   -1.227 
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Kenya", d$country) & grepl("Homabay", d$location)
	d$longitude[i] <- abs(d$longitude[i])
	
	i <- grepl("Ghana", d$country) 
	d$longitude[i] <- - abs(d$longitude[i])
	
	i <- grepl("Togo", d$country) & grepl("Sekou", d$location)
	d$longitude[i] <- 1.225
	d$latitude[i] <-  6.125
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Benin", d$country) & grepl("Glidji", d$location)
	d$longitude[i] <- 1.6009
	d$latitude[i] <-  6.2498
	d$geo_from_source[i] <- FALSE
	
	## Glidji is not located in Benin but in Togo
	i <- grepl("Glidji", d$location)
	d$country[i] <- "Togo"
	
	### drop duplicate rows
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)

}

