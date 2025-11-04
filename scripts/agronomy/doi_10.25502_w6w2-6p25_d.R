# R script for "carob"
# license: GPL (>=3)

## ISSUES
## More than 70% of the data has a missing yield value.

carob_script <- function(path) {

"
Maize-grain zinc and iron concentrations as influenced by agronomic management and biophysical factors: a meta-analysis

This dataset supports a global meta-analysis that assesses how agronomic practices and soil characteristics influence zinc (Zn) and iron (Fe) levels in maize grain. Drawing on 5,874 data points from 138 studies (for Zn) and 3,187 points from 65 studies (for Fe) across multiple countries, the analysis quantifies the probability of maize grain meeting nutritional benchmarks and evaluates the effect of combined Zn/Fe applications, especially with NPK fertilizer and organic treatments, on both nutrient concentrations and grain yield. Key biophysical drivers include soil organic matter, pH, and available Zn levels; the study provides a data-rich foundation for agronomic biofortification strategies.
"

	uri <- "doi:10.25502/w6w2-6p25/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; CIAT; IRRI; AfricaRice; ICARDA",
		publication = "doi:10.1007/s12571-024-01478-5",
		project = NA,
		carob_date = "2025-10-01",
		design = NA,
		data_type = "compilation",
		treatment_vars = "Zn_fertilizer;Fe_fertilizer;N_fertilizer;K_fertilizer;P_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "maize-nutritional-data.csv"]
	#f1 <- ff[basename(ff) == "data_dictionary.csv"]
	
	rr <- read.csv(f, fileEncoding = "latin1", na= "")
	

	Process <- function(d){
	   if(is.null(d$FYM_Cnt)) d$FYM_Cnt <- NA
	   if(is.null(d$MGF_1_Description_Cnt)) d$MGF_1_Description_Cnt <- "control"
	   if(is.null(d$Proteins_Cnt)) d$Proteins_Cnt <- NA
	   if(is.null(d$GrainN_Cnt)) d$GrainN_Cnt <- NA
	   Name <- list(c("Irrigation_Trt", "Irrigation_Cnt"), c("N_Applied_Trt", "N_Applied_Cnt"),
	               c("P_Applied_Trt", "P_Applied_Cnt"), c("K_Applied_Trt", "K_Applied_Cnt"),
	               c("Zn_Applied_Trt", "Zn_Applied_Cnt"), c("Fe_Applied_Trt", "Fe_Applied_Cnt"),
	               c("FYM_Trt", "FYM_Cnt"), c("MGF_1_Description_Trt", "MGF_1_Description_Cnt"), 
	               c("GrainYld_Trt", "GrainYld_Cnt"), c("InorgP_Trt", "InorgP_Cnt"),
	               c("GrainZn_Trt", "GrainZn_Cnt"), c("GrainFe_Trt", "GrainFe_Cnt"), 
	               c("Proteins_Trt", "Proteins_Cnt"), c("GrainN_Trt", "GrainN_Cnt"), c("InorgP_Trt.1", "InorgP_Cnt"))
	   
	   New_Name <- c("Irrigated", "N_fertilizer", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "Fe_fertilizer",
	                "OM_amount", "Treatment", "yield", "InorgP", "grain_Zn", "grain_Fe", "grain_protein", "grain_N", "InorgP_1")
	   
	   df <- reshape(d, varying = Name, v.names = New_Name, timevar ="step", 
	                 direction = "long") 
	   return(df)
	}
	
	r <- Process(rr)
	
#### Process
	
   d <- data.frame(
      reference = trimws(r$Study),
      country =  r$Country,
      location = trimws(r$TrialSite),
      planting_date = as.character(substr(r$Yr_experiment, 1, 4)),
      latitude = r$Latitude,
      longitude = r$Longitude,
      rain = r$SeasonalRainfall,
      inoculated = ifelse(grepl("1", r$Inoculation), TRUE, FALSE),
      #inoculated1= r$Innoculation, ? ambiguity
      land_prep_method = c("tillage","none", "reduced tillage", "unknown")[r$Tillage], 
      #r$`Treatment corrected`,
      irrigated = ifelse(grepl("yes|Yes", r$Irrigated), TRUE, 
                         ifelse(grepl("No|no", r$Irrigated), FALSE, NA)),
      N_fertilizer = r$N_fertilizer,
      P_fertilizer = r$P_fertilizer,
      K_fertilizer = r$K_fertilizer,
      Zn_fertilizer = r$Zn_fertilizer,
      Fe_fertilizer = r$Fe_fertilizer,
      OM_amount = r$OM_amount,
      OM_used = ifelse(!is.na(r$OM_amount), TRUE, FALSE),
      OM_type = ifelse(grepl("Farm Yard Manure", r$Organic_matter_type), "farmyard manure", 
                ifelse(grepl("Organic fertiliser", r$Organic_matter_type), "unknown", tolower(r$Organic_matter_type) )),
      treatment = trimws(r$Treatment),
      yield = r$yield*1000, ## kg/ha
      grain_Zn = r$grain_Zn,
      grain_Fe = r$grain_Fe,
      grain_protein= r$grain_protein,
      grain_N = r$grain_N,
      soil_type = r$Soil_type_New,
      soil_pH = ifelse(grepl("Soil:water|Water|H2O", r$PH_Method), r$pH, NA) ,
      soil_pH_KCl = ifelse(grepl("KCl", r$PH_Method), r$pH, NA),
      soil_pH_CaCl2 = ifelse(is.na(r$pH_CaCl2) & grepl("CaCl|CaCl2|0.01 M CaCl2", r$PH_Method),  r$pH, r$pH_CaCl2) ,
      soil_texture =  tolower(r$Texture),
      soil_sand = r$Sand,
      soil_silt = r$Silt,
      soil_clay = r$Clay,
      soil_SOC = r$SOC,
      soil_SOM = r$SOM,
      soil_N = r$Tot_N,
#      soil_P = ifelse(!grepl("Mehlich", r$AveP_method), r$Avail_P, NA) ,
#      soil_P_method = ifelse(!grepl("Mehlich", r$AveP_method), r$AveP_method, NA),
#     soil_P_mehlich3 = ifelse(grepl("Mehlich", r$AveP_method) & is.na(r$Avail_P_Mehlich), r$Avail_P, r$Avail_P_Mehlich),
#      soil_P_method_Mch3 = "Mehlich3",
	  soil_P = r$Avail_P,
	  soil_P_method = r$AveP_method,
      soil_K = r$Soil_K,
      soil_Zn = r$Soil_Zn,
      soil_Zn_method= r$Zn_method,
      soil_Fe = r$Soil_Fe,
      #record_id = as.integer(1:nrow(r)),
      trial_id = paste(r$TrialSite, r$Yr_experiment, sep = "-"), 
      on_farm = ifelse(grepl("Surveys", r$Data_source), FALSE, NA), ## 
      is_survey = grepl("Surveys", r$Data_source), 
      crop = "maize", 
      yield_part = "grain",
      yield_moisture = as.numeric(NA), 
      geo_from_source = TRUE,
      fertilizer_used = TRUE
   )	
	
	###  Fixing fertilizer
	d$N_fertilizer[is.na(d$N_fertilizer)] <- 0
	d$P_fertilizer[is.na(d$P_fertilizer)] <- 0
	d$K_fertilizer[is.na(d$K_fertilizer)] <- 0


	#soil <- d[, grepl("soil_P|soil_Zn|record_id", names(d))]   
	  
	### We transform soil_P and soil_Zn  into a long format to capture different method of extraction
	## This need to be check( can we have a value inside var_meta ?)
    
## No, we cannot.
#	soilmeta <- reshape(soil, varying = list(c("soil_P", "soil_P_mehlich3", "soil_Zn"), c("soil_P_method", "soil_P_method_Mch3", "soil_Zn_method")),
#			  v.names = c("value", "method") , 
#			  timevar = "variable",
#			  times = c("P", "P", "Zn"),
#			 direction = "long")
#	soilmeta <- soilmeta[!is.na(soilmeta$value),]  
#	soilmeta$method <- ifelse(is.na(soilmeta$method), "unspecified", soilmeta$method)
#	soilmeta$id <- d$soil_P_method <- d$soil_P_method_Mch3 <- d$soil_Zn_method <- d$soil_P_mehlich3 <- d$soil_P <- d$soil_Zn <- NULL
	 

	### Fixing country
	d$country <- gsub("USA", "United States" , d$country)

	### Fixing longitude and latitude
	x <- d$longitude
	y <- gsub("28°38'S;77°10'E", "28°38'S", d$latitude)

	convert_to_decimal <- function(x) {
	   
	   # normalize Unicode symbols
	   x <- gsub("[˚ºᵒ]|°", "°", x)    
	   x <- gsub("[’′ʹ]|'", "'", x)    
	   x <- gsub("″|”|“|''", "\"", x) 
	   x <- gsub("[?]|''|“|”|\u0091|\u0092", "'", x)    # replace ? and odd quotes with '
	   x <- gsub("°", " ", x)                           # replace ° with space
	   x <- gsub("'", " ", x)                           # replace ' with space
	   x <- gsub("\"", " ", x)                          # replace " with space
	   x <- gsub("\\s+", " ", x)
	   
	   # Extract direction (N/S/E/W)
	   dir <- ifelse(grepl("[NnSsEeWw]$", x), substr(x, nchar(x), nchar(x)) , "")
	   x <- trimws(substr(x, 1, nchar(x)-1))
	   
	   # Split into D M S (degrees, minutes, seconds)
	   parts <- strsplit(x, " ")
	   result <- sapply(parts, function(p) {
		  p <- p[p != ""]  # remove empty parts
		  d <- as.numeric(p[1])
		  m <- ifelse(length(p) >= 2, as.numeric(p[2]), 0)
		  s <- ifelse(length(p) >= 3, as.numeric(p[3]), 0)
		  d + m/60 + s/3600
	   })
	   # Apply sign for South and West
	   result[dir %in% c("S", "W")] <- -result[dir %in% c("S", "W")]
	   
	   return(result)
	}


	# get long and lat in decimal

	d$lon <- suppressWarnings(convert_to_decimal(x))
	d$lat <- suppressWarnings(convert_to_decimal(y))

	i <- !is.na(d$lon)
	d$longitude[i] <- d$lon[i]
	P <- carobiner::fix_name(d$longitude)
	P[grepl("79°15", P)] <- as.character(79+ 15/60)
	d$longitude <-  as.numeric(P)


	i <- !is.na(d$lat)
	d$latitude[i] <- d$lat[i]
	P <- carobiner::fix_name(d$latitude)
	P[grepl("32°6'37", P)] <- as.character(32 + 6/60 + 37/3600)
	P[grepl("28°38'", P)] <- as.character(28 + 38/60)
	d$latitude <- as.numeric(P)
	d$lon <- d$lat <- NULL

	### Fixing geo coordinate
	geo <- data.frame(
	   location= c("Faisalabad", "Delhi", "Oaxaca", "Research Station of Safiabad Dezful", "New Delhi", "Bilecik Seyh Edebali University", "Palampur", "Kiboko", "Nyankpala", "Ismailia Governorate", "Karnataka"),
	   long= c(73.1181, 77.2121, -96.728, 48.4190, 77.2121, 29.9696, 76.5372, 37.70000, -0.97833, 32.237, 75.647),
	   lat= c(31.43635, 28.7161, 17.072439, 32.263, 28.7161, 40.1950, 32.112, -2.220, 9.3966, 30.577, 14.7621),
	   country= c("Pakistan", "India", "Mexico", "Iran", "India", "Turkey", "India", "Kenya", "Ghana", "Egypt", "India"),
	   geo_from= FALSE
	)

	d <- merge(d, geo, by= c("location", "country"), all.x = TRUE)
	d$longitude[!is.na(d$long)] <- d$long[!is.na(d$long)]
	d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
	d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
	d$long <- d$lat <- d$geo_from <- NULL 

	### Fixing mistake on latitude and longitude


	### Gabura and Tulatuli is not located in India but in Bangladesh
	i <- grepl("India", d$country) & grepl("Gabura|Tulatuli", d$location)
	d$country[i] <- "Bangladesh"

	i <- grepl("Zimbabwe", d$country) & grepl("Hwedza|Mutasa", d$location)
	d$latitude[i] <- - abs(d$latitude[i])

	i <- grepl("United States", d$country) & grepl("Mississippi State University|Sampson County", d$location)
	d$longitude[i] <- - abs(d$longitude[i])


	i <- grepl("Bosnia and Herzegovina", d$country) & grepl("Northwest", d$location)
	d$longitude[i] <-  abs(d$longitude[i])


	i <- grepl("Argentina", d$country) & grepl("Pampas", d$location)
	d$longitude[i] <- - abs(d$longitude[i])
	d$latitude[i] <- - abs(d$latitude[i])


	## Fixing soil texture
	d$soil_texture <- gsub("_", " ", d$soil_texture)
	d$soil_texture <- gsub("^silty$", "silt", d$soil_texture)

	## Fixing country names
	P <- carobiner::fix_name(d$country, "title")
	P[grepl("China", P)] <- "China"
	P[grepl("Serbia", P)] <- "Serbia"
	P <- gsub("Phillipines", "Philippines", P)
	P <- gsub("Korea", "South Korea", P)
	d$country <- P


	### drop rows with missing yield
	d <- d[!is.na(d$yield),]

	## The missing values in longitude and latitude are due to the missing location in the raw data 

	carobiner::write_files(path, meta, d)

}


