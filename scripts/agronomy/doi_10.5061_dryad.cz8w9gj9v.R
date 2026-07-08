# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
A global meta-analysis of yield-scaled N2O emissions and its mitigation efforts for maize, wheat, and rice

Maintaining or even increasing crop yields while reducing nitrous oxide (N2O) emissions is necessary to reconcile food security and climate change, while the metric of yield-scaled N2O emission (i.e., N2O emissions per unit of crop yield) is at present poorly understood. Here we conducted a global meta-analysis with more than 6000 observations to explore the variation patterns and controlling factors of yield-scaled N2O emissions for maize, wheat, and rice and associated potential mitigation options. Our results showed that the average yield-scaled N2O emissions across all available data followed the order wheat (322 g N Mg-1, with the 95% confidence interval (CI): 301-346) &gt; maize (211 g N Mg-1, CI: 198-225) &gt; rice (153 g N Mg-1, CI: 144-163). Yield-scaled N2O emissions for individual crops were generally higher in tropical or subtropical zones than in temperate zones, and also showed a trend towards lower intensities from low to high latitudes. This global variation was better explained by climatic and edaphic factors than by N fertilizer management, while their combined effect predicted more than 70% of the variance. Furthermore, our analysis showed a significant decrease in yield-scaled N2O emissions with increasing N use efficiency or in N2O emissions for production systems with cereal yields &gt; 10 Mg ha-1 (maize), 6.6 Mg ha-1 (wheat) or 6.8 Mg ha-1 (rice), respectively. This highlights that N use efficiency indicators can be used as valuable proxies for reconciling trade-offs between crop production and N2O mitigation. For all three major staple crops, reducing N fertilization by up to 30%, optimizing the timing and placement of fertilizer application or using enhanced-efficiency N fertilizers significantly reduced yield-scaled N2O emissions at similar or even higher cereal yields. Our data-driven assessment provides some key guidance for developing effective and targeted mitigation and adaptation strategies for the sustainable intensification of cereal production.
"

	uri <- "doi:10.5061/dryad.cz8w9gj9v"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=NA,
		data_organization = "IAPC; KIOT",
		publication = "doi:10.1111/gcb.17177",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-02",
		carob_completion = 100,	
		carob_effort = 4
	)
	
	f1 <- ff[basename(ff) == "data_used_in_the_meta_anlysis.xlsx"]
	f2 <- ff[basename(ff) == "README.md"]
	
	sheets <- c("Maize", "Wheat", "Rice")  
	proc <- function(f){
	  r1 <- suppressWarnings(carobiner::read.excel(f1, sheet= f))
	  data.frame(
	    reference = r1$Reference,
	    location = r1$Location,
	    country = r1$Country,
	    longitude = r1$`Longitude (E,W)`,
	    latitude = r1$`Latitude (N,S)`,
	    temp = r1$`MAT (°C)`,
	    rain = r1$`MAP (mm)`,
	    soil_sand = r1$`Soil properties`,
	    crop = tolower(r1$`Crop type`),
	    rep = r1$Replicates,
	    N_fertilizer = r1$`N rate (kg N ha-1)`,
	    irrigation_method = tolower(r1$`Water regime`),
	    irrigated = !is.na(r1$`Water regime`),
	    land_prep_method = gsub("no-tillage", "none", tolower(r1$`Tillage type`)),
	    N2O_emission = r1$`Cumulative N2O fluxes (kg N ha-1)`,
	    yield = r1$`Grain yield (Mg ha-1)`,
	    soil_silt = r1$...16,
	    soil_clay = gsub("10~20", NA, r1$...17),
	    soil_texture = gsub("soil texture", NA, tolower(r1$...18)),
	    soil_SOC = gsub("<10", NA, r1$...19),
	    soil_N_total = r1$...21,
	    soil_pH = r1$...25,
	    soil_bd = r1$...27,
	    trial_id = paste(r1$Continent, f, sep = "-"), 
	    on_farm = NA, 
	    is_survey = FALSE, 
	    yield_part = "grain", 
	    yield_moisture = NA_real_, 
	    geo_from_source = TRUE, 
	    planting_date = NA_character_,
	    yield_isfresh = NA
	  )
	}	
	
	d <- lapply(sheets, proc)
	d <- do.call(rbind, d)[-c(1, 2078, 3528),]
	d$yield <- d$yield*1000
	d$rep <- as.integer(d$rep)
	
	dms_to_decimal <- function(x) {
	  sapply(x, function(z) {
	    if (is.na(z) || trimws(z) == "") return(NA_real_)
	    # Already numeric
	    if (is.numeric(z)) return(z)
	    z <- toupper(trimws(z))
	    # Standardize symbols
	    z <- gsub("[′’]", "'", z)
	    z <- gsub("[″“”]", '"', z)
	    z <- gsub("[º˚]", "°", z)
	    # Hemisphere
	    sign <- ifelse(grepl("[SW]", z), -1, 1)
	    # Extract all numbers
	    nums <- as.numeric(unlist(regmatches(z, gregexpr("[0-9]+\\.?[0-9]*", z))))
	    if (length(nums) == 0) return(NA_real_)
	    deg <- nums[1]
	    min <- ifelse(length(nums) >= 2, nums[2], 0)
	    sec <- ifelse(length(nums) >= 3, nums[3], 0)
	    # Correct impossible values
	    if (min >= 60) {
	      deg <- deg + floor(min / 60)
	      min <- min %% 60
	    }
	    if (sec >= 60) {
	      min <- min + floor(sec / 60)
	      sec <- sec %% 60
	    }
	    sign * (deg + min/60 + sec/3600)
	  })
	}
	
	d$latitude <- round(dms_to_decimal(d$latitude),4)
	d$longitude <- round(dms_to_decimal(d$longitude), 4)
	
	## Fixing land_prep_method
	d$land_prep_method <- gsub("reduce-tillage|reduced-tillage","reduced tillage",  d$land_prep_method)
	### country name
	d$country <- gsub("USA", "United States", d$country)
	d$country <- gsub("Indonesian", "Indonesia", d$country)
	d$country <- gsub("Korea", "South Korea", d$country)
	
	### Fixing irrigation method
	P <- carobiner::fix_name(d$irrigation_method)
	P <- gsub("conventional irrigation|water-saving irrigation", "unknown", P)
	P <- gsub("flood irrigation", "flood", P)
	d$irrigation_method <- P
	
	cov_Numeric <- function(x) {
	  
	  col <- grep("soil", names(x), value = TRUE)
	  col <- setdiff(col, "soil_texture")
	  
	  x[col] <- lapply(x[col], as.numeric)
	  
	  x
	}
	
	
	d <- cov_Numeric(d)
	
	d$soil_SOC <- d$soil_SOC/10 # from g/kg to %
	d$soil_N_total <- d$soil_N_total/1000 #  from g/kg to ppm
	
	### solve conflict countries issue
	i <- grepl("Guangzhou, China|Guangzhou, Guangdong province, China", d$location)
	d$country[i] <- "China"
	
	i <- grepl("Telangana State, India", d$location)
	d$country[i] <- "India"
	d$location <- ifelse(grepl("Changshu", d$location), "Changshu", d$location)
	d$location <- ifelse(grepl("Jiangsu", d$location), "Jiangsu", d$location)
	geo <- data.frame(
	  location = c("Nebraska.USA", "Reese ,USA", "Lonoke County, Arkansas,USA", "Dingxi City, Gansu Province", "Baitu Town, Jurong City, Jiangsu Province, China", "Changsha Hunan Province, China", "Hubei Province,China", "Hue province, Central Vietnam", "Jurong City, Jiangsu Province, China", "Mvomero, Morogoro, Tanzania", "Hubei Province, China","Akita Prefecture,Japan", "Alberta,Canada", "Almudévar,Spain", "Anhui Province, China", "Anhui,China", "Anhui.China", "Arezzo province, Italy", "Arkaute, Spain","Beijing,China", "Ontario, Canada", "Woodslee, ON, Canada", "Woodslee, ON,Canada", "Bangladesh", "Changshu", "Jiangsu", "Shanghai Municipality, China", "Samutsakorn,Thailand", "Chongming Island, Shanghai, China"),
	  lon = c(-99.193, -88.846, -92.4426, 102.152, 118.513, 112.947, 111.129, 107.441, 119.165, 37.448, 111.085, 140.282, -115.446, -0.5814, 117.6813, 117.6813, 117.6813, 11.88, -2.635, 116.419, -86.994, -82.713, -82.713, 89.942, 120.7341, 119.809, 121.509, 100.230, 121.567),
	  lat = c(41.593, 43.305, 34.732, 38.4308, 34.629, 28.244, 31.773, 16.393, 31.956, -6.301, 31.773,39.736, 56.512, 42.0434, 30.995, 30.995, 30.995, 43.462, 42.851,39.926, 51.588, 42.201, 42.201, 24.180, 31.674, 32.515, 31.262, 13.564, 31.645),
	  geo_from = FALSE
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	d$longitude[!is.na(d$lon)] <- d$lon[!is.na(d$lon)]
	d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
	d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
	d$lon <- d$lat <- d$geo_from <- NULL
	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	d <- unique(d) 
	carobiner::write_files(path, meta, d)
}

