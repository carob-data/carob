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
		data_organization = "IOAP; KIOT",#IOAP: Institute of Atmospheric Physics; KIOT:Karlsruhe Institute of Technology
		publication = "doi:10.1111/gcb.17177",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-02",
		carob_completion = 0,	
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
	
  lon_lat_to_dec <- function(x) {
    
    x <- trimws(x)
    # Standardize symbols
    x <- gsub("[′’`]", "'", x)
    x <- gsub("[″“”]", '"', x)
    x <- gsub("''", '"', x)
    x <- gsub("Ν", "N", x)
    x <- toupper(x)
    sapply(x, function(z) {
      
      if (is.na(z) || z == "") return(NA_real_)
      
      hemi <- ifelse(grepl("[SW]", z), -1, 1)
      
      nums <- as.numeric(unlist(regmatches(z, gregexpr("[0-9]+\\.?[0-9]*", z))))
      deg <- min <- sec <- 0
      if (length(nums) == 1) {
        deg <- nums[1]
        
      } else if (length(nums) == 2) {
        if (grepl('"', z))
          sec <- nums[2]
        else
          min <- nums[2]
        
      } else {
        
        deg <- nums[1]
        min <- nums[2]
        sec <- nums[3]
        
      }
      
      # normalize values
      min <- min + sec %/% 60
      sec <- sec %% 60
      deg <- deg + min %/% 60
      min <- min %% 60
      hemi * (deg + min/60 + sec/3600)
    })
  }
  
  d$latitude <- round(lon_lat_to_dec(d$latitude),4)
  d$longitude <- round(lon_lat_to_dec(d$longitude), 4)
  
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
  
  d$soil_SOC <- d$soil_SOC/10 # %
  d$soil_N_total <- d$soil_N_total*1000 # ppm
  
  ### solve conflict countries issue
  i <- grepl("Telangana State, India", d$location)
  d$country[i] <- "India"
  geo <- data.frame(
    location = c("Nebraska.USA", "Reese ,USA", "Lonoke County, Arkansas,USA", "Dingxi City, Gansu Province", "Baitu Town, Jurong City, Jiangsu Province, China", "Changsha Hunan Province, China", "Hubei Province,China", "Hue province, Central Vietnam", "Jurong City, Jiangsu Province, China", "Mvomero, Morogoro, Tanzania", "Hubei Province, China","Akita Prefecture,Japan", "Alberta,Canada", "Almudévar,Spain", "Anhui Province, China", "Anhui,China", "Anhui.China", "Arezzo province, Italy", "Arkaute, Spain"),
    lon = c(-99.193, -88.846, -92.4426, 102.152, 118.513, 112.947, 111.129, 107.441, 119.165, 37.448, 111.085, 140.282, -115.446, -0.5814, 117.6813, 117.6813, 117.6813, 11.88, -2.635),
    lat = c(41.593, 43.305, 34.732, 38.4308, 34.629, 28.244, 31.773, 16.393, 31.956, -6.301, 31.773,39.736, 56.512, 42.0434, 30.995, 30.995, 30.995, 43.462, 42.851),
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

