# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"Maize (Zea mays L.) productivity has increased globally as a result of improved genetics and agronomic practices. Plant population and row spacing are two key agronomic factors known to have a strong influence on maize grain yield. A systematic review was conducted to investigate the effects of plant population on maize grain yield, differentiating between rainfall regions, N input, and soil tillage system (conventional tillage [CT] and no-tillage [NT]). Data were extracted from 64 peer-reviewed articles reporting on rainfed field trials, representing 13 countries and 127 trial locations. In arid environments, maize grain yield was low (mean maize grain yield = 2448 kg ha−1) across all plant populations with no clear response to plant population. Variation in maize grain yield was high in semiarid environments where the polynomial regression (p < 0.001, n = 951) had a maximum point at ∼140,000 plants ha−1, which reflected a maize grain yield of 9000 kg ha−1. In subhumid environments, maize grain yield had a positive response to plant population (p < 0.001). Maize grain yield increased for both CT and NT systems as plant population increased. In high-N-input (r2 = 0.19, p < 0.001, n = 2 018) production systems, the response of plant population to applied N was weaker than in medium-N-input (r2 = 0.49, p < 0.001, n = 680) systems. There exists a need for more metadata to be analyzed to provide improved recommendations for optimizing plant populations across different climatic conditions and rainfed maize production systems. Overall, the importance of optimizing plant population to local environmental conditions and farming systems is illustrated."
   
   uri <- "doi:10.5061/dryad.n73v54m"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
      data_organization = "SUN", # Stellenbosch University
      publication="doi:10.2135/cropsci2018.01.0003", 
      project=NA, 
      data_type= "compilation", 
      treatment_vars= "land_prep_method; row_spacing; plant_density", 
      response_vars = "yield", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-06-19",
      completion=100,
      notes="N_fertilizer applied is not recorded."
   )
   
   
   f <- ff[basename(ff) == "Appendix C. Supplementary data.xlsx"]

	locf <- function(x) {
		for (i in 2:length(x)) {
			if (is.na(x[i])) x[i] <- x[i-1]
		}
		x
	}
   
   ### process General information 
   r <- carobiner::read.excel(f, na=c("*"))
   d <- data.frame(
      trial_id= locf(r$ID),
      reference= locf(r$Reference), 
      location= r$`City /  Town / County`,
      adm1= r$`Province / state`,
      country= gsub("USA", "United States", r$Country), #
      lonLat= ifelse(grepl(";", r$Coordinates), NA , r$Coordinates), ### Keep rows with unique entries
      rain= as.numeric(ifelse(grepl(";", r$`Mean annual precipitation (mm)`), NA , gsub("  ", "", r$`Mean annual precipitation (mm)`))),
      planting_date=as.character(substr(r$`Year of trial`, 1, 4)), ## Not sure
      land_prep_method= tolower(ifelse(grepl("No tillage", gsub("-", " ", r$`Tillage system`)), "minimum tillage", 
                        ifelse(grepl("Rotary",gsub("-", " ", r$`Tillage system`)), "rotovating", 
                        ifelse(grepl("Conventional", gsub("-", " ", r$`Tillage system`)), "conventional", gsub("-", " ", r$`Tillage system`))))),
      plant_density= ifelse(is.na(r$`Harvest / final plant density (plants/ha)`) & !is.na(r$`Plant density (plants/ha)`) , r$`Plant density (plants/ha)`, r$`Harvest / final plant density (plants/ha)`),
      row_spacing= as.numeric(ifelse(grepl("row|;|Twin", r$`Row width (m)`), NA , r$`Row width (m)`))*100,#cm
      yield= as.numeric(r$`Grain yield (kg/ha)`),
      crop= "maize",
      on_farm= TRUE,
      is_survey= FALSE,
      yield_part= "grain",
      geo_from_source= TRUE,
      irrigated= FALSE ## From paper
      
   )
   
  d$plant_density <- as.numeric(ifelse(grepl(";|Various", d$plant_density), NA, d$plant_density))
  ## Remove missing values in row_spacing, as it's part of the treatment variable..
  d <- d[!is.na(d$row_spacing),] 
  
   ### Fixing longitude and latitude 
  P <- carobiner::fix_name(d$lonLat)
  P <- gsub(" S| W| E longitude| N latitude| N Lat.| E Long.|N latitude| N|N|E|  N|W|", "" , P)
  P <- gsub(" latitudeouth and | longitudeest", "", P)
  P <- gsub("Long. |Lat. ", "", P)
  d$lonLat <- P
  lonlat <- do.call(rbind, strsplit(d$lonLat, ","))
  d$lat <- lonlat[,1] 
  d$lon <- lonlat[,2] 
  
  ### latitude
  #d$lat <- gsub("42°52' – 42°59'|42°53' – 42°52'|42°51'–42°53'|27º52'50º18'|28°48'S and 52°77'", NA, d$lat) 
  P <- carobiner::fix_name(d$lat)
  P <- gsub("42°52' – 42°59'", "-42°56" , P)
  P <- gsub("42°53' – 42°52'", "-42°53", P)
  P <- gsub("42°51'–42°53'", "-42°52", P)
  P <- gsub("27º52'50º18'", "38°35", P)
  P <- gsub("28°48'S and 52°77'", "-28°48", P)
  d$lat <- P
  
  lat <- do.call(rbind, strsplit(d$lat, "°"))
  d$latitude <- as.numeric(lat[,1])+ (as.numeric(gsub(" |'|′", "",  substr(lat[,2], 1, 2))))/60
  
  ### longitude
  #d$lon <- gsub(" 76°36'–76°40'|28°48'S and 52°77'| 76°49'–76°46'|27º52'50º18'", NA, d$lon)
  P <- carobiner::fix_name(d$lon)
  P <- gsub("76°36'–76°40'", "-76°38" , P)
  P <- gsub("76°49'–76°46'", "-76°48", P)
  P <- gsub("42°51'–42°53'", "-42°52", P)
  P <- gsub("27º52'50º18'", "38°35", P)
  P <- gsub("28°48'S and 52°77'", "-52°77", P)
  d$lon <- P
  
  lon <- do.call(rbind, strsplit(d$lon, "°"))
  d$longitude <- as.numeric(lon[,1])+ (as.numeric(gsub(" |'|′", "",  substr(lon[,2], 1, 2))))/60
  d$lon <- d$longitude
  d$lat <- d$latitude
  
  ### Fixing  longitude and latitude
  i1 <- grepl("Argentina", d$country)
  i2 <- grepl("United States|Canada|Brazil", d$country)
  d$longitude[i1] <- -abs(d$longitude[i1])
  d$latitude[i1] <- -abs(d$latitude[i1])
  d$longitude[i2] <- -abs(d$longitude[i2])
  i <- grepl("Botswana", d$country)
  d$latitude[i] <- -abs(d$latitude[i])
  
  i <- grepl("Zimbabwe", d$country)
  d$latitude[i] <- -abs(d$lon[i])
  d$longitude[i] <- d$lat[i]
  
  i <- grepl("Pakistan", d$country)
  d$latitude[i] <- d$lon[i]
  d$longitude[i] <- d$lat[i]
  
  i <- grepl("China", d$country) &  grepl("Harbin",d$location)
  d$latitude[i] <- d$lon[i]
  d$longitude[i] <- d$lat[i]
  
  i <- grepl("China", d$country) &  grepl("Wenkou",d$location)
  d$latitude[i] <- 33.761
  d$longitude[i] <-  111.0873
  d$geo_from_source[i] <- FALSE
  
  i <- grepl("United States", d$country) &  grepl("Mt Holly",d$location)
  d$latitude[i] <- 35.2967
  d$longitude[i] <- -81.016
  d$geo_from_source[i] <- FALSE
  
  i <- grepl("Slovenia",d$country) &  grepl("Maribor",d$location)
  d$latitude[i] <- 46.554
  d$longitude[i] <- 15.644
  d$geo_from_source[i] <- FALSE
  
  i <- grepl("Canada",d$country) &  grepl("Ste. Anne de Bellevue",d$location)
  d$latitude[i] <- 45.432
  d$longitude[i] <- -73.933
  d$geo_from_source[i] <- FALSE
  
  
  i <- grepl("Argentina",d$country) &  grepl("La Plata",d$location)
  d$latitude[i] <- -34.9207
  d$longitude[i] <- -57.9539
  d$geo_from_source[i] <- FALSE
  
   geoc <- data.frame(
      location= c("Guelph", "Elora", "Lethbridge", "Burdett", "Woodstock", "Ottawa", "Isabela", "Tokat" ),
      lonc= c(-80.258, -80.4296, -112.8581, -116.6204, -80.7462, -75.729, -67.0226, 36.551),
      latc= c(43.546, 43.6811, 49.6877, 49.591, 43.1321, 45.4181, 18.500, 40.324),
      country= c(rep("Canada", 6), "Puerto Rico", "Turkey"),
      geo_from= FALSE
   )
   
   d <- merge(d, geoc, by= c("country","location"), all.x = TRUE)
   d$longitude[!is.na(d$lonc)] <- d$lonc[!is.na(d$lonc)] 
   d$latitude[!is.na(d$latc)] <- d$latc[!is.na(d$latc)]
   d$geo_from_source[!is.na(d$geo_from)] <-  d$geo_from[!is.na(d$geo_from)] 
   d$latc <-  d$lonc <- d$geo_from <- NULL
 
  i <- grepl("Brazil", d$country) &  grepl("Não-Me-Toque",d$location)
  d$latitude[i] <- -28.455
  d$longitude[i] <- -52.822
  d$geo_from_source[i] <- FALSE
  
  i <- grepl("Brazil", d$country) &  grepl("Lages",d$location)
  d$latitude[i] <- -27.801
  d$longitude[i] <- -50.338
  d$geo_from_source[i] <- FALSE
  
  i <- grepl("Brazil", d$country) &  grepl("Goiânia",d$location)
  d$latitude[i] <- -16.637
  d$longitude[i] <- -49.323
  d$geo_from_source[i] <- FALSE
  
  d$lon <- d$lat <- d$lonLat <- NULL

  geo <- data.frame(
      adm1= c("Montana", "North Dakota", "Colorado", "Illinois", "Iowa",  "Michigan" ,"Alabama" , 
              "Minnesota", "Missouri", "Virginia", "Wisconsin", "New York", "Wisconcin", "Maryland", "Iowa; California; Illinois"), 
      long= c(-109.3433, -100.4523, -105.5495, -89.4515, -93.1514, -84.5068, -86.7238, -94.5025, -92.4924, -78.9136, -89.723, -76.2152, -78.2254, -77.0332,  -93.1514),
      lat= c(46.6796, 47.4678, 39, 39.7386, 41.9395, 45.0034, 32.5762, 46.4419, 38.304, 37.2752, 44.8975, 42.7466, 38.0036,  39.28712,  41.9395),
      country= c(rep("United States", 15))
   )
                    
   
   d <- merge(d, geo, by= c("country", "adm1"), all.x = TRUE)
   
   d$longitude[!is.na(d$long)] <- d$long[!is.na(d$long)]
   d$latitude[!is.na(d$lat)] <-  d$lat[!is.na(d$lat)]
   d$long <- d$lat <- NULL
   
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   ### removing extreme value. appear probably during the data entries process   
   d <- d[!grepl("12560439560439500", d$yield), ]
   
   ### Duplicate rows; original data also has duplicates
   d <- unique(d)
   
   carobiner::write_files(path, meta, d)
   
}


