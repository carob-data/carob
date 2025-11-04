# R script for "carob"

carob_script <- function(path) {
  
"Estimates of crop nitrogen (N) uptake and offtake are critical in estimating N balances, N use efficiencies and potential losses to the environment. Calculation of crop N uptake and offtake requires estimates of crop product yield (e.g. grain or beans) and crop residue yield (e.g. straw or stover) and the N concentration of both components. Yields of crop products are often reasonably well known, but those of crop residues are not. While the harvest index (HI) can be used to interpolate the quantity of crop residue from available data on crop product yields, harvest indices are known to vary across locations, as do N concentrations of residues and crop products. The increasing availability of crop data and advanced statistical and machine learning methods present us with an opportunity to move towards more locally relevant estimates of crop harvest index and N concentrations using more readily available data. This dataset includes maize field experiment data. It is a culmination of summary statistic data collected from the literature as well as raw data requested from various researchers and organisations from around the world. These data will enable more locally relevant estimates of crop nutrient offtake, nutrient balances and nutrient use efficiency at national, regional or global levels, as part of strategies towards more sustainable nutrient management."
  
	uri <- "doi:10.5061/dryad.j3tx95xhc"
	group <- "agronomy"
	ff	 <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		publication= "doi:10.1016/j.fcr.2022.108578",
		data_organization = "WUR;APNI;IFA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-03-08",
		data_type="compilation",
		project=NA,
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer"
	)
	
	f <- ff[basename(ff) == "MAIZE_DATA_HI_CPN_CRN_FIELD_CROPS_RESEARCH_2022.csv"][1]
	e <- ff[basename(ff) == "DATA_ID_MAIZE_DATA_HI_CPN_CRN_FIELD_CROPS_RESEARCH_2022.csv"][1]
	
	d <- read.csv(f, fileEncoding="latin1")
	dd <- read.csv(e, fileEncoding="latin1")
	
	d <- merge(d,dd,by = "Data_id")

	
	x <- data.frame(reference=d$Publication)
	x$on_farm <- FALSE
	x$is_survey <- FALSE
	x$irrigated <- d$Water_regime == "Irrigated"
	## the treatment code	
	# x$reference <- d$Publication
	x$trial_id <- paste0("lud_", as.integer(as.factor(d$Publication)))
	
	x$country <- d$Country
	x$country[x$country == "Côte d\u0092Ivoire"] <- "Côte d'Ivoire"	
	x$location <- carobiner::fix_name(d$Site, "title")
	x$adm1 <- carobiner::fix_name(d$Region_province, "title")

	## each site must have corresponding longitude and latitude
	x$longitude <- d$GPS_long_DD
	x$latitude <- d$GPS_lat_DD
	x$geo_from_source <- TRUE

	x$crop <- tolower(d$Crop)
	
	## Fertilizers 
	# Converting from kg/m2 -> kg/ha
	x$P_fertilizer <- d$FP * 10000 
	x$K_fertilizer <- d$FK * 10000
	x$N_fertilizer <- d$FN * 10000
	#Fertilizer type not specified
	x$fertilizer_type <- "unknown"
	
	# Megagram to kilogram
	x$yield <- d$CPY * 1000 
	x$yield_part <- "grain"
	
	i <- which(is.na(x$reference) & x$adm1 == "Veracruz")
	x$longitude[i] <- -x$longitude[i]

	i <- which(x$reference == "Khalafi et al 2021" & x$country=="Iran" & x$adm1 == "Khuzestan")
#	x$location==Dezful Khuzestan
	temp <- x$longitude[i]
	x$longitude[i] <- x$latitude[i]
	x$latitude[i] <- temp
	
	i <- x$reference == "Van Reuler Janssen 1996"
	# Tai, CdI
	x$longitude[i] <- -7.4669
	x$latitude[i] <- 5.8721

	i <- which(x$adm1 == "Bihar" & x$longitude < 1)
	x$longitude[i] <- 86

	i <- which(x$adm1 == "Bihar" & x$latitude > 94)
	x$latitude[i] <- 84.7

	i <- which(x$country == "India" & x$location == "Chowk Azam")
	x$country[i] <- "Pakistan"
	x$latitude[i] <- 31.2

	i <- which(x$country == "India" & x$location == "Multan")
	x$country[i] <- "Pakistan"


	i <- which(x$reference == "Setiyono et al 2010" & x$country == "Ethiopia" & x$longitude < 10)	
	temp <- x$longitude[i]
	x$longitude[i] <- x$latitude[i]
	x$latitude[i] <- temp

	i <- which(x$reference == "Setiyono et al 2010" & x$adm1 == "Maguindanao" & x$location=="Margues")
	x$longitude[i] <- 124.3
	x$latitude[i] <- 7.136
	
	i <- which(x$reference == "Setiyono et al 2010" & x$adm1 == "Bihar" & x$location=="Begusarai" & x$longitude > 94)
	x$longitude[i] <- 86.14


## ????? why remove? 
#	i <- which(x$reference == "Setiyono et al 2010" & x$adm1 == "Bihar" & is.na(x$location) & x$latitude > 35)
#	x <- x[-i,]

#	i <- which(x$reference == "Setiyono et al 2010" & x$adm1 == "North Vietnam")
#	x <- x[-i,]

#	i <- which(x$longitude == -44.84954 & x$latitude == -21.97502)
#	x <- x[-i,]
### ????? 

	i <- which(x$reference == "Setiyono et al 2010" & x$adm1 == "Bukidnon" & x$location=="Crossing Poblacion")
	x$longitude[i] <- 124.80
	x$latitude[i] <- 8.32
	x$geo_from_source[i] <- FALSE
	i <- which(x$reference == "Setiyono et al 2010" & x$adm1 == "Bukidnon" & x$location=="Sta. Ana")
	x$longitude[i] <- 124.805
	x$latitude[i] <- 8.538
	x$geo_from_source[i] <- FALSE
	i <- which(x$reference == "Setiyono et al 2010" & x$adm1 == "Bukidnon" & x$location=="Kisolon")
	x$longitude[i] <- 124.974
	x$latitude[i] <- 8.334
	x$geo_from_source[i] <- FALSE
	i <- which(x$reference == "Setiyono et al 2010" & x$adm1 == "Bukidnon" & x$location=="Calatcat")
	x$longitude[i] <- 124.476
	x$latitude[i] <- 8.554
	x$geo_from_source[i] <- FALSE
	i <- which(x$reference == "Setiyono et al 2010" & x$adm1 == "Leyte" & x$location=="Maslug")
	x$longitude[i] <- 124.7697
	x$latitude[i] <- 10.5786
	x$geo_from_source[i] <- FALSE
	
	# the locations for Rakshaskhali, India that are flagged as not on land
	# are OK, not in the ocean (needs to be fixed in GADM)
  
    # EGB:
    #message("1492 records are missing yield. Should it be removed?")
	x <- x[!is.na(x$yield), ]
	x$planting_date <- as.character(NA)

	x <- unique(x)
	x$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, x, path=path)
}
