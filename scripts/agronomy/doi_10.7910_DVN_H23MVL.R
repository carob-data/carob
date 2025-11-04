# R script for "carob"


carob_script <- function(path) {
  
"Data sets included in the database evaluated Site-Specific Nutrient Management (SSNM) and Farmers’  Fertilizer Practice (FFP) on maize, rice and wheat in Africa and Asia. Information captured includes, with countries, cropping systems, crop type, seasons, climatic conditions, number of replications, number of nitrogen (N) splits, N, phosphorus (P) and potassium (K) fertilizer rates, grain yield, agronomic efficiency of N (AEN), partial factor of productivity N (PFP N), total fertilizer cost (TFC), gross return, and gross return above fertilizer cost (GRF).
(2020-10-27)   "
  
	uri <- "doi:10.7910/DVN/H23MVL"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication=NA,
		data_organization = "IRRI",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-06-02",
		data_type="compilation",
		project=NA,
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer"				
	)
  
  
	f <- ff[basename(ff) =="SSNM_Meta-analysis_data.csv"] 
  
	r <- read.csv(f,header=FALSE, fileEncoding="latin1")
	colnames(r) <- r[1,]
	r <- r[-1,] # drop the first rows

	#do not use numeric indexing!
	##d <- r[,c(4,7,8,9,10,14,15,17,28,29,31,32,34,35,38,41,43,44,49,53,57,62,64)]
	#colnames(d) <-  c("reference","country","location","longitude","latitude","soil_type","soil_pH","soil_SOC","previous_crop","crop", "water_mangement","variety","observation_date","season","land_prep_method","treatment","N_splits","N_fertilizer", "P_fertilizer","K_fertilizer","Zn_fertilizer","rep","yield")

	d <- data.frame(
		reference = r$Reference,
		country = r$Country,
		location = r$Site,
		longitude = r$Latitude,
		latitude = r$Longitude,
		soil_type = r$`Soil texture`,
		soil_pH = r$pH,
		soil_SOC = r$`OC (g/kg)`,
		previous_crop = r$`Previous crop`,
		crop = r$`Current Crop`,
		water_mangement = r$`Water management`,
		variety = r$Variety,
		observation_date = r$Year,
		season = r$Season,
		land_prep_method = r$Tillage,
		treatment = r$`Type of SSNM`,
		N_splits = r$`SSNM No. of N splits`,
		N_fertilizer = r$`SSNM Average N (kg/ha)`,
		 P_fertilizer = r$`SSNM Average P (kg/ha)`,
		K_fertilizer = r$`SSNM Average K (kg/ha)`,
		Zn_fertilizer = r$`SSNM Zn fertilizer rate (kg/ha)`,
		rep = r$Replicates,
		yield = r$`SSNM Grain yield (kg/ha)`
	)

  
  d[c('planting_date', 'harvest_date')] <- stringr::str_split_fixed(d$observation_date, "-", 2)   
  d[c('latitude1', 'Latitude2')] <- stringr::str_split_fixed(d$latitude, "\u0096", 2)
  d[c('longitude1', 'Longitude2')] <- stringr::str_split_fixed(d$longitude, "\u0096", 2)
  
  d[c('soil_pH', 'soil_pH1')] <- stringr::str_split_fixed(d$soil_pH, "-", 2)
  
  d[c('soil_SOC', 'soil_SOC1')] <- stringr::str_split_fixed(d$soil_SOC, "-", 2)
 
  d$k <- d$harvest_date
  d$harvest_date <- ifelse((d$planting_date=="2006 and 2007")& is.na(d$harvest_date),"2007",
                     ifelse((d$planting_date=="2005 and 2006")& is.na(d$harvest_date),"2006",d$k))
  #fix start date 
  p <- carobiner::fix_name(d$planting_date)
  p <- gsub("2006 and 2007","2006",p)
  p <- gsub("2005 and 2006","2005",p)
  d$planting_date <- p
  
  d[d==""] <- NA
  rownames(d) <- NULL
  lat <- trimws(d$longitude1)
  lon <- trimws(d$latitude1)
  # Fix long and lat columns
#RH?  d <- d[c(361,362),]
  
  lon <- gsub("\n46\u0092", "46'", lon)
  lon <- gsub("\002", "", lon)
  lon <- gsub("75°02' E to 77°04' E", "76°03'E", lon)
  lon <- gsub(" ", "", lon)
  lon <- gsub("°","-", lon)
  lon <- gsub("'","-", lon)
  lon <- gsub("\\?","-", lon)
  lon <- stringr::str_split_fixed(lon, "-", 3)
  lon[lon[,2]=="", 2] <- NA
  d$longitude = as.numeric(lon[,1]) + as.numeric(lon[,2])/60
  d$longitude <- ifelse(lon[,3] == "W", -d$longitude, d$longitude)
  
  lat <- gsub("29°07' N to 30°08' N", "29°33' N", lat)
  lat <- gsub("\u0092", "'",lat)
  lat <- gsub(" ", "", lat)
  lat <- gsub("°","-", lat)
  lat <- gsub("'N","", lat)
  lat <- gsub("-N","", lat)
  lat <- gsub("'","", lat)
  lat <- gsub("\\?N", "", lat)
  lat <- stringr::str_split_fixed(lat, "-", 2)
  lat[lat[,2]=="", 2] <- NA
  d$latitude = as.numeric(lat[,1]) + as.numeric(lat[,2])/60
  
  # extract relevant columns 
  d <- d[c("reference","country","location","longitude","latitude","crop","previous_crop","variety","yield","land_prep_method", "N_fertilizer", "P_fertilizer","K_fertilizer","N_splits","planting_date","harvest_date","season","soil_pH",'soil_type',"soil_SOC")]
  # Add columns
  
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  # fill missing data in long and lat columns

  yx <- c("latitude", "longitude")
  d$location <- gsub("\n", "",  d$location)
  locs <- data.frame(
	location=c("Anhui (AH)", "Guangdong", "Guangdong (GD)","Guangdong, Hunan, Jiangsu, Hubei", "Hebei","Hubei (HB)","Hubei, An'hui"  ,"Heilongjiang","Heilongjiang (HLJ)","Heilongjiang and Jilin provinces","Heilongjiang, Jilin","Henan","Hunan (HN)","Hunan, Guangdong, Jiangxi","Jiangsu","Jiangxi (JX)", "Jiaxing", "Jilin", "Jilin (JL)","Jilin Province Northeast China","Jinhua-Quzhou Basin in Central Zhejiang","Jinhua, Jin-Qu, Zheijang","Liaoning","Shandong","Shanxi","Tianmushan in Jinhua", "Xushan Village, Xiaonan County, Hubei Province","Zhejiang","East Java, Lampung, North Sumatra, South Sulawesi","Grobogan, Central Java (GRO)","Jeneponto, South Sulawesi (SUL)","Kediri, East Java (EJA)","Lampung Tengah, Lampung (LAM)","Sukamandi, West Java","Tanah Karo, North Sumatra (SUM)","An Giang, Mekong River Delta","Binh Tay, Mekong Delta","Binh Thanh, Mekong Delta","Can Tho, Mekong River Delta", "Cau Ke, Tra Vinh (TRA)","Chau Phu An Giang, Mekong River Delta","CuM\u0092gar, Dak Lak (DAK)","Dinh Mon, Mekong Delta","Go Cong Tay Tien Giang, Mekong River Delta","Hanoi, Red River Delta","Mai Son, Son La (SON)","Omon Cantho, Mekong River Delta","Omon, Mekong Delta","Tan Chau, An Giang (ANG)","Tien Giang, Mekong River Delta","Trang Bang, Tay Ninh (TAY)","Vinh Phuc & Bac Giang (DEG)","Aduthurai, Cauvery Delta, Tamil Nadu","Andhra Pradesh","Bihar","Bihar, Haryana, Karnataka, Punjab, Uttar Pradesh","Control villages in Nadia, West Bengal, India","East and south eastern coastal plain - Kendrapara and Puri, Odisha","Gurdaspur, Hoshiarpur, Ludhiana, Patiala, Faridkot, and Firozpur in Punjab province in Northwest India","Haryana","Haryana, Punjab","Intervention villages in Nadia, West Bengal, India","Karnal, Kurukshetra, Kaithal, Ambala,Yamunanagar, Panipat, and Sonepat districts of Haryana","Karnataka","Kumbakonam, Tamil Nadu","Mewat, Haryana", "Mid central table land - Dhenkanal, Odisha","Needamangalam, Tamil Nadu","Norman E. Borlaug Crop Research Centre of G.B. Pant University of Agriculture and Technology, Pantnagar","North central plateau - Keonjhar and Mayurbhanj, Odisha", "North eastern coastal plain - Balasore, Bhadrak and Jajpur, Odisha","North western plateau - Sundergarh","Odisha","Old and New Cauvery Delta in Thanjavur District","Orathanadu, Tamil Nadu","Papanasam, Tamil Nadu","Pattukkottai, Tamil Nadu","Peravurani, Tamil Nadu","Punjab","Site 1 in Siruguppa, Bijapur, and Navalgund Talukas of Northern Karnataka","Tamil Nadu","Thanjavur, Cauvery Delta, Tamil Nadu","Thiruvaiyaru, Tamil Nadu", "Thiruvonam, Tamil Nadu","Uttar Pradesh","Uttarakhand","West Bengal","Western central table land - Sambalpur","Damak and Gauradaha in Jhapa district","Itahara and Babiyabirta municipalities in Morang district of eastern Nepal","Lamjung Campus, Sundarbazar", "Ilagan, Isabela (ISA)","Ilocos Norte/Sur, La Union, Pangasinan, Isabela, Zambales, Nueva Ecija, Bulacan","IRRI farm", "La Union, Pangasinan, Zambales, Bulacan","Malaybalay, Bukidnon (BUK)", "Maligaya, Nueva Ecija, Central Luzon","Munoz, Nueva Ecija, Central Luzon", "Nueva Ecija and Tarlac","Pangasinan and Tarlac","Pangasinan, Laguna, Occidental Mindoro, Iloilo, Negros Occidental, Davao, Maguindanao","Sto. Domingo, Nueva Ecija, Central Luzon","Gazipur and Comilla","Kpong","Senegal River delta or middle valley"),
	lon = c(117.226,  113.324, 113.324, 113.324, 111.7091, 112.2708, 112.2708, 127.6447,  127.6447, 127.6447, 127.6447, 113.6143, 111.7091, 111.7091, 119.8092,  115.7222, 120.8863, 126.1887, 126.1887, 126.1887, 119.9521, 119.9521,  122.488, 118.7169, 112.2894, 119.4713, 102.4791, 120.6636, 105.2176,  110.927, 119.6985, 112.0153, 105.2176, 107.6222, 98.4416, 105.1827,  107.0772, 105.5763, 105.5306, 106.061, 108.2351, 108.2078, 105.6127,  106.6563, 105.7001, 104.017, 105.6186, 105.6186, 105.1866, 106.369,  106.3375, 105.5594, 79.4582, 79.9629, 85.6097, 85.6097, 88.5152,  86.6098, 75.3167, 76.3398, 76.2043, 88.3213, 76.8968, 76.1675,  79.3754, 77.0495, 85.5237, 79.4855, 79.4832, 86.4084, 86.9217,  84.0362, 84.4261, 79.0504, 79.2479, 79.2544, 79.3352, 79.2047,  75.415, 76.8939, 78.4083, 79.0504, 79.0138, 76.9283, 80.5657,  79.2042, 88.3213, 84.2736, 87.6767, 87.6345, 84.4162, 121.8886,  120.1673, 121.2624, 121.0197, 125.0185, 120.599, 124.8836, 121.0602,  120.1673, 121.35, 121.014, 89.1595, 0.0604, -16.3075), 
	lat = c(31.826,  22.913, 22.913, 22.913, 27.6097, 30.9759, 30.9759, 47.9849, 47.9849,  47.9849, 47.9849, 33.8818, 27.6097, 27.6097, 33.0853, 27.6141,  30.6017, 43.6669, 43.6669, 43.6669, 29.1183, 29.1183, 40.9893,  36.4825, 37.5717, 30.2738, 35.8469, 29.2188, -5.1232, -7.1169,  -5.5674, -7.826, -5.1232, -6.7177, 2.9906, 10.5113, 10.349, 9.6998,  10.1145, 9.8697, 16.0596, 12.8243, 10.0546, 10.4056, 20.9991,  21.1682, 10.1087, 10.1087, 10.8009, 10.3801, 11.0794, 21.3618,  11.0847, 15.7538, 25.6792, 25.6792, 23.4749, 20.5528, 32.1667,  29.1978, 29.959, 22.5157, 29.6885, 14.7102, 10.9573, 27.9854,  20.8266, 10.7247, 29.0284, 21.8954, 21.5017, 22.1182, 20.5118,  10.7511, 10.588, 10.8525, 10.4074, 10.2547, 30.8425, 15.6336,  11.0147, 10.7511, 10.8584, 8.5769, 26.9232, 30.1569, 22.5157,  21.4537, 26.6729, 26.6603, 28.126, 17.1486, 16.0814, 14.1516,  14.9558, 8.0172, 15.44, 11.7806, 14.3551, 16.0814, 14.2465, 15.6207,  22.4389, 6.1574, 13.9842)
	)


  d <- merge(d, locs, by="location", all.x=TRUE)
	i <- is.na(d$longitude)
	d$longitude[i] <- d$lon[i]
	i <- is.na(d$latitude)
	d$latitude[i] <- d$lat[i]
   d$lat <- d$lon <- d$reference <- NULL
   d$geo_from_source <- TRUE
   d$geo_from_source[i] <- FALSE
    # fix soil_SOC range and fertilizer
    d$soil_SOC[d$soil_SOC>20] <- NA
    d$N_fertilizer[is.na(d$N_fertilizer)] <- 0
    d$P_fertilizer[is.na(d$P_fertilizer)] <- 0
    d$K_fertilizer[is.na(d$K_fertilizer)] <- 0
    # remove rows without location
    d <- d[!is.na(d$location),]
   # fix error in long and lat 
    d$longitude[d$location=="Alukdia"] <- 88.82096
    d$latitude[d$location=="Alukdia"] <- 23.6578
    d$longitude[d$location=="Badarganj"] <- 89.1808277
    d$latitude[d$location=="Badarganj"] <- 25.7071259
    d$longitude[d$location=="Chandbill"] <- 90.2934413
    d$latitude[d$location=="Chandbill"] <- 24.4769288
    d$longitude[d$location=="Jhapa "] <- 87.885701
    d$latitude[d$location=="Jhapa "] <- 26.5837354
    d$longitude[d$location=="Grobogan, Central Java (GRO)"] <- 110.8966767
    d$latitude[d$location=="Grobogan, Central Java (GRO)"] <- -7.0980947
    d$longitude[d$location=="Ilocos Norte/Sur, La Union, Pangasinan, Isabela, Zambales, Nueva Ecija, Bulacan"] <- 121
    d$latitude[d$location=="Ilocos Norte/Sur, La Union, Pangasinan, Isabela, Zambales, Nueva Ecija, Bulacan"] <- 15.583333
    d$longitude[d$location=="Jeneponto, South Sulawesi (SUL)"] <- 120.2948856
    d$latitude[d$location=="Jeneponto, South Sulawesi (SUL)"] <- -1.9758004
    d$longitude[d$location=="Kediri, East Java (EJA)"] <- 112.0046051
    d$latitude[d$location=="Kediri, East Java (EJA)"] <- -7.8111057
    d$longitude[d$location=="Lampung Tengah, Lampung (LAM)"] <- 105.0272986
    d$latitude[d$location=="Lampung Tengah, Lampung (LAM)"] <- -4.8555039
    d$longitude[d$location=="Liaoning" ] <- 122.9955469
    d$latitude[d$location== "Liaoning"] <- 40.9975197
    d$longitude[d$location=="Pangasinan and Tarlac" ] <- 120.4964091
    d$latitude[d$location=="Pangasinan and Tarlac"] <- 15.4937252
    d$longitude[d$location=="Sukamandi, West Java"  ] <- 107.6221628
    d$latitude[d$location=="Sukamandi, West Java" ] <- -6.7177474
    d$longitude[d$location=="Trang Bang, Tay Ninh (TAY)"] <- 106.3623675
    d$latitude[d$location=="Trang Bang, Tay Ninh (TAY)"] <- 11.0315517
    # fix crop name 
  p <- carobiner::fix_name(d$crop, "lower")
  d$crop <- p
  p1 <- carobiner::fix_name(d$previous_crop,"lower")
  p1 <- gsub("upland crop", "unknown", p1)
  p1 <- gsub("mungbean","mung bean",p1)
  d$previous_crop <- p1
  #fix white space in soil_SOC column
  e <- carobiner::fix_name(d$soil_type)
  e <- gsub(" ",NA,e)
  d$soil_type <- e
  #data type
  d$yield <-  (as.numeric(d$yield)) 
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  d$N_fertilizer <- as.double(d$N_fertilizer)
  d$P_fertilizer <- as.double(d$P_fertilizer)
  d$K_fertilizer <- as.double(d$K_fertilizer)
  d$N_splits <- as.integer(d$N_splits)
  d$soil_pH <- as.double(d$soil_pH)
  d$soil_SOC <- as.double(d$soil_SOC)
  d$yield_part <- "grain"
	
	d$trial_id <- as.character(1:nrow(d))
	#fixing land prep method
	d$land_prep_method <- ifelse(grepl("Conventional tillage", d$land_prep_method),"conventional", "none")  
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

  carobiner::write_files(meta, d, path=path)
  
}

                 



