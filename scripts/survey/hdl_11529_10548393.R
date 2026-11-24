# R script for "carob"

carob_script <- function(path) {

"
Replicated crop-cuts from farmers' fields at harvest at multiple locations in Tanzania (180 to 2400 m), and Soil analysis from 0-20 cm and 20-50 cm depths from crop-cut fields in Southern Highlands, Eastern and Northern Zones of Tanzania in 2015.
"
	uri <- "hdl:11529/10548393"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-08-22",
		carob_effort = NA,
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "TAMASA_TZ_CC_Soil_2015 (Blurred).xlsx"]
	f2 <- ff[basename(ff) == "TAMASA_TZ_CC_Yield_2015 (Blurred).xlsx"]
  r1 <- carobiner::read.excel(f1, sheet ="Data")
  r2 <- suppressWarnings(carobiner::read.excel(f2, sheet ="Data"))
  
  #remove last rows with summary stats 
  r1 <- r1[which(r1$Country == "Tanzania"), ]
  r2 <- r2[which(r2$Country == "Tanzania"), ]
  
	soil <- data.frame(
		country = r1$Country,
		adm1=r1$Region,
		adm2=r1$District,
		adm3=r1$Ward,
		location =r1$Village,
		trial_id=r1$FarmID,
		depth = r1$Depth,
		soil_C=r1$C,
		soil_pH=r1$pH,
		soil_Al=r1$Al,
		soil_Ca= r1$Ca,
		soil_P_Mehlich= r1$P,
		soil_S = r1$S,
		soil_Mn = r1$Mn,
		soil_Zn = r1$Zn,
		soil_K = r1$K,
		soil_Na = r1$Na,
		soil_Mg = r1$Mg,
		soil_Fe = r1$Fe,
		soil_B = r1$B,
		soil_N = r1$N,
		soil_EC = r1$EC.S,
		date = r1$Date
	)
	i <- grepl("^4", soil$date)
	soil$date[i] <- as.character(as.Date(as.numeric(soil$date[i]), origin="1900-01-01") - 2)
	soil$date[!i] <- as.character(as.Date(soil$date[!i], "%d/%m/%Y"))
	
	d <- data.frame(
	  country = r2$Country,
		adm1=r2$Region,
		adm2=r2$District,
		adm3=r2$Ward,
		location =r2$Village,
		trial_id=r2$Farm_ID,
		cob_density = r2$`Number of Cobs`,
		fwy_total = r2$`FWt of Cobs_all (kg)`,
		dmy_total = r2$`Grain dry weight (kg/25m2 @12.5%)`,
		yield= r2$`Grain yield (kg/ha@12.5%)`,
		date=r2$`sampling Date`
	)
	
   d$date[d$date == 374] <- NA
   i <- grepl("^4", d$date)
   d$date[i] <- as.character(as.Date(as.numeric(d$date[i]), origin = "1900-01-01") - 2)
   d$date[!i] <- as.character(as.Date(d$date[!i], "%d/%m/%Y"))
   
   #all farms 
   usid <- unique(soil$trial_id) 
   usid <- usid[!usid %in% unique(d$trial_id)]
   addid <- unique(soil[, c('country', 'adm1', 'adm2', 'adm3', 'location', 'trial_id', 'date')])
   addid <- addid[addid$trial_id %in% usid, ]
   
   d <- carobiner::bindr(d, addid)   

   
   #### Use location to fill the geo coordinates (# from geocode)
    geo <- data.frame(
      location = c("Kikatiti", "Kikwe", "Leguruki", "Maji ya chai", "Manyire", "Mateves", "Mbatakero", "Bangu", "Gole", "Kang'ata", "Kilimamzinga", "Kwabaya", "Kwamagome", "Mdoe", "Itale", "Lupembe lwa Senga (Mawambala)", "Nzihi", "Magulilwa", "Endabash", "Kambi ya Faru", "Kilimatembo", "Qaru", "Kilindi", "Kimbe", "Lusane", "Songe", "Vyadigwa", "Mawala (Vitono)", "Masege (Kihesa Mgagao)", "Isagwa", "Udekwa", "Mkalanga", "Ikula", "Magila", "Mkiu (Kyombo)", "Ludende", "Mbugani", "Madindo", "Gare", "Kwemkomole", "Lukozi", "Mziragembei", "Nole (Ihalula2)", "Kigala", "Upami", "Malanduku", "Ukange", "Ukwama (Utweve )", "Ujuni", "Ilota", "Inyala", "Mwala", "Hezya", "Itumpi", "Mpanda", "Mpui", "Nkangamo", "Ikana", "Machindo", "Chilulumo", "Mlomba", "Nkala", "Itumbula", "Namtambalala", "Chipumpu", "Momba", "Chindi", "Langasani", "Mninga", "Kiyowela (Kitowela)", "Kitiru", "Nyigo (Ikiliminzowo)", "Ikongosi", "Lwanga", "Kidete Mdabulo (Mlevelwa)", "Ikwega", "Igomaa", "Saja (Itengelo)", "Vikula", "Kicheba", "Kwemsala", "Mang'ula", "Ngarani", "Mbuta", "Mageuzi", "Mchomoro (Kilangalanga)", "Namanima (Namabengo)", "Songambele", "Kitanda", "Mlilayoyo", "Mkongo (Mkongo Gulioni)", "Suluti (Nahimba)", "Namtumbo (Mkongo Gulioni)", "Mwangata", "Nambecha", "Ihalula", "Ibatu", "Ibumila", "Igominyi  (Nundu 2)", "Isoliwaya", "Kanikelele", "Imalinyi", "Lulanzi", "Makanjaula (Igoma )", "Yakobi", "Nundu", "Wikichi", "Mlowa (Idofi/Kihanga)", "Matiganjola (Nyombo/Mahalule)", "Ihanga", "Uwemba", "igominyi (Igoma)", "Donyomuruak", "Lawate", "Liganga", "Miembeni", "Kikunja", "Kituro", "Gumbiro", "Magingo (Mkongotema)", "Mahanje", "Magagura", "Madaba", "Mhepai", "Luhiraseko", "Kizungu", "Kisa", "Malonje", "Mpona", "Mponda", "Ilambo", "Kipeta", "Tululu", "Chitete", "Ufwala", "Wangama", "Utiga", "Luduga", "Ivigo", "Boma la Ngombe", "Mtanziwa Ilolo", "Utanziwa Ilolo", "Kwamchaga", "ilindine", "Mbalatche", "Mfiriga", "Kinyawambo", "Ntenda", "Mandepedwe"),
      longitude = c(36.94521, 36.83715, 36.93545, 36.90360, 36.80579, 36.57437, 37.22386, 37.97398, 38.00684, 37.89671, 37.87094, 38.06873, 38.00043, 39.26186, 33.50576, 35.67451, 35.53371, 35.74154, 35.61145, 35.60403, 35.73180, 35.62101, 37.61329, 37.63789, 37.36304, 36.71448, 37.52150, 37.43161, 35.75148, 36.11330, 36.38450, 36.13621, 32.93480, 38.76736, 34.61704, 34.68502, 34.94110, 34.72157, 38.33833, 38.56551, 38.29018, 38.34914, 34.66113, 34.15814, 34.90932, 34.36868, 34.37871, 34.13780, 33.98369, 33.25075, 33.63786, 37.99760, 33.10587, 32.98735, 31.07299, 31.83970, 32.62173, 32.50306, 32.31274, 32.36027, 32.51280, 30.33085, 32.47965, 32.24585, 32.26966, 32.30141, 32.58194, 37.32660, 35.22494, 35.20257, 35.29372, 33.80546, 35.38900, 39.24673, 35.47903, 34.69155, 34.94368, 34.76338, 35.55173, 38.78847, 38.79467, 36.89615, 38.75728, 32.45493, 35.68720, 36.33665, 36.24572, 33.42403, 37.31520, 35.71697, 38.74340, 36.07449, 36.19024, 35.67512, 36.12713, 34.68976, 35.06173, 34.84692, 34.88065, 35.20163, 35.25830, 34.62434, 38.95997, 34.81390, 34.93846, 32.65428, 34.72097, 34.89022, 34.89028, 34.38244, 34.78965, 34.88065, 37.08825, 37.10252, 35.24504, 39.20105, 35.73494, 39.22272, 35.38254, 35.44801, 35.27759, 35.28516, 35.43216, 35.32584, 35.68382, 33.73870, 33.98547, 31.73879, 32.29292, 31.71988, 36.16761, 31.36800, 32.28355, 31.64795, 34.67991, 34.47845, 34.64705, 34.52712, 34.54029, 37.1500,33.5833, 34.058, 38.2239, 39.70, 34.40616, 35.2915, 35.3134, 31.616, 36.1330 ),
      latitude = c(-3.388817, -3.432729, -3.254101, -3.343251, -3.450236, -3.386068, -3.361164, -5.482178, -5.858293, -5.659601, -5.731726, -5.428470, -5.551854, -6.860824, -4.315835, -8.095547, -7.720158, -7.959249, -3.581144, -3.621717, -3.335270, -3.517658, -5.504188, -5.795312, -5.922270, -3.389505, -5.796407, -3.547250, -8.183763, -7.832641, -7.752761, -7.899992, -2.855042, -5.149488, -9.769379, -9.906983, -8.836825, -9.908091, -4.795098, -4.827694, -4.662985, -4.666643, -9.443520, -9.072476, -9.057770, -9.580883, -9.590189, -8.788694, -9.112210, -8.875670, -8.858008, -4.556994, -9.248652, -8.913516, -6.342952, -8.352211, -9.242791, -8.802845, -8.831763, -8.642454, -8.794590, -4.647491, -8.450475, -8.702542, -9.120049, -8.790479, -9.026690, -3.543877, -8.580699, -8.764202, -8.536459, -1.492375, -8.376054, -6.873909, -8.491830, -8.991757, -8.223816, -8.719440, -8.317068, -5.109270, -5.238757, -7.856877, -5.233735, -4.011471, -10.476494, -10.584661, -10.606772, -3.679480, -11.111909, -10.498033, -7.880970, -10.526596, -10.672896, -7.777399, -10.215993, -9.459269, -8.894533, -9.130317, -9.449485, -9.216088, -9.214052, -9.191719, -6.750252, -9.545298, -9.417882, -3.368599, -9.331705, -8.869977, -9.237205, -8.712953, -9.473256, -9.449485, -3.268451, -3.225167, -10.676908, -6.172796, -10.827995, -6.809237, -10.029794, -9.901047, -9.889359, -10.838100, -9.865219, -10.408376, -10.672960, -3.042825, -1.953059, -8.070792, -8.492810, -7.935098, -7.374394, -7.863768, -8.562250, -8.260043, -8.841874, -9.172814, -8.833730, -8.969110, -9.255119, -3.3333,-9.1667, -9.331, -5.596, -9.99, -9.43965, -8.3005, -8.25637, -7.948, -10.466)
    )
   
    d <- merge(d, geo, by="location", all.x = TRUE)
    d$geo_from_source <- FALSE
    
    ### Use adm3 to fill the coordinates where location is unknown 
    geo1 <- data.frame(
      adm3 = c("Arash", "Ibumu", "Bumilayinga", "Masama Rundugai", "Kiru", "Isuto", "Kitandililo", "Ikuwo", "Ukwama", "Kifanya", "Lusaka", "Kilagano", "Lwande", "Kwamndolwa", "Mnazi", "Tongoni"),
      lon = c(35.3861, 36.0912, 35.0826, 37.2243, 35.6352, 33.2568, 35.0566, 34.0815, 34.1906, 35.0986, 32.0355, 35.2825, 37.4290, 38.5136, 38.2887, 39.0659),
      lat = c(-2.4304, -7.4268, -8.3955, -3.4375, -4.0959, -9.0753, -8.8507, -9.0371, -9.4792, -9.6071, -8.4758, -10.4174, -5.6933, -5.0894, -4.3648, -5.2047),
      geo_uncertainty = c(29769, 19885, 10467, 11833, 9409, 10868, 17463, 14319, 13695, 23190, 18024, 22605, 12601, 6323, 29404, 10287),
      geo_source = "GADM 4.1, adm3"
    )
    
    d <- merge(d, geo1, by="adm3", all.x = TRUE)
    x <- is.na(d$longitude)| is.na(d$latitude)
    d$longitude[x] <- d$lon[x]
    d$latitude[x] <- d$lat[x]
    
    d$lon <- d$lat <- NULL
    
    d$is_survey <- TRUE
    d$irrigated <- NA
	 	d$planting_date <- "2015"
    d$crop <- "maize"
  	d$yield_part <- "grain"
  	d$on_farm <- TRUE
  	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

  	
  	soil$country <- soil$adm1 <- soil$adm2 <- soil$adm3 <- soil$location <- soil$date <- NULL 
    soil$depth_top <- 0
    soil$depth_bottom <- 20
    soil$depth_top[soil$depth == "20 – 50"] <- 20
    soil$depth_bottom[soil$depth == "20 – 50"] <- 50
    soil$depth <- NULL
      	
  	carobiner::write_files(path, meta, d, long=soil)
}

