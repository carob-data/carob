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
		adm4=r1$Village,
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
		adm4=r2$Village,
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
   addid <- unique(soil[, c('country', 'adm1', 'adm2', 'adm3', 'adm4', 'trial_id', 'date')])
   addid <- addid[addid$trial_id %in% usid, ]
   
   d <- carobiner::bindr(d, addid)   

   
   geo <- data.frame(
           adm2 = c("Arumeru", "Babati", "Hai", "Handeni", "Ileje", "Iringa Rural", "Karatu", "Kilindi", "Kilolo", "Korogwe", 
                    "Ludewa", "Lushoto", "Makete", "Mbeya Rural", "Mbozi", "Momba", "Moshi Rural", "Mufindi", "Muheza", "Namtumbo", "Ngorongoro", 
                    "Njombe", "Siha", "Songea Rural", "Songea Urban", "Sumbawanga", "Tanga", "Wangingombe"), 
           longitude = c(36.8372, 35.75,  37.1687, 38.4074, 33.3101, 35.6991, 35.6581, 37.6133, 36.3134, 
                         38.4192, 34.6801, 38.448, 34.1295, 33.2403, 32.9516, 32.3014, 37.3404, 35.2783, 38.7731, 36.1902, NA, 
                         34.7702, 37.3507, 35.655785, 35.6205, 31.6232, 38.2922, 34.54852), 
           latitude = c(-3.3593, -4.2167, -3.1471, -5.4948, -9.3921, -7.7731, -3.3386, -5.5042, -7.7859, 
                        -5.0827, -10.113, -4.4953, -9.2747, -9.2648, -8.9742, -8.7905, -3.3349, -8.5742, -5.1815, -10.6729, NA,
                        -9.6026, -3.0665, -10.6768, -10.657, -7.9524, -5.0665, -9.020)
           ) 
   
    d <- merge(d, geo, by="adm2", all.x = TRUE)
    d$geo_from_source <- FALSE
    
    d$is_survey <- TRUE
    d$irrigated <- NA
	 	d$planting_date <- "2015"
	
    d$crop <- "maize"
  	d$yield_part <- "grain"
  	d$on_farm <- TRUE
  	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

  	
  	soil$country <- soil$adm1 <- soil$adm2 <- soil$adm3 <- soil$adm4 <- soil$date <- NULL 
    soil$soil_sample_top <- 0
    soil$soil_sample_bottom <- 20
    soil$soil_sample_top[soil$depth == "20 – 50"] <- 20
    soil$soil_sample_bottom[soil$depth == "20 – 50"] <- 50
    soil$depth <- NULL
      	
  		 carobiner::write_files(path, meta, d, long=soil)
}

