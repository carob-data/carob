# R script for "carob"


carob_script <- function(path) {
   
"The data set were generated through on-farm field trials that were established in Maluti-a-Phofung municipality, situated in the eastern Free State province of South Africa as part of a project entitled Innovations in Technology, Institutional and Extension Approaches towards Sustainable Agriculture and enhanced Food and Nutrition Security in Africa (Acronym-InnovAfrica). This project was implemented by a consortium of 16 institutions from Africa and Europe continents across six African countries (viz. Ethiopia, Kenya, Malawi, Rwanda, South Africa and Tanzania) from June 2017 to November 2021. Farmer-led trials were established for three planting seasons (2017/2018 to 2019/2020) to demonstrate, test and upscale the best-bet sustainable agricultural intensification practices for improved food and nutrition security in smallholder settings."
   
   uri <- "doi:10.5061/dryad.8w9ghx3q6"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=NA, 
      data_organization = "CREA;CIMMYT;ARC",
      publication="doi:10.3390/su13179902", 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety;intercrops", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-08-10",
      notes="30-60cm soil data not yet processed"	  
   )
   
  
   process <- function(f){ 
      
	  fill_missing <- function(x, v) {
		a <- !is.na(x[v[1]])
		x[, v] <- x[, v][which(a)[c(1, 1:sum(a))][cumsum(a)+1], ]
		x
	  }
	  
      r1 <- carobiner::read.excel.hdr(f, sheet = "Agronomy Data", skip=0)
	  r1 <- fill_missing(r1, c("Farm.No", "Latitute.degree", "Longitude.degree", "altitude.m"))

 
   ## Agronomy data
      d1 <- data.frame(
		 trial_id = r1$Farm.No,
         latitude= r1$Latitute.degree,
         longitude= r1$Longitude.degree,
         elevation= r1$altitude.m,
         crop= r1$Crop.species,
         variety= r1$Cultivar.variety,
         intercrops = ifelse(grepl("Maize/beans Intercrop", r1$Cropping.System)& r1$Crop.species=="maize", "common bean",
                      ifelse(grepl("Maize/beans Intercrop", r1$Cropping.System)& r1$Crop.species=="Beans", "maize", "none")),
      
         planting_date= as.character(as.Date(r1$Plant.date, "%d/%m/%Y")),
         weeding_dates= paste0(as.Date(r1$First.weeding.date, "%d/%m/%Y"), ";", as.Date(r1$Second.weeding.date, "%d/%m/%Y")),
         harvest_date=  as.character(as.Date(r1$Harvest.date, "%d/%m/%Y")),
         yield= r1$Yield.production.Ton.ha*1000, # kg/ha
         season= substr(gsub("_", "-", basename(f)), 26, 34),
         country= "South Africa",
		 f = f
       )
  
   ## soil data 
   #here you could do 
   #    r2 <- carobiner::read.excel.hdr(f, sheet = "Soil data", hdr=1, skip=1)

       r2 <- carobiner::read.excel(f, sheet = "Soil data", fix_names=TRUE)
       r2 <- r2[-1,] ## removing the first row
        names(r2) <- gsub("Farmer.No", "Farm.No", names(r2))

   ## Fixing longitude and latitude
 	  r2 <- fill_missing(r2, c("Farm.No", "Latitute.degree", "Longitude.degree", "altitude.m"))
 
        names(r2) <- gsub("pH.H2O", "pH", names(r2))
        names(r2) <- gsub("Particle.size.distribution", "Sand", names(r2))
        names(r2) <- gsub("X.14", "Silt", names(r2))
        names(r2) <- gsub("X.15", "Clay", names(r2))

       if (!is.null(r2$Depth))  r2 <- r2[r2$Depth == "0-30", ]

		for (v in c("S", "EC", "CEC", "Clay", "Silt", "Sand", "EA", "AS")) {
			if (is.null(r2[[v]])) r2[v] <- NA
		}
	
      d2 <- data.frame(
		 trial_id = r2$Farm.No,
         depth_bottom= 30,
         depth_top = 0,
         soil_pH= as.numeric(r2$pH),
         soil_P= as.numeric(r2$P),
         soil_Ca= as.numeric(r2$Ca),
         soil_Mg= as.numeric(r2$Mg),
         soil_K= as.numeric(r2$K),
         soil_Na= as.numeric(r2$N),
         soil_EC= as.numeric(r2$EC),
         soil_sand= as.numeric(r2$Sand),
         soil_silt= as.numeric(r2$Silt),
         soil_clay= as.numeric(r2$Clay),
         soil_S= as.numeric(r2$S),
         soil_CEC= as.numeric(r2$CEC),
		 soil_ex_Al = as.numeric(r2$EA),
		 soil_Al_sat = as.numeric(r2$AS)
         )
      
        # merge(d1,d2,by=c("longitude", "latitude", "elevation"), all.x = TRUE)
        merge(d1, d2, by="trial_id", all.x = TRUE)
 
   }
   
   d <- lapply(ff, process)
   d <- do.call(rbind,d)
	d$trial_id = as.character(as.integer(as.factor(paste(d$trial_id, d$f))))
	d$f <- NULL
   d$weeding_dates[grepl("2029-01-15",d$weeding_dates)] <- "2019-01-15"
   d$crop <- gsub("Beans", "common bean", d$crop)
   
   d$irrigated <- NA
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   d$adm3 <- "Maluti a Phofung"
   d$adm2 <- "Thabo Mofutsanyana" # District Municipality
   d$adm1 <- "Free State"
   d$geo_from_source <- TRUE
   
   ## adding information from data description
   d$plant_spacing <- 90
   d$row_spacing <- 25
   d$row_spacing[d$crop=="common bean"] <- 5
   d$fertilizer_amount <- 200 # kg/ha
   d$N_fertilizer <- 84
   d$P_fertilizer <- 56/2.29
   d$K_fertilizer <- 28/1.2051
   d$herbicide_product  <- "metolachlor"
   d$herbicide_used  <- TRUE
 
     
## weather data 
   
  weather <- function(f){
     Sheet <- "Climate data"
     if (grepl("2019_2020", basename(f)) ) Sheet <- "Sheet1"
     w <- carobiner::read.excel(f, sheet = Sheet, na=c("--",NA))
     # set names
     names(w) <- w[grep("Year", w[,1])[1], ]
	 # keep rows with useful information 
	 keeprows <- which(substr(w$Year, 1, 1) == "2")
     w <- w[keeprows, ] 
     remrows <- grep("Average|Total|Highest|Lowest|Day", w[,3]) 
     w <- w[-remrows,]
	 
     data.frame(
        country = "South Africa",
        date = as.character(as.Date(paste(w$Day, w$Month, w$Year, sep="-"), "%d-%m-%Y")),
        location = "Maluti a Phofung",
        station_name= "QWAQWA; UNIQWA",
        longitude= 28.82518,
        latitude= -28.48291,
		elevation = 1696,
        tmin = as.numeric(w$Tn), 
        tmax = as.numeric(w$Tx), 
        prec = as.numeric(w$Rain),
        rhmn = as.numeric(w$RHn), 
        rhmx = as.numeric(w$RHx), 
        wspd = as.numeric(w$U2), 
        srad = as.numeric(w$Rs),
		eto = as.numeric(w$ET0)
     )
   }
   
    dw <- lapply(ff, weather)
    dw <- do.call(rbind,dw)  
    ## remove empty rows in date 
    dw <- dw[!is.na(dw$date), ] 

	d$soil_pH[d$soil_pH < 1] <- NA
	d$geo_from_source <- TRUE
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d,wth = dw)
}

