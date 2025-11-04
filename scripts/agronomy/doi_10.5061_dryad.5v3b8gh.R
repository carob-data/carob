# R script for "carob"

carob_script <- function(path) {
     
"Sorghum (Sorghum bicolor L. Moench)–groundnut (Arachis hypogaea L.) intercropping is important in the Sahel and Sudan Savanna. Nineteen trials were conducted during 2014 and 2015 in Mali and Niger for determination of: response functions for sorghum sole crop (SSC) and intercrop to N, P, and K; effects of the P×N interaction and of a nutrient package for diagnosis of other deficiencies; and a procedure for estimation of intercrop functions from SSC response functions. Six Niger on-station trials included treatments for determination of intercrop effects on sorghum yield and response. Mean grain and fodder yield increases for SSC were, respectively, 18.8 and 17.6% with 10 kg ha–1 P, 30.7 and 18.8% with 30 kg ha–1 N, and 0 and 6.8% with 10 kg ha–1 K applied. The diagnostic treatment increased grain yield for only 1 of 19 site-years (SYs). Sorghum grain yield was 30% less with intercropping compared with SSC for 2 of 6 SY but 11% less overall. Overall, sorghum fodder yield was 23% more with intercropping. Sorghum response to P was 86% greater for SSC compared with intercrop. With intercropping, groundnut pod yield exceeded sorghum grain yield. The value of intercrop production was higher than for SSC. Fodder added 23 and 30% to the value of `SSC and intercrop harvests, respectively. Equations were developed for determining optimal nutrient rates and intercrop response functions from SSC functions. Fertilizer applied to the intercrop compared with SSC has much more profit potential for this part of West Africa."


   uri <- "doi:10.5061/dryad.5v3b8gh"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)

   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA, 
      data_organization = "UNL;IER;INRAN", 
      publication="doi:10.2134/agronj2017.02.0120", 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;intercrops", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-06-21"
   )

   d1 <- lapply(c("NigerOFT", "NigerOST", "Mali", "BurkinaFasoDori"), \(i){
      r <- carobiner::read.excel(ff[basename(ff)=="W Africa Sorghum data set.xlsx"], sheet = i)
      names(r) <- gsub("Year", "Y", names(r))
      names(r) <- gsub("Soil", "Site", names(r))
      names(r) <- gsub("SorGrYld", "SorGrYd", names(r))
      names(r) <- gsub("SorStYd|SorStYld", "SorFoYd", names(r))
      if (is.null(r$`Manure rate, Mg ha-1`)) r$`Manure rate, Mg ha-1` <- NA
      if (is.null(r$GNGrYd)) r$GNGrYd <- NA
 
      ### sorghum
      data.frame(
         location= r$Site, 
         planting_date= as.character(r$Y), 
         rep= as.integer(r$Rep), 
         treatment= r$Trt, 
         N_fertilizer= r$Nrate, 
         P_fertilizer= r$Prate, 
         K_fertilizer= r$Krate, 
         OM_amount= r$`Manure rate, Mg ha-1`*1000, # kg/ha
         crop= "sorghum", 
         yield= r$SorGrYd*1000, # in kg/ha
         fwy_total= r$SorFoYd*1000, # in kg/ha, 
         intercrops= ifelse(is.na(r$GNGrYd), "none", "groundnut"),
		 on_farm = ifelse(grepl("NigerOST", i), FALSE, TRUE),
         trial_id= paste0(r$Site, r$Y),
		  country = i
         )
      }
   )
     
   d1 <- do.call(rbind, d1) 
      # 9 records with no yield
	d1 <- d1[!is.na(d1$yield), ]
	
	## groundnut intercrop
	d2 <- lapply(c("NigerOFT", "NigerOST"), \(i) {
	   
	   r <- carobiner::read.excel(ff[basename(ff)=="W Africa Sorghum data set.xlsx"], sheet = i)
	   out <- data.frame(
	      location= r$Site, 
	      planting_date= as.character(r$Y), 
	      rep= as.integer(r$Rep), 
	      treatment= r$Trt, 
	      N_fertilizer= r$Nrate, 
	      P_fertilizer= r$Prate, 
	      K_fertilizer= r$Krate, 
	      OM_amount= 2.5*1000, # kg/ha ## from data description
	      crop= "groundnut", 
	      yield= r$GNGrYd *1000, # in kg/ha
	      fwy_total= r$GNFoYd*1000, # in kg/ha, 
	      intercrops= "sorghum", 
  		  on_farm = TRUE,
          trial_id= paste0(r$Site, r$Y),
		  country = i
	    )
		# only keep the treatments with groundnut
		out[!is.na(out$yield), ]
	  }
	)  
	
	d2 <- do.call(rbind, d2)
	
	d <- rbind(d1, d2)

   d$irrigated <- as.logical(NA)
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   d$plot_area <- 36 # m2

   d$country[grep("Niger", d$country)] <- "Niger"
   d$country[grep("Mali", d$country)] <- "Mali"
   d$country[grep("Burkina", d$country)] <- "Burkina Faso"

   d$treatment <- paste0("N", d$N_fertilizer, "P", d$P_fertilizer, "K", d$K_fertilizer)

   # see paper
   d$on_farm[d$location %in% c("Bema", "KebilaOSR")] <- FALSE
   d$on_farm[d$location == "Maradi"] <- FALSE

	#this introduces duplicates. Except for one case because all data for "Kebila COU_Issaf" is also included under "Kebila Coulibali"
	d$location[grepl("Kebila", d$location)] <- "Kebila"
	d <- unique(d)
    d$location[d$location == "Kol"] <- "Kolombada" 

# based on the paper there should be more sites near Bema, assuming Cam is one of them. 
# some locations are from the paper  
	geo <- data.frame(
		location = c("Cam", "Bengou", "Kagara", "Maradi", "Konni", "Maradi_Angoual", "Kolombada", "Bema", "Kou", "Kebila", "Dori", "Boni", "Hounde"), 
		latitude = c(15.0430, 11.983, 13.3748, 13.46, 13.814, 13.533, 12.694, 15.058, 13.3785, 11.258, 14.0328, 11.5125, 11.4922), 
		longitude = c(-9.3532, 3.559,  7.0099, 7.107,  5.192, 7.9488, -7.013,  -9.34, -4.5978,  -7.02, -0.0348, -3.3864, -3.5206)
	)
      
   d <- merge(d, geo, by="location", all.x = TRUE)
	d$geo_from_source <- FALSE
 
	#from Table 1
	soil <- data.frame(
		location=c("Bengou", "Maradi", "Konni", "Bema", "Kolombada", "Kebila"),
		#Latitude=c(11.983, 13.460, 13.814, 15.058, 12.694, 11.258),
		#Longitude=c(3.559, 7.107, 5.192, -9.340, -7.013, -7.020),
		elevation=c(175, 356, 270, 295, 280, 360),
		soil_pH = c(6.48, 5.96, 5.91, 7.73, 5.53, 5.45),
		soil_SOC = c(1.60, 1.26, 1.57, 4.76, 2.80, 3.80),
		soil_N = c(0.27, 0.11, 0.17, 0.35, 0.22, 0.35),
		soil_P  = c(87.6, 14.4, 32.3, 8.13, 12.9, 13.5),
		soil_K = c(0.19, 0.14, 0.20, 0.37, 0.19, 0.16),
		soil_Ca = c(4.70, 1.07, 1.26, 13.2, 1.22, 1.35),
		soil_Mg = c(1.30, 0.41, 0.46, 0.30, 0.53, 0.48),
		soil_S = c(18.5, 11.0, 16.8, 8.2, 9.1, 12.0),
		soil_Zn = c(10.3, 7.9, 6.4, 1.5, 1.1, 2.1),
		soil_B = c(0.17, 0.22, 0.14, 0.19, 0.29, 0.09),
		soil_sand = c(525, 863, 794, 301, 547, 460) / 10,
		soil_silt = c(182, 69, 70, 191, 279, 210) / 10,
		soil_clay = c(293, 68, 136, 503, 209, 330) / 10
	)

   d <- merge(d, soil, by="location", all.x = TRUE)


 
	d$yield_moisture <- as.numeric(NA) #needs to be checked

    carobiner::write_files(path, meta, d)
   
}


