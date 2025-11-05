# R script for "carob"


carob_script <- function(path) {
   
"Small land holdings are among the main constraints for smallholders to produce enough food and feed to meet household demands. During the main cropping season, when all the land is covered by stable crops, feed is critically in short supply. Africa RISING’s diagnostic surveys showed that during such times weeds growing with faba bean crop are used as important feed resource by farmers in Ethiopia. Farmers leave the weed to grow with the faba bean until a certain stage, which is against the recommended agronomic practice through the extension system. A series of experiments were conducted to explore the rationale behind farmers preference, involving three faba bean production practices: 1) the traditional management practice (where weeds are used as forage), 2) improved practice (where weeds are frequently removed from faba bean plots) and 3) intercropping faba bean with fodder oat. The dataset contains the grain yield, straw yield, forage yield and gross income from the different practices and faba bean varieties in the Lemo district of Africa RISING site."
  
   uri <- "doi:10.7910/DVN/JAOXFJ"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1, 
      data_organization = "ILRI", 
      publication = NA,
      project ="AfricaRISING", 
      data_type = "experiment",
      response_vars = "yield;fwy_total",
      treatment_vars = "variety;intercrops;weeding_times", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-10-14"
   )
   
   f <- ff[basename(ff)=="data.xlsx"]
   
   r <- carobiner::read.excel(f)
   
   ### faba bean yield
   r1 <- r[r$`Management practice`!= "Traditional",]
   d1 <- data.frame(
      treatment= ifelse(r1$`Management practice` == "Improved", "two late weedings",
                        "two late weedings + intercrop"),
      variety= r1$`Faba bean Variety`,
      planting_date= as.character(r1$Year + 2014),
      farmer_gender= r1$`Gender of HH`,
      yield= r1$`Grain Yield (t/ha)` * 1000,
      fwy_residue= r1$`Straw Yield (t/ha)` * 1000,
      weed_biomass = r1$`Forage Yield (t/ha)` * 1000,
      crop_price= r1$`Gross Return (ETB/ha)`,
      crop="faba bean",
      currency= "ETB",
      intercrops= ifelse(r1$`Management practice` == "Intercropped", "oats", "none"),
      weeding_times= 3L,
      weeding_done= TRUE 
   )
   d1$weed_biomass[d1$intercrops == "oats"] <- NA
   d1$crop_price <- d1$crop_price/d1$yield ### ETB/kg
   
   ### oats yield
   r2 <- r[r$`Management practice` == "Intercropped", ]
   d2 <- data.frame(
      treatment= "two late weedings + intercrop",
      planting_date= as.character(r2$Year + 2014),
      farmer_gender= r2$`Gender of HH`,
      fwy_total = r2$`Forage Yield (t/ha)` * 1000,
      crop="oats",
      intercrops= "faba bean",
      weeding_times= 3L,
      weeding_done= TRUE 
   )
   
   ### weed biomass
   r3 <- r[r$`Management practice` == "Traditional", ]
   d3 <- data.frame(
      treatment= "one late weeding",
      planting_date= as.character(r3$Year + 2014),
      farmer_gender= r3$`Gender of HH`,
      variety= r3$`Faba bean Variety`,
      crop= "faba bean",
      yield= r3$`Grain Yield (t/ha)` * 1000,
      fwy_residue= r3$`Straw Yield (t/ha)` * 1000,
      weed_biomass = r3$`Forage Yield (t/ha)` * 1000,
      weeding_times= 1L,
      weeding_done= TRUE,
      crop_price= r3$`Gross Return (ETB/ha)`,
      currency= "ETB"
   )
   d3$crop_price <- d3$crop_price/d3$yield ### ETB/kg 
   
   d <- carobiner::bindr(d1, d2, d3)
   d$country= "Ethiopia"
   d$location <- "Lemo"
   d$trial_id <- d$planting_date
   d$row_spacing <- 40
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   d$latitude <- 5.432054 
   d$longitude <- 37.8433
   d$geo_from_source <- FALSE
   
   ### Adding fertilizer
   d$fertilizer_type <- "DAP"  ## DAP: 18% N, 20.1% P (DAP= 100kg/ha)
   d$fertilizer_amount <- 100
   d$N_fertilizer <- 18
   d$P_fertilizer <- 20.1
   d$K_fertilizer <- 0
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)
}


