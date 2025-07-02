# R script for "carob"


carob_script <- function(path) {
   
"
Promoting Sustainable Intensification Through Efficient Application of Different Sources of Phosphorus (Minjingu Granular, Minjingu Mazao, DAP and TSP) (CIAT)

This study contains data from twelve (12) sites/farmers on sustainable intensification through efficient application of different fertilizers. These 12 sites/farmers were selected from Hallu, Long, Seloto, and Sabilo villages. Farmer selection was participatory, but among farmers who were willing to provide land and were also committed to implementing the trials.The project tested intercropping of maize/pigeonpea varieties that are differentiated by maturity periods.The maize varieties include PAN 691, a long maturing maize variety, Mkombozi, an early to medium maturity high yielding maize variety, and SC 627, an early maturing variety. These will be intercropped with pigeonpea variety Mali from ICRISAT, a long maturing and high yielding variety that is considered new in the study sites. 

About the project  Project title: Identification of the Key Biophysical Production Constraints to Crops and Livestock at Farm and Landscape Levels  Project abstract  Maize and pigeonpeas intercropping will be microdosed with different sources of P fertilizer to enhance BNF and environmental quality. Project website:  http://africa-rising.net  Project start date: 01/11/2012   Project end date : 01/12/2016
" 
   
   uri <- "doi:10.7910/DVN/G2CSNP"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0, 
      data_organization = "CIAT; SARI", 
      publication=NA, 
      project="Africa RISING", 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "fertilizer_type;variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-05-19",
      notes="The amounts of different fertilizers  used are missing"
   )
   
   f <- ff[grep("csv", basename(ff))]
   
   ## Processing data 
   r <- read.csv(f)
   
   d <- data.frame(
      country= "Tanzania",
      planting_date= as.character(r$Year),
      location= r$Village,
      trial_id= r$FarmerID,
      farmer_gender= r$Gender,
      treatment= gsub("YARA ", "YARA", r$Treatment_Fertilizer.Type),
      fertilizer_type= ifelse(grepl("Control", r$Treatment_Fertilizer.Type),"none", gsub("YARA ", "YARA", r$Treatment_Fertilizer.Type) ),
      variety= r$Treatment_Seed.type,
      yield= r$Yield..t.ha.*1000, ## kg/ha
      crop="maize",
      intercrops= "pigeon pea",
      yield_part= "grain",
      irrigated= NA,
      on_farm = TRUE,
      is_survey= FALSE,
      inoculated= FALSE
      )
   
   geo <- data.frame(
      location= c("Mlali", "Molet", "Njoro"),
      latitude= c(-6.28188, -6.1775, -5.2482),
      longitude= c(36.7511, 36.8130, 36.50025)
   ) 
 
   d <- merge(d, geo, by="location", all.x = TRUE) 
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files(path, meta, d)
}

