# R script for "carob"

### Protocol is needed to clarify the treatment 

carob_script <- function(path) {
   
" Over exploitation of soil and absence of fallow system expose soil to degradation. In addition climate variability is another threat. Under such conditions use of copping innovation may be the alternative. However in contact with other continents and regions, farmers in Sub-Saharan Africa and particularly in WCA use almost no production input. All this results in low yield a consequence of many years of mining agriculture to get good sustainable yield input must be used that would replace nutrient exported by crops. In addition soil organic pool build up is necessary to sustain soil quality and productivity.Poor plant stand is among the cause of low yield therefore optimal plant density is required which may be reached with adequate seed treatment.The combination of appropriate nutrient input, seed treatment and genotype will yield good and sustainable yield. "
   
   uri <- "doi:10.21421/D2/TVF4M5"
   group <-  "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization = "ICRISAT", 
      publication=NA, 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_effort = NA,
      carob_date="2024-09-17",
      notes= "Protocol is needed to clarify the treatment (Apron: Appropriate nutrient)"
   )
   
   r <- carobiner::read.excel.hdr(ff[basename(ff)== "Data file of Managment options to improve Millet and Sorghum productivity rainy season 2015.xlsx"],skip=0, na=c("NA"))
   
   
   d <- data.frame(
      treatment= r$Seed.treatment.l,
      OM_type= ifelse(grepl("200g Man", r$Organic.amend.l), "farmyard manure",
                            ifelse(grepl("Boadcast", r$Organic.amend.l), "unknown", "none")), 
                     
      OM_amount= ifelse(grepl("200g", r$Organic.amend.l), 0.2, ## Kg
                        ifelse(grepl("Boadcast", r$Organic.amend.l), NA, 0)),
      Mineral_type= ifelse(grepl("NPK", r$Minerals.l),"NPK","none"),
      Mineral_amount= as.numeric(gsub("N|R", NA, substr(r$Minerals.l, 1 , 1))),
      variety= r$Millet_genotype.l,
      flowering_days= as.numeric(r$DF_50),
      plant_height= r$Hauteur.moyenne.plants*100, #cm
      maturity_days= as.numeric(r$DM_50),
      yield= as.numeric(r$GYld_E_1to5),
      fwy_total= as.numeric(r$Biomass.total),
      harvest_index= as.numeric(r$HI_C_pct),
      trial_id = "1"
      
   )
   
   d$country <- "Niger"
   d$location <- "Maradi"
   d$crop <- "pearl millet"
   d$planting_date <- "2015"
   d$irrigated <- NA
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain"
   d$longitude <- 7.10254
   d$latitude <- 13.50178
   d$geo_from_source <- FALSE

   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files(path, meta, d)
   
}


