# R script for "carob"


carob_script <- function(path) {
   
"The trial was conducted to evaluate the reaction of different sorghum varieties to striga. Significant losses in sorghum biomass and grain yield occur in sub‐Saharan Africa due to infection by the root‐parasitic weed Striga hermonthica. This trial will assist in selecting striga tolerant varieties that can be cultivated in striga infested fields. The ontents of this data set include the plant height, number of plants that germinated, time to flower, date of maturity, pest and disease scores, grain weight and the yield. The trial was conducted in Alupe (Nairobi) from July, 2018. Data collection began from the second week after planting. Sixteen different sorghum varieties were evaluated."
   
   uri <- "doi:10.21421/D2/NIBHAU"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization ="ICRISAT", 
      publication= NA, 
      project= NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety;striga_damage", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-11-21"
   )
   
   f <- ff[basename(ff)=="Data file of sorghum cultivars to determine their reaction to striga.xlsx"]
   
   r1 <- carobiner::read.excel(f,sheet = "Sheet1")
   
   ### Processing 
   d1 <- data.frame(
      rep= as.integer(r1$`Replication number`),
      variety_pedigree= r1$`Entry name`,
      variety= r1$Genotypes,
      planting_date= as.character(as.Date(r1$Dop, origin = "1899-12-30")),
      flowering_days= r1$`Days to flowering`,
      flowering_date= as.character(as.Date(r1$Flo_C_day, origin = "1899-12-30")),
      plant_height=  r1$PH_M_cm,
      diseases= "leaf blight;anthracnose",
      disease_severity= paste(r1$`leaf blight`, r1$`Anthracnose score`, sep="; "),
      striga_damage= TRUE,
      severity_scale= "1-9",
      maturity_days= r1$Mat_C_day,
      maturity_date= as.character(as.Date(r1$Mat_date_jd, origin = "1899-12-30")),
      yield= r1$GY*1000, ## kg/ha
      plot_Nr= r1$`Plot no`,
      crop= "sorghum",
      trial_id ="1",
      country= "Kenya",
      adm1="Nairobi",
      location= "Alupe",
      latitude= 0.50306738,
      longitude= 34.1272343,
      geo_from_source= FALSE
   )
   
   r2 <- carobiner::read.excel(f,sheet = "Sheet2")
   
   d2 <- data.frame(
      plot_Nr= r2$`Plot no`,
      rep= as.integer(r2$`Replication number`),
      seed_weight= r2$GW_100grnM_g*10
   )
   
   d <- merge(d1, d2, by=c("rep","plot_Nr"), all.x = TRUE)
   d$plot_Nr <- NULL
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

