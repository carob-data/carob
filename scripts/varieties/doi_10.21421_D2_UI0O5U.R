# R script for "carob"


carob_script <- function(path) {
   
"The experimental design adopted was an alpha lattice design with 2 replications and 6 entries in 6 blocks, with 4 rows of each entry in 0.2 ha. The field was fertilized with diammonium phosphate at 80 kg/ha and 40 kg KCl/ha."
   
   uri <- "doi:10.21421/D2/UI0O5U"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2, 
      data_organization ="ICRISAT", 
      publication="doi:10.17138/TGFT(5)40-49", 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-12-03"
   )
   
   f1 <- ff[grepl("Main crop", basename(ff))]
   f2 <- ff[grepl("Ratoon crop", basename(ff))]
   
   r1 <- carobiner::read.excel(f1)
   ### Processing main_crop field
   d1 <- data.frame(
      rep= as.integer(r1$`Replication number`),
      variety= r1$Genotype,
      plant_height= r1$Plantht * 100,
      fwy_total= r1$`Fresh weight` * 1000,
      fwy_stems= r1$SY * 1000,
      planting_date= "2014-07-15",
      harvest_days= 80,
      crop= "sorghum",
      N_fertilizer=18*80/100 + 145*46/100 ,
      P_fertilizer=20.1*80/100,
      K_fertilizer=40*49.8/100,
      fertilizer_dap= "0(50.4);21(66.7)",
      row_spacing=60,
      plant_spacing=20,
      fertilizer_type="DAP;urea;KCl",
      irrigated= FALSE,
      adm1="Telangana",
      adm2= "Hyderabad",
      location="ICRISAT Patancheru",
      country= "India",
      latitude= 17.51803, 
      longitude= 78.27904,
      trial_id= "1"
      
   )
   
   r2 <- carobiner::read.excel(f2)
   ### Processing ratoon crop field
   d2 <- data.frame(
      rep= as.integer(r2$`Replication number`),
      variety= r2$Genotype,
      plant_height= r2$`Ratoon Plant height`*100,
      fwy_total= r2$`Fresh weight`*1000,
      fwy_stems= r2$`Ratoon SY`*1000,
      planting_date= as.character(as.Date("2014-07-15")+80),
      harvest_days= 80,
      crop= "sorghum",
      N_fertilizer=45*46/100 ,
      P_fertilizer=0,
      K_fertilizer=0,
      fertilizer_dap= "0(20.7)",
      row_spacing=60,
      plant_spacing=20,
      fertilizer_type="urea",
      irrigated= TRUE,
      irrigation_number=1L,
      adm1="Telangana",
      adm2= "Hyderabad",
      location="ICRISAT Patancheru",
      country= "India",
      latitude= 17.51803, 
      longitude= 78.27904,
      trial_id="2"
   )
   
   d <- carobiner::bindr(d1, d2)
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "aboveground biomass"
   d$geo_from_source <- FALSE
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
   
}



