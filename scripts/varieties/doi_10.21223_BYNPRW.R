# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {


"
Dataset for: Assessment of at least 10 advanced clones from inter-population crosses (LBHT x LTVR) in Ethiopia and introduce them to Kenya for clean and further distribution

Yield evaluation for 15 advanced clones from LBHT x LTVR population 2018 growing season at Holetta and Adet, Ethiopia. The dataset includes data about potato clones. The experiment was created with an RCBD design, with 3 replication. Each plot contains 2 rows, for a total of 8 plants. Row length is 3 m, spaced 0.75 m between rows and 0.3 m within rows and alleys are spaced 1m. The farmer selection process is based on yield, organoleptic  characteristic and appearance at flowering.
"

  
   uri <- "doi:10.21223/BYNPRW"
   group <- "varieties"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
         data_organization = "CIP", 
         publication=NA, 
         project=NA, 
         design= "RCBD", 
         data_type= "experiment", 
         treatment_vars= "variety", 
         response_vars = "yield;yield_marketable", 
         carob_contributor= "Cedric Ngakou", 
         carob_date="2025-08-01",
         completion=100,
         notes=NA
   )
   
   
   f1 <- ff[basename(ff) == "PTPVS062018_HOLE_exp1.xlsx"]
   f2 <- ff[basename(ff) == "PTYield062018_ADET_exp1.xlsx"]
   
   r1  <- carobiner::read.excel(f1, sheet = "Harvest")
   r11  <- carobiner::read.excel(f1, sheet = "Minimal")
   r2  <- carobiner::read.excel(f2, sheet = "Fieldbook")
   r22  <- carobiner::read.excel(f2, sheet = "Minimal")
   
   ### Process 
   
   ## locality: HOLE
   rr <- data.frame(rbind(r11$Value))
   names(rr) <- r11$Factor
   d1 <- data.frame(
      plot_id= as.character(r1$PLOT),
      rep= as.integer(r1$REP),
      variety= r1$INSTN,
      yield_marketable= r1$MTYNA,
      yield= r1$TTYNA,
      crop= rr$Crop,
      trial_id= rr$Trial_name,
      planting_date= as.character(as.Date(rr$Begin_date, format = "%d/%m/%Y")),
      harvest_date= as.character(as.Date(rr$End_date, format = "%d/%m/%Y")),
      country= rr$Country,
      adm1= rr$Admin1,
      adm2= rr$Admin2,
      adm3= rr$Admin3,
      location= rr$Locality,
      elevation= as.numeric(rr$Elevation),
      latitude= as.numeric(rr$Latitude),
      longitude= as.numeric(rr$Longitude),
      row_spacing= 75,
      plant_spacing= 30,
      plot_area= 1.8/10000,
      plant_density= (8/1.8)*10000
      
   ) 
   
   ### locality: ADET
   rr <- data.frame(rbind(r22$Value))
   names(rr) <- r22$Factor
   d2 <- data.frame(
      plot_id= as.character(r2$PLOT),
      rep= as.integer(r2$REP),
      variety= r2$INSTN,
      yield_marketable= r2$MTYNA,
      yield= r2$TTYNA,
      crop= rr$Crop,
      trial_id= rr$Trial_name,
      planting_date= as.character(as.Date(rr$Begin_date, format = "%d/%m/%Y")),
      harvest_date= as.character(as.Date(rr$End_date, format = "%d/%m/%Y")),
      country= rr$Country,
      adm1= rr$Admin1,
      adm2= rr$Admin2,
      adm3= rr$Admin3,
      location= rr$Locality,
      elevation= as.numeric(rr$Elevation),
      latitude= as.numeric(rr$Latitude),
      longitude= as.numeric(rr$Longitude),
      row_spacing= 75,
      plant_spacing= 30,
      plot_area= 1.8/10000,
      plant_density= (8/1.8)*10000
   ) 
   
 d <- rbind(d1, d2)   

 d$geo_from_source <- TRUE
 d$yield_part <- "tubers"
 d$is_survey <- FALSE
 d$on_farm <- TRUE
 d$irrigated <- NA
 
 d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
 
    
carobiner::write_files(path, meta, d)
   
}


