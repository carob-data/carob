# R script for "carob"

carob_script <- function(path) {
   
"Traits related with drought resistance in recombinant inbred lines (RILs). A set of 121 bean genotypes (111 RILs, 2 parents, 8 checks) belonging to the Mesoamerican gene pool were evaluated under field conditions with two levels of water supply (irrigated and rainfed) over three seasons. To complement field studies, a greenhouse study was conducted using plastic cylinders with soil inserted into PVC pipes, to determine the relationship between grain yield obtained under field conditions with different root traits measured under greenhouse conditions. Resistance to drought stress was positively associated with a deeper and vigorous root system, better shoot growth, and superior mobilization of photosynthates to pod and seed production. Among the shoot traits measured, pod harvest index and seed number per area could serve as useful selection criteria for assessing sink strength and for genetic improvement of drought resistance in common bean (2017-03-03)"
   
   uri <- "doi:10.7910/DVN/XCHQQT"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=4, 
      data_organization ="CIAT", 
      publication="doi:10.3389/fpls.2017.00296", 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield;fwy_total", 
      treatment_vars = "variety;irrigated", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-10-29"
   )
   
   f <- ff[basename(ff)=="02. MD2324 X SEA5 Field 2003 2004 2007.xlsx"]
   
   r <- carobiner::read.excel(f)
   
   ## processing data
   d <- data.frame(
      country= "Colombia",
      location= "Palmira",
      crop= ifelse(r$LINE=="Cowpea Mouride", "cowpea", "common bean"),
      planting_date= as.character(r$YEAR),
      irrigated= grepl("Riego", r$STRESS),
      rep= as.integer(r$REP),
      LAI= r$LAI,
      variety= r$LINE,
      treatment= ifelse(r$STRESS=="Riego", "Irrigated", "drought"),
      yield= r$YDHA,
      fwy_total= r$CB,
      trial_id= paste0(r$YEAR, "_", r$STRESS),
      irrigation_number= ifelse(r$STRESS=="Riego", ifelse(r$YEAR==2003, 6L, 7L), 3L),
      irrigation_amount= 35,
      irrigation_method= "furrow",
      plant_spacing= 7
   )
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "seed"
   d$latitude <- 3.53782
   d$longitude <- -76.2968
   d$geo_from_source <- TRUE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

