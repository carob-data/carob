# R script for "carob"


carob_script <- function(path) {
   
"Traits and genotypes related with resistance to intermittent and terminal drought stress. Three common bean elite lines (NCB 226, SEN 56, SER 125) were identified with superior levels of adaptation to both intermittent and terminal drought stress conditions. The greater performance of these lines under drought stress was associated with their ability to remobilize photosynthate to increase grain yield based on higher values of harvest index, pod harvest index, leaf area index and canopy biomass. Two wild bean germplasm accessions (G 19902, G 24390) showed very poor adaptation to both types of drought stress. One small-seeded black line (NCB 226) was superior in combining greater values of canopy biomass with greater ability to mobilize photosynthates to grain under both types of drought stress. Two small-seeded red lines (SER 78, SER 125) seem to combine the desirable traits of enhanced mobilization of photosynthates to seed with effective use of water through canopy cooling under terminal drought stress. Pod harvest index showed significant positive association with grain yield under both types of drought stress and this trait can be used by breeders as an additional selection method to grain yield in evaluation of breeding populations for both types of drought stress."
  
    uri <- "doi:10.7910/DVN/SAM3CY"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=4, 
      data_organization ="CIAT", 
      publication="doi:10.1017/S0021859616000915", 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield;fwy_total", 
      treatment_vars = "variety;irrigated", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-10-23"
   )
   
   f <- ff[basename(ff)=="02. 36 Lineas frijol 2007, 2008 y 2009.xlsx"]
   
   r <- carobiner::read.excel(f, na=c("."))
   names(r) <- gsub("100SW", "SW100", names(r))
   ## processing data
   d <- data.frame(
      country= "Colombia",
      location= "Palmira",
	  site = "CIAT campus",	  
      crop= "common bean",
      planting_date= as.character(r$YEAR),
      irrigated= grepl("Riego", r$STRESS),
      rep= as.integer(r$REP),
      LAI= r$LAI,
      variety= r$LINE,
      treatment= ifelse(r$STRESS=="Riego", "Irrigated", "drought"),
      yield= as.numeric(r$YDHA),
      fwy_total= as.numeric(r$CB),
      flowering_days= r$DF,
      trial_id= paste0(r$YEAR, "_", r$STRESS),
      seed_weight= r$SW100 * 10,
      #harvest_index= r$HI,
      irrigation_number= ifelse(grepl("2007|2008", r$YEAR), ifelse(r$STRESS=="Riego", 4L, 2L), ifelse(r$STRESS=="Riego", 6L, 3L)),
      irrigation_amount= 35,
      plant_spacing= 7
   )
   
   d$on_farm <- FALSE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "seed"

   # CIAT campus
   d$latitude <- 3.4961
   d$longitude <-  -76.3553
   d$geo_from_source <- TRUE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   d <- d[!is.na(d$yield),]
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

