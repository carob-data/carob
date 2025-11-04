# R script for "carob"

##ISSUES
### Soil information is missing in the data ( not clear in the protocol)
#### 

carob_script <- function(path) {
   
"Characterising soils of the maize belt in Nigeria to determinie limiting nutrients based on which new fertilzer formulations 
are developed that are tested on farmer's fields in validation trials in a large number of locations against the commonly used NPK 15-15-15 fertilizer."

   uri <- "doi:10.25502/RHV4-6G25/D"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
      project=NA, 
      publication= NA, 
      data_organization = "IITA", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-06-04", 
      data_type="experiment",
      response_vars = "yield",
      treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;Zn_fertilizer;S_fertilizer;B_fertilizer"
   )
   
	r <- read.csv(ff[basename(ff)=="NAERLS_VT_yield_summ.csv"])
	d <- data.frame(
		treatment = r$trt_name, 
		yield = r$yld.plot_kg.ha,
		trial_id = "1"
	)

    # from VT protocol OCP Project Document (see raw data)  

    d$country <- "Nigeria"
    d$location <- "Nasarawa-Taraba" 
    d$crop <- "maize"
	d$variety <- "Sammaz 15" # From VT protocol
	d$row_spacing <- 75 
	d$plant_spacing <- 25 

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$yield_part <- "grain"   

	# More information can be found in the VT protocol ( see raw data)  
	# NPK Apply  15-15-15 	
	#OCPF1   : N   P2O5  k2O   S    Zn  B2O3
	#          11  22    21   5     1   1
   
	#OCPF2  :  N   P2O5  k2O   S    Zn  B2O3
	#             14   31    0    9    1    1
   
	fert <- data.frame(
		treatment=c("Control", "NPK", "OCPF1", "OCPF2" ),
		N_fertilizer=c(0, 15, 11, 14),
        P_fertilizer=c(0, 15, 22, 31) / 2.29,
		K_fertilizer=c(0, 15, 21, 0) / 1.2051,
		Zn_fertilizer=c(0, 0, 1, 1),
        S_fertilizer=c(0, 0, 5, 9),
		B_fertilizer=c(0, 0, 1, 1)  #B2O3?
	)  
   
   d <- merge(d, fert, by="treatment", all.x = TRUE) 

   d$longitude <- 8.2383
   d$latitude <- 8.4388
	d$geo_from_source <- FALSE
 # Planting and harvest date ( from VT protocol )
   d$planting_date <- "2017-04-01"
   d$harvest_date <- "2017-11-01"

   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)   
}

