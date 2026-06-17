# R script for "carob"

carob_script <- function(path) 

	uri <- "hdl:11529/10548759"
	group <- "conservation_agriculture"
	ff <- carobiner::get_data(uri, path, group)
 
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		project=NA,
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "doi:10.1002/ldr.4321",
		data_organization = "CIMMYT",
   		data_type="on-farm experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor="Hope Mazungunye", 
		carob_effort = NA,
		carob_date="2023-08-02"
	)




	f <- ff[basename(ff) == "Jat et al 2022 Final row data for LDD_SK.xlsx"]
	r <- readxl::read_excel(f, sheet="ESP") |> as.data.frame()

	d <- r[4:15, 1:8]


	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE

	d$treatment[d$...1 == "Sc1"] <- "Conventional rice - wheat system"
	d$treatment[d$...1== "Sc2"] <- "partial CSA based rice-wheat-mungbean system"
	d$treatment[d$...1== "Sc3"] <- "full CSA based rice-wheat-mungbean system"
	d$treatment[d$...1== "Sc4"] <- "full CSA based maize-wheat-mungbean system"
	d$country <- "INDIA"
	d$site <- "ICAR-CSSRI"
	d$adm1 <- NA
	d$adm2 <- NA
	d$adm3 <- NA
	d$elevation <- 243
	d$longitude <- 76.95527778
	d$latitude <- 29.70555556 
	
 d$crop[d$...1 == "Sc1"] <- "rice" 
	d$crop[d$...1 == "Sc2"] <- "rice"	
	d$crop[d$...1 == "Sc3"] <- "rice"
	d$crop[d$...1 == "Sc4"] <- "maize"
	d$variety <- NA
	d$crop_rotation[d$...1 == "Sc1"] <- "wheat"
	d$crop_rotation[d$...1 == "Sc2"] <- "wheat:mungbean"
	d$crop_rotation[d$...1 == "Sc3"] <- "wheat:mungbean"
	d$crop_rotation[d$...1 == "Sc4"] <- "wheat:mungbean"

	d$planting_date <- NA
	d$harvest_date  <- NA

   d$P_fertilizer[d$...1 == "Sc1"] <- "P2O5/46"
	d$P_fertilizer[d$...1 == "Sc2"] <- "P2O5/60"
	d$P_fertilizer[d$...1 == "Sc3"] <- "P2O5/60"
	d$P_fertilizer[d$...1 == "Sc4"] <- "P2O5/60"
   d$K_fertilizer[d$...1 == "Sc1"] <- "K2O/0"
   d$K_fertilizer[d$...1 == "Sc2"] <- "K2O/60"
   d$K_fertilizer[d$...1 == "Sc3"] <- "K2O/60"
   d$K_fertilizer[d$...1 == "Sc4"] <- "K2O/60"
   d$N_fertilizer[d$...1 == "Sc1"] <- "175"
   d$N_fertilizer[d$...1 == "Sc2"] <- "150"
   d$N_fertilizer[d$...1 == "Sc3"] <- "150"
   d$N_fertilizer[d$...1 == "Sc4"] <- "150"
   d$S_fertilizer <- NA
   d$lime <- NA
 
   d$fertlizer_type[d$...1 == "Sc1"] <- "DAP:urea"
   d$fertlizer_type[d$...1 == "Sc2"] <- "DAP:KCl:urea"
   d$fertlizer_type[d$...1 == "Sc3"] <- "DAP:KCl:urea"
   d$fertlizer_type[d$...1 == "Sc4"] <- "DAP:KCl:urea"
   d$inoculated <- FALSE
   d$inoculant <- NA
   
	d$dmy_total <- NA
   
	d$yield <- NA

	d$yield_part <- NA
	
	d <- d[,c("on_farm", "is_experiment", "treatment", "crop", "crop_rotation", "elevation", "longitude", "latitude", "crop", "crop_rotation", "P_fertilizer","K_fertilizer", "N_fertilizer", "fertlizer_type")]
	
	carobiner::write_files(meta, d, path=path)
}

