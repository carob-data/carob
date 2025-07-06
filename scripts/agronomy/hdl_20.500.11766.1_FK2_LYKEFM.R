# R script for "carob"

carob_script <- function(path) {

"Final dataset from agronomic experiment in Gumara Maksegnit (2016), as elaborated by GARC researcher in charge for this trial (Baye Ayalew). Please contact author and contact person at ICARDA to obtain more detailed metadata or to propose collaboration."
   
   uri <- "hdl:20.500.11766.1/FK2/LYKEFM"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
  
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
      project=NA, 
      publication="hdl:20.500.11766/5084",
      data_organization = "ARARI", 
      data_type="on-farm experiment", 
      carob_contributor="Cedric Ngakou", 
      carob_date="2023-11-10",
	  response_vars = "yield",
	  treatment_vars = "N_fertilizer;N_splits"
   )
   
   
  ## process file(s)
    
    r1 <- read.csv(ff[basename(ff)=="Mandie_s_Farm.csv"],sep = ";")
    r2 <- read.csv(ff[basename(ff)=="Worku_s_Farm.csv"],sep = ";") 

    newname <- c("year", "treatment", "rep", "plant_height", "dmy_total", "yield")
    oldname <- c("Year", "Treatment", "Replicate", "Plant_Height", "Biomass", "Yield")
    d1 <- carobiner::change_names(r1[, oldname], oldname, newname)
    d2 <- carobiner::change_names(r2[, oldname], oldname, newname)
    d1$site <- "Mandie"
    d2$site <- "Worku"
    d1$dmy_total <- d1$dmy_total / 1000
	
    # append d1 and d2
    d <- rbind(d1, d2)
    
    ## process the treatment file 
    r3 <- read.csv(ff[basename(ff)=="Treatment.csv"], sep = ";") 
	r3 <- r3[!is.na(r3$Treatment), ]
	
	r3$N_splits <- rowSums(r3[, c("N_Planting", "N_Tillering", "N_Booting")] > 0) - 1
	r3$N_splits <- as.integer(pmax(0, r3$N_splits))
    r3$Treatment1 <- paste("plant_till_boot", r3$N_Planting, r3$N_Tillering, r3$N_Booting, sep = "_")

    d3 <- r3[,c("Treatment", "Nitrogen_Rate", "Treatment1", "N_splits")]
    colnames(d3) <- c("treatment", "N_fertilizer", "Treatment1", "N_splits")
    
    # merge d3 with d
    d <- merge(d, d3, by="treatment", all.x = TRUE)
    d$treatment <- d$Treatment1
    d$Treatment1 <- d$year <- NULL
    #add columns
	d$crop <- "wheat"
	
	d$country <- "Ethiopia"
	d$location <- "Gumara Maksegnit"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$inoculated <- FALSE
	d$yield_part <- "grain" 
	d$trial_id <- paste(d$rep,d$location,sep = "_")
   ## add lon and lat
   # EGB:
   # EPSG:32637 (https://epsg.io/map#srs=32637&x=349106&y=1373852&z=5&layer=streets)
	d$longitude <- 38.056
	d$latitude <- 10.401
	d$longitude[d$site == "Mandie"] <- 37.583
	d$latitude[d$site == "Mandie"] <- 12.421
  d$geo_from_source <- FALSE
  
    d$planting_date <- "2013-06-03"
    d$harvest_date <- "2013-10-25"

	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   ##  fix dmy_total range
   #d$dmy_total[d$dmy_total>20000] <- NA
   ## CN 
   # the plant_height values are very low. The unit is given in cm, but I suppose this is probably an error.
   # assuming it was in dm and convert into cm
   d$plant_height <- d$plant_height * 10
   carobiner::write_files(meta, d, path=path)	
}


