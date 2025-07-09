# R script for "carob"


carob_script <- function(path) {
   
"The current study was aimed at identifying mega-environments in Ghana and evaluating adaptability of superior sweetpotato genotypes from a targeted breeding effort. Three sets of genotypes were evaluated in multi-environment trials (MET). In the first part, twelve sweetpotato varieties were evaluated across nine environments representing the main agro-ecological zones in Ghana. Analysis of MET data was conducted using a stage-wise approach withthe genotype-by-environment (GxE) table of means used as a starting point to model the GxE interaction for sweetpotato yield. Emphasis was given to the genetic correlation matrix used in a second-order factor analytic model that accommodates heterogeneity of genetic variances across environments. A GGE plot of storage root yield explained 82 percent of the variation in the first principal component and visualized the genetic variances and discriminating power of each environment, and the genetic correlation between the environments. Two mega- environments, corresponding northern and southern trial sites, were delineated. In the second part of the study, six breeding lines selected from the south and eight breeding lines selected from the north were tested and compared to two common check clones at five locations in Ghana. A Finlay-Wilkinson stability analysis resulted in stable performances within the target mega-environment from which the genotypes were selected, but predominantly without adaptation to the other region. Our results provide a strong rationale for running separate programs to allow for faster genetic progress in each of these two major West African mega-environments by selecting for specific and broad adaptation. (2019-04-11) "
   
   uri <- "doi:10.21223/US6GHA"
   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization ="CIP;CSIR", 
      publication = "doi:10.1002/csc2.20034", 
      project =NA, 
      data_type = "experiment",
      response_vars = "yield;fwy_residue",
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-09-28",
      notes= NA
   )
   
   f <- ff[basename(ff)=="dataset_Swanckaert2019.xlsx"]
   
   d <- lapply(c("dataset1","dataset2"), \(i){
      
      r <- carobiner::read.excel(f, sheet = i)
      
      data.frame(
         season= as.character(r$studyYear),
         trial_id= r$studyName,
         location= gsub(", Ghana", "", r$locationName),
         variety= r$germplasmName,
         rep= as.integer(r$replicate),
         plant_density= as.numeric(r$`Plants planted counting number per plot|CO_331:0000678`)*1000,
         yield_marketable= as.numeric(r$`Weight of commercial storage roots measuring kg per plot|CO_331:0000220`)*1000,
         yield= as.numeric(r$`Weight of non-commercial storage roots measuring kg per plot|CO_331:0000223`+ r$`Weight of commercial storage roots measuring kg per plot|CO_331:0000220`)*1000,
         fwy_residue= as.numeric(r$`Weight of vines measuring kg per plot|CO_331:0000227`)*1000,
         flesh_color= r$`Predominant Flesh color estimating 1-9|CO_331:0000178`
      )
   })
  
   d <- do.call(rbind, d) 
   
   d$country <- "Ghana"
   d$crop <- "sweetpotato"
   d$plot_area <- 10 # m2
   d$plant_spacing <- 30 # cm
   d$row_spacing <- 100  # Cm
   d$on_farm <- TRUE
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "roots"
   
   ## Fixing flesh color
   cols <- c("white", "cream", "dark cream", "pale yellow", "dark yellow", "pale orange", "intermediate orange", "dark orange", "strongly 	pigmented with anthocyanins")
   d$flesh_color <- cols[d$flesh_color]
   ## Adding longitude and latitude
   
   geo <- data.frame(
      location= c("Botanga", "Fumesua", "Komenda", "Ohawu", "Tono", "Nyankpala", "Ejura", "Bawku", "Wa"),
      latitude= c(9.5733, 6.711056, 5.05559, 6.13123, 10.68180, 9.396561, 7.3846, 11.0538, 10.06029),
      longitude= c(-1.01465, -1.51761, -1.49375, 0.89789, -1.117886, -0.988542, -1.3585, -0.23814, -2.50979)
   ) 
   
   d <- merge(d, geo, by= "location", all.x = TRUE)
   d$geo_from_source <- FALSE
   
   ### Adding planting and harvest date from publication
   
   dtes <- data.frame(
      season= c("2018", "2017", "2018", "2016", "2017", "2016","2018", rep("2017", 6), "2018"),
      location= c("Ejura", "Fumesua", "Fumesua", "Fumesua", "Ohawu", "Ohawu", "Ohawu", "Komenda", "Nyankpala", "Botanga", "Tono", "Wa", "Bawku", "Botanga"),
      planting_date= c("2018-07-09", "2017-06-08", "2017-09-27", "2016-05-17", "2017-05-15", "2016-06-01","2018-06-14", "2017-06-15", "2017-09-27", "2017-08-30", "2017-08-05", "2017-07-17", "2017-07-08", "2017-12-12"),
      harvest_date= c("2018-11-23", "2017-10-23", "2018-01-27",  "2016-10-03", "2017-10-12", "2016-10-18","2018-10-15", "2017-10-06", "2018-01-27", "2018-01-06", "2017-12-02", "2017-11-13", "2017-11-02", "2018-04-24")
   )
   
   d <- merge(d, dtes, by= c("location", "season"), all.x = TRUE)
   
   d$planting_date[is.na(d$planting_date)] <- d$season[is.na(d$planting_date)]
   
   d$N_fertilizer <- 40
   d$P_fertilizer <- 40/2.29
   d$K_fertilizer <- 70/1.2051
	d$season <- NULL
   
   carobiner::write_files (path, meta, d)
}


