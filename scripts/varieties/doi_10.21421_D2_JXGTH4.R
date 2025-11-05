# R script for "carob"


carob_script <- function(path) {
   
"High-Oil lines trial comprising of 36 entries [32 elite breeding lines from ICRISAT and 4 improved varieties (SAMNUT-22, SAMNUT 23, SAMNUT 24 and SAMNUT 26) as checks] was conducted at BUK locations."

   uri <- "doi:10.21421/D2/JXGTH4"
   group <- "varieties" 
   
   ff  <- carobiner::get_data(uri, path, group)
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization = "ICRISAT", 
      publication =NA, 
      project = NA, 
      data_type = "experiment", 
      response_vars = "yield",
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-07-10"
   )
   f <- ff[basename(ff) == "Data file of High oil genotype for yield and agronomic traits in Buk.xlsx"] 	
   r <- carobiner::read.excel(f, sheet = "Sheet1")
   
   d <- data.frame(
      rep= as.integer(r$`Replication number`),
      variety= r$Genotypes,
      emergence_days= r$`Date of emergence`,
      flowering_days= r$DF50,
      dmy_storage= r$DPWkgha,
      dmy_residue= r$DFWkgha,
      seed_weight = r$HSWg * 10 ## 1000 seed weight
   )
   
   
   d$country= "Nigeria"
   d$location= "Bayero University, Kano"
   d$crop= "groundnut"
   d$yield_part <- "pod"
   
   ## Adding disease 
   r1 <- carobiner::read.excel(f, sheet = "Sheet2")
   
	d1 <- data.frame(
      rep= r1$`Replication number`,
      variety= r1$Genotypes,
      diseases = "early leaf spot;late leaf spot;rust",
      disease_severity=	apply(r1[, c("ELS80", "LLS90", "RUST90")], 1, \(i) paste0(i, "(1-9)", collapse=";"))
	)

   d <- merge(d, d1, by=c("rep","variety"), all.x = TRUE)
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- NA 
   d$inoculated <- FALSE
   d$trial_id <- "1"
   d$planting_date <- "2016"
   d$longitude  <- 8.4269711
   d$latitude  <-  11.9703215
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d) 
   
}
