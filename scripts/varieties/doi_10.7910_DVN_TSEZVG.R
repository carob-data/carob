# R script for "carob"


carob_script <- function(path) {
   
"Phenotypic and Genotypic data based on 358 genotypes used to estimate genomic estimated breeding values (GEBV’s) for cooking time (CKT) Seed iron content (SeedFe), Seed Zin content (SeedZn) and Grain yield (GY). The data was used to select parents for the Rapid bean cooking project (RCBP) supported by the ACIAR"
   
   uri <- "doi:10.7910/DVN/TSEZVG"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1, 
      data_organization ="CIAT", 
      publication= NA, 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-10-22"
   )
   
   f <- ff[basename(ff)=="TPG_Phenotypedata.csv"]
   
   r <- read.csv(f)
   
   ## processing data
   d <- data.frame(
     trial_id= r$Trial.Code,
     location= gsub(", Uganda|, Tanzania", "", r$LOCATION),
     country= ifelse(grepl("Uganda", r$LOCATION), "Uganda", "Tanzania"),
     planting_date= as.character(r$Year),
     variety= r$DESIGNATION,
     rep= r$REP_NO,
     crop= "common bean",
     yield= r$GY,
     grain_Fe= r$Fe,
     grain_Zn= r$Zn
     #CKT= r$CKT ## cooking time
   )
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   
   geo <- data.frame(
      location= c("Kawanda", "Kachwekano", "Kitengule"),
      latitude= c(0.42236, -1.253845, -1.3229),
      longitude= c(32.54110, 29.94199, 31.28111)
   )
   d$geo_from_source <- FALSE
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   d <- d[!is.na(d$yield),]
   
	i = which(d$variety == "M\x92sole")
	d$variety[i] <- "M'sole"
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

