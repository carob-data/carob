# R script for "carob"


carob_script <- function(path) {
   
"These datasets contain phenotypic and genotypic data of a panel of elite Andean breeding lines of common bean (Phaseolus vulgaris L.) from CIAT. This population has been tested in twelve yield trials carried out in Palmira and Darien (Colombia) between 2013 and 2018 to assess its performance under irrigated, drought and variable soil P conditions. These trials were laid out in the field with an alpha-lattice experimental design using two to three replicates, and a non-replicated trial in 2016. Meteorological data for these trials is also provided. Different agronomic traits were evaluated including Days to Flowering (DF), Days to Physiological Maturity (DPM), 100 seed weight (100SdW) and Yield (Yd). The agronomic performance of the population was modeled using linear mixed models with spatial correction. From these models, best linear unbiased estimators (BLUEs) and their corresponding standard errors (SE) were obtained. This population was genotyped by sequencing (GBS) using the ApeKI-based restriction digestion. The genotypic data is presented in a variant call format (VCF) file of 5,820 SNPs and 481 lines. The genotypic matrix was imputed using Beagle (v4.1). These datasets were used to test genomic prediction models on the panel in order to assess their prediction ability under different scenarios and parameter settings"
   
   uri <- "doi:10.7910/DVN/XCD67U"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1, 
      data_organization ="CIAT", 
      publication=NA, 
      project=NA, 
      data_type= "experiment", 
      response_vars= "yield", 
      treatment_vars = "variety;irrigated", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-10-28"
   )
   
   f <- ff[basename(ff)=="VEF_raw_phenotypic_data.csv"]
   
   r <- read.csv(f)
   
    ## processing data
   d <- data.frame(
      country= "Colombia",
      location= ifelse(grepl("Pal", r$Trial), "Palmira", "Darien") ,
      crop= "common bean",
      irrigated= grepl("Irrigated", r$Management),
      rep= ifelse(r$Replicate=="R1", 1L, 
           ifelse(r$Replicate=="R2", 2L, 3L)),
      variety= r$Line,
      treatment= r$Management,
      yield= r$Yd,
      seed_weight= r$X100SdW*10,
      trial_id= r$Unique.Identifier,
      planting_date= ifelse(grepl("13", r$Trial), "2013",
                     ifelse(grepl("14", r$Trial), "2014",
                     ifelse(grepl("15", r$Trial), "2015",
                     ifelse(grepl("16", r$Trial), "2016",
                     ifelse(grepl("17", r$Trial), "2017", "2018")))))
   )
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "seed"
   d$latitude[d$location=="Palmira"] <- 3.53782
   d$longitude[d$location=="Palmira"] <- -76.2968
   d$latitude[d$location=="Darien"] <- 3.932135
   d$longitude[d$location=="Darien"] <- -76.4938
   d$geo_from_source <- TRUE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   ## Adding weather data
   w <- read.csv(ff[basename(ff)=="VEF_meteoro.csv"])

   wh <- data.frame(
      date= as.character(as.Date(carobiner::eng_months_to_nr(w$Date),  "%d-%m-%y")),
      station_name= ifelse(grepl("Pal", w$Season), "Palmira", "Darien"), 
      tmax= w$Maximum.temperature,
      tmin= w$Minimum.temperature,
      rhum= rowMeans(w[,c("Relative.humidity.07.H","Relative.humidity.13.H", "Relative.humidity.19.H")]) ,
      prec= w$Total.precipitation,
      rhmn= w$Minimum.relative.humidity,
      vapr= w$Total.evaporation
   ) 
   
   d <- d[!is.na(d$yield),]
   
   carobiner::write_files(path, meta, d, wth = wh)
}

