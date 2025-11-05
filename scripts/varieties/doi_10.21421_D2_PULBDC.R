# R script for "carob"


carob_script <- function(path) {
   
"This database includes the research work carried out on development of short-duration and medium-duration pigeonpea cultivars including elite hybrids (e.g. ICSH 2740) at ICRISAT Center, Patancheru (17o N and 38oE). Pigeon pea [Cajanus cajan (L.) Millspaugh] is a very important grain legume crop for food other uses in Asia and Africa. It is often cross-pollinated species with a diploid number of 2n= 2x22 and genome size of 858Mbp. Every year 50 to 100 and above new crosses (and also CMS hybrids) will be made evaluated in nurseries to develop new high yielding cultivars with adaptability to different climatic/agronomic zones. Based on their agronomic performance in nurseries for maturity time, branching pattern and number of branches, pod color, pod yield and other pest and diseases tolerance characters etc, the superior progenies will be selected and advanced to further generations (to F5s). The F5 progenies selected based on preliminary/nursery data will be evaluated along with controls in replicated (twice or thrice) trials every year for further agronomic evaluation and selection. The agronomic data (days to 50% flowering and/or maturity, plant height, grain yield, grain size and color etc) of the progenies evaluated in years 2015 were presented herewith. The trial details and plot sizes were given. This data helps us to select and advance further. Finally the few best progenies among them will be evaluated in on-farm trials (OFTs) and in multi-location trials. The best performed progenies will be considered to promote/release in respective agronomic zones."
   
   uri <- "doi:10.21421/D2/PULBDC"
   group <- "varieties"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2, 
      data_organization ="ICRISAT", 
      publication= NA, 
      project= NA, 
      data_type= "on-station experiment", 
      response_vars= "yield", 
      treatment_vars = "variety", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2024-12-06"
   )
   
   ff <- ff[grepl("Medium Duration", basename(ff))]
   
   ### Processing 
   process <- function(f){
      r <- carobiner::read.excel(f, fix_names = TRUE, na=c("-","*"))
      r$trial_id <- "1"
      if(is.null(r$ICPR.NO)) r$ICPR.NO <- r$ICPB.NO
      if(grepl("Trial-1", f)) r$trial_id <- "2"
      if(grepl("Trial-2", f)) r$trial_id <- "3"
      data.frame(
         variety= r$ICPR.NO, 
         flowering_days= r$DFTFL,
         maturity_days= r$Days.to.maturity,
         plant_height= r$Plant.height.in.cm,
         seed_weight= as.numeric(r$Hundred.Seed.weight.in.g)*10,
         yield= as.numeric(r$Grain.weight.in.kg.per.plot)*10000/10.2,
         plant_density= r$Total.plants*10000/10.2,
         crop= "pigeon pea",
         planting_date= "2015",
         adm1="Telangana",
         adm2= "Hyderabad",
         location="ICRISAT Patancheru",
         country= "India",
         latitude= 17.51803, 
         longitude= 78.27904,
         trial_id= r$trial_id
      )
   }
   
  d <- lapply(ff, process)
  d <- do.call(rbind, d)  
  # removing NA in yield (4 rows)
  d <- d[!is.na(d$yield),]
   d$on_farm <- FALSE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   d$plant_height[d$plant_height > 500] <- NA
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

