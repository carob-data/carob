# R script for "carob"


carob_script <- function(path) {
   
"This database includes the research work carried out on development of medium duration pigeonpea cultivars including advanced varieties at ICRISAT Center, Patancheru (17°30'N 78°16'46E). Pigeon pea is a very important grain legume crop for food other uses in Asia and Africa. It is often cross-pollinated species with a diploid number of 2n= 2x22 and genome size of 858Mbp. Every year 50 to 100 and above new crosses (and also CMS hybrids) will be made evaluated in nurseries to develop new high yielding cultivars with adaptability to different climatic/agronomic zones. Based on their agronomic performance in nurseries for maturity time, branching pattern and number of branches, pod color, pod yield and other pest and diseases tolerance characters etc, the superior progenies will be selected and advanced to further generations (to F5s). The F5 progenies selected based on preliminary/nursery data will be evaluated along with controls in replicated (twice or thrice) trials every year for further agronomic evaluation and selection. The agronomic data (days to 50% flowering and/or maturity, plant height, grain yield, grain size and color etc) of the progenies evaluated in years 2015 were presented herewith. The trial details and plot sizes were given. This data helps us to select and advance further. Finally the few best progenies among them will be evaluated in on-farm trials (OFTs) and in multi-location trials. The best performed progenies will be considered to promote/release in respective agronomic zones."
  
   uri <- "doi:10.21421/D2/ZS6XX1"
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
      carob_date="2024-12-04"
   )
   
   f <- ff[basename(ff)=="Medium duration  advanced varietal Pigeonpea trial (MD AVT-2).xlsx"]
   
   r <- carobiner::read.excel(f)
   ### Process data
   
   d <- data.frame(
      rep= as.integer(r$Rep),
      variety= r$`ICPH No.`,
      flowering_days= r$`Days to flowering`,
      maturity_days= r$`Days to maturity`,
      plant_height= r$`Plant height in cm`,
      seed_weight= r$`Hundred Seed weight in g`*10,
      yield= r$`Grain weight in kg per plot`*10000/10.2,
      plant_density= r$`Total plants`*10000/10.2,
      crop= "pigeon pea",
      planting_date= "2014",
      adm1="Telangana",
      adm2= "Hyderabad",
      location="ICRISAT Patancheru",
      country= "India",
      latitude= 17.51803, 
      longitude= 78.27904,
      trial_id="1"
   )
   
   d$on_farm <- FALSE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$irrigated <- NA
   d$yield_part <- "grain"
   d$geo_from_source <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

