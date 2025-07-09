# R script for "carob"


carob_script <- function(path) {
  
"In 2019-2020 season, three clones with high levels of resistance to late blight were evaluated in adaptation and efficiency trials for tuber yield, dry matter content, frying and baking quality throughout Peru in six locations, compared to two varieties planted by farmers. and very well accepted by final consumers, Canchan and Unica, these are currently also used for frying in sticks, but without stability in all crops due to the genotype x environment interaction. The randomized complete block design was used with three replications of 150 plants, the fertilization dose was 200-220-180 Kg of NPK, using potassium sulfate as a source of potassium to improve frying quality. At harvest, samples were taken to determine dry matter, reducing sugar content, traditional and blanched frying color, and baking quality. The clone was equal to or superior to the controls for the yield of tubers, it presented good quality of frying color in all localities compared to the control varieties that did not present good quality of frying color in all localities, It is expected to complete all the documents requested by the Peruvian Seed Authority (SENASA) to be registered as a new potato variety with resistance to late blight and quality for frying and / or baking."
  
  uri <- "doi:10.21223/WCLRKI"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
      data_organization = "CIP;PATAZ",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable", 
      carob_contributor = "Henry Juarez",
      carob_date = "2024-09-13",
      notes = NA
  )
  
  f <- ff[grep("Combinado", basename(ff))]
  
  r <- carobiner::read.excel(f[1], sheet="UÑIGAN") 
  
  colnames(r) <- r[9,]
  r <- r[-c(1:9),-c(20:38)]
  colnames(r)[20] <- "Locations"
  
  d <- data.frame(
      rep = as.integer(r$Rep),
      variety = r$Clone,
      yield = as.numeric(r$MTYNA) *1000,
      yield_marketable = as.numeric(r$TTYNA) * 1000,
      AUDPC = as.numeric(r$AUDPC) / 100,
      country = 'Peru',
      location = r$Locations,
      planting_date = "2020-02-17" ,
      harvest_date = "2020-06-17",
      trial_id = gsub(".xls", "", basename(f[1]))
  )
  d <- d[!is.na(d$yield),]
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$crop <- "potato"
  d$pathogen <- "Phytophthora infestans"
  d$yield_part <- "tubers"
  d$geo_from_source = FALSE
  d$N_fertilizer = 200
  d$P_fertilizer = 220
  d$K_fertilizer = 180 
  
  d <- d[!is.na(d$location),]
  
  geo <- data.frame(
      location = c("Chinchao", "Jauja", "Licame", "Majes", "Uñigan", "Yanac"),
      longitude = c(-78.611901,-77.859298, -77.871786, -76.067057, -75.419384, -72.245653),
      latitude = c(-7.098178,-7.906375, -8.617975, -9.634471, -11.763526,-16.305186)
      
  )
  
  d <- merge(d, geo, by = 'location', all.x=T)
  
  carobiner::write_files(path = path,metadata = meta,wide=d)
  
}

