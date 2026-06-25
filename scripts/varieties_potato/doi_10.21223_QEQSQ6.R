# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
"The effect of heterosis when two genetically divergent populations are crossed, allows us to obtain potato clones that are superior to their parents. The objective of this study was to quantify the effect of heterosis in progeny for tuber yield and to select clones with high levels of resistance to late blight and high tuber yield. In 2014, to determine the effect of heterosis on progenies, the mating design line by tester was used, crossing clones from two divergent populations, forty-five progenies were evaluated in San Ramon, Peru, a warm environment under a randomized complete block design with 3 replications. The heterosis effect was determined with respect to mid parent value. Progenies CIP312909, CIP312923, CIP312918, CIP312924, CIP312926, and CIP312930, showed significant heterosis effect for marketable yield per plant and hectare. The progenies CIP312917, CIP312920, CIP312927, presented significant heterosis effect for average tuber weight in addition to marketable yield with increments above 40%. At harvest, 528 clones were selected, which were evaluated from 2015 to 2018 in contrasting environments in Peru: Huancayo in Highlands, San Ramon in warm conditions and mid elevation for tuberculous yield and Oxapampa for late blight resistance. Twenty eight clones were selected with yields from 16.25 to 334.42 tha-1under warm conditions and in highlands from 21.90 to 33.10 tha-1, resistance to late blight with Area Under Disease Progress Curve values from 112 to 1210 less than susceptible variety Yungay, with 1382.66. These clones can be used to release new varieties or as parents in breeding programs in African, Asian and Latin American."
  
  uri <- "doi:10.21223/QEQSQ6"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
      data_organization = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable", 
      carob_contributor = "Henry Juarez",
      carob_effort = NA,
      carob_date = "2024-09-13",
      notes = NA
  )
  
  process <- function(filename){
      
      r <- carobiner::read.excel(filename, sheet="Fieldbook")               
      minimal <- carobiner::read.excel(filename, sheet="Minimal")
      installation <- carobiner::read.excel(filename, sheet="Installation")
      
      m <- as.list(minimal$Value)
      names(m) <- minimal$Factor
      
      n <- as.list(installation$Value)
      names(n) <- installation$Factor
      
      if((!"yield"%in%colnames(r)) && ("TTWP" %in% colnames(r))){
          
          TTYNA = (as.numeric(r$TTWP) / as.numeric(n$`Plot_size_(m2)`)) * 10
          
          r$yield = TTYNA * 1000
          
      } 
      
      if((!"yield_marketable"%in%colnames(r)) && ("MTWP" %in% colnames(r))){
          
          MTYNA = (as.numeric(r$MTWP) / as.numeric(n$`Plot_size_(m2)`)) * 10
          r$yield_marketable = MTYNA * 1000
          
      }
      
      df <- data.frame(
          rep = as.integer(r$REP),
          variety = r$INSTN,
          yield = if('yield' %in% colnames(r)) {
              r$yield
          } else {
              as.numeric(NA)
          },
          yield_marketable = if('yield_marketable' %in% colnames(r)) {
              r$yield_marketable
          } else {
              as.numeric(NA)
          },
          AUDPC = if('AUDPC' %in% colnames(r)) {
              as.numeric(r$AUDPC) / 100
          } else {
              as.numeric(NA)
          },
          rAUDPC = if('rAUDPC' %in% colnames(r)) {
              as.numeric(r$rAUDPC)
          } else {
              as.numeric(NA)
          },
          country = m$Country,
          adm1 = m$Admin1,
          adm2 = m$Admin2,
          adm3 = m$Admin3,
          location = m$Locality,
          longitude = as.numeric(m$Longitude),
          latitude = as.numeric(m$Latitude),
          elevation = as.numeric(m$Elevation),
          planting_date = m$Begin_date |> 
              as.numeric() |> 
              as.Date(origin = "1899-12-30") |> 
              as.character(),
          harvest_date = m$End_date |> 
              as.numeric() |> 
              as.Date(origin = "1899-12-30") |> 
              as.character(),
          trial_id = gsub(".xls", "", basename(filename))
      )
  }
  
  f <- ff[grep("PT", basename(ff))]
  d <- lapply(f, process)
  d <- do.call(rbind, d)
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$crop <- "potato"
  d$pathogen <- "Phytophthora infestans"
  d$yield_part <- "tubers"
  d$geo_from_source <- TRUE
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
  
}

