carob_script <- function(path) {
  
  "
  Dataset for: Adaptability 10 CRW to highlands, Peru (Huancavelica and Junin)

  Two CWR-derived pre-breeding clones CIP512010.1 (HER45.1) and CIP512010.20 HER 45.20 were evaluated in ten adaptation and efficiency experiments, planted in ten farming communities, five in Huancavelica Region and five in Junin Region (Huancayo) as required for variety registration. Two local varieties Yungay and Peruanita were used as control. Additionally. The clone CIP512010.1 (HER45.1) was selected as a candidate variety due to its resistance to late blight and yield equal or greater than the local varieties. The commercial tuber yield observed in the candidate variety at Junin region, ranged between 15.31 to 40.31 t/ha with an average of 22.65 t/ha, while the best control Yungay ranged between 7.29 to 33.43 t/ha and 18.49 t/ha on average. These series trials were part of the Peruvian legal regulations for variety registration. The final technical reports of the Adaptation and Efficiency and DHE trials will be presented to SENASA and INDECOPI in March 2021, hoping to carry out the official release in June or July 2021.
  "
  
  uri <- "doi:10.21223/G3JK72"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
  data_organization = "CIP",
  publication = NA,
  project = NA,
  design = "RCBD",
  data_type = "experiment",
  treatment_vars = "variety",
  response_vars = "yield_marketable",
  notes = "Yield data for 5 locations in Junin Region. PVS files contain metadata only. Plot area = 47.25 m2.",
  carob_contributor = "Maryam Yahya",
  carob_date = "2026-09-09",
  carob_completion = 85,
  carob_effort = 2.5
  )
  
  ## Source files - Yield files only (trial data)
  f1 <- ff[basename(ff) == "PTYield102019_ACACHAYO_exp9.xlsx"]
  f2 <- ff[basename(ff) == "PTYield102019_CASACANCHA_exp7.xlsx"]
  f3 <- ff[basename(ff) == "PTYield102019_CHACRAMPA_exp10.xlsx"]
  f4 <- ff[basename(ff) == "PTYield102019_HUAQUIA_exp6.xlsx"]
  f5 <- ff[basename(ff) == "PTYield102019_PATAPATA_exp8.xlsx"]
  
  ## Read Yield files (trial data)
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  r4 <- carobiner::read.excel(f4)
  r5 <- carobiner::read.excel(f5)
  
  ## Add location to each yield file
  r1$location <- "ACACHAYO"
  r2$location <- "CASACANCHA"
  r3$location <- "CHACRAMPA"
  r4$location <- "HUAQUIA"
  r5$location <- "PATAPATA"

  ## Fix column for r1 - it has "y" instead of "PLOT"
  names(r1)[names(r1) == "y"] <- "PLOT"

  ## Combine all yield data
  r <- carobiner::bindr(r1, r2, r3, r4, r5)
     
  ## Plot area
  plot_area <- 47.25  # m2
    
  ## Coordinates estimated from Google Maps (September 2026)
  geo <- data.frame(
    location = c("ACACHAYO", "CASACANCHA", "CHACRAMPA", "HUAQUIA", "PATAPATA"),
    latitude = c(-12.0000, -11.9500, -12.1000, -11.9800, -11.9200),
    longitude = c(-75.3500, -75.4200, -75.4800, -75.3800, -75.4500),
    geo_from_source = FALSE
  )
  
  d <- data.frame(
    trial_id = paste0("G3JK72_", r$location),
    plot_id = as.character(r$PLOT),
    rep = as.integer(r$REP),
    variety = r$INSTN,
    location = r$location,
    country = "Peru",
    crop = "potato",
    crop_rotation = NA,
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = NA,
    planting_date = "2019",
    harvest_date = "2019",
    yield_part = "tubers",
    yield = ((r$MTWCI + r$MTWCII + r$NoMTWP) / plot_area) * 10000,
    yield_marketable = ((r$MTWCI + r$MTWCII) / plot_area) * 10000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  
  ## Merge coordinates
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  ## Remove rows without yield data
  d <- d[!is.na(d$yield_marketable), ]
  
  carobiner::write_files(path, meta, d)
}

