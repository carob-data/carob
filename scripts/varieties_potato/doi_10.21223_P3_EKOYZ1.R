carob_script <- function(path) {
  
  "
  Dataset for: Heritability for yield components in LBHT potato clones, 
  under warm conditions
  
  In anticipation of the effects of global warming on potato cultivation 
  in both tropical and subtropical environments. Since 2004, efforts have 
  turned to the development of a new group in Population B with improved 
  adaptation to warm environments, resistance to late blight and virus, 
  mid-season maturity (90 day growing period under short day length 
  conditions), adaptation to mid elevations, low glycoalkaloids content, 
  along with economically important traits such as high tuber yield, 
  quality for table and industry, denominated LBHT (late blight, heat 
  tolerance). In this context, in order to determine the narrow-sense 
  heritability for yield components in the LBHT population, 32 clones 
  were crossed at the Huancayo station in Peru in 2012 under greenhouse 
  conditions using the North Carolina II mating design. The design was 
  comprised of 4 sets, with 4 female and 4 male progenitors producing 
  16 full-sib progenies per set, for a total of 64 full-sib families. 
  Tuber families of each progeny were generated in 2013 for evaluation 
  under field conditions. And during 2014-2015, the progenies were 
  evaluated under field conditions, in three contrasting environments 
  in Peru: San Ramon, La Molina, and Majes, where average temperatures 
  at night were between 15.9 to 22.6 degC, and during the day fluctuated 
  between 22.9 to 27.5 degC. The randomized complete block (RCB) design 
  was used, with 4 sets and 3 replications of 50 genotypes per progeny.
  "
  
  uri <- "doi:10.21223/P3_EKOYZ1"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(
    uri, path, group,
    major = 2,
    minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-15",
    carob_completion = 80,
    carob_effort = 3.5
  )
  
  ## Source files (processed data)
  f3 <- ff[basename(ff) == "PTYield072014_CIPSRM_exp2_processed.xlsx"]
  f5 <- ff[basename(ff) == "PTYield102015_CIPHQ_exp9_processed.xlsx"]
  f7 <- ff[basename(ff) == "PTYield112014_MAJ_exp1_processed.xlsx"]
  
  ## Read processed data
  r3 <- carobiner::read.excel(f3)
  r5 <- carobiner::read.excel(f5)
  r7 <- carobiner::read.excel(f7)
  
  ## Add location identifiers
  r3$location <- "San Ramon"
  r5$location <- "La Molina"
  r7$location <- "Majes"
  
  ## Combine the three trials
  r <- carobiner::bindr(r3, r5, r7)
  
  ## Create final standardized data.frame
  d <- data.frame(
    trial_id = paste("EKOYZ1", gsub("[ ,]+", "_", r$location), sep = "_"),
    plot_id = as.character(r$plot),
    rep = as.integer(r$rep),
    variety = r$variety,
    location = r$locality,
    adm1 = r$admin1,
    adm2 = r$admin2,
    adm3 = r$admin3,
    country = "Peru",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    ## yield_fresh = TTYA (Total tuber yield adjusted) - t/ha
    yield = as.numeric(r$yield_fresh) * 1000,
    ## mtya = MTYA (Marketable tuber yield adjusted) - t/ha
    yield_marketable = as.numeric(r$mtya) * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    latitude = as.numeric(r$latitude),
    longitude = as.numeric(r$longitude),
    elevation = as.numeric(r$elevation),
    geo_from_source = TRUE,
    planting_date = as.character(as.Date(r$planting_date)),
    harvest_date = as.character(as.Date(r$harvest_date)),
    maturity_date = as.character(as.Date(as.POSIXct(r$maturity_date, origin = "1970-01-01"))),
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    fertilizer_type = NA,
    soil_texture = tolower(r$soil_texture),
    ## NEW VARIABLE
    set_ = as.integer(r$set),
    parent_female_ = r$female,
    parent_male_ = r$male
  )
  
  carobiner::write_files(path, meta, d)
}