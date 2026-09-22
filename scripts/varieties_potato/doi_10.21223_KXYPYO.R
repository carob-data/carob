carob_script <- function(path) {

"
Dataset for: Generation of new progenies to use in development Product Profile to replace Canchan variety, introgression of new sources of resistance of late blight, bacterial wilt and drought tolerance

With the objective of generating potato progenies to be used in the product profile to search for candidate clones to replace the Canchan variety with resistance to late blight, precocity, high tuber yield, quality for frying and fresh consumption. A block of crosses was planned between advanced clones with resistance to late blight, frying quality, and high yields of tubers between them and also with clones from wild diploid species with non-reduced gametes that possess new resistance genes and clones with resistance to bacterial wilt. 306 hybrid crosses were generated with an average of 1000 seeds per crossing, which will be used to start the activities of the product profile within the Excellence in Breeding platform in 2021.
"

  uri <- "doi:10.21223/KXYPYO"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "tps_count_",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-17",
    carob_completion = 80,
    carob_effort = 3.0
  )
  
  ## Source file
  f1 <- ff[basename(ff) == "Data_Generation_of_new_progeniesl_2020-06-14_1.xlsx"]
  
  r1 <- carobiner::read.excel(f1)
  
  ## Clean dates (remove leading quote)
  r1$`Fruit Harvest Date` <- gsub("'", "", r1$`Fruit Harvest Date`)
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = "KXYPYO",
    variety = paste(r1$`Female Accession Number`, "x", r1$`Male Accession Number`),
    location = "Huancayo",
    country = "Peru",
    adm1 = "Junin",
    adm2 = "Huancayo",
    adm3 = "El Tambo",
    adm4 = "CIP-Santa Ana",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "seed",
    latitude = -12.010394,
    longitude = -75.224111,
    elevation = 3290,
    geo_from_source = FALSE,
    yield_moisture = NA,
    yield_isfresh = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    planting_date = "2019-10",
    harvest_date = r1$`Fruit Harvest Date`,
    ## NEW VARIABLES
    parent_female_ = r1$`Female Accession Number`,
    parent_male_ = r1$`Male Accession Number`,
    tps_count_ = as.numeric(r1$`Total Number of Seeds`)
    
  )
  
  carobiner::write_files(path, meta, d)
}
