carob_script <- function(path) {
  
  "
  Dataset for: Generation of new progenies to use in development Product Profile to replace Canchan variety, introgression of new sources of resistance of late blight, bacterial wilt and drought tolerance.

  The crossing plan was planted in October 2019, under greenhouse conditions in Huancayo at 3280 masl and 12º07'S, 55 clones of the groups B3C1, B3C2, B3C3, LBHT, and LBHTxLTVR (LB), were crossed as female parents and 170 clones with resistance to Late Blight (LB), tolerance to drought and new late blight clones from wild species Solanum cajamarquense, Solanum Chiquidenum (Clones ER) with new genes for resistance to LB, 5 clones with resistance to bacterial wilt (BW) and three Tuberosum varieties: Alpha, Desiree, and Kathadn, as male parents. To ensure greater flowering, the duration of the day was extended to 16 hours with 4 hours of artificial light, the stolons were eliminated to avoid the formation of tubers and induce a greater production of flowers. The crosses began in December 2020, The berry harvest was in the month of April 2021. 133 LB x BW, 52 LB x ER, 110 LB x tuberosum varieties, and 65 LB x LB progenies were obtained with a range of true potato seeds (TPS) from 4 to 3350 seeds, el 50% of the progenies had more than 500 seeds each.
  "
  
  uri <- "doi:10.21223/GMYIOP"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 4,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars =   "tps_count_",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-15",
    carob_completion = 80,
    carob_effort = 2.5
)
  
  ## Source files
  f1 <- ff[basename(ff) == "01_crossingTF120231952_RESULT_GENERAL.xlsx"]
  f2 <- ff[basename(ff) == "02_Report.xlsx"]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  
  r1$`Fruit Harvest Date` <- gsub("'", "", r1$`Fruit Harvest Date`)
  
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = "GMYIOP",
    variety = paste(r1$`Female Accession Number`, "x", r1$`Male Accession Number`),
    location = "Huancayo",
    adm1 = "Junin",
    adm2 = "Huancayo",
    adm3 = "El Tambo",
    adm4 = "CIP-Santa Ana",
    country = "Peru",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    planting_date = "2019-10",
    harvest_date = r1$`Fruit Harvest Date`,
    yield_moisture = NA,
    yield_isfresh = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    yield_part ="seed",
    latitude = -12.0651,
    longitude = -75.2049,
    elevation = 3280,
    geo_from_source = FALSE,
    # NEW VARIABLES
    parent_female_ = r1$`Female Accession Number`,
    parent_male_ = r1$`Male Accession Number`,
    tps_count_ = as.numeric(r1$`Total Number of Seeds`)
    
  )
  # Add cross type from r2
  key_d <- paste(d$parent_female_, d$parent_male_, sep = "_")
  key_r2 <- paste(r2$Female, r2$Male, sep = "_")
  d$cross_type_ <- r2$`Type of cross`[match(key_d, key_r2)]
  
  carobiner::write_files(path, meta, d)
}