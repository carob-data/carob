carob_script <- function(path) {
  
  "
  Dataset for: Phenotypic Stability for Tuber Yield and Late Blight Resistance in Advanced Clones from B3C3
  
  The objective of these experiments were to study the phenotypic stability of late blight resistance and tuber yield in 30 advanced clones belonging to population B group B3, third cycle 3 - B3C3. From 2016 to 2018, eight experiments were performed. Three in Oxapampa (2000masl) to study the phenotypic stability of resistance to late blight. And five in La Molina (150masl), Huancayo (3200 masl) and San Ramon (800masl) for the tuber yield. The randomized complete block statistical design was used, with three repetitions of 10 plants each. Information of late blight resistance was recorded, such as: percentage of leaf area damaged by this disease for 6 weeks, at 7-day intervals. With the information obtained, the area under the disease progress curve (AUDPC) and the scale of susceptibility to late blight (SAUDPC), were calculated. At harvest, for tuber yield: the number of plants harvested, the number and weight of commercial and non-commercial tubers were recorded. Then, the commercial (MTY) and total yield (TTY) per hectare were calculated.
  "
  
  uri <- "doi:10.21223/P3_XVRHST"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 3, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-16",
    carob_completion = 80,
    carob_effort = 4.0
  )
  
  # Source files (processed data only)
  f1 <- ff[basename(ff) == "01_PTLate_blight092015_OXAPMP_processed.xlsx"]
  f2 <- ff[basename(ff) == "02_PTLate_blight092016_OXAPMP_processed.xlsx"]
  f3 <- ff[basename(ff) == "03_PTLate_blight092017_OXAPMP_processed.xlsx"]
  f4 <- ff[basename(ff) == "04_PTYield072016_CIPSRM-2_processed.xlsx"]
  f5 <- ff[basename(ff) == "05_PTYield082016_CIPHQ_processed.xlsx"]
  f6 <- ff[basename(ff) == "06_PTYield082017_CIPSRM-2_processed.xlsx"]
  f7 <- ff[basename(ff) == "07_PTYield112015_CIPHYO_processed.xlsx"]
  f8 <- ff[basename(ff) == "08_PTYield112016_CIPHYO_processed.xlsx"]
  
  # Read processed data
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  r4 <- carobiner::read.excel(f4)
  r5 <- carobiner::read.excel(f5)
  r6 <- carobiner::read.excel(f6)
  r7 <- carobiner::read.excel(f7)
  r8 <- carobiner::read.excel(f8)
  
  # Combine all trials
  r <- carobiner::bindr(r1, r2, r3, r4, r5, r6, r7, r8)
  
  # Create final data.frame
  d <- data.frame(
    trial_id = paste("XVRHST_", r$locality),
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
    yield = r$yield_fresh * 1000,
    yield_marketable = r$mtya * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    latitude = r$latitude,
    longitude = r$longitude,
    elevation = r$elevation,
    geo_from_source = TRUE,
    planting_date = as.character(as.Date(r$planting_date)),
    harvest_date = as.character(as.Date(r$harvest_date)),
    maturity_date = as.character(as.Date(as.POSIXct(r$maturity_date, origin = "1970-01-01"))),
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    soil_texture = tolower(r$soil_texture)
  )
  
  carobiner::write_files(path, meta, d)
}