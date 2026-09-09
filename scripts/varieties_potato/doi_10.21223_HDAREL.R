carob_script <- function(path) {
  
  "
  Dataset for: Selection and evaluation of at least ten advanced clones of the LBHT x LTVR population with low glycoalkaloid content in contrasting environments, with high resistance to late blight, high tuber yield, heat tolerance and adapted to mid and highlands in Huancayo.

  Twenty-eight advanced clones of the LBHT x LTVR population and three control varieties Yungay, Canchan and Desiree were planted in Quilcas, Huancayo between 2019 and 2020. The objective was to study the glycoalkaloid content under contrasting environments, a randomized complete block design was used with three replications of ten plants each, the dose of 200-180-160 kg NPK per ha was used. No fungicides were applied to control late blight. The glycoalkaloid content was determined in the laboratory. At harvest, fifteen tubers were collected from each sample and taken to the Quality and Nutrition Laboratory of CIP-Lima, Peru, for sample preparation and glycoalkaloid analysis. Freeze-dried and ground samples were prepared from each tuber and stored at 20°C until analysis. Total glycoalkaloids analysis was performed using the method described by Burgos et al. (2014) in which the extraction of glycoalkaloids was executed using methanol and chloroform prior to concentration at 60 ° C in a rotary evaporator. The extract was transferred to 2% acetic acid solution and then purified using ammonium hydroxide at 85 ° C and ultracentrifugation at 27,000 rpm. The pellet was reacted with 85% orthophosphoric acid and read at 408 nm in a spectrophotometer. The determination of total glycoalkaloids was performed against a standard curve of α-chaconine as a reference. This parameter was used as a criterion to select the clones to be further evaluated in the next season. Only clones with a total glycoalkaloid concentration below the safety limit for human consumption (20 mg / 100 g fresh weight) were selected.
  "
  
  uri <- "doi:10.21223/HDAREL"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "glycoalkaloid_total_",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-09",
    carob_completion = 85,
    carob_effort = 3.0
  )
  
  ## Source files
  f1 <- ff[basename(ff) == "01_PTGlycoalkaloids082019_CIPHYO_exp1_data.xlsx"]
  f2 <- ff[basename(ff) == "02_PTGlycoalkaloids082019_CIPHYO_exp1_material_list.xlsx"]

  ## Read source data 
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)

  ## Create variety lookup from r2 using Accession_code
  r2$variety_name <- ifelse(!is.na(r2$Accession_Name) & r2$Accession_Name != "",
                            r2$Accession_Name,
                            r2$Accession_code)
  variety_lookup <- setNames(r2$variety_name, r2$Accession_Number)
  
  d <- data.frame(
    trial_id = "HDAREL_Huancayo",
    plot_id = as.character(r1$PLOT),
    rep = as.integer(r1$REP),
    variety = as.character(variety_lookup[as.character(r1$INSTN)]),
    location = "Huancayo",
    country = "Peru",
    crop = "potato",
    crop_rotation = NA,
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    planting_date = "2019-11-21",
    harvest_date = "2020-05-18",
    N_fertilizer = 200,
    P_fertilizer = 180 / 2.29,
    K_fertilizer = 160 / 1.2051,
    fertilizer_type = "NPK",
    lime = NA_real_,
    yield_part = "tubers",
    latitude = -12.0651,
    longitude = -75.2048,
    geo_from_source = FALSE,
    #new variables
    glycoalkaloid_total_ = as.numeric(r1$GLIDW)
  )
  # Remove rows where all key variables are NA
  d <- d[!is.na(d$glycoalkaloid_total), ]
  # Write CAROB files
  carobiner::write_files(path, meta, d)
}