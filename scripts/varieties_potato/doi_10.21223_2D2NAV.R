carob_script <- function(path) {
  
  "
  Dataset for: At least ten advanced clones of the LBHTXLTVR population selected with low glycoalkaloid content in contrasting environments. In addition to high levels of late blight resistance, high tuber yield, heat tolerance, adapted to midland and highland in San Ramon.

  Twenty-eight advanced clones of the LBHT x LTVR population and three control varieties Yungay, Canchan and Desiree were planted in San Ramon, Chanchamayo between 2019 and 2020. The objective was to study the glycoalkaloid content under contrasting environments, a randomized complete block design was used with three replications of ten plants each, the dose of 200-180-160 kg NPK per ha was used. No fungicides were applied to control late blight.  The glycoalkaloid content was determined in the laboratory. At harvest, fifteen tubers were collected from each sample and taken to the Quality and Nutrition Laboratory of CIP-Lima, Peru, for sample preparation and glycoalkaloid analysis. Freeze-dried and ground samples were prepared from each tuber and stored at 20°C until analysis. Total glycoalkaloids analysis was performed using the method described by Burgos et al. (2014) in which the extraction of glycoalkaloids was executed using methanol and chloroform prior to concentration at 60 ° C in a rotary evaporator. The extract was transferred to 2% acetic acid solution and then purified using ammonium hydroxide at 85 ° C and ultracentrifugation at 27,000 rpm. The pellet was reacted with 85% orthophosphoric acid and read at 408 nm in a spectrophotometer. The determination of total glycoalkaloids was performed against a standard curve of α-chaconine as a reference. This parameter was used as a criterion to select the clones to be further evaluated in the next season. Only clones with a total glycoalkaloid concentration below the safety limit for human consumption (20 mg / 100 g fresh weight) were selected.
  "
  
  uri <- "doi:10.21223/2D2NAV"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "glycoalkaloids",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-10",
    carob_completion = 90,
    carob_effort = 2.0
)
  
  ## Source files
  f1 <- ff[basename(ff) == "01_PTGlycoalkaloids062019_CIPSRM-2_exp1_Data.xlsx"]
  f2 <- ff[basename(ff) == "02_PTGlycoalkaloids062019_CIPSRM-2_exp1_Material_list.xlsx"]
  
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  
  d <- data.frame(
    trial_id = "2D2NAV_SanRamon",
    plot_id = as.character(r1$PLOT),
    rep = as.integer(r1$REP),
    variety = r1$INSTN,
    location = "San Ramon",
    country = "Peru",
    crop = "potato",
    crop_rotation = NA,
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    planting_date = "2019-08-01",
    harvest_date = "2019-11-05",
    N_fertilizer = 200,
    P_fertilizer = 180 / 2.29,
    K_fertilizer = 160 / 1.2051,
    fertilizer_type = "NPK",
    lime = NA,
    yield_part = "tubers",
    latitude = -10.9856,
    longitude = -75.3625,
    geo_from_source = FALSE,
    glycoalkaloids = r1$GLIDW
  )

    ## Create variety lookup from r2 using Accession_code
  variety_name <- ifelse(!is.na(r2$Accession_Name) & r2$Accession_Name != "",
                            r2$Accession_Name, r2$Accession_code)
  variety_lookup <- setNames(variety_name, r2$Accession_Number)
  
  d$variety_code <- variety_lookup[d$variety]
  i <- d$variety == "CIP800048"
  d$variety_code[i] <- d$variety[i]
  d$variety[i] <- "Desiree"
  i <- d$variety == "CIP380389.1"
  d$variety_code[i] <- d$variety[i]
  d$variety[i] <- "Canchan-INIA"
  i <- d$variety == "CIP720201"
  d$variety_code[i] <- d$variety[i]
  d$variety[i] <- "Yungay"
  
  carobiner::write_files(path, meta, d)
}
