
carob_script <- function(path) {
  
  "
  Dataset for: Estimation of late blight severity using multispectral images in advance clones from LBHTC2 population of potato, Oxapampa 2019

  This data set is the result to process and analyses NDVI images acquired of late blight experiments. Through multiple correlations, an NDVI value was found and set as a threshold or the borderline between leaves healthy or infected with late blight. Then, it was computed the number of pixels by each category, which keep correlation with the visual late blight evaluation performed by experts in the field.
  "
  
  uri <- "doi:10.21223/CQUBLX"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "disease_severity;NDVI;plant_cover_",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-21",
    carob_completion = 80,
    carob_effort = 3.0
  )
  
  ## Source file
  f1 <- ff[basename(ff) == "01_Data.xlsx"]
  
  r1 <- carobiner::read.excel(f1)
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = "CQUBLX_Oxapampa",
    plot_id = as.character(r1$Plot),
    rep = as.integer(r1$Rep),
    variety = r1$`CIP-Number`,
    location = "Oxapampa",
    country = "Peru",
    yield = NA,
    yield_moisture = NA,
    yield_isfresh =NA,
    yield_part = "tubers",
    latitude = -10.6041,
    longitude = -75.41538,
    geo_from_source = TRUE,
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = NA,
    soil_texture = NA,
    elevation = NA,
    disease_severity = as.character(r1$LB),    
    NDVI = r1$NDVI_Coeff / 100,
    planting_date = NA,
    harvest_date = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    ground_cover = r1$`Coberture_%`    
  )
  
  carobiner::write_files(path, meta, d)
}
