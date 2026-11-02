carob_script <- function(path) {

"
Dataset for: Remotely sensed measurements of NDVI and canopy cover taken in advance clones population of potato, Oxapampa 2019

This data set includes, 2 remote sensing measurement campaigns at 66 and 85 day after planting (DAP) were performed. Multispectral images were acquired from a RPA that were used to compute the canopy cover and NDVI per plot.
"
  uri <- "doi:10.21223/NDW1KB"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "NDVI;ground_cover",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-10-18",
    carob_completion = 80,
    carob_effort = 2.0
  )
  
  f2 <- ff[basename(ff) == "Data_Oxapampa_2019.xlsx"]
  f3 <- ff[basename(ff) == "Image_description.xlsx"]
  
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  
  plots <- unique(r2[, c("Plot", "Rep", "Accession_number")])
  
  d <- data.frame(
    trial_id = "NDW1KB_Oxapampa_2019",
    plot_id = as.character(plots$Plot),
    rep = as.integer(plots$Rep),
    variety = plots$Accession_number,
    country = "Peru",
    adm1 = "Pasco",
    location = "Oxapampa",
    latitude = r3$Lat[1],
    longitude = r3$Lon[1],
    geo_from_source = TRUE,
    planting_date = "2019-09-24",
    harvest_date=NA,
    yield_moisture=NA,
    yield_isfresh=NA,
    crop = "potato",
    on_farm = NA,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = NA_real_,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  
  d_long <- data.frame(
    plot_id = as.character(r2$Plot),
    DAP = as.integer(r2$DAP),
    ground_cover = r2$CC * 100,
    NDVI = r2$NDVI_mean
  )
  
  carobiner::write_files(path, meta, d, long = d_long)
}