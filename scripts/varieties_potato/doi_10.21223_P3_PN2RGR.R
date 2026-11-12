# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Description says 2,745 clones; the material list has 2,722 test clones, 24 parents and 8 checks
# coordinates from the dataset metadata


carob_script <- function(path) {
  
  "
Replication Data for: Late blight severity in breeding clones from CIP's LBHTC2 breeding population

This dataset contains field records of late blight progression from Trial A of the study 'Prediction of late blight severity in a large panel of potato genotypes using low-altitude aerial images and machine learning methods.' The trial comprised 2,745 breeding clones from the LBHTC2 population of the CIP breeding program. It was planted on September 24, 2019, under field conditions with high endemic late blight pressure in Oxapampa, Pasco, Peru (1,810 m a.s.l.).  The dataset includes genotype and experimental-unit identifiers and successive visual assessments of disease progression at the plot level, expressed as the percentage of leaf area affected by late blight. The deposited records correspond specifically to the final Trial A dataset analyzed in the associated publication.
"
  
  uri <- "doi:10.21223/P3/PN2RGR"
  group <- "varieties_potato"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 2, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "AUDPC;disease_severity",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-11-11",
    carob_completion = 70,
    carob_effort = 1
  )
  
  f1 <- ff[basename(ff) == "01_Data_LBHTC2.xlsx"]
  r1 <- carobiner::read.excel(f1)
  r1 <- r1[r1$genotype != "empty", ]
  r1$genotype[r1$genotype == "capiro"] <- "Capiro"
  r1$record_id <- seq_len(nrow(r1))
  
  d <- data.frame(
    record_id = r1$record_id,
    trial_id = "PN2RGR_Oxapampa",
    plot_id = as.character(r1$plot),
    rep = as.integer(r1$rep),
    variety = r1$genotype,
    AUDPC = r1$audpc / 100,
    country = "Peru",
    adm1 = "Pasco",
    location = "Oxapampa",
    latitude = -10.6039,
    longitude = -75.4153,
    geo_from_source = TRUE,
    elevation = 1810,
    planting_date = "2019-09-24",
    harvest_date = NA,
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield = as.numeric(NA),
    yield_part = "tubers",
    yield_moisture = as.numeric(NA),
    yield_isfresh = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  
  lb_cols <- paste0("LB", 1:7)
  lon <- r1[, c("record_id", lb_cols)]
  d_long <- reshape(lon, varying = lb_cols, v.names = "disease_severity", timevar = "order", times = 1:7, direction = "long")
  d_long <- d_long[!is.na(d_long$disease_severity), ]
  d_long$id <- NULL
  d_long$disease_severity <- as.character(d_long$disease_severity)
  d_long$severity_scale <- "0-100"
  d_long$disease <- "potato late blight"
  
  carobiner::write_files(path, meta, d, long = d_long)
}
