# R script for "carob"
# license: GPL (>=3)

## ISSUES

# Iburu location searched on Google Maps: 10.270535, 7.789725


carob_script <- function(path) {

"
Agronomy Data, Kano, Nigeria, Under TAMASA project.

TAMASA is a 4-year project seeking to improve productivity and profitability for small-scale
maize farmers in Ethiopia, Nigeria and Tanzania.
"

  uri <- "doi:10.25502/sssg-0b75/d"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "IITA",
                                  publication = NA,
                                  project = "TAMASA",
                                  design = "Maize planting-date experiment",
                                  data_type = "on-farm experiment",
                                  treatment_vars = "planting_date; variety",
                                  response_vars = "yield; dmy_residue; harvest_index; maturity_days",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-09-16",
                                  carob_completion = 80,
                                  carob_effort = 2
  )
  
  f1 <- ff[basename(ff) == "data_dictionary.csv"]
  f2 <- ff[basename(ff) == "maize-planting-date-experiment.csv"]
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)

  d <- data.frame(
    plot_id = as.character(r2[["Plotno"]]),
    location = r2[["Loc"]],
    trial_id = paste(r2[["Year"]], "_", r2[["Loc"]]),
    variety = r2[["Variety"]],
    planting_date = as.character(as.Date(r2[["PlantingDate"]], format="%m/%d/%Y")),   
    rep = as.integer(r2[["Rep"]]),
    
    tassling_days = r2[["DFFL"]],
    silking_days = r2[["DYSK50"]],
    asi = r2[["ASI"]],
    maturity_days = r2[["Maturity"]],
    
    SPAD = r2[["spadRLCC"]],
    frac_int_radiation = r2[["ipar"]] / 100,
    LAI = r2[["lai"]],
    plant_height = r2[["PLHT"]],
    ear_height = r2[["E_HT"]],
    
    dmy_leaves = r2[["lfwtm2"]] * 10,   # g/m2 -> kg/ha
    dmy_stems = r2[["stwtm2"]] * 10,
    dmy_total = r2[["tdmm2"]] * 10,
    
    seed_weight = r2[["swt500"]] * 2,   # 500-seed -> 1000-seed
    harvest_index = r2[["hi"]],
    yield = r2[["Gyieldtha"]],          # already kg/ha
    dmy_residue = r2[["Topwtha"]]
  )
  
  d$crop <- "maize"
  d$country <- "Nigeria"
  
  # Iburu location searched on Google Maps: 10.270535, 7.789725
  d$adm1 <- "Kaduna"
  d$longitude <- ifelse(d$location == "Zaria", 7.7143, 7.789725)
  d$latitude <- ifelse(d$location == "Zaria", 11.0231, 10.270535)
  d$geo_uncertainty <- ifelse(d$location == "Zaria", 16966, NA)
  d$geo_source <- ifelse(d$location == "Zaria", "GADM 4.1, adm2", "Google Maps")
  d$geo_from_source <- FALSE
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  d$irrigated <- FALSE
  d$harvest_date <- NA
  d$yield_moisture <- NA
  d$yield_part <- "grain"
  d$K_fertilizer <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$yield_isfresh <- NA
	
	carobiner::write_files(path, meta, d)
}


