

carob_script <- function(path) {
  
  "
Dataset for: Procesamiento para calidad de color en French fries and Chips Advanced potato clones B3C1 and B3C2, under highlands and lowlands in Peru.

Evaluation of advanced clones from populations: B3C1 and B3C2 for quality of frying color in Chips and French fries in tubers harvested in Huancayo in highlands; Peru at 3200 masl, La Molina in lowlands at 150 masl, Comas in Highlands at 2850 msl and Oxapampa in mid-elevation at 1850 msl. Several clones as CIP395123.6, CIP 393077.159, CIP391585.79, CIP396026.101, CIP396034.103, CIP396036.101, CIP393371.164, had excellent frying color in both highland and lowlands, these clones can be used for the processing industry. In addition, their quality for processing has resistance to late blight.
"
  
  uri <- "doi:10.21223/WQMMSN"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "fries_color;chips_color_",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-23",
    carob_completion = 80,
    carob_effort = 3.0
  )
  
  f1 <- ff[basename(ff) == "01_Data.xlsx"]
  
  r1 <- carobiner::read.excel(f1)
  
  #remove trailing *
  r1$FF <- as.numeric(gsub("[*]", "", r1$FF))
  r1$CH <- as.numeric(gsub("[*]", "", r1$CH))
  
  
  geo <- data.frame(
    location = c("COM", "OXA", "AYM", "HCO", "HYO", "CAJ", "HRL","CHAGLLA - HCO", "COCHACALLA-HCO", "HUARAPA-HCO", "PACAMARCA", "PILLAO-HCO", "Cusco", "Huancani", "LM" ),
    latitude = c(-11.9500, -10.5775, -14.7967, -9.9306, -12.0651, -7.1638, -11.4950,-9.8696, -10.35858, -9.77057,-11.8547, -9.66667, -13.5183, -13.18333, -12.0833 ),
    longitude = c(-75.0333, -75.4022, -73.3833, -76.2421, -75.2049, -78.5003, -77.2078, -75.7742, -76.20876, -76.20659, -75.41985, -75.96667, -71.9781, -75.91667, -76.9500),
    geo_from_source = FALSE
  )
  
  d <- data.frame(
    trial_id = paste0("WQMMSN_", r1$Year, "_", r1$Locality),
    variety = as.character(r1$Clone),
    location = r1$Locality,
    country = "Peru",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = NA,
    planting_date = as.character(r1$Year),
    harvest_date = NA,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    fries_color = as.numeric(r1$FF),
    ## New variable
    chips_color_ = as.numeric(r1$CH)
  )
  
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}