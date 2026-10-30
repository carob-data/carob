carob_script <- function(path) {
  
  "
Dataset for: Yield in all RTB crops in one cropping season along altitude gradients

Yield in potato and sweetpotato in one cropping season along altitude gradients in Tanzania
"
  
  uri <- "doi:10.21223/FOZT2L"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
  data_organization = "CIP",
  publication = NA,
  project = NA,
  design = NA,
  data_type = "on-farm experiment",
  treatment_vars = "variety;fertilizer_used;insecticide_used",
  response_vars = "yield",
  notes = NA,
  carob_contributor = "Maryam Yahya",
  carob_date = "2026-10-30",
  carob_completion = 70,
  carob_effort = 3.5
  )
  
  f1 <- ff[basename(ff) == "01_Yield_sweetpotato.xlsx"]
  f2 <- ff[basename(ff) == "02_Yield_potato.xlsx"]
  
  r1 <- carobiner::read.excel(f1, na = ".")
  r2 <- carobiner::read.excel(f2, na = ".")
  
  d1 <- data.frame(
    crop = r1$Crop,
    location = r1$Location,
    farmer = r1$`Code-farmer`,
    elevation = r1$Altitud_masl,
    rep = as.integer(r1$Rep),
    treatment = r1$Treatment,
    variety = NA,                   
    yield = r1$Yield_t_ha * 1000,
    yield_part = "roots"
  )
  
  d2 <- data.frame(
    crop = "potato",
    location = r2$Location,
    farmer = r2$`Code-farmer`,
    elevation = r2$Altitude,
    rep = as.integer(r2$Rep),
    treatment = r2$Treatment,
    variety = r2$Variety,
    yield = r2$yield_t_ha * 1000,
    yield_part = "tubers"
  )
  
  d <- carobiner::bindr(d1, d2)
  d <- d[!is.na(d$yield), ]
  
  d$trial_id <- paste(d$crop, d$location, d$farmer, sep = "_")
  d$farmer <- NULL
  
  d$fertilizer_used <- d$treatment %in% c("F", "F+I")
  d$insecticide_used <- d$treatment %in% c("I", "F+I")
  d$fertilizer_used[d$treatment == "Farmer"] <- NA
  d$insecticide_used[d$treatment == "Farmer"] <- NA
  
  d$country <- "Tanzania"
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- NA
  d$planting_date <- NA
  d$harvest_date <- NA        
  d$yield_moisture <- NA
  d$yield_isfresh <- TRUE
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$K_fertilizer <- NA
  geo <- data.frame(
    location = c("Shira", "Nyange", "Ntaruka", "Ruhunde", "Gataraga"),
    latitude = c(-3.0337, -3.5971, -2.850, -2.700, -2.550),
    longitude = c(37.2372, 30.5410, 35.500, 35.300, 34.900),
    geo_from_source = FALSE
  )
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}
