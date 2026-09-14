carob_script <- function(path) {
  
  "
  Dataset for: Global multi-environment resistance QTL for foliar late blight resistance in tetraploid potato with tropical adaptation
  
  This dataset only contains summary information. No plot-level data, yield, variety names, or management information are available.
  "
  
  uri <- "doi:10.21223/5O3NT5"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "compilation",
    treatment_vars = "none",
    response_vars = "rAUDPC",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-10",
    carob_completion = 60,
    carob_effort = 3.0
  )
  
  ## Source files
  f2 <- ff[basename(ff) == "Table 2 Geographical location.xlsx"]
  
  ## Read source data
  r2 <- carobiner::read.excel(f2)
  
  ## Clean data
  r2 <- r2[!is.na(r2$Country) & !is.na(r2$Year), ]
  r2$location <- trimws(sub(" [0-9].*", "", r2$Location))
  r2$rAUDPC <- as.numeric(gsub("[^0-9.]", "", r2$`H2 rAUDPC (Cullis et al. 2016)`))
  r2$Year <- as.integer(gsub("[^0-9]", "", r2$Year))
  
  ## Coordinates
  geo <- data.frame(
    location = c("Pasco, Oxapampa", "Yunnan, Kunming", "Oromia, Holetta"),
    latitude = c(-10.5853, 24.8801, 9.0633),
    longitude = c(-75.4053, 102.8329, 38.4902),
    country = c("Peru", "China", "Ethiopia"),
    geo_from_source = FALSE
  )
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = paste("5O3NT5", r2$Country, r2$Year, sep = "_"),
    country = r2$Country,
    location = r2$location,
    variety_release_year = r2$Year,
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    rAUDPC = r2$rAUDPC
  )
  
  ## Merge coordinates
  d <- merge(d, geo, by = c("location", "country"), all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}