carob_script <- function(path) {

#REJECTED because the response_vars values are all NA

"
Dataset for: Variety release documentation

Distinction, Homogeneity, and Stability (DHE) trials were installed in two locations (Junín and Huancavelica) during the rainy season in 2019-2020. Qualitative and quantitative characters were evaluated according to the guidelines of the International Union for the Protection of New Varieties of Plants for varietal descriptors of Solanum tuberosum L. (UPOV, 2004). Likewise, the evaluation of agronomic characters proposed by De Haan et al., 2014 was considered. The objective was to identify which varietal characters distinguish this new variety CIP-MATILDE (CIP512010.1) in comparison with the varieties controls and to determine their variability and stability.
"
  uri <- "doi:10.21223/FGA1DY"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-10-18",
    carob_completion = 50,
    carob_effort = 1.0
  )
  
  f1 <- ff[basename(ff) == "01-Fieldbook.xlsx"]
  r1 <- carobiner::read.excel(f1)
  
  geo <- data.frame(
    location = c("Junin", "Huancavelica"),
    latitude = c(-11.15, -12.78),
    longitude = c(-75.99, -74.97),
    geo_from_source = FALSE
  )
  
  d <- data.frame(
    trial_id = paste0("FGA1DY_", r1$Location),
    plot_id = as.character(r1$Plot),
    rep = as.integer(r1$Rep),
    variety = r1$`Variety name`,
    location = r1$Location,
    country = "Peru",
    crop = "potato",
    on_farm = NA,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = NA,
    yield_marketable = NA,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    planting_date = "2019",
    harvest_date = "2020"
  )
  
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}
