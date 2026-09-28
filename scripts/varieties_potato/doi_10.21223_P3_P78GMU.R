carob_script <- function(path) {
  
  "
  Replication data for: New Elite Potato Clones with Heat Tolerance, Late Blight and Virus Resistance to Address Climate Change

  In 2012–2013, 61 advanced clones resulting from selection at San Ramon, La Molina and Oxapampa were assessed for tuber yield in San Ramon, La Molina and Majes under high temperatures. All trials were conducted in a simple lattice design, with 20 plants per plot, using two varieties as controls: (i) Désirée as heat tolerant; and (ii) Amarilis as heat sensitive. The traits measured were plant vigour, foliage maturity at harvest, tuber appearance, marketable tuber number and marketable and total tuber yield. Analysis of variance for tuber yield and the additive main effects and multiplicative interaction (AMMI) model for stability analysis were performed using the statistical software R (R Core Team, 2012). The results of the AMMI model analysis were interpreted on the basis of two AMMI graphs for principal components and tuber yield.
  "
  
  uri <- "doi:10.21223/P3/P78GMU"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 2,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "simple lattice",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-21",
    carob_completion = 80,
    carob_effort = 4.0
  )
  
  ## Source files
  f2 <- ff[basename(ff) == "PTLate blight092012_OXAPMP.xlsx"]
  f3 <- ff[basename(ff) == "PTYield072012_CIPSRM-1.xlsx"]
  f4 <- ff[basename(ff) == "PTYield102012_CIPHQ.xlsx"]
  f5 <- ff[basename(ff) == "PTYield112012_MAJ.xlsx"]
  
  ## Read source data
  r2f <- carobiner::read.excel(f2, sheet = "Fieldbook")
  r3f <- carobiner::read.excel(f3, sheet = "Fieldbook")
  r4f <- carobiner::read.excel(f4, sheet = "Fieldbook")
  r5f <- carobiner::read.excel(f5, sheet = "Fieldbook")
  
  ## Add location
  r2f$location <- "Oxapampa"
  r3f$location <- "San Ramon"
  r4f$location <- "La Molina"
  r5f$location <- "Majes"
  
  ## Combine
  r <- carobiner::bindr(r2f, r3f, r4f, r5f)
  
  ## Coordinates (from source Minimal sheets)
  geo <- data.frame(
    location = c("Oxapampa", "San Ramon", "La Molina", "Majes"),
    latitude = c(-10.57745, -11.1275, -12.076289, -16.46666667),
    longitude = c(-75.4043, -75.356389, -76.948417, -72.100000),
    geo_from_source = TRUE
  )
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = paste0("P78GMU_", r$location),
    plot_id = as.character(r$PLOT),
    rep = as.integer(r$REP),
    variety = r$INSTN,
    location = r$location,
    country = "Peru",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = r$TTYNA * 1000,
    yield_marketable = r$MTYNA * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    planting_date = "2012",
    harvest_date = "2013",
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  
  ## Merge coordinates
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  ## Write CAROB files
  carobiner::write_files(path, meta, d)
}

