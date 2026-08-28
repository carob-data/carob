
carob_script <- function(path) {
  
"
Dataset for: Potato NPT Trials Amidst Prolonged Rains in Kenyan Regions - October 2009
  
The Potato NPT Trials Amidst Prolonged Rains in Kenyan Regions took place
in October 2009 in various locations in Kenya, including Baraka,
Kibirichia, Kisima, Limuru and Narok. The study was conducted under a
randomized complete block design (RCBD) with eight potato clones and
four replications, aiming to assess the performance of these clones
under prolonged rainfall.
"
  
  uri <- "doi:10.21223/FBZ6JS"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group,  major = 2, minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "randomized complete block design",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-08-27",
    carob_completion = 85,
    carob_effort = 0.5
  )
  
  # Source files
  ffs <- c(ff[basename(ff) == "PTYL200910_BARAKA.xls"], ff[basename(ff) == "PTYL200910_KIBRCH.xls"],
           ff[basename(ff) == "PTYL200910_KISIMA.xls"], ff[basename(ff) == "PTYL200910_LIMURU.xls"],
           ff[basename(ff) == "PTYL200910_NAROK.xls"])

  # Read Minimal/Installation sheets for site information
  mins <- lapply(ffs, \(f) carobiner::read.excel(f, sheet = "Minimal"))
  inst <- lapply(ffs, \(f) carobiner::read.excel(f, sheet = "Installation"))
  
  # Extract coordinates from Minimal/Installation sheets
  get_value <- \(x, factor) {
    x[x[["Factor"]] == factor, "Value"]
  }
  
  latitude <- as.numeric(sapply(mins, \(m) get_value(m, "Latitude")))
  longitude <- as.numeric(sapply(mins, \(m) get_value(m, "Longitude")))
  elevation <- as.numeric(sapply(mins, \(m) get_value(m, "Elevation")))
  adm1 <- sapply(mins, \(m) get_value(m, "Admin1"))
  adm2 <- sapply(mins, \(m) get_value(m, "Admin2"))
  locations <- sapply(mins, \(m) get_value(m, "Locality"))
  planting <- sapply(mins, \(m) get_value(m, "Begin date"))
  harvest <- sapply(mins, \(m) get_value(m, "End date"))
  seeddens <- as.numeric(sapply(inst, \(m) get_value(m, "Planting density (plants/Ha)")))
  
  # Read Fieldbook sheets
  r <- lapply(1:length(ffs), \(i) {
    rr <- carobiner::read.excel(ffs[i], sheet = "Fieldbook", na = c("", "#N/D", "#DIV/0!", "NA"))
    rr$location <- locations[i]
    rr
  })
  #Baraka does not have MTYA. Compute from weight per plant, number of plants, and plot size
  r[[1]]$MTYA <- r[[1]]$MTWPL * r[[1]]$NTP / 1.08
  #Baraka does not have PPE
  r[[1]]$PPH <- 100 * r[[1]]$NPH / r[[1]]$NTP
  r <- do.call(carobiner::bindr, r)
	
  loc_idx <- match(r$location, locations)  
  
  #  final data.frame
  d <- data.frame(
    trial_id = paste("FBZ6JS", r$location, sep = "_"),
    plot_id = as.character(r$PLOT),
    rep = as.integer(r$REP),
    variety = r$INSTN,
    location = r$location,
    country = "Kenya",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = r$MTYA * 1000,
    yield_moisture = NA_real_,
    yield_isfresh = TRUE,
    latitude = latitude[loc_idx],
    longitude = longitude[loc_idx],
	elevation = elevation[loc_idx],
    geo_from_source = TRUE,
    planting_date = planting[loc_idx],
    harvest_date = harvest[loc_idx],
	seed_density = seeddens[loc_idx],
	plant_density = seeddens[loc_idx] * r$PPH / 100,
	tuber_density = r$MTYA * 1000000 / r$ATMW
  )

## original computationin xls went wrong because d$plant_density == 0
  i <- which(is.na(d$yield))
  d$yield[i] <- r$MTYNA[i] * 1000   
  d$plant_density[i] <- NA	
  
  d$yield_marketable <- d$yield	
	
  carobiner::write_files(path, meta, d)
}