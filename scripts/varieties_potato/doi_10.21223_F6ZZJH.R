carob_script <- function(path) {
  
  "
  Dataset for: Assessment of baked potatoes quality traits during 2020-2021

  In 2021, in the CIP physiology and post-harvest laboratory in La Molina, quality tests were carried out for baked in eight potato clones with high levels of resistance to late blight and two varieties Canchan and Unica, the samples came from 5 localities of Peru, where these clones were planted in adaptation and efficiency experiments for the registration of varieties within the CIP-PODEROSA project, under the tax research law, Law 30309, which encourages the Peruvian government to encourage agricultural research in the country. Tests to determine heat for baking, evaluating flavor and texture of the clones after baking. The clone CIP396034.103 was selected for high-quality for baking.
  "
  
  uri <- "doi:10.21223/F6ZZJH"
  group <- "varieties_potato"
  ff <- carobiner::get_data(uri, path, group)

  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "tuber_flavor;tuber_texture",
		notes = NA,
		carob_contributor = "Maryam Yahya",
		carob_date = "2026-09-02",
		carob_completion = 85,
		carob_effort = 1.5
  )
  
  ## Source files
  f1 <- ff[basename(ff) == "01_Potato Baked Processing Results Majes 2020-2021.xlsx"]
  f2 <- ff[basename(ff) == "02_Potato Baked Processing Results Huancayo 2020-2021.xlsx"]
  f3 <- ff[basename(ff) == "03_Potato Baked Processing Results Huamachuco-Licame 2020-2021.xlsx"]
  f4 <- ff[basename(ff) == "04_Potato Baked Processing Results Huamachuco-Yanac 2020-2021.xlsx"]
  f5 <- ff[basename(ff) == "05_Potato Baked Processing Results Cajamarca 2020-2021.xlsx"]
  f6 <- ff[basename(ff) == "06_Potato Baked Processing Results Huanuco 2020-2021.xlsx"]
  
  # Read source data
  r1 <- carobiner::read.excel(f1, na = c("", "#N/D", "#DIV/0!", "NA", "-"))
  r2 <- carobiner::read.excel(f2, na = c("", "#N/D", "#DIV/0!", "NA", "-"))
  r3 <- carobiner::read.excel(f3, na = c("", "#N/D", "#DIV/0!", "NA", "-"))
  r4 <- carobiner::read.excel(f4, na = c("", "#N/D", "#DIV/0!", "NA", "-"))
  r5 <- carobiner::read.excel(f5, na = c("", "#N/D", "#DIV/0!", "NA", "-"))
  r6 <- carobiner::read.excel(f6, na = c("", "#N/D", "#DIV/0!", "NA", "-"))
  
  # Add location
  r1$location <- "Majes"
  r2$location <- "Huancayo"
  r3$location <- "Huamachuco Licame"
  r4$location <- "Huamachuco Yanac"
  r5$location <- "Cajamarca"
  r6$location <- "Huanuco"
  
  # Combine
  r <- carobiner::bindr(r1, r2, r3, r4, r5, r6)

  # It seems that, generally, an Evaluator evaluates each variety, so not much to correct for and values can be averaged over Evaluators. 
  # It is not clear if Evaluator 1 is the same person in each file. 
  # Alternatively, each combination of Repetition/Evaluator could be treated as a repetition
  r <- aggregate(r[, c("Flavor", "Texture")], r[,c("Clone", "Repetition", "location")], mean, na.rm=TRUE)
  
  # Coordinates estimated from Google Maps (September 2026)
  geo <- data.frame(
    location = c("Majes", "Huancayo", "Huamachuco Licame", "Huamachuco Yanac", "Cajamarca", "Huanuco"),
    latitude = c(-16.3625, -12.0651, -7.8133, -7.8000, -7.1638, -9.9306),
    longitude = c(-72.1911, -75.2049, -77.7733, -77.8000, -78.5000, -76.2422),
    geo_source = "Google Maps",
    geo_from_source = FALSE
  )
  
  # Management variables set to NA (not in source)
  d <- data.frame(
    trial_id = "F6ZZJH",
    plot_id = paste(r$location, r$Plot, sep = "_"),
    rep = as.integer(r$Repetition),
    variety = r$Clone,
    location = r$location,
    country = "Peru",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    N_fertilizer = NA_real_,
    P_fertilizer = NA_real_,
    K_fertilizer = NA_real_,
    planting_date = NA,
    harvest_date = NA,
    yield_part = "tubers",
    yield = NA_real_,
    yield_moisture = NA_real_,
    yield_isfresh = NA,
    tuber_flavor = r$Flavor,
    tuber_texture = r$Texture
  )
  
  d <- merge(d, geo, by = "location", all.x = TRUE)
    
  carobiner::write_files(path, meta, d)
}
