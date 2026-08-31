
carob_script <- function(path) {

"
Dataset for: Participatory Varietal Selection of 4 frost tolerant clones in the localities of Aeropuerto, Allato, Chacapunco and Occo Centro in Huancavelica, Peru

The Mother and Baby (M&B) trial methodology was adapted by CIP for Participatory Variety Selection (PVS) through decentralized evaluation networks and multi-year evaluations in potato growing areas in the Andean region. The M&B trial design encourages active participation of farmers through the application of treatments in their own plots called 'Baby trials' (i.e. farmer managed trials) and in fields with an experimental design called 'Mother trials' (i.e. researcher managed trials), as well as through systematic evaluations and selections of treatments. Objective: Analyze characteristics, attributes and preferences that men and women have when selecting a new potato variety at the phase of flowering and harvesting. A M&B trial was performed to evaluate 4 clones with frost tolerance in the localities of Aeropuerto, Allato, Chacapunco y Occo Centro, in Angares province, in Huancavelica department, Peru. The trial design was a Randomized Complete Block Design (RCBD) with 3 replicates for Mother trials and 3 replicates during 2012-2013.  In this experiment characteristics of plant (size, type of foliage), yield, desirable quality, pest and disease resistance during flowering and harvesting were evaluated.  Total number of participants at flowering phase was 12(men=12 and women=0), 7(men=4, women = 3), 6(men=2, women = 4) and 8(men=5, women = 3) in the localities of Aeropuerto, Allato, Chacapunco y Occo Centro respectively. The number of participants at harvesting phase was 23 (men=17 and women=6), 8 (men=4, women = 4), 10 (men=5, women = 5), 21 (men=11, women = 9) in the four localities respectively.
"
  
  uri <- "doi:10.21223/P3/QJ10B7"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 4, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-08-25",
    carob_completion = 90,
    carob_effort = 2.0
  )
  
  ## Source files (processed data only)
  f1 <- ff[grepl("AEROPR.*_processed", ff, ignore.case = TRUE)]
  f2 <- ff[grepl("OCCO.*_processed", ff, ignore.case = TRUE)]
  f3 <- ff[grepl("ALLATO.*_processed", ff, ignore.case = TRUE)]
  f4 <- ff[grepl("CHACAP.*_processed", ff, ignore.case = TRUE)]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1, na = c("", "#N/D", "#DIV/0!", "NA"))
  r2 <- carobiner::read.excel(f2, na = c("", "#N/D", "#DIV/0!", "NA"))
  r3 <- carobiner::read.excel(f3, na = c("", "#N/D", "#DIV/0!", "NA"))
  r4 <- carobiner::read.excel(f4, na = c("", "#N/D", "#DIV/0!", "NA"))
  
  ## Add location identifiers
  r1$location <- "Aeropuerto"
  r2$location <- "Occo Centro"
  r3$location <- "Allato"
  r4$location <- "Chacapunco"
  
  ## Combine the four trials
  r <- carobiner::bindr(r1, r2, r3, r4)
  
  d <- data.frame(
    trial_id = paste0("QJ10B7", "_", r$location),
    plot_id = as.character(r$plot),
    rep = as.integer(r$rep),
    variety = r$variety,
    location = r$location,
    country = "Peru",
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = r$irrigated,
    yield_part = "tubers",
    yield = as.numeric(r$yield_fresh) * 1000,
    yield_marketable = as.numeric(r$mtyna) * 1000, 
    yield_moisture = NA,
    yield_isfresh = TRUE,
    latitude = r$latitude,
    longitude = r$longitude,
    elevation = as.numeric(r$elevation),
    geo_from_source = TRUE,
    planting_date = r$planting_date,
    harvest_date = r$harvest_date,
    N_fertilizer = r$n_fertilizer,
    P_fertilizer = r$p_fertilizer,
    K_fertilizer = r$k_fertilizer,
    soil_texture = r$soil_texture,
    treatment = r$field
  )
  
  ## Remove completely empty rows
  d <- d[!is.na(d$yield), ]
  
  carobiner::write_files(path, meta, d)
}
