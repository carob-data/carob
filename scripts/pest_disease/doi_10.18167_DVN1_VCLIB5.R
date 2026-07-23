# R script for "carob"
# license: GPL (>=3)

## NOTES
# 200 weed abundance surveys (Abidjan, 2015-2016), cassava/maize/okra. One
# row per species observed (rating>0), converted to % cover via the source's
# 1-9 scale. Species linked to Latin name/family via r1d's lookup table.
# Site metadata (soil_type, landscape, crop stage, total cover) repeats per
# row. soil_type covers 5 mutually-exclusive categories from the source
# (sandy/sandy-clay/clayey/hydromorphic/gravelly) - a real terminag field
# with no restricted vocabulary, unlike soil_texture. Source in French,
# translated below.

## ISSUES
# Culture_origine sometimes lists 2 crops - first used as crop, rest go in
# intercrops. f3-f5 duplicate the Excel workbook, not used. Landscape/
# growth-stage are one-hot coded in source, collapsed into single fields
# here. Some species codes may not match the r1d lookup - left as NA.


carob_script <- function(path) {
  
  "
Weed survey in food crops in Abidjan district, Cote d'Ivoire (2015)

Weed abundance survey of food crops (cassava, maize, okra) in the
peri-urban area of Abidjan district, Cote d'Ivoire, 2015-2016. 200 survey
plots, 324 weed species, rated on a cover-abundance scale (1-9, converted
here to % ground cover). Site metadata includes soil type, landscape
position, crop growth stage at survey, and total weed cover. Species codes
are linked to Latin names and plant families via a lookup table included
in the source workbook.
"
  
  uri <- "doi:10.18167/DVN1/VCLIB5"
  group <- "pest_disease"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
                                  data_organization = "CIRAD",
                                  publication = NA,
                                  project = "Amatrop",
                                  design = "weed abundance survey, 200 plots in cassava/maize/okra food-crop fields",
                                  data_type = "survey",
                                  treatment_vars = "none",
                                  response_vars = "weed_species;weed_cover",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-07-23",
                                  carob_completion = 75,
                                  carob_effort = 5
  )
  
  f1 <- ff[basename(ff) == "CDI-SAB-2015-VIV-AD.xlsx"]
  f2 <- ff[basename(ff) == "CDI-SAB-2015-VIV-AD-DOC.txt"]
  f3 <- ff[basename(ff) == "CDI-SAB-2015-VIV-AD-FLO.txt"]
  f4 <- ff[basename(ff) == "CDI-SAB-2015-VIV-FAC.txt"]
  f5 <- ff[basename(ff) == "CDI-SAB-2015-VIV-PA-FLO.txt"]
  
  r1a <- carobiner::read.excel(f1, sheet="M\u00e9ta donn\u00e9es")
  r1b <- carobiner::read.excel(f1, sheet="Facteurs")
  r1c <- carobiner::read.excel(f1, sheet="Floristique")
  r1d <- carobiner::read.excel(f1, sheet="Donn\u00e9es origine")
  
  ## r2/f3/f4/f5: documentation and duplicate plain-text versions of r1b/r1c,
  ## not used further - see ISSUES
  r2 <- readLines(f2, encoding = "latin1")
  
  ### Species Latin name / family lookup, from r1d (columns: 2=name, 3=family,
  ### 4=code). The table starts the row after the "CODE" header and runs to
  ### the last row with a real code value.
  header_row <- which(r1d[[4]] == "CODE")
  last_row <- max(which(!is.na(r1d[[4]]) & r1d[[4]] != "CODE"))
  species_lookup <- data.frame(
    code = r1d[[4]][(header_row + 1):last_row],
    weed_species = r1d[[2]][(header_row + 1):last_row],
    weed_family = r1d[[3]][(header_row + 1):last_row]
  )
  
  ### Site-level metadata, from r1b, translated from French
  get_row <- function(label) unlist(r1b[r1b[[1]] == label, -1])
  
  soil_lookup <- data.frame(
    fr = c("SABLEU", "SABLAR", "ARGILE", "HYDRO", "GRAVIL"),
    en = c("sandy", "sandy-clay", "clayey", "hydromorphic", "gravelly")
  )
  pos_lookup <- data.frame(
    fr = c("PLATEA", "HAUPEN", "VERSAN", "BASPEN"),
    en = c("plateau", "upper slope", "hillside", "lower slope")
  )
  stage_lookup <- data.frame(
    fr = c("DEBCLE", "MILCLE", "FINCLE"),
    en = c("early", "mid", "late")
  )
  
  pick_onehot <- function(labels, lookup) {
    vals <- sapply(labels, function(l) {
      row <- suppressWarnings(as.numeric(get_row(l)))
      ifelse(!is.na(row) & row == 1, lookup$en[lookup$fr == l], NA)
    })
    apply(vals, 1, function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA else x[1] })
  }
  
  crop_lookup <- c(manioc = "cassava", mais = "maize", gombo = "okra", tomate = "tomato", vivrier = "unknown")
  
  culture <- get_row("Culture_origine")
  culture_split <- strsplit(tolower(trimws(culture)), "/")
  crop_main <- sapply(culture_split, function(x) unname(crop_lookup[trimws(x)[1]]))
  crop_extra <- sapply(culture_split, function(x) {
    if (length(x) <= 1) return("none")
    extras <- unname(crop_lookup[trimws(x)[-1]])
    paste(extras, collapse = ";")
  })
  
  d1b <- data.frame(
    trial_id = names(r1b)[-1],
    crop = crop_main,
    intercropped = crop_extra != "none",
    intercrops = crop_extra,
    crop_local = culture,
    crop_group = get_row("Type_culture"),   # suggested field
    obs_year = as.character(as.integer(get_row("Ann\u00e9e"))),
    country = "C\u00f4te d'Ivoire",
    location = get_row("Lieu"),
    soil_type = pick_onehot(soil_lookup$fr, soil_lookup),
    landscape_position = pick_onehot(pos_lookup$fr, pos_lookup),
    growth_stage = pick_onehot(stage_lookup$fr, stage_lookup),
    total_weed_cover = as.numeric(get_row("RECOUVR")),   # suggested field, survey-level total (%)
    irrigated = tolower(get_row("Irrigation")) %in% c("inonde", "inond\u00e9"),
    aez = get_row("Climat")   # suggested field - climate zone
  )
  
  ### Coordinates - single location (Abidjan) for all surveys
  d1b$longitude <- -4.0083
  d1b$latitude <- 5.3600
  d1b$adm1 <- "Abidjan"
  d1b$geo_from_source <- FALSE
  
  ### Weed abundance, one row per species actually observed per survey.
  ### Ratings (1-9) converted to % cover using the source's decoding table.
  rating_to_cover <- data.frame(rating = 1:9, cover = c(1, 7, 15, 30, 50, 70, 85, 93, 100))
  
  present <- which(!is.na(r1c[, -1]) & r1c[, -1] > 0, arr.ind = TRUE)
  d <- data.frame(
    trial_id = names(r1c)[-1][present[, "col"]],
    weed_code = r1c[[1]][present[, "row"]],
    weed_cover = rating_to_cover$cover[match(r1c[, -1][present], rating_to_cover$rating)]
  )
  
  ### Link species codes to Latin name / family
  d <- merge(d, species_lookup, by.x = "weed_code", by.y = "code", all.x = TRUE)
  
  d <- merge(d, d1b, by = "trial_id", all.x = TRUE)
  
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  d$yield <- NA
  d$yield_part <- NA
  d$yield_moisture <- NA
  d$yield_isfresh <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$K_fertilizer <- NA
  d$planting_date <- NA
  d$harvest_date <- NA
  
  carobiner::write_files(path, meta, d)
}