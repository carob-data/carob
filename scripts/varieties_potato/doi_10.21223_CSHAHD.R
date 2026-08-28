# R script for "carob"
# license: GPL (>=3)

## NOTES
# No plot area is provided therefore below columns could not be converted to terminag
#                                :NUM_STEM; could be stem_density (stems/ha)
#                                :NPE; could be plant_density (plants/ha)
#                                :NMTCII+MTWCI; could be tuber_density (tubers/ha)
#                                :MTWCII+NMTCI; could be yield_marketable (kg/ha) - same swap, and
#                                :NTP; could be tubers_density (plants/ha) 
# Changed coordinates from "S 010 14.691'", "E 0360 44.856'" to "S 01 14.691'", "E 036 44.856'"



carob_script <- function(path) {
  
  "
Dataset for: 3 LTVR introduced for bulking and evaluation in Kenya

Field trial results between LTVR x LBHT testing to generate new progenies.
"
  
  uri <- "doi:10.21223/CSHAHD"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
                                  data_organization = "CIP",
                                  publication = NA,
                                  project = "MEL 4038",
                                  design = "Potato clone evaluation trial",
                                  data_type = "on-station experiment",
                                  treatment_vars = "variety",
                                  response_vars = "virus_severity",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-08-26",
                                  carob_completion = 60,
                                  carob_effort = 4
  )
  
  f1 <- ff[basename(ff) == "4038_Data_.xlsx"]
  f2 <- ff[basename(ff) == "4038_DataDictionary_ElementDescription.xlsx"]
  f3 <- ff[basename(ff) == "4038_DataDictionary_UniqueIdentifier.xlsx"]
  
  r1a <- carobiner::read.excel(f1, sheet="Minimal")
  r1b <- carobiner::read.excel(f1, sheet="Material_list")
  r1c <- carobiner::read.excel(f1, sheet="Fieldbook_LTVRxLHT")
  r1d <- carobiner::read.excel(f1, sheet=" LTVRxLBHT extra tubers")
  r1e <- carobiner::read.excel(f1, sheet="Other")
  r2  <- carobiner::read.excel(f2)
  r3  <- carobiner::read.excel(f3)
  
  ## extract site information
  minimal <- setNames(r1a$Value, r1a$Factor)
  site_country   <- minimal[["Country"]]
  site_adm1      <- carobiner::fix_name(minimal[["Admin1"]], "title")
  site_adm2      <- carobiner::fix_name(minimal[["Admin2"]], "title")
  site_adm3      <- carobiner::fix_name(minimal[["Admin3"]], "title")
  site_location  <- gsub("\\s+", " ", trimws(minimal[["Locality"]]))
  site_elevation <- as.numeric(gsub("[^0-9.]", "", minimal[["Elevation"]]))
  site_planting  <- as.character(as.Date(as.numeric(minimal[["Planting date"]]), origin="1899-12-30"))
  site_harvest   <- as.character(as.Date(as.numeric(minimal[["Harvest date"]]), origin="1899-12-30"))
  site_latitude  <- -(1 + 14.691/60)
  site_longitude <- 36 + 44.856/60
  
  ## virus severity codes
  severity_map <- c(L = "low", M = "medium", H = "high")
  
  ## Trial 1: drop trailing blank export rows
  r1c <- r1c[!is.na(r1c$BLOCK), ]
  
  d1c <- data.frame(
    trial_id = "1",
    block_id = as.character(r1c$BLOCK),
    rep = as.integer(r1c$REP),
    plot_id = as.character(r1c$PLOT),
    variety = r1c$INSTN,
    virus_severity = severity_map[r1c$VIRUS_SCORING]
  )
  
  ## Trial 2: same trait set, no BLOCK
  r1d <- r1d[!is.na(r1d$PLOT), ]
  
  d1d <- data.frame(
    trial_id = "2",
    block_id = NA_character_,
    rep = as.integer(r1d$REP),
    plot_id = as.character(r1d$PLOT),
    variety = r1d$`Clone ID`,
    virus_severity = NA_character_
  )
  
  d <- rbind(d1c, d1d)
  
  ## check-cultivar alias + floating-point-safe parent accession lookup
  check_alias <- c(Asante = "CIP381381.20", Tigoni = "CIP381381.13")
  genealogy_key <- function(x) ifelse(x %in% names(check_alias), check_alias[x], x)
  fmt_parent <- function(x) ifelse(is.na(x), NA_character_, trimws(format(x, trim=TRUE, scientific=FALSE)))
  
  pedigree_of <- function(x) {
    gi <- match(genealogy_key(x), r1b[["Accession_Number"]])
    female <- fmt_parent(r1b[["Female_AcceNumb"]][gi])
    male <- fmt_parent(r1b[["Male_AcceNumb"]][gi])
    ifelse(is.na(female) | is.na(male), NA_character_, paste(female, "x", male))
  }
  
  population_of <- function(x) {
    gi <- match(genealogy_key(x), r1b[["Accession_Number"]])
    p <- trimws(r1b[["Population"]][gi])
    ifelse(p == "", NA_character_, p)
  }
  
  d$variety_pedigree <- pedigree_of(d$variety)
  population <- population_of(d$variety)
  d$variety_type <- ifelse(is.na(population), NA_character_,
                    ifelse(population == "LTVR", "breeding clone", "released check cultivar"))
  
  ## site-level fields, constant across all rows
  d$crop <- "potato"
  d$country <- site_country
  d$adm1 <- site_adm1
  d$adm2 <- site_adm2
  d$adm3 <- site_adm3
  d$location <- site_location
  d$elevation <- site_elevation
  d$latitude <- site_latitude
  d$longitude <- site_longitude
  d$geo_from_source <- TRUE
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- NA
  d$yield <- NA
  d$yield_moisture <- NA
  d$yield_part <- "tubers"
  d$K_fertilizer <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$yield_isfresh <- FALSE
  
  d$planting_date <- site_planting
  d$harvest_date <- site_harvest
  
  carobiner::write_files(path, meta, d)
}
