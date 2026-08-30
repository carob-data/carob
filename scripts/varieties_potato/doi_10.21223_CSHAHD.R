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
  
  ## site-level extraction, assigned directly
  minimal <- setNames(r1a$Value, r1a$Factor)
  
  ## virus severity codes - see NOTES on the L/M/H expansion
  severity_map <- c(L = "low", M = "medium", H = "high")
  
  ## Trial 1: drop trailing blank export rows
  r1c <- r1c[!is.na(r1c$BLOCK), ]
  
  d <- data.frame(
    trial_id = "1",
    block_id = as.character(r1c$BLOCK),
    rep = as.integer(r1c$REP),
    plot_id = as.character(r1c$PLOT),
    variety = r1c$INSTN,
    virus_severity = severity_map[r1c$VIRUS_SCORING]
  )
  
  ## genealogy
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
  
  ## site-level fields, assigned directly - no intermediate variables
  d$crop <- "potato"
  d$country <- minimal[["Country"]]
  d$adm1 <- carobiner::fix_name(minimal[["Admin1"]], "title")
  d$adm2 <- carobiner::fix_name(minimal[["Admin2"]], "title")
  d$adm3 <- carobiner::fix_name(minimal[["Admin3"]], "title")
  d$location <- gsub("\\s+", " ", trimws(minimal[["Locality"]]))
  d$elevation <- as.numeric(gsub("[^0-9.]", "", minimal[["Elevation"]]))
  d$latitude <- -(1 + 14.691/60)
  d$longitude <- 36 + 44.856/60
  d$geo_from_source <- TRUE
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- NA
  d$planting_date <- as.character(as.Date(as.numeric(minimal[["Planting date"]]), origin="1899-12-30"))
  d$harvest_date <- as.character(as.Date(as.numeric(minimal[["Harvest date"]]), origin="1899-12-30"))
  
  d$yield <- NA
  d$yield_moisture <- NA
  d$yield_isfresh <- NA
  d$yield_part <- "tubers"
  d$K_fertilizer <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  
  carobiner::write_files(path, meta, d)

}
