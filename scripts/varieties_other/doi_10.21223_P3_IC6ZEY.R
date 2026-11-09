
carob_script <- function(path) {
  
  "
Dataset for: Continuous Storage Root Formation and Bulking in Sweetpotato

The data set for the 'Continuous Storage Root Formation and Bulking in sweetpotato ' Gates Open Research paper has  on-station plant breeding trials conducted at Namulonge and Serere in Wakiso and Serere districts respectively  between 2016 and 2017 leading to an original research article. The on-station data are in Excel format for Sweetpotato Breeding Protocol (manual) http://www.sweetpotatoknowledge.org/). The on-station data on Continuous storage root formation and bulking scores, root numbers, vegetative growth data, and root weight from plots over 4 harvesting times used to estimate growth overtime and compute root yield, and biomass yield was the basis for the article. The pdf file has details on background information,  site description, materials and methods, analysis, results and discussion, and relevant tables and illustrations. Supporting raw data and analysis tables include spreadsheets for singles harvests (at 90, 120, 150 and 180 Days after planting), the combined data sheet for all seasons and locations, and its respective analysis tables, summary means and breeding value estimations.
"
  
  uri <- "doi:10.21223/P3/IC6ZEY"
  group <- "varieties_other"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety;harvest_days",
    response_vars = "yield;fwy_residue;harvest_index;storage_root_bulking_",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-11-01",
    carob_completion = 80,
    carob_effort = 2.5
  )
  f1 <- ff[basename(ff) == "Continuous_Storage_Root_Bulking_Data and Analysis.xlsx"]
  f3 <- ff[basename(ff) == "Continuous_Storage_Root_Bulking_Material_list.xlsx"]
  
  r <- suppressWarnings(carobiner::read.excel(f1, sheet = "8_Combined harvests", na = "*"))
  r3 <- carobiner::read.excel(f3)
  
  d <- data.frame(
    trial_id = paste(c("Namulonge", "Serere")[r$Loc], c("2017A", "2017B")[r$Seas], sep = "_"),
    plot_id = as.character(r$Plot),
    rep = as.integer(r$Rep),
    variety_code = r$ENTRY,
    variety = r3$Names[match(r$ENTRY, r3$Code)],
    location = c("Namulonge", "Serere")[r$Loc],
    adm1 = c("Wakiso", "Serere")[r$Loc],
    country = "Uganda",
    latitude = c(0.530, 1.510)[r$Loc],
    longitude = c(32.615, 33.450)[r$Loc],
    geo_from_source = FALSE,
    season = c("first", "second")[r$Seas],
    planting_date = "2017",
    harvest_date = NA,
    harvest_days = c(90, 120, 150, 180)[r$HT],
    crop = "sweetpotato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield = r$SRY * 1000,
    yield_part = "roots",
    yield_moisture = as.numeric(NA),
    yield_isfresh = TRUE,
    fwy_residue = r$VY * 1000,
    harvest_index = r$HI,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    ## NEW VARIABLES
    # storage_root_bulking_: CSRFAB, visual score 1-9, no unit; ability to keep forming new storage roots (1 = none, 4-9 = 2 to 7 root size classes)
    storage_root_bulking_ = r$CSRFAB
  )
  
  carobiner::write_files(path, meta, d)
}
