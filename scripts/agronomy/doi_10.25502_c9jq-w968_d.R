# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station trial (IITA HQ, Ibadan, Nigeria), 2 sites/seasons (site and
# year are confounded: WB_East=2018, WB_West=2019), testing planting stake
# orientation (horizontal/slanted/vertical) and variety (TME419/TMS0581),
# 3-4 reps. Yields already in Mg/ha (t/ha), converted to kg/ha (x1000).

## ISSUES
# "nd" (not determined, per dictionary) in the stems_m2_*map columns
# converts to NA on read - all WB_West/2019 rows lack intermediate stem
# counts, only stem_density_harvest is available for that site.
# stem_density_*map, plant_survival, and root_density have no terminag
# equivalent - suggested as new terms, following the existing X_density
# (per ha) naming pattern.


carob_script <- function(path) {
  
  "
Cassava storage root yield as affected by planting stake orientation, and variety in two sites in SW Nigeria

An on-station trial at IITA's Ibadan station, Nigeria, testing cassava
planting stake orientation (horizontal, buried 5-7cm; slanted, ~45 degrees;
vertical, ~90 degrees) and variety (TME419 vs TMS0581), across two site x
season combinations (WB_East 2018, WB_West 2019), 3-4 replicates. Stem
counts were tracked from 1 to 11 months after planting (WB_East only), with
survival, root counts, and fresh/dry root yield recorded at final harvest.
"
  
  uri <- "doi:10.25502/c9jq-w968/d"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "IITA",
                                  publication = NA,
                                  project = "AKILIMO",
                                  design = "on-station trial",
                                  data_type = "on-station experiment",
                                  treatment_vars = "variety;treatment",
                                  response_vars = "yield;dmy_storage",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-07-20",
                                  carob_completion = 75,
                                  carob_effort = 2
  )
  
  f1 <- ff[basename(ff) == "staggered-planting-stake-orientation-expt-3.csv"]
  f2 <- ff[basename(ff) == "data_dictionary.csv"]
  
  r1 <- read.csv(f1)
  r1 <- r1[!is.na(r1$ID) & r1$ID != "", ]
  ## r2 is a reference/dictionary file, not data - not used further.
  
  d <- data.frame(
    trial_id = as.character(as.integer(as.factor(r1$Site))),
    rep = r1$rep,
    
    country = "Nigeria",
    adm1 = "Oyo",
    adm2 = "Ibadan",
    location = r1$Site,   # WB_East / WB_West - specific field within the IITA station
    
    on_farm = FALSE,
    is_survey = FALSE,
    
    crop = "cassava",
    variety = r1$variety,
    treatment = r1$orientation,
    
    plant_density = r1$plants_m2_at_harvest * 10000,   # plants/m2 -> plants/ha
    
    yield = r1$useful_fresh_root_yield_Mg_ha * 1000,
    yield_part = "roots",
    yield_isfresh = TRUE,
    yield_moisture = (1 - r1$ppt_root_DM) * 100,
    
    dmy_storage = r1$useful_dry_root_yield_Mg_ha * 1000,
    
    ## suggested new terms, following terminag's X_density (per ha) pattern
    ## converted from /m2
    stem_density_1map = as.numeric(r1$stems_m2_1map) * 10000,
    stem_density_3map = as.numeric(r1$stems_m2_3map) * 10000,
    stem_density_5map = as.numeric(r1$stems_m2_5map) * 10000,
    stem_density_7map = as.numeric(r1$stems_m2_7map) * 10000,
    stem_density_9map = as.numeric(r1$stems_m2_9map) * 10000,
    stem_density_11map = as.numeric(r1$stems_m2_11map) * 10000,
    stem_density_harvest = r1$stems_m2_at_harvest * 10000,
    root_density = r1$no_useful_roots_m2 * 10000,
    plant_survival = r1$Perc_survival,
    
    longitude = 3.9470,
    latitude = 7.4419,
    geo_from_source = FALSE,
    
    planting_date = NA,
    
    irrigated = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  
  carobiner::write_files(path, meta, d)
}
