# R script for "carob"
# license: GPL (>=3)

## NOTES
# 4 site files (04, 05, 06, 07), 2017-18 season.
# Each site has different column structure
# Lat/Long hardcoded for CGIAR ocation website; IITA-SARAH for all sites

## ISSUES
# GRAIN_YIELDPLOT_KG is mislabeled across all 4 sites - metadata
# confirms it is actually in GRAMS formula: YIELD (kg/ha) = GRAIN_YIELDPLOT_KG(g) * 10000/6/1000,
# No coordinates found in the source data for any site. 
# Fertilizer, irrigation, and rotation information not present in any site's columns
# Disease/quality scores (PL_VIGOR, RUST, RUST_R6, Shattering, FLW_COLOR) for soybean not present
# DFFL and DF_P are duplicated column names in sites 06/07 (days then date) and in site 04 for DFFL only.

### Suggested new terms: rust_score (soybean rust); harvest_count (number of plants counted at harvest, per plot);
###                      

carob_script <- function(path) {
  
  "
Preliminary Variety Trials (PVT) Zambia-2018

Soybean (Glycine max (L.) Merrill.) is one of the most important oil crops of the world 
which also has tremendous importance as a food legume. The work on soybean aims at providing farmers, 
both commercial and subsistence, varieties with their preferred attributes to increase yield and income. 
These include high yield, resistance to deadly diseases, such as soybean rust, 
and insect pests, early maturity, good seed quality, and resistance to other stresses such as 
drought and soil acidity.The International Institute of Tropical Agriculture (IITA) 
is a key player in tropical soybean research and a partner of the Soybean Innovation Lab.
"
  
  uri <- "doi:10.25502/67ny-tg43"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "IITA",
                                  publication = NA,
                                  project = NA,
                                  design = "Preliminary Variety Trial (PVT), RCBD, 4 sites, single 2017-18 season",
                                  data_type = NA,
                                  treatment_vars = "variety",
                                  response_vars = "yield; plant_height; seed_weight",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-08-21",
                                  carob_completion = 80,
                                  carob_effort = 4
  )
  
  f1  <- ff[basename(ff) == "18pvt-04-site-1.csv"]
  f4  <- ff[basename(ff) == "18pvt-05-site-1.csv"]
  f7  <- ff[basename(ff) == "18pvt-06-site-1.csv"]
  f10 <- ff[basename(ff) == "18pvt-07-site-1.csv"]
  
  r1  <- read.csv(f1)
  r4  <- read.csv(f4)
  r7  <- read.csv(f7)
  r10 <- read.csv(f10)
  
  ####--------------------------------------------------------------------------
  # Site 04 - IITA-SARAH
  ####--------------------------------------------------------------------------
  ### YIELD confirmed already kg/ha.
  ### DFFL appears twice - first is days, second (auto-renamed DFFL.1) is date.
  d1 <- data.frame(
    trial_id = "18PVT_Site04",
    location = "IITA-SARAH",
    country = "Zambia",
    adm1 = "Lusaka Province",
    adm2 = "Chongwe District",
    latitude = -15.1809, # source CGIAR locations website
    longitude = 28.18173,
    on_farm = FALSE,
    is_survey = FALSE,
    geo_from_source = FALSE,
    plot_id = r1$PLOT_NO,
    rep = r1$REP_NO,
    block = r1$BLOCK_NO,
    entry_no = r1$ENTRY_NO,
    variety = r1$DESIGNATION,
    pedigree = r1$CROSS,
    seed_source = r1$SOURCE,
    planting_date = as.character(as.Date(r1$DATE_PLANTED, format = "%d/%m/%Y")),
    plant_vigor = r1$PL_VIGOR,
    flowering_days = r1$DFFL,
    flowering_date = as.character(as.Date(r1$DFFL.1, format = "%d/%m/%Y")),
    podding_days = NA,
    podding_date = as.character(as.Date(NA)),
    rust_score = NA,
    flower_color = NA,
    maturity_date = as.character(as.Date(r1$Date_PM, format = "%d/%m/%Y")),
    maturity_days = r1$DM,
    plant_height = r1$PLHT,
    pod_clearance = r1$POD_CL,
    harvest_count = r1$HARVEST,
    yield = r1$YIELD,
    yield_part = "seed",
    plot_area = ifelse(r1$YIELD > 0, r1$GRAIN_YIELDPLOT_KG * 10 / r1$YIELD, NA),
    seed_weight = r1$SWT100 * 10,
    shattering_score = r1$Shattering
  )
  #####-------------------------------------------------------------------------
  ### Site 05 - IITA- SARAH
  ####--------------------------------------------------------------------------
  # No planting/flowering/ maturity dates at all in this site
  ### DFFL and DF_P here are single columns (days only, no date variant).
  d4 <- data.frame(
    trial_id = "18PVT_Site05",
    location = "IITA-SARAH",
    country = "Zambia",
    adm1 = "Lusaka Province",
    adm2 = "Chongwe District",
    latitude = -15.1809, # source CGIAR locations website
    longitude = 28.18173,
    on_farm = FALSE,
    is_survey = FALSE,
    geo_from_source = FALSE,
    plot_id = r4$PLOT,
    rep = r4$REP,
    block = r4$BLOCK,
    entry_no = r4$ENTRY,
    variety = r4$VARIETY,
    pedigree = r4$PEDIGREE,
    seed_source = NA,
    planting_date = as.character(as.Date(NA)),
    plant_vigor = r4$PL_VIGOR,
    flowering_days = r4$DFFL,
    flowering_date = as.character(as.Date(NA)),
    podding_days = r4$DF_P,
    podding_date = as.character(as.Date(NA)),
    rust_score = r4$RUST,
    flower_color = NA,
    maturity_date = as.character(as.Date(NA)),
    maturity_days = r4$DM,
    plant_height = r4$PLHT,
    pod_clearance = r4$POD_CL,
    harvest_count = r4$HARVEST_COUNT,
    yield = r4$YIELD,
    yield_part = "seed",
    plot_area = ifelse(r4$YIELD > 0, r4$GRAIN_YIELDPLOT_KG * 10 / r4$YIELD, NA),
    seed_weight = r4$SWT100 * 10,
    shattering_score = r4$SHATTERING
  )
  
  ####--------------------------------------------------------------------------
  ### Site 06 - IITA=SARAH
  ####--------------------------------------------------------------------------
  d7 <- data.frame(
    trial_id = "18PVT_Site06",
    location = "IITA-SARAH",
    country = "Zambia",
    adm1 = "Lusaka Province",
    adm2 = "Chongwe District",
    latitude = -15.1809, # source CGIAR locations website
    longitude = 28.18173,
    on_farm = FALSE,
    is_survey = FALSE,
    geo_from_source = FALSE,
    plot_id = r7$Plot,
    rep = r7$Rep,
    block = r7$Block,
    entry_no = r7$Entry,
    variety = r7$Name,
    pedigree = r7$Pedigree,
    seed_source = NA,
    planting_date = as.character(as.Date(r7$DATE_PLANTED, format = "%d/%m/%Y")),
    plant_vigor = r7$PL_VIGOR,
    flowering_days = r7$DFFL,
    flowering_date = as.character(as.Date(r7$DFFL.1, format = "%d/%m/%Y")),
    podding_days = r7$DF_P,
    podding_date = as.character(as.Date(r7$DF_P.1, format = "%d/%m/%Y")),
    rust_score = NA,
    flower_color = r7$FLW_COLOR,
    maturity_date = as.character(as.Date(r7$DATE_PM, format = "%d/%m/%Y")),
    maturity_days = r7$DM,
    plant_height = r7$PH,
    pod_clearance = r7$POD_CL,
    harvest_count = r7$HARVEST_COUNT,
    yield = r7$YIELD,
    yield_part = "seed",
    plot_area = ifelse(r7$YIELD > 0, r7$GRAIN_YIELDPLOT_KG * 10 / r7$YIELD, NA),
    seed_weight = r7$SWT100 * 10,
    shattering_score = NA
  )
  
  ####--------------------------------------------------------------------------
  ### Site 07 - IITA- SARAH
  ###---------------------------------------------------------------------------
  ### Adds RUST_R6 and PLANT_COUNT, not seen in any other site. 
  ### No shattering or flower color recorded.
  d10 <- data.frame(
    trial_id = "18PVT_Site07",
    location = "IITA-SARAH",
    country = "Zambia",
    adm1 = "Lusaka Province",
    adm2 = "Chongwe District",
    latitude = -15.1809, # source CGIAR locations website
    longitude = 28.18173,
    on_farm = FALSE,
    is_survey = FALSE,
    geo_from_source = FALSE,
    plot_id = r10$PLOT,
    rep = r10$REP,
    block = r10$BLOCK,
    entry_no = r10$ENTRY,
    variety = r10$NAME,
    pedigree = r10$PEDIGREE,
    seed_source = NA,
    planting_date = as.character(as.Date(r10$DATE_PLANTED, format = "%d/%m/%Y")),
    plant_vigor = r10$PL_VIGOR,
    flowering_days = r10$DFFL.1,
    flowering_date = as.character(as.Date(r10$DFFL, format = "%d/%m/%Y")),
    podding_days = r10$DF_P.1,
    podding_date = as.character(as.Date(r10$DF_P, format = "%d/%m/%Y")),
    rust_score = r10$RUST_R6,
    flower_color = NA,
    maturity_date = as.character(as.Date(r10$DATE_PM, format = "%d/%m/%Y")),
    maturity_days = r10$DM,
    plant_height = r10$PLHT,
    pod_clearance = r10$POD_CL,
    harvest_count = r10$PLANT_COUNT,
    yield = r10$YIELD,
    yield_part = "seed",
    plot_area = ifelse(r10$YIELD > 0, r10$GRAIN_YIELDPLOT_KG * 10 / r10$YIELD, NA),
    seed_weight = r10$SWT100 * 10,
    shattering_score = NA
  )
  
  d <- rbind(d1, d4, d7, d10)
  
  d$crop <- "soybean"
  d$yield_moisture <- NA
  d$yield_isfresh <- NA
  d$irrigated <- FALSE
  d$K_fertilizer <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$harvest_date <- as.character(as.Date(NA))
  
  names(d)[names(d) == "block"] <- "block_id"
  names(d)[names(d) == "pedigree"] <- "variety_pedigree"
  names(d)[names(d) == "DM"] <- "dm"
  
  d$block_id <- as.character(d$block_id)
  d$variety <- trimws(d$variety)
  d$variety_pedigree <- trimws(d$variety_pedigree)
  
  d$flower_color <- ifelse(d$flower_color == "" | is.na(d$flower_color), NA_character_, d$flower_color)
  d$plot_id <- as.character(d$plot_id)
  d$location <- "Iita-Sarah"
  d$flowering_days <- as.numeric(d$flowering_days)
  d$podding_days <- as.numeric(d$podding_days)
  
  # all rows kept - no dropping for NA fields absent at a given site
  
  carobiner::write_files(path, meta, wide=d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)