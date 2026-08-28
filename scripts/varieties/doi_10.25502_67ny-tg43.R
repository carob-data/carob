# R script for "carob"
# license: GPL (>=3)

## NOTES
# 4 site files (04, 05, 06, 07), 2017-18 season.
# Each site has different column structure
# All confirmed to be on the same site but different materials by Creator of the data
# location confirmed by data creator as Lat; -15.3000901, Lon; 28.304867
# Trial_ID hard coded per CSV title because S04 and S06 metadata title are the same
# plant_density calculated from harvest/plant_count through plot_area

# suggested terms: 
#  plant_vigor (Visual score of plant health; no stated bounds);
#  flower_color (visual color of flower - only one site);
#  pod_clearance (first pod height in cm);
#  Shattering_score (visual score of pre-harvest pod shattering / seed loss)

## ISSUES
# S04 "harvest_index" exceeds 100% - most likely mislabeled "Harvest count"?
# S05 - No planting/flowering/maturity DATES (only-day counts)
# S06 has blank location in metadata
# No actual lat/lon from metadata
					 

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
  
  ## do not use excessive indentation (change RStudio Global Options?)
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
  f2  <- ff[basename(ff) == "metadata-04-site-1.csv"] # metadata for site 4
  f4  <- ff[basename(ff) == "18pvt-05-site-1.csv"]
  f5  <- ff[basename(ff) == "metadata-05-site-1.csv"] # metadata for site 5
  f7  <- ff[basename(ff) == "18pvt-06-site-1.csv"]
  f8  <- ff[basename(ff) == "metadata-06-site-1.csv"] # metadata for site 6
  f10 <- ff[basename(ff) == "18pvt-07-site-1.csv"]
  f11  <- ff[basename(ff) == "meatadata-07-site-1.csv"] # metadata for site 7
  
  r1  <- read.csv(f1)
  r2  <- read.csv(f2) # Metadata for site 4
  r4  <- read.csv(f4)
  r5 <- read.csv(f5) # Metadata for site 5
  r7  <- read.csv(f7)
  r8 <- read.csv(f8) # Metadata for site 6
  r10 <- read.csv(f10)
  r11 <- read.csv(f11) # Metadata for site 7
  
  ####--------------------------------------------------------------------------
  # Site 04 - IITA-LUSAKA, Zambia
  ####--------------------------------------------------------------------------
  d1 <- data.frame(
    trial_id = "18pvt-04-site-1",       # hard coded per CSV naming
    
    plot_id = as.character(r1$PLOT_NO),
    rep = r1$REP_NO,
    block_id = r1$BLOCK_NO,
    variety_code = r1$ENTRY_NO,
    variety = r1$DESIGNATION,
    variety_pedigree = r1$CROSS,
##    seed_source = r1$SOURCE, not the seed _source_ (e.g. market). Also same as CROSS
    
    planting_date = as.character(as.Date(r1$DATE_PLANTED, format = "%d/%m/%Y")),
    plant_vigor = r1$PL_VIGOR,          # suggested term for plant health
    flowering_days = r1$DFFL,
    flowering_date = as.character(as.Date(r1$DFFL.1, format = "%d/%m/%Y")),
    maturity_date = as.character(as.Date(r1$Date_PM, format = "%d/%m/%Y")),
    maturity_days = r1$DM,
    plant_height = r1$PLHT,
    pod_clearance = r1$POD_CL,          # suggested term; first pod height in cm
    
    yield = r1$YIELD,
    yield_part = "seed",
    plot_area = ifelse(!is.na(r1$YIELD) & r1$YIELD > 0, r1$GRAIN_YIELDPLOT_KG * 10 / r1$YIELD, NA),
    seed_weight = r1$SWT100 * 10,       # estimate of 1000-seed weight
    shattering_score = r1$Shattering    # suggested term; pre-harvest pod splitting and seed loss
  )
  d1$plant_density <- ifelse(!is.na(d1$plot_area) & d1$plot_area > 0, r1$HARVEST / d1$plot_area * 10000, NA) # Harvest not an index assumed to be count
  
  #####-------------------------------------------------------------------------
  ### Site 05 - IITA-Lusaka
  ####--------------------------------------------------------------------------
  d4 <- data.frame(
    trial_id = "18pvt-05-site-1",
    
    plot_id = as.character(r4$PLOT),
    rep = r4$REP,
    block_id = r4$BLOCK,
    variety_code = r4$ENTRY,
    variety = r4$VARIETY,
    variety_pedigree = r4$PEDIGREE,
   
    plant_vigor = r4$PL_VIGOR,
    flowering_days = r4$DFFL,
    podding_days = r4$DF_P,
    disease = "rust",
    disease_severity = r4$RUST,
    maturity_days = r4$DM,
    plant_height = r4$PLHT,
    pod_clearance = r4$POD_CL,           # first pod height, cm
    
    yield = r4$YIELD,
    yield_part = "seed",
    plot_area = ifelse(!is.na(r4$YIELD) & r4$YIELD > 0, r4$GRAIN_YIELDPLOT_KG * 10 / r4$YIELD, NA),
    seed_weight = r4$SWT100 * 10,        # estimate of 1000-seed weight
    shattering_score = r4$SHATTERING
  )
  d4$plant_density <- ifelse(!is.na(d4$plot_area) & d4$plot_area > 0, r4$HARVEST_COUNT / d4$plot_area * 10000, NA)
  
  ####--------------------------------------------------------------------------
  # Site 06 - location NOT stated in data
  ####--------------------------------------------------------------------------
  d7 <- data.frame(
    trial_id = "18pvt-06-site-1",
    
    plot_id = as.character(r7$Plot),
    rep = r7$Rep,
    block_id = r7$Block,
    variety_code = r7$Entry,
    variety = r7$Name,
    variety_pedigree = r7$Pedigree,
   
    planting_date = as.character(as.Date(r7$DATE_PLANTED, format = "%d/%m/%Y")),
    plant_vigor = r7$PL_VIGOR,
    flowering_days = r7$DFFL,
    flowering_date = as.character(as.Date(r7$DFFL.1, format = "%d/%m/%Y")),
    podding_days = r7$DF_P,
    podding_date = as.character(as.Date(r7$DF_P.1, format = "%d/%m/%Y")),
    flower_color = r7$FLW_COLOR,         
    
    maturity_date = as.character(as.Date(r7$DATE_PM, format = "%d/%m/%Y")),
    maturity_days = r7$DM,
    plant_height = r7$PH,                
    pod_clearance = r7$POD_CL,
    
    yield = r7$YIELD,
    yield_part = "seed",
    plot_area = ifelse(!is.na(r7$YIELD) & r7$YIELD > 0, r7$GRAIN_YIELDPLOT_KG * 10 / r7$YIELD, NA),
    seed_weight = r7$SWT100 * 10
  )
  d7$plant_density <- ifelse(!is.na(d7$plot_area) & d7$plot_area > 0, r7$HARVEST_COUNT / d7$plot_area * 10000, NA)
  
  ####--------------------------------------------------------------------------
  # Site 07 - IITA-Lusaka, Zambia
  ####--------------------------------------------------------------------------
  d10 <- data.frame(
    trial_id = "18pvt-07-site-1",
    
    plot_id = as.character(r10$PLOT),
    rep = r10$REP,
    block_id = r10$BLOCK,
    variety_code = r10$ENTRY,
    variety = r10$NAME,
    variety_pedigree = r10$PEDIGREE,
    
    planting_date = as.character(as.Date(r10$DATE_PLANTED, format = "%d/%m/%Y")),
    plant_vigor = r10$PL_VIGOR,
    flowering_days = r10$DFFL.1,
    flowering_date = as.character(as.Date(r10$DFFL, format = "%d/%m/%Y")),
    podding_days = r10$DF_P.1,
    podding_date = as.character(as.Date(r10$DF_P, format = "%d/%m/%Y")),
    disease = "rust",
    disease_severity = r10$RUST_R6,
    timing = "R6",
    
    maturity_date = as.character(as.Date(r10$DATE_PM, format = "%d/%m/%Y")),
    maturity_days = r10$DM,
    plant_height = r10$PLHT,
    pod_clearance = r10$POD_CL,
    
    yield = r10$YIELD,
    yield_part = "seed",
    plot_area = ifelse(!is.na(r10$YIELD) & r10$YIELD > 0, r10$GRAIN_YIELDPLOT_KG * 10 / r10$YIELD, NA),
    seed_weight = r10$SWT100 * 10
  )
  d10$plant_density <- ifelse(!is.na(d10$plot_area) & d10$plot_area > 0, r10$PLANT_COUNT / d10$plot_area * 10000, NA)
  
  d <- carobiner::bindr(d1, d4, d7, d10)

  d$country <- "Zambia"
  d$location <- "IITA-SARAH"
  d$latitude <- -15.38753
  d$longitude <- 28.32282
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$geo_from_source <- FALSE
  
  # datatype fixes
  d$block_id <- as.character(d$block_id)
  
  # text cleaning
  d$variety <- trimws(d$variety)
  d$variety_code <- as.character(d$variety_code)
  d$disease_severity <- as.character(d$disease_severity	)
  
  d$variety_pedigree <- trimws(d$variety_pedigree)
  d$flower_color <- ifelse(is.na(d$flower_color) | trimws(d$flower_color) == "", NA_character_, trimws(d$flower_color))
  
  d$crop <- "soybean"
  d$yield_moisture <- NA
  d$yield_isfresh <- NA
  d$irrigated <- FALSE

  carobiner::write_files(path, meta, wide=d)
}
