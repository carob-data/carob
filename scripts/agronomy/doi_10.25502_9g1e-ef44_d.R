# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station factorial trial (irrigation x fertilizer x variety x harvest
# timing), Ibadan, Nigeria (IITA). All biomass values are dry matter.
# Soil data (r3/r4) covers only 6 of 64 plots at 2 depths each, kept as its own table (d3) rather than
# merged into d, to avoid duplicating biomass rows for a different- dimension measurement.

## ISSUES
# Fert "75:20:90 NPK" treated as elemental kg/ha N:P:K (not oxide)
# green_branch_DM/Stem_branches_DM combined into dmy_stems (no separate
# terminag field for green vs woody branches); both kept individually too.
# Planting_stake_DM and HI_total have no terminag equivalent, kept under
# their original names.
# No planting date exists; only harvest round (H1/H2) is recorded.


carob_script <- function(path) {
  
"
Irrigation by fertilizer trial with cassava

A factorial on-station cassava trial at IITA's Ibadan station, Nigeria,
testing irrigation (rainfed vs supplemental drip irrigation), fertilizer
(none vs 75:20:90 kg/ha N:P:K), and variety (TME 419 vs TMS 0581), with two
harvest rounds and 4 replicates (64 plots total). Above- and below-ground dry
matter was partitioned into leaves, green branches, stems/brown branches,
planting stake, and roots (marketable/OK vs diseased/undersized), with
harvest index calculated two ways. A separate soil characterization (pH,
organic carbon, N, P, K, texture) was sampled at 6 of the 64 plots, at two
depths each.
"
  
  uri <- "doi:10.25502/9g1e-ef44/d"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "AKILIMO",
		design = "on-station factorial trial",
		data_type = "on-station experiment",
		treatment_vars = "irrigated;N_fertilizer;variety",
		response_vars = "dmy_storage;dmy_total",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_date = "2026-07-17",
		carob_completion = 65,
		carob_effort = 2
  )
  
  f1 <- ff[basename(ff) == "summaryyielddatairrigation4ckan.csv"]
#  f2 <- ff[basename(ff) == "data_dictionary_summaryyielddata.csv"]
  f3 <- ff[basename(ff) == "soildata.csv"]
#  f4 <- ff[basename(ff) == "data_dictionary_soildata.csv"]
  
  r1 <- read.csv(f1)
  r3 <- read.csv(f3)
   
  ### Main table: dry matter biomass (t/ha -> kg/ha)
  d <- data.frame(
    trial_id = "1",
    plot_id = as.character(r1$Plot),
    rep = r1$Rep,
    
    country = "Nigeria",
    adm1 = "Oyo",
    location = "Ibadan",
    on_farm = FALSE,
    is_survey = FALSE,
    crop = "cassava",

    N_fertilizer = ifelse(r1$Fert == "F0", 0, ifelse(r1$Fert == "F1", 75, NA)),
    P_fertilizer = ifelse(r1$Fert == "F0", 0, ifelse(r1$Fert == "F1", 20, NA)),
    K_fertilizer = ifelse(r1$Fert == "F0", 0, ifelse(r1$Fert == "F1", 90, NA)),
    fertilizer_used = r1$Fert == "F1",
    irrigated = r1$Irrigate == "I1",
    variety = ifelse(r1$V == "V419", "TME 419", ifelse(r1$V == "V581", "TMS 0581", NA)),
    
    harvest_round = r1$H,   # suggested field, no actual dates given
    
    yield = r1$OK_Root_DM * 1000,
    yield_part = "roots",
    yield_isfresh = FALSE,
    yield_moisture = 0,
    
    dmy_leaves = r1$leaf_DM * 1000,
    dmy_stems = (r1$green_branch_DM + r1$Stem_branches_DM) * 1000,
    dmy_storage = (r1$OK_Root_DM + r1$bad_root_DM) * 1000,
    dmy_total = r1$TOTAL * 1000,

    seed_rate = r1$Planting_stake_DM * 1000,  # DM not captured
    
	harvest_index = r1$HI_total,
	## not that important as it is a derived value
    #harvest_index_OKroots = r1$HI_OK_roots * 100,    
	
    ## suggested fields, kept under their original names
    #green_branch_DM = r1$green_branch_DM * 1000,
    #stem_branches_DM = r1$Stem_branches_DM * 1000,
    
    
    longitude = 3.9470,
    latitude = 7.4419,
    geo_from_source = FALSE
  )
  d$planting_date <- NA
  
  ### Soil table (d3) - not merged into d, see NOTES
  depth_parts <- strsplit(r3$DEPTH, "-")
  d3 <- data.frame(
    plot_id = as.character(r3$PLOT),
    sample_id = as.character(r3$Sample_ID),
    depth_top = as.numeric(sapply(depth_parts, `[`, 1)),
    depth_bottom = as.numeric(sapply(depth_parts, `[`, 2)),
    
    soil_pH = r3$pH,
    soil_C_total = r3$OC_perc,
    soil_N_total = r3$N_Perc * 10000,   # % -> mg/kg
    soil_P = r3$MehP_ppm,               # ppm == mg/kg
    soil_K_exch = r3$K,                 # cmol(+)/kg
    soil_sand = r3$Sand,
    soil_silt = r3$Silt,
    soil_clay = r3$Clay
  )  
  #d3$trial_id <- "1"
  
  soil <- aggregate(d3[, c("soil_pH", "soil_C_total", "soil_N_total", "soil_P", "soil_K_exch", "soil_sand", "soil_silt", "soil_clay")],
					d3[, c("depth_top", "depth_bottom")], mean)
  
  soil <- data.frame(plot_id = rep(d$plot_id[!(d$plot_id %in% d3$plot_id)], each=2), soil)
  
  carobiner::write_files(path, meta, d, long=soil)
}