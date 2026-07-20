# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station trial (IITA HQ, Ibadan, Nigeria), 2 sites/seasons (site and
# year are confounded: WB_East=2018, WB_West=2019), testing planting stake
# orientation (horizontal/slanted/vertical) and variety (TME419/TMS0581),
# 3-4 reps. Yields in Mg/ha (t/ha), converted to kg/ha (x1000).

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
 
  r1 <- read.csv(f1, na.strings="nd")
  r1 <- r1[!is.na(r1$ID) & r1$ID != "", ]
  
  d <- data.frame(

	trial_id = r1$Site,
    rep = r1$rep,
	planting_date = r1$year,
 
    country = "Nigeria",
    adm1 = "Oyo",
    adm2 = "Ibadan",
    location = "Ibadan, IITA HQ",
	site = r1$Site,   # WB_East / WB_West - specific field within the IITA station
    
    on_farm = FALSE,
    is_survey = FALSE,
    
    crop = "cassava",
    variety = r1$variety,
    treatment = r1$orientation,
    
	stake_orientation = r1$orientation,
    plant_density = r1$plants_m2_at_harvest * 10000,   # plants/m2 -> plants/ha
    
    yield = r1$useful_fresh_root_yield_Mg_ha * 1000,
    yield_part = "roots",
    yield_isfresh = TRUE,
    yield_moisture = (1 - r1$ppt_root_DM) * 100,
    
    dmy_storage = r1$useful_dry_root_yield_Mg_ha * 1000,
    
    root_density = r1$no_useful_roots_m2 * 10000,
    plant_survival = r1$Perc_survival,
    irrigated = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )

    # metadata has two lon/lat pairs. Assigning to site based on East/West names
	i <- d$site == "WB_East"
	d$longitude[i] = 3.88401
    d$latitude[i] = 7.4482
	d$longitude[!i] = 3.88285
    d$latitude[!i] = 7.48907 
    geo_from_source = TRUE

	d$record_id <- 1:nrow(d)
    sdens <- r1[, grepl("stems_m2_", names(r1))]
	dnms <- names(sdens)
	sdens$record_id <- 1:nrow(sdens)
	dns <- reshape(sdens, varying = dnms, v.names = "stem_density",
				times = dnms, timevar = "DAP",  direction = "long")
	dns$DAP <- gsub("stems_m2_|map", "", dns$DAP)
	dns$DAP <- gsub("at_harvest", "13", dns$DAP) # speculative
	dns$DAP <- as.integer(as.numeric(dns$DAP) * 30.4)
	dns$variable <- "stem_density"
	dns$id <- NULL

	carobiner::write_files(path, meta, d, long=dns)
}
