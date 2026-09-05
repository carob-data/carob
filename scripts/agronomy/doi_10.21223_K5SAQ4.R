# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1 whats left is to add the season

carob_script <- function(path) {

"
Dataset for: Innovations to improve on farm seed quality in potato production systems in Kenya

Farmer managed on-farm trials, with 8 sites, each site is a replication"

	uri <- "doi:10.21223/K5SAQ4"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "seed_source",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-08-27",
		carob_completion = 90,	
		carob_effort = 6
	)
	
	f <- ff[basename(ff) == "4095_02-Data-master.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
	  country = "Kenya",
	  adm1 = r$Country,
	  crop="potato",
	  rep = as.integer(r$Rep),
	  treatment = r$Treatment,
	  variety = r$Variety,
	  location = r$Site,
    trial_id = paste(r$Site, gsub("Season ", "S", r$Season), sep = "_"),
	  yield = r$`Yield_t/ha`*1000,
	  yield_part ="tubers",
	  yield_moisture = NA,
	  yield_isfresh = NA,
	  record_id = seq_len(nrow(r))
	)

	# disease severity columns
    disease <- data.frame(
	  record_id = seq_len(nrow(r)),
  	bacterial_wilt = r$`BW_L_%`, 
	  potato_virus_Y = r$`PVY_%`,
	  potato_virus_X = r$`PVX_%`,
	  potato_leafroll_virus = r$`PLRV_%`,
	  potato_virus_A = r$`PVA_%`,
	  potato_virus_M = r$`PVM_%`,
	  potato_virus_S = r$`PVS_%`
    )
	
	# computing plant density
  #rows <- 14
	#plants_per_row <- 25
	plot_area_m2 <- 10 * 7.5
	
	#initially i assumed planting stations = plants planted, which might be wrong because not all can make it, so instead im using the variables already in the dataset
	d$plant_density <- r$No_of_plants_emerged_per_plot / plot_area_m2 * 10000
	d$seed_density <- r$No._of_tubers_planted/plot_area_m2 * 10000
	d$emergence_rate <- d$plant_density/d$seed_density
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  d$harvest_date <- NA
  d$season <- r$Season
  
  season <- c(
    "Season 1" = "first", 
    "Season 2" = "second",
    "Season 3" = "third"
  )
  
  d$season <- season[d$season]
  
  #adding planting date
  #planting dates were obtained from one of the dataset files "4095_05_Layout-and-Replications", and the planting dates varied per site, without the seasons included
#  planting <- data.frame(
#        location = c("Tharuni", "Ngecha", "Lari", "Kuresoi", "Keringet-Sabtet", "Keringet - Pompo", "Passengga", "Rurii"),
#        planting_date = c("2015-03-31", "2015-04-02", "2015-10-10", "2015-04-18", "2015-04-17", "2015-04-16", "2015-04-21", "2015-04-20")
#	)  
#   d <- merge(d, planting, by="location", all.x=TRUE)
  
  #adding treatment variable
  abbrev_lookup <- c(
    "CF"   = "certified seed",
    "PS"   = "positive selection",
    "RSFS" = "randomly selected farmer seed",
    "SSPT" = "seed plot technology"
  )
  
  d$seed_source <- d$treatment
  for (abbr in names(abbrev_lookup)) {
    d$seed_source <- gsub(abbr, abbrev_lookup[abbr], d$seed_source, fixed = TRUE)}
  
  treatment_lookup <- c(
    "100% CF" = "Certified seed",
    "100% PS" = "Positive selection from farm",
    "5% SSPT + PS" = "5% certified seed of total seed requirement purchased in previous season and bulked in SSPT - balance filled with PS selected from farmers field of previous season",
    "20% CF + RSFS" = "20% certified seed + 80% randomly selected seed from farmers field",
    "20% CF + PS" = "20% certified seed + 80% PS seeds selected from previous season",
    "100% RSFS" = "100% randomly selected seed from farmers field (RSFS)",
    "5% SSPT + RSFS" = "5% certified seed of total seed requirement purchased in previous season and bulked in SSPT - balance filled with RSFS")
  
  d$treatment <- treatment_lookup[d$treatment]
  
  geo <- data.frame(
    location=c("Keringet-Sabtet", "Keringet - Pompo", "Kuresoi", "Lari", "Ngecha","Passengga", "Rurii", "Tharuni"), 
    longitude=c(35.691, 35.691, 35.533, 36.647, 36.671, 36.329, 36.389, 36.625),
    latitude=c(-0.420, -0.420, -0.303, -0.983, -1.168, -0.219, -0.208, -1.133),
	geo_from_source = TRUE
  )
  d <- merge(d, geo, by="location", all.x = TRUE)  

  # reshaping disease columns from wide to long
  disease_cols <- names(disease)[-1]
  disease <- reshape(disease, varying = disease_cols, v.names = "disease_severity",
                 timevar = "disease",  times = disease_cols, idvar = "record_id", direction = "long")
  rownames(disease) <- NULL
  disease$disease <- gsub("_", " ", disease$disease) 
  disease$disease_severity <- as.character(disease$disease_severity/10)
  disease$severity_scale <- "1-10"

  carobiner::write_files(path, meta, d, long=disease)
}

