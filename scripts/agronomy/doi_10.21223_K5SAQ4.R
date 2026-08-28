# R script for "carob"
# license: GPL (>=3)

## ISSUES


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
	  yield = r$`Yield_t/ha`*1000,
	  yield_part ="tubers",
	  yield_moisture = NA,
	  yield_isfresh = NA,
	  disease_sample_size = 10,
	  bacterial_wilt = r$`BW_L_%`,  # disease severity columns
	  potato_virus_Y = r$`PVY_%`,
	  potato_virus_X = r$`PVX_%`,
	  potato_leafroll_virus = r$`PLRV_%`,
	  potato_virus_A = r$`PVA_%`,
	  potato_virus_M = r$`PVM_%`,
	  potato_virus_S = r$`PVS_%`
	)
	
	# computing plant density
	rows <- 14
	plants_per_row <- 25
	plot_area_m2 <- 10 * 7.5
	d$plant_density <- rows * plants_per_row
	d$plant_density <- d$plant_density/plot_area_m2 * 10000
	
	d$emergence_rate <- r$`%_Emergence`
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	d$harvest_date  <- NA
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA) 
  
  #adding planting date
  planting <- data.frame(
    location = c("Tharuni", "Ngecha", "Lari", "Kuresoi", "Keringet-Sabtet", 
                "Keringet - Pompo", "Passengga", "Rurii"),
    planting_date = as.Date(c("31-03-2015","02-04-2015","10-10-2015","18-04-2015",
                      "17-04-2015","16-04-2015","21-04-2015","20-04-2015"),format="%d-%m-%Y"),
    stringsAsFactors =FALSE)
    
  d <- merge(d,planting,by="location", all.x = T)
  d$planting_date <- as.character(d$planting_date)
  
  #adding treatment variable
  abbrev_lookup <- c(
    "CF"   = "Certified seed",
    "PS"   = "Positive selection",
    "RSFS" = "Randomly selected farmer seed",
    "SSPT" = "Seed Plot Technology"
  )
  
  d$seed_source <- d$treatment
  for (abbr in names(abbrev_lookup)) {
    d$seed_source <- gsub(abbr, abbrev_lookup[abbr], d$seed_source, fixed = TRUE)}
  
  treatment_lookup <- c(
    "100% CF" = "100% Certified seed",
    "100% PS" = "100% PS selected from farmers field in previous season",
    "5% SSPT + PS" = "5% Certified seed of total seed requirement purchased in previous season and bulked in SSPT - balance filled with PS selected from farmers field of previous season",
    "20% CF + RSFS" = "20% Certified seed + 80% randomly selected seed from farmers field (RSFS)",
    "20% CF + PS" = "20% Certified seed + 80% PS seeds selected from previous season",
    "100% RSFS" = "100% randomly selected seed from farmers field (RSFS)",
    "5% SSPT + RSFS" = "5% Certified seed of total seed requirement purchased in previous season and bulked in SSPT - balance filled with RSFS")
  
  d$treatment <- treatment_lookup[d$treatment]
  
  loc <- data.frame(
    location=c("Keringet-Sabtet", "Keringet - Pompo", "Kuresoi", "Lari", "Ngecha","Passengga", "Rurii", "Tharuni"), 
    longitude=c(35.691,35.691,35.533,36.647,36.671,36.329,36.389,36.625),
    latitude=c(-0.420,-0.420,-0.303,-0.983,-1.168,-0.219,-0.208,-1.133))
  
  d <- merge(d,loc,by="location", all.x = TRUE)
  
  d$trial_id <- paste(d$location,d$planting_date,sep = "_")
  
  # reshaping disease columns from wide to long
  d$row_id <- seq_len(nrow(d))
  disease_cols <- c("bacterial_wilt", "potato_virus_Y", "potato_virus_X",
                    "potato_leafroll_virus", "potato_virus_A",
                    "potato_virus_M", "potato_virus_S")
  
  disease <- reshape(d[, c("row_id", disease_cols)],
                 varying = disease_cols,
                 v.names = "disease_severity",
                 timevar = "disease",
                 times = disease_cols,
                 idvar = "row_id",
                 direction = "long")
  rownames(disease) <- NULL
  
  d[, disease_cols] <- NULL
  d <- merge(d, disease, by = "row_id")
  d$row_id <- NULL
  d$disease_severity <- d$disease_severity/10
  d$disease_severity <- as.character(d$disease_severity)
  d$severity_scale <- "1-10"
  
  carobiner::write_files(path, meta, d)
}

