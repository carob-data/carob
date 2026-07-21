# R script for "carob"
# license: GPL (>=3)

## ISSUES

## Treatment
# 1- control with no banded fertilizer, 
# 2- three organic fertilizer: starter fertilizer treatments were applied: 15-0-2 sodium nitrate , 5-4-3 poultry litter, and 8-2-2 feather meal. 
# 3- All fertilizers were applied at proper rates to provide a total nitrogen application rate of 28 kg N·ha−

carob_script <- function(path) {

"
No yield benefit from starter fertilizer in soybean no-till planted into rolled-crimped cereal rye

No-till planting organic soybean (Glycine max [L.] Merr.) into rolled-crimped cereal rye (Secale cereale L.) can improve soil health while decreasing labor and fuel costs but yield declines from tillage-based production hinder wider adoption. In addition to soil moisture depletion, nitrogen (N) tie-up in cereal rye biomass and cool soil temperatures under mulches may contribute to yield differences by decreasing N availability early in the production system before the onset of biological N fixation. A field experiment was conducted to determine if starter fertilizers could increase early-season soybean growth and subsequent yields. Over five site-years in New York and Wisconsin on organically managed land, soybeans were no-till planted into rolled-crimped cereal rye and four treatments were applied in a randomized complete block design: feather meal (FM), poultry litter (PL), and sodium nitrate (SN) starter fertilizer treatments supplied at 28 kg N ha-1 and a zero fertilizer control. Early-season growth, weed biomass, soybean biomass, soybean density, and soybean yield were compared among treatments. The relationships between soybean yield and these other variables were also explored. Some differences among treatments were observed in early-season growth, but the results were inconsistent and only found in two site-years. Soybean yield did not differ between the control (2664 kg ha-1) and any treatment (FM: 2760, PL: 2812, and SN: 2769 kg ha-1), though greater cereal rye biomass and soybean density were associated with higher yield. The results suggest that soybeans in organic cover crop-based no-till production do not consistently respond to starter fertilizer.
"

	uri <- "doi:10.5061/dryad.xksn02vn6"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		data_organization = "CU; UWM",
		publication = "doi:10.1002/agg2.20434",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_organic;OM_type",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-20",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "AllenEtal.StarterN.allplots.csv"]
	f2 <- ff[basename(ff) == "AllenEtal.StarterN.blocklevel.csv"]
	f3 <- ff[basename(ff) == "AllenEtal.StarterN.SPAD_height_gs.csv"]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	

	d1 <- data.frame(
		year = r1$siteyr,
		rep = r1$block,
		treatment = r1$treatment,
		weed_biomass = r1$weed.bm.kgha,
		plant_density = r1$soy.stand.ha,
		dmy_total = r1$soy.bm.kgha,
		yield = r1$yield.kgha,
		crop = "soybean",
		country = "United States"
	)
	
	d2 <- data.frame(
	  year = r2$siteyr,
	  #soil_P = r2$Soil_P, kg/ha 
	  #soil_K = r2$Soil_K,
	  soil_SOM = r2$SOM/10, # %
	  soil_pH = r2$Soil_pH
	)
	
	Agg <- aggregate(. ~ year,d2, function(X) mean(X) )
	d <- merge(d1, Agg, by= "year", all.x = TRUE)
	
	d3 <- data.frame(
	  year = r3$siteyr,
	  rep = r3$block,
	  treatment = r3$treatment,
	  growth_stage = r3$gs_cat,
	  plant_height = r3$height
	  #spad = r3$spad
	)
	
	d <- merge(d, d3, by= c("year", "rep", "treatment"), all = TRUE)
	
	d$location <- ifelse(grepl("NY", d$year), "New york", "Wisconsin")
  d$longitude <- ifelse(grepl("NY", d$year), -76.6511, - 89.34537) 
  d$latitude <- ifelse(grepl("NY", d$year), 42.73633, 43.30263)
	d$planting_date <- ifelse(grepl("20", d$year), "2020",
                     ifelse(grepl("21", d$year), "2021", "2022"))
  d$N_organic <- ifelse(grepl("SN|PL|FM", d$treatment), 28, 0)
  d$OM_type <- ifelse(grepl("PL", d$treatment), "poultry litter", 
                      ifelse(grepl("FM", d$treatment), "feather meal", "none"))

  
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  d$trial_id <- ifelse(grepl("NY", d$year), "1", "2") 
  d$yield_moisture <- NA_real_
  d$yield_part <- "grain"
  d$geo_from_source <- TRUE # from publication
  d$irrigated <- NA
  d$yield_isfresh <- TRUE
  d$harvest_date <- NA_character_
    
  d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
  d$year <- NULL
  
	carobiner::write_files(path, meta, d)
}

