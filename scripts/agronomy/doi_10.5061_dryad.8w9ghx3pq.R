# R script for "carob"
# license: GPL (>=3)

## ISSUES

# treatment 
# - Cover crop and nitrogen rate management practices
## - Urea ammonium nitrate-N was applied to obtain relative N rates (0×, 0.25×, 0.5×, and 1×) of the recommended 157–190 kg N ha−1 rate
## - we used the mean(157–190 ) to get the amount 

carob_script <- function(path) {

"
Cover crop and nitrogen rate management practices influence corn (Zea mays) NDVI and nitrogen content

Cover crops are rarely adopted in the northern Corn Belt because of short growing periods but could provide benefits when grown between wheat (Triticum aestivum L.) and corn (Zea mays L.). We evaluated corn NDVI (normalized difference vegetation index) and leaf N (at six leaf, ear leaf, 75% silk, and physiological maturity), over three growing seasons in response to factorial treatments of cover crop (annual ryegrass [Lolium perenne L. ssp. multiflorum (Lam.) Husnot], radish [Raphanus sativus L.], and no-cover control) and N rate (0X, 0.25X, 0.5X, and 1X) relative to the recommended rate based on pre-plant soil tests (157–190 kg N ha–1). Grain N was also measured in the last study year (2016) to evaluate if leaf N indicated grain N. Radish cover crop increased corn NDVI relative to the no-cover control, but annual ryegrass decreased NDVI relative to no-cover control. This response to cover crop treatment suggests that radish cover crop may improve corn nutritional status. Corn receiving 1X N rate had the slowest decrease in leaf N over the growing season, but 2016 data revealed that grain from all treatments receiving some level of N had similar N content. Root biomass was also highest in the 0.5X N rate treatment and could explain the previously reported result that 0.5X N rate results in highest corn yield. Taken together, these results suggest that half the recommended N fertilizer can be used with little effect on nutritional status of corn following spring wheat in the northern Corn belt.
"

	uri <- "doi:10.5061/dryad.8w9ghx3pq"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		data_organization = "USDA-ARS",
		publication = "doi:10.1002/agj2.21085",
		project = NA,
		design = "RCB",
		data_type = "experiment",
		treatment_vars = "cover_crop;N_fert_level",
		response_vars = "grain_N;leaf_N;root_N;root_C", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-27",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "Cons_Temporal_2016_grain_N.csv"]
	f2 <- ff[basename(ff) == "consolidated_root_data_for_analyses.csv"]
	f3 <- ff[basename(ff) == "DAS_CoNS_temporal_leaf_samples.csv"]
	f4 <- ff[basename(ff) == "DAS_ConsTemporal_NDVI.csv"]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	

#### process
	
	d1 <- data.frame(
	  grain_C = r1$C..g.kg.,
	  grain_N = r1$N..g.kg.,
	  cover_crop = gsub("tillage radish", "radish",tolower(r1$Crop)),
	  rep = as.integer(r1$rep),
	  plot_id = as.character(r1$plot),
	  treatment_code = r1$trt..,
	  N_fertilizer = r1$culture*173.5,
	  N_fert_level = as.character(r1$culture),
	  planting_date = "2016",
	  DAP = 169L, ## 
	  harvest_days = 169L
	
	)
	
	d1$cover_crop <- gsub("rye grass", "ryegrass", d1$cover_crop)
	
	d2 <- data.frame(
	  cover_crop = gsub("no cover", "none",  tolower(r2$CoverCrop)),
	  N_fertilizer = r2$Nrate*173.5,
	  N_fert_level = as.character(r2$Nrate),
	  rep = as.integer(r2$rep),
	  treatment_code = r2$treatment,
	  root_C_10_60 = r2$RootC_10to60cm_.g.kg,
	  root_C_0_10 = r2$RootC_0to10cm_.g.kg,
	  root_C_0_60 = r2$Roots0_60cm_kg.C.ha*1.1/1000, #mg/g or g/kg
	  root_N_10_60 = r2$RootN_10to60cm_.g.kg,
	  root_N_0_10 = r2$RootN_0to10cm_.g.kg,
	  root_N_0_60 = r2$RootN_0to60cm_.kg.N.ha*1.1/1000,
	  fmy_roots_0_60 = r2$Roots.mass..0.60.cm.kg.ha,
	  fmy_roots_10_60 = r2$Roots.Mass_10to60cm_kg.ha,
	  fmy_roots_0_10 = r2$Roots.mass_0to10cm_kg.ha,
	  plot_id = as.character(r2$Plot),
	  DAP = as.integer(r2$DaysAfterPlanting),
	  planting_date = as.character(r2$Year)
	)
	d2$cover_crop <- gsub("annual rye", "annual ryegrass", d2$cover_crop)
	var1 <- c("root_C_10_60", "root_C_0_60", "root_C_0_10")
	var2 <- c("root_N_10_60", "root_N_0_60", "root_N_0_10")
	var3 <- c("fmy_roots_10_60", "fmy_roots_0_60", "fmy_roots_0_10")
	d2 <- reshape(d2, varying = list(var1, var2, var3), v.names = c("root_C", "root_N", "fwy_roots"), timevar = "depth", times = c("10-60", "00_60", "00-10"), direction = "long")
	d2$id <- NULL
	d2$depth_bottom  <- as.numeric(substr(d2$depth, 4, 5))
	d2$depth_top  <- as.numeric(substr(d2$depth, 1, 2))
	d2$depth <- NULL
	d <- merge(d1, d2, by = c("cover_crop", "rep", "plot_id", "treatment_code", "N_fertilizer", "N_fert_level", "planting_date", "DAP"), all = TRUE)
	
	#######
	d3 <- data.frame(
	  plot_id = as.character(r3$plot),
	  rep = as.integer(r3$rep),
	  N_fertilizer = r3$Nrate*173.5,
	  N_fert_level = as.character(r3$Nrate),
	  cover_crop = gsub("tillage radish", "radish", tolower(r3$CoverCrop)),
	  treatment_code = r3$treatment,
	  DAP = as.integer(r3$DaysAfterPlanting),
	  planting_date = as.character(r3$Year),
	  crop = r3$Crop,
	  growth_stage = trimws(r3$stage),
	  leaf_N = r3$N_gkg,
	  leaf_C = r3$C_gkg
	  
	)
	
	d3$cover_crop <- gsub("annual rye", "annual ryegrass", d3$cover_crop)
	
	####
	d <- merge(d, d3,  by = c("cover_crop", "rep", "plot_id", "treatment_code","N_fertilizer", "N_fert_level", "planting_date", "DAP"), all = TRUE)
	
	d4 <- data.frame(
	  N_fertilizer = r4$Nrate*173.5,
	  N_fert_level = as.character(r4$Nrate),
	  cover_crop = gsub("tillage radish", "radish", tolower(r4$CoverCrop)),
	  treatment_code = r4$Trt,
	  plot_id = as.character(r4$Plot),
	  DAP = as.integer(r4$DaysAfterPlanting),
	  NDVI = r4$NDVI,
	  rep = as.integer(r4$rep),
	  planting_date = as.character(r4$Year)
	)
	
	d4$cover_crop <- gsub("annual rye", "annual ryegrass", d4$cover_crop)
	
	d <- merge(d, d4, by = c("cover_crop", "rep", "plot_id", "treatment_code", "N_fertilizer", "N_fert_level", "planting_date", "DAP"), all = TRUE)
	d$treatment_code <- NULL
	
	
	d$is_survey <- FALSE
	d$crop <- "maize"
	d$on_farm <- TRUE
	d$trial_id <- "1"
	d$yield <- NA_real_
	d$yield_moisture <- NA_real_
	d$yield_part <- "none"
	d$country <- "United States"
	d$geo_from_source <- TRUE ## from publication
	d$location <- "west central Minnesota"
	d$latitude <- 45.58333
	d$longitude <- -95.9
	d$elevation <- 344 
	d$irrigated <- NA
	d$yield_isfresh <- TRUE
	d$harvest_date <- NA_character_
	
	d$K_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	############### long format 
	d$record_id <- as.integer(1:nrow(d))
	cols <- grep("NDVI|DAP|fwy|leaf|root|record_id|depth", names(d))
	d_lon <- d[, cols]
	
	col <- grep("NDVI|DAP|fwy|leaf|root|depth", names(d))
	d <- d[, -col]

	carobiner::write_files(path, meta, d, long = d_lon)
}

