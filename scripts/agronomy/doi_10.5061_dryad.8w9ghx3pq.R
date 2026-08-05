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
		treatment_vars = "cover_crop;N_fertilizer",
		response_vars = "grain_N;grain_C;NDVI", 
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

	## 2016 only, grain C and N at harvest 
	d1 <- data.frame(
	  plot_id = r1$plot,
	  grain_C = r1$C..g.kg.,
	  grain_N = r1$N..g.kg.,
	  harvest_date = as.character(as.Date(r1$Date, "%m/%d/%Y"))
	)	
	d1$year <- substr(d1$harvest_date, 1, 4)

	## root biomass and C/N at two depths (0-10, 10-60cm, not using redundant 0-60), in three years
	d2 <- data.frame(
	  plot_id = r2$Plot,
	  root_C_0_10 = r2$RootC_0to10cm_.g.kg,
	  root_C_10_60 = r2$RootC_10to60cm_.g.kg,
	  root_N_0_10 = r2$RootN_0to10cm_.g.kg,
	  root_N_10_60 = r2$RootN_10to60cm_.g.kg,
	  fmy_roots_0_10 = r2$Roots.mass_0to10cm_kg.ha,
	  fmy_roots_10_60 = r2$Roots.Mass_10to60cm_kg.ha,
	  date = as.character(as.Date(r2$Date.Sample.Taken, "%m/%d/%Y")),
	  growth_stage = trimws(r2$sample.timing)
	)
	d2$year <- substr(d2$date, 1, 4)

	
	## leaf N and C. 5 measurements per year, in two years
	d3 <- data.frame(
	  plot_id = r3$plot,
	  date = as.character(as.Date(r3$Date, "%m/%d/%Y")),
	  DAP = as.integer(r3$DaysAfterPlanting),
	  growth_stage = trimws(r3$stage),
	  leaf_N = r3$N_gkg,
	  leaf_C = r3$C_gkg	  
	)
	d3$planting_date = as.Date(d3$date)- d3$DAP
	d3$year <- substr(d3$date, 1, 4)
	d3$DAP <- NULL

	## NDVI for one or two years 5-6 measurments per year
	d4 <- data.frame(
	  plot_id = r4$Plot,
	  treatment = as.character(r4$Trt),
	  rep = as.integer(r4$rep),
	  N_fertilizer = r4$Nrate*173.5,
	  cover_crop = gsub("tillage radish", "radish", tolower(r4$CoverCrop)),
	  date = as.character(as.Date(r4$date, "%m/%d/%Y")),
	  NDVI = r4$NDVI
	)
	d4$cover_crop <- gsub("annual rye", "annual ryegrass", d4$cover_crop)
	d4$year <- substr(d4$date, 1, 4)


### treatments and year combination
	d <- unique(d4[, c("plot_id", "treatment", "cover_crop", "N_fertilizer", "year")])
	d$record_id <- 1:nrow(d)

	d2 <- merge(d2, d[, c("plot_id", "year", "record_id")], all.x=TRUE)
	d3 <- merge(d3, d[, c("plot_id", "year", "record_id")], all.x=TRUE)
	d4 <- merge(d4, d[, c("plot_id", "year", "record_id")], all.x=TRUE)



## d1 has one observation per variable for one year (wide)
    d <- merge(d, d1, by=c("plot_id", "year"), all.x=TRUE)

# add planting_date (there is two-day variation within d3
	pd <- aggregate(d3["planting_date"], d3[, c("plot_id", "year")], mean)
	pd$planting_date <- as.character(pd$planting_date)
	d <- merge(d, pd, all.x=TRUE) 

# reshape d2 to long
  	var1 <- c("root_C_0_10", "root_C_10_60")
	var2 <- c("root_N_0_10", "root_N_10_60")
	var3 <- c("fmy_roots_0_10", "fmy_roots_10_60")
	d2 <- reshape(d2, varying = list(var1, var2, var3), v.names = c("root_C", "root_N", "fwy_roots"), timevar = "depth", times = c("00-10", "10_60"), direction = "long")
	d2$id <- NULL
	d2$depth_bottom  <- as.numeric(substr(d2$depth, 4, 5))
	d2$depth_top  <- as.numeric(substr(d2$depth, 1, 2))
	d2$depth <- NULL

	d4$cover_crop <- d4$N_fertilizer <- d4$treatment <- NULL
 
	d_long <- carobiner::bindr(d2, d3, d4)
	d_long$year <- d_long$planting_date <- d_long$plot_id <- d_long$rep <- NULL

	vars <- c('root_C', 'root_N', 'fwy_roots', 'leaf_N', 'leaf_C', 'NDVI')
	d_long <- reshape(d_long, varying=vars, v.names="value", timevar="variable", times=vars, direction="long")
	d_long <- d_long[!is.na(d_long$value), ]
	d_long$id <- NULL
  
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
	d$K_fertilizer <- d$P_fertilizer <- as.numeric(NA)

	d$year <- NULL
	d$plot_id <- as.character(d$plot_id)
	
	carobiner::write_files(path, meta, d, long = d_long)
}
