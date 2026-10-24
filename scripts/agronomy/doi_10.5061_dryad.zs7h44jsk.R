# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Interseeded clover into corn: Yield and soil N data

Inter-seeding red clover (Trifolium pratense L.) provides an alternative method to incorporate cover crops into continuous corn (Zea mays L.) in the Upper US Midwest. Red clover is a leguminous cover crop that can grow in low-radiation environments and is winter-hardy. Systems with red clover have demonstrated improved corn yield and a fertilizer N equivalence, but understanding these effects with inter-seeding warrants further investigation. The objectives of this study were to determine the effect of inter-seeding red clover on (i) plant-available N during and after red clover decomposition, (ii) optimum N rates for corn, and (iii) corn yields. The experimental design was a randomized, complete block-split plot design, with cover crop as the main plot factor (treatments with and without inter-seeded red clover) and N-rate as a split-plot factor (N-rates between 0 and 315 kg-N ha−1 in 45 kg-N ha−1 intervals). Quadratic plateau response curves with a bootstrapping technique were used to determine differences in optimum N rate among treatments. Corn yields were evaluated with or without red clover inter-seeded following the first inter-seeding year. Red clover accumulated biomass values of 50 kg ha−1 up to 300 kg ha−1 pre-termination when inter-seeded with corn at the V4–V5 growth stage without detriment to yield. Corn yield was improved with clover treatments in one of four site years tested, resulting in a (15–17 kg ha−1) lower N requirement that year. Overall, our results indicate that inter-seeding red clover into continuous corn did not provide an agronomically meaningful nitrogen fertilizer equivalence to the cropping system.
"

	uri <- "doi:10.5061/dryad.zs7h44jsk"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		data_organization = "UWM;SCSU", # South Carolina state university
		publication = "doi:10.1002/agj2.70133",
		project = NA,
		design = NA,
		data_type = "on-station experiment",
		treatment_vars = "N_fertilizer;cover_crop;intercrops",
		response_vars = "yield;soil_NO3;soil_NH4", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-10-23",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "Dryad_InterseedData.xlsx"]
	#f2 <- ff[basename(ff) == "README.md"]

	r1 <- carobiner::read.excel(f1, sheet="Yield")
	r2 <- carobiner::read.excel(f1, sheet="Soil N_Raw")
	#r3 <- carobiner::read.excel(f1, sheet="Soil N_averages")
	#r4 <- carobiner::read.excel(f1, sheet="READ ME")
	
### process yield data
	d1 <- data.frame(
	  trial_id = as.character(r1$Field),
	  year = as.character(r1$Year),
	  rep = as.integer(r1$Block),
	  treatment = tolower(r1$`Whole plot treatment`),
	  N_fertilizer = r1$`Split plot treatment`,
	  yield = r1$Yield_Mgha*1000,
	  crop = "maize"
	)
	
	d1 <- aggregate(yield ~ year + rep + trial_id + treatment + N_fertilizer + crop, data = d1, FUN = mean, na.rm = TRUE)
	d1$record_id <- seq_len(nrow(d1))
	
	### process soil data 
	d2 <- data.frame(
	  trial_id = as.character(r2$Field),
	  year = as.character(r2$Year),
	  rep = as.integer(r2$Block),
	  treatment = tolower(r2$`Whole plot treatment`),
	  N_fertilizer = r2$Nrate,
	  depth = r2$Depth,
	  soil_NO3 = r2$NO3_ppm,
	  soil_NH4 = r2$NH4_ppm
	)
	
	d2$treatment <- c("clover _yp" = "clover_yp", "clover always" = "clover", "none" = "none", "rye" = "rye")[d2$treatment]

	#### aggregating soil data 
	d2ag <- aggregate(d2[, c("soil_NO3", "soil_NH4")], d2[, c("year", "rep", "trial_id", "treatment", "N_fertilizer", "depth")], mean, na.rm = FALSE)
	
	## merge yield data and soil data
	d <- merge(d1, d2ag, by= c("year", "trial_id", "rep","treatment", "N_fertilizer"), all.x = TRUE)
  d <- d[!duplicated(d$record_id),]
  
  d$N_fertilizer <- c(0, 45, 90, 135,180, 225, 270, 315)[d$N_fertilizer]
  d$intercrops <- ifelse(grepl("^clover$", d$treatment), "red clover", "none")
  d$intercrops <- ifelse(grepl("^rye$", d$treatment), "rye", "none")
  d$cover_crop <- gsub("clover_yp|^clover$", "red clover", d$treatment)
  d$cover_crop <- gsub("no clover", "none", d$cover_crop)
	d$location <- "University of Wisconsin Arlington Agricultural Research Station"
  d$depth_top <- c(0, 0, 30)[d$depth]
  d$depth_bottom <- c(15, 30, 60)[d$depth]
	### Adding lon and lat coordinate from publication
  
  geo <- data.frame(
    trial_id = c( rep("1", 3), rep("2", 2)),
    year = c("2017", "2018", "2019", "2019", "2020"),
    longitude = c(rep(-89.33717, 3), rep(-89.38347, 2)),
    latitude = c(rep(43.301, 3),rep(43.29897, 2)),
    planting_date = c("2017-05-25", "2018-05-24", "2019-05-23", NA, "2020-05-22"),
    harvest_date =  c("2017-11-03", "2018-11-03", "2019-11-05", "2019-11-05", "2020-10-27"),
    geo_from_source = TRUE
  )
  
  d <- merge(d, geo, by= c("trial_id", "year"), all.x = TRUE)
  d$planting_date[is.na(d$planting_date)] <- d$year[is.na(d$planting_date)]
  d$year <- d$depth <- NULL
  
  
  d$is_survey <- FALSE
  d$on_farm <- FALSE
  d$yield_moisture <- NA
  d$yield_part <- "grain"
  d$country <- "United States"
  d$irrigated <- NA
  d$yield_isfresh <- NA
  
  d$K_fertilizer <- d$P_fertilizer <- as.numeric(NA)
  
  carobiner::write_files(path, meta, d)
}


