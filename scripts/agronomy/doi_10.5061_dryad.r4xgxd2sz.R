# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Quantifying the response of Goldrush potato to nitrogen on sandy soil

Optimizing nitrogen (N) application rate is essential for effective N management and requires an understanding of crop growth dynamics, nutrient partitioning, cultivar traits, and soil characteristics. This dataset contains plot-level measurements from a three-year nitrogen (N) rate experiment with the potato cultivar 'Goldrush' grown on irrigated sandy soils at the Hancock Agricultural Research Station in central Wisconsin, USA (2015–2017). Seven in-season N fertilizer rates, ranging from 37 to 391 kg N ha -1 including starter N, were applied in a randomized complete block design with four replications each year. The dataset is structured at the plot × year level and includes repeated measurements at early- and late-growth stages as well as final harvest. For each plot, the dataset provides aboveground biomass and tuber dry matter, tissue N concentration, and calculated N uptake in aboveground and tuber fractions at approximately 45 and 75 days after emergence and in total crop biomass. Final harvest data includes total and marketable tuber yield, tuber number and tuber mean weight derived from graded size classes, specific gravity, and the incidence of internal defects and relative total and marketable yield. These data are suitable for regression analyses of crop response to N, evaluation and comparison of quadratic, linear-plateau, and quadratic-plateau models, development of cultivar- and site-specific N recommendations, and calibration or validation of empirical and process-based potato models on coarse-textured soils. The dataset does not contain personal or sensitive information, and there are no known legal or ethical restrictions on reuse beyond citation of the associated article and dataset.
"

	uri <- "doi:10.5061/dryad.r4xgxd2sz"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "UWM; UMINN",
		publication = "doi:10.1002/agj2.70411",
		project = NA,
		design = "RCB",
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-12",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "Nrate_rawdata.csv"]
	f2 <- ff[basename(ff) == "README.md"]

	r1 <- read.csv(f1, na=".")
	
### process	
	d <- data.frame(
	  N_fertilizer = r1$TotalNapplied,
	  rep = r1$block,
	  year = r1$year,
	  yield = r1$TuberYield*1000,
	  yield_marketable = r1$MarketYield*1000,
	  dmy_total_75 = r1$TotalDM75,
	  dmy_total_45 = r1$TuberDM45,
	  tuber_N_45 = r1$TuberNC45,
	  tuber_N_75 = r1$TuberNC75,
	  crop = "potato", 
	  is_survey = FALSE, 
	  on_farm = TRUE, 
	  trial_id = "1", 
	  yield_moisture = NA_real_, 
	  yield_part = "tubers", 
	  country = "United States", 
	  irrigated = NA,
	  location = "Hancock Agricultural Research Station",
	  yield_isfresh = TRUE,
	  record_id = as.integer(1:nrow(r1)),
	  soil_texture = "sand"
	 
	)
	
	### Adding information from publication
	
	info <- data.frame(
	  year = c(2015, 2016, 2017),
	  soil_pH = c(6.6, 6.5, 6.4),
	  soil_SOM = c(8,7, 9),
	  soil_P = c(42, 66, 66),
	  soil_P_method = "BrayI",
	  soil_K_exch =  c(73, 72, 85)/390,
	  soil_Ca_exch = c(318, 359, 366)/200,
	  soil_Mg_exch = c(66, 72, 94)/120,
	  soil_B = c(0.7, 0.3, 0.2),
	  soil_Mn = c(20, 11, 11),
	  soil_Zn = c(0.8, 2, 2.1),
	  soil_S = c(3.2, 0.5, 0.9),
	  plant_density = c(33690, 36158, 33450),
	  planting_date = c("2015-04-22", "2016-04-20", "2017-04-19"),
	  harvest_date = c("2015-09-02", "2016-08-29", "2017-08-31"),
	  longitude = -89.54417,
	  latitude = 44.11722,
	  geo_from_source = TRUE
	)

	d <- merge(d, info, by= "year", all.x = TRUE)
	
	d$K_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	#####################
	i <-  grepl("tuber_|dmy|record_id", names(d))
	Nm <- names(d)[i]   
	dlon <- d[, Nm]
	dlon <-  reshape(dlon, varying=c("dmy_total_45", "dmy_total_75", "tuber_N_45", "tuber_N_75"),
                   v.names = "value", direction = "long")
	dlon$variable <- c(rep("dmy_total", 2), rep("tuber_N", 2))[dlon$time]
	dlon$DAE <-  c(rep(45L, 2), rep(75L, 2))[dlon$time]
	dlon$time <- dlon$id <- d$year <- NULL
	dlon <- na.omit(dlon)
  
	i <-  grepl("tuber_|dmy", names(d))
	d <- d[, !names(d) %in% names(d)[i]]
  
	carobiner::write_files(path, meta, d, long = dlon)
}


