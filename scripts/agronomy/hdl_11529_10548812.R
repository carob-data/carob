# R script for "carob"
# license: GPL (>=3)

#ISSUES
#1. Since there are no variables for NDVI data in terminag, i am not merging d with NDVI data.
#2. i have created some extra columns (sheet_id and unique_id) which helps in merging the two accurately without mismatch of entries.

carob_script <- function(path) {

"Wheat experiment with increasing rates of nitrogen to develop a calibration for the GreenSeeker in Oaxaca
  
This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Oaxaca. (2022-10-17)"

	uri <- "hdl:11529/10548812"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		completion = 99,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-10-17",
		notes = NA, 
		design = NA
	)
	

	f <- ff[basename(ff) == "GreenSeeker Oaxaca 2016-2019.xlsx"]
 
#Function to standardize other sheets with same structure
	make_standard_df <- function(sheet_name) {
		r <- carobiner::read.excel.hdr(f, sheet=sheet_name, hdr=1, skip=1, fix_names=TRUE)
		data.frame(
		  country = "Mexico",
		  adm2 = r$Municipality,
		  location = r$Locality,
		  latitude = r$Latitude,
		  longitude = r$Longitude,
		  planting_date = as.character(r$Planting.Date),
		  irrigation_date = as.character(r$Prepalanting.sowing.irrigation),
		  land_prep_method = tolower(paste0(r$Tillage, ";", r$Planting.method))	,
		  variety = r$Hibrid,
		  treatment= paste0("N", r$Rate.N.kg.ha),
		  seed_rate = r$planting.density.Kg.ha,
		  rep = as.integer(r$Inf.Experiment_REP),
		  N_fertilizer = r$Rate.N.kg.ha,
		  yield =  r$Yield.at.14pct.hum,
		  plot_area = r$Plot.size.m2,
		  sheet_id = sheet_name,
		  row_id = seq_len(nrow(r))
		)
	}
	
	# reading all sheets
	years <- as.character(2016:2019)
	d <- do.call(rbind, lapply(years, make_standard_df))
	

	# INIFAP Sitio Experimental Mixteca, Santo Domingo Yanhuitlán 
	d$latitude <- 17.511  
	d$longitude <- -97.352
	d$geo_from_source <- FALSE
	d$crop <- "wheat"
	d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$land_prep_method <- gsub("conservation", "minimum tillage", d$land_prep_method)
	d$land_prep_method <- gsub("bed", "raised beds", d$land_prep_method)

	d$record_id <- 1:nrow(d)
	
	d$irrigated <- TRUE
	i <- d$irrigation_date == "Rain"
	d$irrigated[i] <- FALSE
	d$irrigation_date[i] <- NA
	
	
####################
# NDVI_data

## reading the same sheets as above again. This is not efficient, but it helps keep the code simple
	standardize_ndvi <- function(sheetname) { 
		df <- carobiner::read.excel.hdr(f, sheet=sheetname, hdr=1, skip=1, fix_names=TRUE, na="-")
	    ndvi_cols <- grep("ndvi", names(df), ignore.case = TRUE)
		date_cols <- grep("Measure.*.date", names(df), ignore.case = TRUE)
		data.frame(
			sheet_id = sheetname,
			row_id = 1:nrow(df),
			date=as.vector(as.matrix(df[, date_cols])), 
			NDVI=as.numeric(as.vector(as.matrix(df[, ndvi_cols])))
		)
	 }
	NDVI <- do.call(rbind, lapply(years, standardize_ndvi))

	NDVI <- merge(NDVI, d[, c("record_id", "sheet_id", "row_id")], by=c("sheet_id", "row_id"))
	NDVI <- NDVI[order(NDVI$record_id, NDVI$date), ]

	NDVI$row_id <- NDVI$sheet_id <- NULL
	d$row_id <- d$sheet_id <- NULL

	carobiner::write_files(path, meta, d, long=NDVI)
}
