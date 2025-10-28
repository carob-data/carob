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
	make_standard_df <- function(file, sheet_name) {
		r <- carobiner::read.excel.hdr(file, sheet = sheet_name, hdr=1, skip=1, fix_names=TRUE)
		main<- data.frame(
		  record_id = seq_len(nrow(r)),
		  country = "Mexico",
		  adm2 = r$Municipality,
		  location = r$Locality,
		  latitude = r$Latitude,
		  longitude = r$Longitude,
		  planting_date = as.character(r$Planting.Date),
		  land_prep_method = tolower(paste0(r$Tillage, ";", r$Planting.method))	,
		  variety = r$Hibrid,
		  treatment= paste0("N", r$Rate.N.kg.ha),
		  seed_rate = r$planting.density.Kg.ha,
		  rep = as.integer(r$Inf.Experiment_REP),
		  N_fertilizer = r$Rate.N.kg.ha,
		  yield =  r$Yield.at.14pct.hum,
		  plot_area = r$Plot.size.m2
		)
	}
	
	#Loading all sheets and creating sheet_id
	x1 <- make_standard_df(f, "2016")
	x1$sheet_id <- 1
	x2 <- make_standard_df(f, "2017")
	x2$sheet_id <- 2
	x3 <- make_standard_df(f, "2018")
	x3$sheet_id <- 3
	x4 <- make_standard_df(f, "2019")
	x4$sheet_id <- 4
	
	#unique_id for merging
	x1$unique_id <- paste(x1$sheet_id, x1$record_id, sep = "_")
	x2$unique_id <- paste(x2$sheet_id, x2$record_id, sep = "_")
	x3$unique_id <- paste(x3$sheet_id, x3$record_id, sep = "_")
	x4$unique_id <- paste(x4$sheet_id, x4$record_id, sep = "_")
	
	d <-rbind(x1,x2,x3,x4)
	
	# INIFAP Sitio Experimental Mixteca, Santo Domingo Yanhuitlán 
	d$latitude <- 17.511  
	d$longitude <- -97.352
	d$geo_from_source <- FALSE
	d$crop <- "wheat"
	d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE #there is a variable called sowing irrigation in raw dataset which shows date of irrigation
	d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$land_prep_method <- gsub("conservation","minimum tillage", d$land_prep_method)
	d$land_prep_method <- gsub("bed","raised beds", d$land_prep_method)
	
##########
#NDVI_data
##########
	standardize_ndvi <- function(file, sheetname) {
	  all_data <- list()
	  
	  for (sheet in sheetname) {
	    
	    df <- readxl::read_excel(file, sheet = sheet, skip = 1)
	    ndvi_cols <- grep("date\\.|ndvi", names(df), ignore.case = TRUE)
	    df <- df[, ndvi_cols]
	    
	    n_meas <- ncol(df) / 2
	    long_list <- list()
	    
	    for (i in seq_len(n_meas)) {
	      date_col <- df[[ (i - 1) * 2 + 1 ]]   
	      ndvi_col <- df[[ (i - 1) * 2 + 2 ]]   
	      
	      meas_name <- paste0("Measurement_", i)
	      ndvi_col <- as.numeric(ndvi_col)
	      temp <- data.frame(
	        record_id = seq_len(nrow(df)),
	        year = sheet,
	        variable = meas_name,
	        date = date_col,
	        value = ndvi_col,     
	        stringsAsFactors = FALSE
	      )
	      
	      temp <- temp[!is.na(temp$value), ]
	      long_list[[i]] <- temp
	    }
	    
	    sheet_long <- do.call(rbind, long_list)
	    all_data[[sheet]] <- sheet_long
	  }
	  
	  final_long <- do.call(rbind, all_data)
	  return(final_long)
	}
	
	n1 <- standardize_ndvi(f,"2016")
	n1$sheet_id <- 1
	n2 <- standardize_ndvi(f,"2017")
	n2$sheet_id <- 2
	n3 <- standardize_ndvi(f,"2018")
	n3$sheet_id <- 3
	n4 <- standardize_ndvi(f,"2019")
	n4$sheet_id <- 4
	
	n1$unique_id <- paste(n1$sheet_id, n1$record_id, sep = "_")
	n2$unique_id <- paste(n2$sheet_id, n2$record_id, sep = "_")
	n3$unique_id <- paste(n3$sheet_id, n3$record_id, sep = "_")
	n4$unique_id <- paste(n4$sheet_id, n4$record_id, sep = "_")
	
	NDVI_data <- rbind(n1,n2,n3,n4)
	
	#d <- merge(d,NDVI_data,by ="unique_id")
	
	carobiner::write_files(path, meta, d)
}
