# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Dataset for: Participatory variety trial on selected clones from diversity panel population (Yield, growth and quality data)

The dataset contains data about a participatory variety trial on selected clones from diversity panel population for potato varieties in Amhara region, Ethiopia.
"

   uri <- "doi:10.21223/GZYPKO"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
   	data_organization = "CIP",
		publication = NA,
		project = NA,
		carob_date = "2025-08-13",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Calculated_Yield_Data (Participatory_variety_trial_on_selected_clones_fro...).csv"]
	f2 <- ff[basename(ff) == "Growth_Disease_Quality_Data (Participatory_variety_trial_on_selected_clones_fro...).csv"]
	
	r1 <- read.csv(f1, sep = ";")
	r2 <- read.csv(f2, sep = ";")
	

	d1 <- data.frame(
		plot_id = as.character(r1$Plot),
		variety = r1$CIP_Clone_No,
		rep = r1$Replication,
		yield= r1$Tot_Tuber_Yield_NonAdjusted*1000,
		yield_marketable= r1$Marketable_Tuber_Yield_NonAdjusted*1000,
		planting_date= "2016-07-20",
		harvest_date= "2016-11-02",
		crop= "potato",
		plot_area= 3.6/10000, # ha
		row_spacing= 75,
		plant_spacing= 30,
		plant_density= (r1$No_Plant_Harvested/3.6)*10000,
		location= "Amhara region",
		country= "Ethiopia",
		longitude= 37.9526,
		latitude= 12.2048 
	)

	d2 <- data.frame(
	   plot_id= as.character(r2$Plot),
	   variety= r2$CIP_Clone_No,
	   emergence_days= r2$DAP_50_Emergence,
	   flowering_days= r2$DAP_50_Flowering,
	   AUDPC= r2$AUDPC,
	   plant_height= r2$Average_Plant_Height,
	   r2[, grepl("Late", names(r2))],
	   record_id= as.integer(1:nrow(r2))
	)

d <- merge(d1, d2, by= c("plot_id", "variety"), all.x = TRUE)

d$trial_id <- "1"	
d$on_farm <- TRUE 
d$is_survey <- FALSE
d$yield_part <- "tubers" 
d$yield_moisture <- as.numeric(NA)
d$irrigated <- NA 
d$geo_from_source <- FALSE
d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

### Adding disease
x <- d[, grepl("Late", names(d))]
x$record_id <- as.integer(1:nrow(x))
x <- reshape(x, direction="long", varying=c("LateBlight_46DAP", "LateBlight_58DAP"), v.names="disease_severity", timevar="DAP")
x$DAP <- as.integer(c(46, 58)[x$DAP])
x$disease <- "Late blight"
x$disease_severity <- as.character(x$disease_severity) 
x$severity_scale <- "0-100" ## in % (Could we convert it into a 0 to 9 scale?)
x$id <- NULL

d <- d[, !grepl("Late", names(d))]


carobiner::write_files(path, meta, d, long = x)

}

