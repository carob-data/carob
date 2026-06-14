# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
TAMASA Ethiopia.  Variety phenology calibration dataset, 2016

Experiments at five locations (Dedessa, Uke, Bako, Ambo, Holleta)  in Ethiopia on an altitude gradient (1231 to 2351 m) to calibrate development or phenology of 20 maize varieties. There were two to three sowing dates at each location. Observations include dates of emergence, tassel, silking and maturity; biomass and grain yields.
"

	uri <- "hdl:11529/11011"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "CIMMYT; EIAR",
		publication = NA,
		project = "TAMASA",
		carob_date = "2026-06-11",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "TAMASA_ET_VT_2016F.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="Metadata")
	r2 <- carobiner::read.excel(f1, sheet="Protocol")
	r3 <- carobiner::read.excel(f1, sheet="GxE Layout")
	r4 <- carobiner::read.excel(f1, sheet="Variables_modified")
	r5 <- carobiner::read.excel(f1, sheet="Raw-Data")
	#r6 <- carobiner::read.excel(f1, sheet="Corrected Data")
	#r7 <- carobiner::read.excel(f1, sheet="Summary")
	
	d <- data.frame(
	  location = r5$`Site no. = EXPsit`,
	  latitude = r5$`GPS Coordinate Latitude`,
	  longitude = r5$`GPS Coordinate Longitude`,
	  elevation = r5$`GPS Coordinate Altitude`,
	  tmin = r5$`Temperature Average Minimum = AVMINTemp`,
	  tmax = r5$`Temperature Average Maximum = AVMAXTemp`,
	  variety = r5$`Maize variety name = MVnam`,
	  planting_date = as.character(r5$`Planting date = PLNdat`),
	  rep = as.integer(r5$`Replicate = EXPrep`),
	  plot_id = as.character(r5$`Plot number = PLOTnumb`),
	  plot_area = r5$`Plot size (sq. m)`,
	  emergence_date = as.character(r5$`Emergence date = EMdat`),
	  flowering_date = r5$`Tasseling_silking_date=flowering_date = FLWdat`,
	  maturity_date = as.character(r5$`Pysioligic_ maturity_dat = MDAT/date`),
	  harvest_date = as.character(r5$`Harvest_date = HDATE`),
	  yield = r5$`Grain yield t/ha = GRNyld`*1000,
	  country = "Ethiopia",
	  trial_id = paste(r5$`Site no. = EXPsit`, r5$`Plot number = PLOTnumb`, sep = "-"), 
	  on_farm = TRUE, 
	  is_survey = FALSE, 
	  crop = "maize", 
	  yield_part = "grain", 
	  yield_moisture = as.numeric(NA), 
	  geo_from_source = TRUE,
	  irrigated = NA,
	  yield_isfresh = NA
		
	)

	i <- !grepl("/", d$flowering_date)
	d$flowering_date[i] <- as.character(as.Date(as.numeric(d$flowering_date[i]), origin= "1899-12-30"))
	d$flowering_date = ifelse(grepl("9/9/2016", d$flowering_date), "2016-09-09", d$flowering_date)
	
	cols <- c("elevation" , "longitude", "latitude")
	
	for(v in cols) {
	  i <- cumsum(!is.na(d[[v]]))
	  d[[v]] <- d[[v]][match(i, i)]
	}
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	d <- d[which(d$yield > 0),]
	
	carobiner::write_files(path, meta, d)
}


