# R script for "carob"



carob_script <- function(path) {

"Dataset for yield and stability advanced trial for late blight and heat tolerant (LBHT) potato population conducted in Huancayo, Peru. 150 advanced clones of the LBHT and heat-tolerant population, with three control varieties Yungay, Kory, and Amarilis, besides with 23 parents were planted in Huancayo, Peru between 2021 and 2022. (16 Rows x 12 Columns)"


	uri <- "doi:10.21223/SBHNHD"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "variety_code", 
		carob_contributor = "Mitchelle Njukuya",
		carob_effort = NA,
		carob_date = "2024-07-04"
	)
	
	f <- ff[basename(ff) == "01_data_Advanced Trial LBHTC2 Huancayo.xlsx"]
	r <- carobiner::read.excel(f)

	lbvars <- grep('^LB', colnames(r), value=TRUE)
	d <- data.frame(
	  country="Peru",
	  crop="potato",
	  variety_code= r$CIPN,
	  variety_type= tolower(r$Type),
	  rep= as.integer(r$Rep),
	  
	  on_farm= TRUE,
	  inoculated= FALSE,
	  irrigated= TRUE,
	  is_survey = FALSE,
	
	  herbicide_used= TRUE,
	  herbicide_product = "diuron;metribuzin",

	  weeding_done =TRUE,
	  weeding_dates = "2021-11-27;2021-12-03;2022-01-11;2022-03-02",
	  weeding_times = 4L,
	  weed_species = "Bouteloua;Pennisetum clandestinum",

	  fungicide_used= TRUE,
	  fungicide_product = "mancozeb;benomyl",
	  fungicide_dates = "2021-12-16",
	  
	  insecticide_used = TRUE,
	  insecticide_product = "deltamethrin;fipronil;beta-cyfluthrin",
	  insecticide_dates = "2021-12-16;2021-12-30;2022-02-25;2022-03-11;2022-03-25",

	  foliar_fertilizer_used = TRUE,

	  yield_part= "tubers",
	  record_id= as.integer(r$Plot),
	  trial_id= "1"         
	)

	# It does not seem possible to compute yield with the data provided.
	# We can use this strong assumption, based on other data 
	approx_plant_density <- 37037
	
	d$yield_marketable <- approx_plant_density * r$MTbWg / r$PltHrv
	d$yield <- approx_plant_density * (r$MTbWg + r$NoMTWg)  / r$PltHrv
	d$yield[d$yield > 150000] <- NA	

	d$dmy_storage <- d$yield * r$`DM %` / 100
	d$land_prep_method <- "rotovating;ploughing;hilling"

	d$location <- "Huancayo"
	# from "02_Field_layout_Advanced Trial LBHTC2 Huancayo.xlsx"
	d$longitude <- -75.39938
	d$latitude <- -11.84895
	d$elevation <- 3324
	d$geo_from_source <- FALSE
	d$planting_date <- "2021-11-25"
	d$maturity_date <- "2022-04-11" # Corte follaje
	# see 03_Management_Field_Advanced Trial LBHTC2 Huancayo.xlsx
	# not constistent with 
	d$harvest_date  <- "2022-04-25"
	d$irrigation_dates <- "2021-12-29"
	d$herbicide_dates <- "2021-11-13;2021-12-06;2022-03-07"

  
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$pathogen <- "Phytophthora infestans"
	d$diseases <- "potato late blight"
 
#?? d$disease_severity <- as.character(d$disease_severity) 

  
  #Adding disease scores for late blight
  dd <- r[,lbvars]
  dd$record_id <- as.integer(1:nrow(dd))
  dates <- paste0("2022-", c("01-10", "01-17", "01-24", "01-31", "02-07",  "02-14", "02-21","02-28","03-07", "03-14", "03-21"))
  x <- reshape(dd, direction="long", varying =lbvars, v.names="disease_severity", timevar="step")
  x$time <- dates[x$step]
  x$step <- x$id <- NULL 
  x$disease_severity <- as.character(x$disease_severity)
 
	carobiner::write_files(path, meta, d, long=x)
}



