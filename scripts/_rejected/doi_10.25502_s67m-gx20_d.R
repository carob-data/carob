# R script for "carob"
## This dataset is already included in doi:10.25502/s67m-gx20/d.


carob_script <- function(path) {

"ACAI fraction of photosynthetically rational for cassava.The African Cassava Agronomy Initiative (ACAI) aims at improving cassava root yield and quality, and cassava supply to the processing sector. The project has 6 use cases of which best planting practices (BPP) is one. BPP is focusing on guiding farmers in choosing best-suited planting practices for cassava, with a focus on tillage operations and in close relation with improved weed control recommendations."

	uri <- "doi:10.25502/s67m-gx20/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1016/j.eja.2021.126242",
		project = "ACAI",
		carob_date = "2026-06-17",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "K_fertilizer",
		response_vars = "none", 
		carob_contributor = "Mitchelle Njukuya",
		carob_completion = 100,	
		carob_effort = NA,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "fpar-selected-treatments-benue-edo-2016-and-2017.csv"]
	f2 <- ff[basename(ff) == "treatment.csv"]
	#f3 <- ff[basename(ff) == "column_dictionary_fpar_selectedtreatments_2016_2017.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#r3 <- read.csv(f3)

	d1 <- data.frame(
		country = r1$Country,
		year = r1$Year,
		location = r1$Loc,
		treatment = r1$Treatment,
		PAR = r1$fPAR, # photosynthetic active radiation
		DAP = r1$DAP,
		crop = "cassava",                         #from publication
		plant_density = 12500,
		variety = "TME 419"
	)
	
	d2 <- data.frame(
		treatment = r2[["treatment_code"]],
		N_fertilizer = r2[["N_kg_ha"]],
		P_fertilizer = r2[["P_kg_ha"]],
		K_fertilizer = r2[["K_kg_ha"]],
		S_fertilizer = r2[["S_kg_ha"]],
		Mg_fertilizer = r2[["Mg_kg_ha"]],
		Zn_fertilizer = r2[["Zn_kg_ha"]],
		B_fertilizer = r2[["B_kg_ha"]]
	)

	d <- merge(d1, d2, by = "treatment", all.x = TRUE)
	
	d$N_splits <- as.integer(3)
	d$fertilizer_type <- "urea;KCl;TSP"
  
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	#from publication
	d$plot_length <- 10                              
	d$plot_width <- 8
	d$plot_area <- 80
	d$planting_date[d$year=="2016" & d$location=="Edo"] <- as.character(2016-05-24)
	d$planting_date[d$year=="2016" & d$location=="Benue"] <- as.character(2016-08-16)
	d$harvest_date[d$year=="2017" & d$location=="Edo"] <- as.character(2018-05-04)
	d$harvest_date[d$year=="2017" & d$location=="Benue"] <- as.character(2018-06-15)
	
	d$longitude[d$year=="2016" & d$location=="Edo"] <- 6.13
	d$latitude[d$year=="2016" & d$location=="Edo"] <- 7.05
	d$longitude[d$year=="2016" & d$location=="Benue"] <- 8.69
	d$latitude[d$year=="2016" & d$location=="Benue"] <- 6.67
	d$longitude[d$year=="2017" & d$location=="Edo"] <- 6.13
	d$latitude[d$year=="2017" & d$location=="Edo"] <- 6.80
	d$longitude[d$year=="2017" & d$location=="Benue"] <- 8.19
	d$latitude[d$year=="2017" & d$location=="Benue"] <-7.27
	d$geo_from_source <- FALSE
  d$year <- NULL 
  
	yield <- NA
	d$yield_part <- "tubers"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	d$trial_id <- as.character(as.integer(as.factor(1)))

	carobiner::write_files(path, meta, d)
}


