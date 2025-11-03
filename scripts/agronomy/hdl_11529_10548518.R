# R script for "carob"
# license: GPL (>=3)

#ISSUES
#1. Added Odisha University of Agriculture and Technology (OUAT)
#2. I could not find coordinates for most of the adm3 places, so i filled in coordinates for adm2(Mayurbhanj)
#3. some dates have a full date format, and some only have a year and carobiner is expecting a full date,not sure how to proceed

carob_script <- function(path) {

"Maize on-farm trial data on crop management, hybrid selection and nutrient management from plateau ecology of Odisha, India
  
Maize is the staple crop cultivated during the monsoon season in the rainfed uplands in tribal-dominated plateau regions of Odisha in eastern India. However, productivity is low because of multiple factors, including poor adoption of best management practices. We conducted three types of experiments viz..single vs. layered best management practices, hybrids, and decision support tools on nutrient management for two years (2013 and 2014) to explore the opportunities for reducing rainfed maize yield gaps. On-farm trials were conducted in Mayurbhanj district in Odisha and data were collected manually from the treatments in each experiment and processed in excel file and analyzed using R software. (2020-09-30)"


	uri <- "hdl:11529/10548518"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;IRRI;CU;OUAT",
		publication = NA,
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-10-27",
		notes = NA, 
		design = NA
	)
	
	f1 <- ff[basename(ff) == "Experiment-I.csv"]
	f2 <- ff[basename(ff) == "Experiment-II.csv"]
	f3 <- ff[basename(ff) == "Experiment-III.csv"]
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

	required_cols <- c("STATE", "District", "Village", "Treat_details", "Rep",
	                     "Tillage", "Var", "SdDate", "CropEst", "SdRate",
	                     "FN_amt", "FP_amt", "FK_amt", "Weedmgt_type", "PN",
	                     "GY", "Fert_cost", "Price")

	standardize_data <- function(df) {
	  
	  rc <- required_cols[!(required_cols %in% colnames(df))]
	  df[, rc] <- NA
	  
	  data.frame(
		country = "India",
		adm1 = trimws(df$STATE),
		adm2 = df$District,
		adm3 = df$Village,
		treatment = df$Treat_details,
		rep = as.integer(df$Rep),
		land_prep_method = df$Tillage,
		variety = df$Var,
		planting_date=as.character(as.Date(df$SdDate, "%m/%d/%Y")),
		planting_method = tolower(df$CropEst),
		seed_rate = df$SdRate,
		N_fertilizer = df$FN_amt,
		P_fertilizer = df$FP_amt,
		K_fertilizer = df$FK_amt,
		weeding_method = df$Weedmgt_type,
		plant_density = df$PN,
		yield = df$GY*1000,
		fertilizer_price = as.character(df$Fert_cost),
		crop_price = df$Price,
		currency = "USD"
	  )
	}
	
	d1 <- standardize_data(r1)
	d2 <- standardize_data(r2)
	d2$planting_date <- as.character(r2$Year)
	d2$treatment <- r2$Treatment
	d3 <- standardize_data(r3)
	d3$planting_date <- as.character(r3$Year)
	d3$treatment <- r3$Treat_details
	
	d2_d3 <- rbind(d2,d3)
	d1 <- d1[, names(d2_d3)]
	d <- rbind(d1,d2_d3)

	d$on_farm <- TRUE 
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	d$lime <- as.numeric(NA) 
	d$yield_moisture <- 15  
	d$treatment <- iconv(d$treatment, from = "latin1", to = "UTF-8", sub = "")
	d$treatment <- trimws(d$treatment)
	d$trial_id <- paste(d$adm3, substr(d$planting_date, 1, 4), sep = "_")
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$land_prep_method <- gsub("Conventional", "conventional", d$land_prep_method)
	d$land_prep_method <- gsub("Zerotillage", "none", d$land_prep_method)
	d$planting_method <- gsub("machine", "mechanized", d$planting_method)
	
	d$treatment <- gsub("\u0092", "", d$treatment)

	carobiner::write_files(path, meta, d)
}


