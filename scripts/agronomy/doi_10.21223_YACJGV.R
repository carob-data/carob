# R script for "carob"

carob_script <- function(path) {
	  
"Fertilizer response trials to compare the performance of 5 'best-bet' fertilizer recommendations with the current blanket recommendation in multiple locations and farms. The best-bets differ in N:P:K ratios and rates and are designed based on assumptions on how fertilizer responses may vary across locations and fields. The five treatments are compared in each site with only the reference treatment replicated. Tuber yield as well as secondary agronomic data were assessed. Data were collected using the ODK-based digital data collection tool 'Smart Agronomy Data Management System (SAnDMan)'."
	  
	uri <- "doi:10.21223/YACJGV"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		project=NA,
		publication= NA,
		data_organization = "CIP",
		data_type="experiment", 
		carob_contributor="Mary Njogu; Stephen Gichuhi",
		carob_date="2023-01-22",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer"
	)
	 
	f1 <- ff[basename(ff) == "Scaling_AKILIMO_yield_data.csv"]
	r <- read.csv(f1)

	f2 <- ff[basename(ff) == "DataDictionary_SA-VAP-1.xlsx"]  
	fert <- carobiner::read.excel(f2, sheet="Fertilizer treatments", skip=1)
	 
	d <- data.frame(
		country="Rwanda", 
		crop="potato", 
		yield_part = "tubers",
		yield=r$tuberY * 1000, 
		latitude=r$Latitude, 
		longitude=r$Longitude, 
		elevation=r$Altitude,
		geo_from_source = TRUE,
		treatment=r$treat
	)
 
	d$harvest_date <- as.character(as.Date(r$today, format= "%d-%b-%y"))
	d$trial_id <- as.character(as.integer(as.factor(paste(d$latitude, d$longitude))))

## we can read this directly from the file 
##	npk <- data.frame(
##		treatment = c("NPK6", "NPK4_MOP2", "NPK4_UREA2", "NPK4_DAP2", "NPK11"), 
##		N_fertilizer = c(51, 34, 80, 52, 94), 
##		P_fertilizer = c(22, 15, 15, 35, 41),
##		K_fertilizer = c(42, 78, 28, 28, 78)) 
##		d <- merge(d, npk, by="treatment")

	colnames(fert)[1] <- "treatment"
	fert$treatment <- gsub("_rep.$", "", fert$treatment)
	fert <- unique(na.omit(fert[, c("treatment", "N", "P", "K")]))
	colnames(fert)[-1] <- paste0(colnames(fert)[-1], "_fertilizer")

	d <- merge(d, fert, by="treatment")
	
## fixing two cases after inspecting 
## other yield values in the same trial (location)
	i <- d$yield > 100000
	d$yield[i] <- d$yield[i] / 10


	d$planting_date <- as.character(NA)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d <- unique(d)
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}


