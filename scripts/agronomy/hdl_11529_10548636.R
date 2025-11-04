# R script for "carob"


carob_script <- function(path) {
  
"Adoption of agronomic practices based on zero tillage and residue retention is increasing, due to their potential for climate change adaptation and mitigation. Genotype by tillage interactions for yield are not well understood. Fourteen CIMMYT bread (Triticum aestivum) and durum (Triticum turgidum) wheat genotypes, created between 1964 and 2011, were tested for yield and agronomic performance at CIMMYT’s experimental station near Ciudad Obregon, Mexico, over nine years (harvest years 2010 to 2018). Treatments included conventional and permanent raised beds with full and reduced irrigation. The dataset contains data on wheat performance (days to emergence, flowering and maturity, plant height, harvest index, grain yield, thousand grain weight, test weight, NDVI), performance traits calculated from these data (e.g. number of grains per m2, number of grains per spike, grain production rate from flowering to maturity) and daily weather data during the study period (precipitation, maximum and minimum temperature). This is an updated version of a dataset published in Honsdorf et al. (2018), where we have added three more years of data (harvest years 2016, 2017 and 2018) and one additional bread wheat genotype, Borlaug 100 (created in 2011) that was tested only in the six last years of the experiment (harvest years 2013 to 2018). (2021-12-14)"
  
  
	uri <- "hdl:11529/10548636"
	group <- "agronomy"

	ff	<- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		project=NA,
		publication="doi:10.1016/j.fcr.2017.11.011",
		data_organization = "CIMMYT",
		data_type="experiment", 
		carob_contributor="Mitchelle Njukuya",
		carob_date="2023-02-06",
		response_vars = "yield",
		treatment_vars = "land_prep_method;irrigation"
	)
	
	f <- ff[basename(ff) == "PUB-201-DIB_2021-Data_2021-12-12_corrected.xlsx"]
	
	r1 <- carobiner::read.excel(f,sheet = "Genotype list", skip = 3)
	r2 <- carobiner::read.excel(f, sheet = "Data", skip = 2)

	d1 <- data.frame(
		GID = r1$`Germplasm ID`, 
		variety = r1$Name, 
		variety_code = r1$`Selection History`
	)

	d <- data.frame(
		GID = r2$GID, 
		trial_id = as.character(r2$year), 
		planting_date = as.character(r2$SOW),
		emergence_days = r2$EMER, 
		flowering_days = r2$FLO, 
		maturity_days = r2$MAT, 
		plant_height = r2$HEI, 
		yield = r2$YLD)
					
	d$flowering_days[d$flowering_days == "NA"] <- NA
	d$flowering_days <- as.integer(d$flowering_days)
				
	#BW -> Bread Wheat, DW -> Durum Wheat
	d$crop <- ifelse(r2$type == "BW", "wheat", "durum wheat")

	#excel sheet for abbreviations indicated the following system names:
	treatcode = c("PB-FI", "PB-RI", "CB-RI", "CB-FI")
	treatname = c("Permanents beds, Full irrigation", "Permanents beds, Reduced irrigation", "Conventional tilled beds, Reduced irrigation", "Conventional tilled beds, Full irrigation")

	d$treatment <- treatname[match(r2$syst, treatcode)]
 	d$land_prep_method <- ifelse("PB" %in% r2$syst, "permanent beds", "conventional tilled beds")
 
	#From the publication :
	# At the start of the season, 103 kg N ha−1 and 23 kg P ha−1 was applied as a band application in the center of the beds. At first node, the RI treatment received an additional 100 kg N ha−1 and the FI treatment received 175 kg N ha−1 banded in the furrow.

	d$K_fertilizer <- 0
	d$P_fertilizer <- 23
	d$N_fertilizer <- 103
	d$N_fertilizer[r2$syst =="PB-RI"] <- 203 
	d$N_fertilizer[r2$syst =="CB-RI"] <- 203
	d$N_fertilizer[r2$syst =="CB-FI"] <- 278
	d$N_fertilizer[r2$syst =="PB-FI"] <- 278
	d$fertilizer_type <- "urea" 



	#site information was obtained from publication
	d$country <- "Mexico"
	d$adm1 <- "Sonora"
	d$location <- "Ciudad Obregón"
	d$longitude <- -109.926
	d$latitude <- 27.368
	d$geo_from_source <- FALSE
	
	d$yield_part <- "grain"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$irrigation_method <- "furrow"

	
	d <- merge(d, d1, by= "GID", all.x=TRUE)
	d$GID <- NULL
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}



