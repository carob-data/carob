# R script for "carob"

## only processing the soil samples. The omission trials are extracted from different datasets 

carob_script <- function(path) {
  
"Omission trials conducted in 5 countries under AfSIS Phase 1 under CIAT"
  
	uri <- "doi:10.7910/DVN/C6DIIC"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=5,
		data_organization = "CIAT",
		publication= "doi:10.1007/s1070",
		project="AfSIS",
		data_type= "experiment",
		carob_contributor= "Andrew Sila",
		carob_date="2024-05-14"
	)
  
	f1 <- ff[basename(ff) == "03. DiagnosticTrials_2009-2012_SSA_Wetchem.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "02. DiagnosticTrials_2009-2012_SSA_Yld.xlsx"]
	r2 <- suppressWarnings(carobiner::read.excel(f2))
	r2 <- r2[, c("Site", "Cluster", "Field", "Flat", "Flong")]
	r <- merge(r1, r2, all.x=TRUE)
   
	d <- data.frame(
		trial_id = r$FieldID,
		soil_pH = r$pH,
		soil_EC = r$m3.ECS/1000,
		soil_Al = r$m3.Al,
		soil_B = r$m3.B,
		soil_Ca = r$m3.Ca,
		soil_Cu = r$m3.Cu,
		soil_Fe = r$m3.Fe,
		soil_K = r$m3.K,
		soil_Mg = r$m3.Mg,
		soil_Mn = r$m3.Mn,
		soil_Na = r$m3.Na,
		soil_P_total = r$m3.P,
		soil_S = r$m3.S,
		soil_Zn = r$m3.Zn,
		soil_ex_acidity = r$m3.Hp,
		soil_PSI = r$PSI,
		soil_ex_Na = r$ExNa,
		soil_ex_Ca = r$ExCa,
		soil_ex_K = r$ExK,
		soil_ex_Bas = r$ExBas,
		soil_clay = r$psa.c4clay,
		soil_silt = r$psa.c4silt,
		soil_sand = r$psa.c4sand,
		soil_C = r$Total_Carbon,
		soil_SOC = r$Acidified_Carbon,
		soil_N = r$Total_Nitrogen,
		country = r$Country,
		location = r$Site,
		longitude = r$Flong,
		latitude = r$Flat
	)
	d$soil_PSI <- ifelse(d$soil_PSI < 0, 0,d$soil_PSI)
  
	d$latitude[d$latitude == 0] <- NA
	d$longitude[d$longitude == 0] <- NA
	d$longitude[d$location == "Finkolo"] <- -5.5113
	d$latitude[d$location == "Finkolo"] <- 11.2692


	##miss <- unique(d[is.na(d$latitude), c("country", "location")])
	##geo <- carobiner::geocode(miss$country, miss$location)
	##geo$put
	
	geo <- data.frame(
		country = c("Nigeria", "Kenya", "Malawi", "Tanzania", "Tanzania", "Malawi", "Malawi"), 
		location = c("Ibi", "Kandara", "Kasungu", "Kiberashi", "Mbinga", "Nkhata Bay", "Thuchila"), 
		lon = c(9.9328, 37.0249, 33.4724, 37.4431, 34.9557, 34.0416, 35.5829), 
		lat = c(8.3951, -0.8823, -12.9925, -5.3752, -10.7049, -11.6084, -15.888)
	)

	d <- merge(d, geo)
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lon[is.na(d$latitude)]
	d$lat <- d$lon <- NULL
	
    carobiner::write_files(path, meta, d)
}
