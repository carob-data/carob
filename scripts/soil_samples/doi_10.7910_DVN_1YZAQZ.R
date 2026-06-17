# R script for "carob"


carob_script <- function(path) {
  
"Landscape-scale variability of soil health indicators: Effects of cultivation on soil organic carbon in the Usambara Mountains of Tanzania"

	uri <- "doi:10.7910/DVN/1YZAQZ"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=3,minor=7,
		data_organization = "CIAT",
		publication= "doi:10.1007/s10705-015-9750-1",
		project="CCAFS",
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_effort = NA,
		carob_date="2024-05-16"
	)
  
	f1 <- ff[basename(ff) == "2. Crop Nutrition Results-Lushoto_30 Reference Samples-June 2013.xlsx"]
	r1 <- carobiner::read.excel(f1, TRUE)
	f2 <- ff[basename(ff) == "4. Lushoto organic carbon and nitrogen data_30 Reference Samples.xlsx"]
	r2 <- carobiner::read.excel(f2, TRUE)
	f3 <- ff[basename(ff) == "6. LushotoLDSFRaw.csv"]
	r3 <- read.csv(f3)
	
	r1$SSN <- gsub("icraf ", "icr", r1$Field)
	r2$SSN[r2$SSN=="icr077496"] <- "icr077476"

	r <- merge(r1, r2, by = "SSN", all=T)	
	r <- merge(r, r3[,c("Latitude", "Longitude", "Country","Site", "Cluster")], by = c("Site", "Cluster"))

	d <- data.frame(
		country = r$Country,
		location = r$Site,
		longitude = r$Longitude,
		latitude = r$Latitude,
		geo_from_source= TRUE,
		#trial_id = r$SSN,
		soil_pH = as.numeric(r$pH),
		soil_EC = as.numeric(r$EC.S.uS.cm)/1000,
		soil_Al = as.numeric(r$Al.ppm),
		soil_B = as.numeric(r$B.ppm),
		soil_Ca = as.numeric(r$Ca.ppm),
		soil_Cu = as.numeric(r$Cu.ppm),
		soil_Fe = as.numeric(r$Fe.ppm),
		soil_K = as.numeric(r$K.ppm),
		soil_Mg = as.numeric(r$Mg.ppm),
		soil_Mn = as.numeric(r$Mn.ppm),
		soil_Na = as.numeric(r$Na.ppm),
		soil_P_total = as.numeric(r$P.ppm),
		soil_S = as.numeric(r$S.ppm),
		soil_Zn = as.numeric(r$Zn.ppm),
		soil_ex_acidity = as.numeric(r$Hp.meq.100g),
		soil_PSI = as.numeric(r$PSI.meq.100g),
		soil_C = r$Carbon.Content.pct,
		soil_N = r$Nitrogen.Content.pct
	)

	carobiner::write_files(path, meta, d)
}

