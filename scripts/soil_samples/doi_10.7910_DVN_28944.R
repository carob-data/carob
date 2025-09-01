# R script for "carob"


carob_script <- function(path) {
  
"During the Phase II of the SOIL project in the Maringa-Lopori-Wamba Landscape, a wide range of activities were implemented during the 2011- 2013 in order to develop and expand sustainable agriculture activities within the landscape. This report highlights the land and soil health assessment that was carried out to inform these and future activities using the Land Degradation Surveillance Framework (LDSF). (2015)"

	uri <- "doi:10.7910/DVN/28944"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=2,
		data_organization = "CIAT",
		publication = NA,
		project="CARPE Phase II-Sustainable Opportunities for Improving Livelihoods Project",
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_date="2024-05-29"
	)
  
	f1 <- ff[basename(ff) == "Yamb_CN_Crop_LDPSA_SSN.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "YamboyoLDSFRawdataforDataverse.csv"]
	r2 <- read.csv(f2)

	# Merge r1 and r2 to get lat and lon columns to be appended into the soils data
	r <- merge(r1, r2[, c('Cluster', "Plot", "Latitude", "Longitude")], by = c("Cluster", "Plot"), all.x = TRUE)

	i <- r$Depthcode=="Subsoil"
	r$depth_top <- ifelse(i, 20, 0)
	r$depth_bottom  <- ifelse(i, 50, 20)

	r$Cu.ppm.[r$Cu.ppm.=="< 0.2"] <- 0.001
	r$B.ppm.[r$B.ppm.=="< 0.02"] <- 0.001
	
	d <- data.frame(
		trial_id = r$ldsfID,
		soil_pH = as.numeric(r$pH..),
		soil_SOC = as.numeric(r$SOC...),
		soil_Ca = as.numeric(r$Ca.ppm.),
		soil_K = as.numeric(r$K.ppm.),
		soil_Mg = as.numeric(r$Mg.ppm.),
		soil_Mn = as.numeric(r$Mn.ppm.),
		soil_Na = as.numeric(r$Na.ppm.),
		soil_EC = as.numeric(r$EC.S...uS.cm.)/1000, # to convert from micro to milli Siemens/cm
		soil_Al = as.numeric(r$Al.ppm.),
		soil_S = as.numeric(r$S.ppm.),
		soil_B = as.numeric(r$B.ppm.),
		soil_Cu = as.numeric(r$Cu.ppm.),
		soil_Zn = as.numeric(r$Zn.ppm.),
		soil_Fe = as.numeric(r$Fe.ppm.),
		soil_P_available = as.numeric(r$P.ppm.),
		soil_CEC = as.numeric(r$C.E.C.meq.100g.),
		soil_clay = as.numeric(r$Clay...),
		soil_silt = as.numeric(r$Silt...),
		soil_sand = as.numeric(r$Sand...),
		depth_top = r$depth_top,
		depth_bottom = r$depth_bottom,
		longitude = r$Longitude,
		latitude = r$Latitude,
		geo_from_source= TRUE,
		country = "Democratic Republic of the Congo"
	)

    carobiner::write_files(path, meta, d)
}
