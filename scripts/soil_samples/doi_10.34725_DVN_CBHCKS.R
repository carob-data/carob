# R script for "carob"

carob_script <- function(path) {
"These data were collected within the Kenya Cereal Enhancement Programme-Climate Resilient Agricultural Livelihoods (KCEP-CRAL) Window. This study was conducted within the action sites of the Kenya Cereal Enhancement Programme-Climate Resilient Agricultural Livelihoods (KCEP-CRAL) Window, which is a programme supported by the Government of Kenya and International Fund for Agricultural Development (IFAD) within the Adaptation for Smallholder Agriculture Programme (ASAP). The project assessed key indicators of land and soil health in order to understand drivers of degradation, and monitor changes over time using the Land Degradation Surveillance Framework (LDSF) methodology (http://landscapeportal.org/blog/2015/03/25/the-land-degradation-surveillance-framework-ldsf/). The LDSF provides a field protocol for measuring indicators of the 'health' of an ecosystem. The LDSF was developed by the World Agroforestry (ICRAF) in response to the need for consistent field methods and indicator frameworks to assess land health in landscapes. The framework has been applied in projects across the global tropics, and is currently one of the largest land health databases globally with more than 30,000 observations, shared at http://landscapeportal.org. This project will benefit from existing data in the LDSF database, while at the same time contributing to these critically important global datasets through on-going data collection. Earth Observation (EO) data will be combined with the LDSF framework to develop the outputs for the project, including land degradation and soil health. Specific Activities on the ICRAF Component as Stated in the Agreement 1. Develop survey methodology detailing study design, methodology, tools, work plan and timelines documented 2. Procure LDSF field survey equipment 3. Conduct five LDSF surveys across the KCEP-CRAL action areas 4. Process, analyse and document the soil samples 5. Conduct Earth Observation-based assessment of biophysical indicators over time 6. Conduct capacity development opportunities with members of the PCU M&E staff and Government counterparts on LDSF field methodology This dataset contains LDSF field data from five LDSF sites in Kenya totalling 787 LDSF plots . It also contains the predictions using MIR Spectroscopy for 1500 Topsoil (0-20cm) and Subsoil (20-50cm) samples for six soil variables, including soil organic carbon, total nitrogen and pH."
	
	uri <- "doi:10.34725/DVN/CBHCKS"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
		data_organization = "ICRAF",
		publication = NA,
		project="Kenya Cereal Enhancement Programme-Climate Resilient Agricultural Livelihoods (KCEP-CRAL) Window",
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_date="2024-05-29"
	)
  
	f1 <- ff[basename(ff) == "stdfine_ICRAF_mir_soil_predictions_KCEP_25Jan2021.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "KCEP CRAL_ldsf_plotpropertiesICRAF 2021.csv"]
	r2 <- read.csv(f2)

	# Merge r1 and r2 to get lat and lon columns to be appended into the soils data
	r <- merge(r1,r2[, c("Cluster", "Plot", "Latitude", "Longitude", "Site", "Date")], by = c("Site", "Cluster", "Plot"), all = TRUE)

	d <- data.frame(
		#trial_id = r$SSN,
		soil_pH = as.numeric(r$predpH),
		soil_SOC = as.numeric(r$predSOC),
		soil_ex_bases = as.numeric(r$predExBas),
		soil_clay = as.numeric(r$predClay),
		soil_sand = as.numeric(r$predSand),
		longitude = r$Longitude,
		latitude = r$Latitude,
		geo_from_source= TRUE,
		country = "Kenya",
		location = r$Site
	)

	d$depth_top <- as.numeric(r$DepthTop)
	d$depth_bottom <- as.numeric(r$DepthBottom)

	
	d$geo_from_source[is.na(d$latitude)] <- FALSE

	geo <- data.frame(
	   location= c("Chasimba", "Gatunga", "KuboSouth", "Muminji","Thange" ),
	   lat=c(-3.7260,-0.101312,-4.20839, -0.6213,-2.49635),
	   lon=c(39.6907, 38.00761, 39.3126, 37.7229, 37.9612)
	)
	
	d <- merge(d, geo, by="location", all.x=TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$lat <- d$lon <- NULL
	
	carobiner::write_files(path, meta, d)
}
