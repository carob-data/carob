# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {


"Supporting data for von Fromm et al. (2023) Controls on timescales of soil organic carbon persistence across sub-Saharan Africa"
  
	uri <- "doi:10.5281/zenodo.10211071"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
		data_organization = "MONTSU;ETH;UCB;HARAU;UCM;ROTH;BAU;ILRI;UCD;NMBU;Yale;ISRIC",
		publication = "doi:10.1111/gcb.17089",
		project = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-04-24",
		notes = "none", 
		design = NA
	)

	f <- ff[basename(ff) == "AfSIS_data_all.csv"]
	r <- read.csv(f)
	
	d <- data.frame(
	  country = r$Country,
	  location=r$Site,
	  longitude=r$Longitude,
	  latitude=r$Latitude,
	  elevation=r$Altitude,
	  soil_C_total=r$Total_C,
	  soil_pH=r$pH,
	  soil_clay=r$Clay_8um,
	  soil_Al=r$Alox,
	  soil_Fe=r$Feox)
	  
	d$depth_top <-ifelse(r$Depth=="Topsoil","0","30")
	d$depth_bottom <- ifelse(r$Depth=="Topsoil","30","100")
	d$depth_bottom <- as.numeric(d$depth_bottom)
	d$depth_top <- as.numeric(d$depth_top)
	soilmeta <- data.frame(
		variable = c("soil_Al", "soil_Ca", "soil_Fe"),
		method = c("X-ray Power Diffraction")
	)
	
	d$country <-gsub("SAfrica","South Africa",d$country)
	
	d$geo_from_source <- TRUE

	carobiner::write_files(path, meta, d)
}

