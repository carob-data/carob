# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"Soil properties predicted from mid-infrared spectral (MIRS) analysis of soil samples collected in 2022 before establishing on-farm trials on yield response to lime rates in Ethiopia
  
Selected soil properties were predicted from 79 topsoil samples subjected to spectral analysis (MIRS). A subset of samples were also subjected to wet chemistry analysis, and results were used to calibrate a machine-learning algorithm developed by the International Centre for Research in Agroforestry (ICRAF) in Kenya.

Coordinates were truncated to protect farmer's privacy. Unless specified, all properties were predicted. When calculated from other predicted properties, the variable name contained the string: Estimated  "


	uri <- "hdl:11529/10549136"
	group <- "soil_samples"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
	                                data_organization = "CIMMYT; CIRAD; EIAR; ICRAF",
	                                publication =NA,
	                                project = "GAIA",
	                                carob_date = "2025-08-30",
	                                design = NA,
	                                data_type = "survey",
	                                treatment_vars = "none",
	                                response_vars = "none", 
	                                carob_contributor = "Blessing Dzuda",
	                                completion = 100,	
	                                notes = NA
	)
	
	f <- ff[basename(ff) == "GAIA_Eth_on_farm_trials_soil_properties_yr1_v0.1-1.csv"]
	r <- read.csv(f)
	
	  d <- data.frame(
	    country = r$country,
	    adm1 = r$adm1,
	    adm2 = r$adm2,
	    adm3=r$adm3,
	    longitude = r$longitude,
	    latitude = r$latitude,
	    soil_depth = r$soil_depth,
	    depth_top = r$soil_sample_top,
	    depth_bottom = r$soil_sample_bottom,
	    soil_sand = r$Sand,
	    soil_clay = r$Clay,
	    soil_silt = r$Estimated_Silt,
	    soil_texture = tolower(r$Soil_Textural_Class),
	    soil_pH = r$pH,
	    soil_SOC = r$SOC,
	    soil_N = r$TN*10000,# from % to mg/ka
	    soil_PSI=r$PSI,
	    soil_Al = r$m3.Al,
	    soil_B = r$m3.B,
	    soil_Ca = r$m3.Ca,
	    soil_Fe = r$m3.Fe,
	    soil_K = r$m3.K,
	    soil_Mg = r$m3.Mg,
	    soil_Mn = r$m3.Mn,
	    soil_Na = r$m3.Na,
	    soil_S = r$m3.S,
	    soil_CEC=r$CEC,
	    soil_acidity_sat=r$Estimated_Acid_Saturation,
	    geo_from_source = TRUE
	  )
	  
	  soilmeta <- data.frame(
	    soil_element = c("Al", "B", "Ca", "Fe", "K", "Mg", "Mn", "Na", "S"),
	    soil_method = "spectroscopy (Mehlich3 extraction estimate)"
	  )
	  
	  carobiner::write_files(path, meta, d, var_meta=soilmeta)
}
