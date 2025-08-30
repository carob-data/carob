# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. Coordinates were provided within the data set, but they are reporting a different country

carob_script <- function(path) {

"Soil properties predicted from mid-infrared spectral (MIRS) analysis of soil samples collected in 2023 (second year) before and/or after establishing on-farm trials on yield response to lime rates in Rwanda
  
Selected soil properties were predicted from 524 topsoil samples subjected to spectral analysis (MIRS). A subset of samples were also subjected to wet chemistry analysis, and results were used to calibrate a machine-learning algorithm developed by the International Centre for Research in Agroforestry (ICRAF) in Kenya. Coordinates were truncated to protect farmer's privacy.  "


	uri <- "doi:10.71682/10549329"
	group <- "soil_samples"

	ff  <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;CIRAD;RAB;ICRAF;",
		publication = NA,
		project = "GAIA",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-08-29",
		notes = NA,
		design = NA
	)
	
	f <- ff[basename(ff) == "GAIA_RWA_on_farm_trials_soil_properties_yr2_v0.1.csv"]
	r <- read.csv(f)

	d <- data.frame(
	  country = r$country,
	  adm1 = r$adm1,
	  adm2 = r$adm2,
	  longitude = r$longitude,
	  latitude = r$latitude,
	  soil_depth = r$soil_depth,
	  depth_top = r$soil_sample_top,
	  depth_bottom = r$soil_sample_bottom,
	  soil_sand = r$Sand,
	  soil_clay = r$Clay,
	  soil_silt = r$Silt,
	  soil_texture = tolower(r$Soil_Textural_Class),
	  soil_pH = r$pH,
	  soil_SOC = r$SOC,
	  soil_N = r$TN*10000,# from % to mg/ka
	  soil_PSI=r$PSI,
	  soil_Al = r$m3.Al,
	  soil_B = r$m3.B,
	  soil_Ca = r$m3.Ca,
	  soil_Fe = r$m3.Fe,
	  soil_K = as.numeric(r$m3.K),
	  soil_Mg = r$m3.Mg,
	  soil_Mn = as.numeric(r$m3.Mn),
	  soil_Na = as.numeric(r$m3.Na),
	  soil_S = as.numeric(r$m3.S),
	  soil_ex_acidity = r$ExAc,
	  soil_CEC=r$CEC,
	  geo_from_source = TRUE
	) 
	
	d$soil_texture <- trimws(gsub("y", "y ", d$soil_texture))

	soilmeta <- data.frame(
		variable = c("soil_Al", "soil_B", "soil_Ca", "soil_Fe", "soil_K", "soil_Mg", "soil_Mn", "soil_Na", "soil_S"),
		method = "Mehlich3 (estimated from spectroscopy)"
	)

	d <- d[d$longitude < 30.4, ]
	carobiner::write_files(path, meta, d, var_meta=soilmeta)
}

