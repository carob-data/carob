# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Soil properties predicted from mid-infrared spectral (MIRS) analysis of 366 soil samples collected in 2024  before and/or after establishing on-farm trials on yield response to lime rates  in Zambia

Selected soil properties were predicted from 366 topsoil samples subjected to spectral analysis (MIRS). A subset of samples were also subjected to wet chemistry analysis (see link in the PDF report), and results were used to calibrate a machine-learning algorithm developed by the International Centre for Research in Agroforestry (ICRAF) in Kenya. Coordinates were truncated to protect farmer's privacy. Unless specified, all properties were predicted. This dataset can be linked with yield data (coming soon) through the unique farm identifer 'fid'. A link is provided to match terms used in the 'terminag' GitHub (https://github.com/reagro/terminag/) as of January 2026
"

	uri <- "doi:10.71682/10549395"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;CIRAD;ICRAF",
		publication = NA,
		project = NA,
		design = "unitOfAnalysis: Soil sample; collectionMode: Lab analysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-06-23",
		carob_completion = 100,	
		carob_effort = 1
	)
	
	f <- ff[basename(ff) == "GAIA_ZMB_on_farm_trials_soil_properties_v0.1.xlsx"]
	r <- carobiner::read.excel(f, sheet="soil_data")
	r <- unique(r) # reduces records from 366 to 279!
	
	d <- data.frame(
	  sample_id = r$ssn,
	  country=r$country,
	  adm1=r$adm1,
	  adm2=r$adm2,
	  longitude=r$longitude,
	  latitude=r$latitude,
	  geo_from_source = TRUE,
	  soil_depth=r$soil_depth,
	  depth_top=r$soil_sample_top,
	  depth_bottom=r$soil_sample_bottom,
	  soil_sand=r$Sand,
	  soil_clay=r$Clay,
	  soil_silt=r$Silt,
	  soil_texture=tolower(r$Soil_Textural_Class),
	  soil_pH=r$pH,
	  soil_SOC=r$SOC,
	  soil_N=r$TN,
	  soil_PSI=r$PSI,
	  soil_Al=r$m3.Al,
	  soil_B=r$m3.B,
	  soil_Ca=r$m3.Ca,
	  soil_Fe=r$m3.Fe,
	  soil_K=r$m3.K,
	  soil_Mg=r$m3.Mg,
	  soil_Na=r$m3.Na,
	  soil_ex_acidity=r$ExAc,
	  soil_CEC=r$CEC
	)
	
	d$date <- "2024"
	d$date[r$period == "after harvest"] <- "2025"
	
	d$soil_texture <- gsub("sandy", "sandy ", d$soil_texture)
	d$soil_texture <- trimws(gsub("clay", "clay ", d$soil_texture))
	 
	 d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}

