# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"
Soil properties predicted from mid-infrared spectral (MIRS) analysis of 61 soil samples collected in 2021 before establishing on-farm trials on yield response to lime rates  in Kakamega Kenya

Selected soil properties were predicted from 61 topsoil and 61 subsoil samples subjected to spectral analysis (MIRS). A subset of samples were also subjected to wet chemistry analysis (see links in the attached reports), and results were used to calibrate a machine-learning algorithm developed by the International Centre for Research in Agroforestry (ICRAF) in Kenya. Coordinates were truncated to protect farmer's privacy. Unless specified, all properties were predicted. This dataset can be linked with yield data (coming soon) through unique farm identifier (fid).
"

	uri <- "doi:10.71682/10549397"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT; KALRO;ICRAF",
		publication = NA,
		project = NA,
		design = "unitOfAnalysis: Soil sample; collectionMode: Lab analysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-06-22",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f <- ff[basename(ff) == "GAIA_KEN_on_farm_trials_soil_properties_v0.1.xlsx"]

	r <- carobiner::read.excel(f, sheet="soil_data")
	
	d <- data.frame(
	  country=r$country,
	  adm1=r$adm1,
	  adm2=r$adm2,
	  adm3=r$adm3,
	  adm4=r$adm4,
	  latitude=r$latitude,
	  longitude=r$longitude,
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
	  soil_Al_exch=r$m3.Al/90,#mg/kg to cmol/kg for exchangeables(stated in raw data)
	  soil_B=r$m3.B,
	  soil_Ca_exch=r$m3.Ca/200,#mg/kg to cmol/kg for exchangeables(stated in raw data)
	  soil_Fe=r$m3.Fe,
	  soil_K_exch=r$m3.K/390,#mg/kg to cmol/kg for exchangeables(stated in raw data)
	  soil_Mg_exch=r$m3.Mg/120,#mg/kg to cmol/kg for exchangeables(stated in raw data)
	  soil_Na_exch=r$m3.Na/230,#mg/kg to cmol/kg for exchangeables(stated in raw data)
	  soil_P=r$m3.P,
	  soil_ex_acidity=r$ExAc,
	  soil_PSI=r$PSI,
	  soil_CEC=r$CEC
	)
	
	d$geo_from_source=TRUE
	
	d$soil_texture <-gsub("clayloam","clay loam",d$soil_texture)
	d$soil_texture <-gsub("sandyclayloam","sandy clay loam",d$soil_texture)
	d$soil_texture <-gsub("sandyclay loam","sandy clay loam",d$soil_texture)
	
	carobiner::write_files(path, meta, d)
}

