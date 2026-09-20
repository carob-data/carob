# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Drivers of hydrogen cyanide in roots of field-grown cassava in Malawi

This dataset provides field and environmental data from multi-location cassava trials conducted in Malawi to examine hydrogen cyanide (HCN) content, yield, and related physiological and soil factors across different cultivars, sites, and harvest times. Two complementary files are included.  Driver_of_cassava_hcn_field.xlsx  &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Contains agronomic, physiological, yield, and soil property data collected at the same sites and harvest intervals. &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Variables include: leaf area index (LAI), plant height (cm), dry matter content (%), yield (t ha⁻&sup1;), HCN concentration (mg kg⁻&sup1;), and soil chemical/physical properties (pH, organic carbon, organic matter, nitrogen, phosphorus, potassium, calcium, magnesium, iron, manganese, zinc, clay %, silt %, texture class, and soil moisture content). &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Data are structured by location, harvest time (MAP), replicate, and cultivar.  Weather_driver_cassava_hcn_field.xlsx    Provides site-specific environmental variables recorded during the experiment. Variables include total rainfall (mm), maximum and minimum temperature (&deg;C), daily temperature range (DTR), solar radiation (MJ m⁻&sup2;), and growing degree days (GDD). Data are organised by location (Makoka, Chitedze, Chitala, Mkondezi), harvest time (6, 8, 10, 12 months after planting, MAP), and cultivar (Mbundumali, Sauti, Mpale, Sagonja). Corresponding HCN values are reported for each cultivar &times; site &times; harvest combination.   These datasets together provide a comprehensive resource for examining the environmental and physiological factors influencing cassava cyanogenic potential and yield across different site conditions. They can be used to replicate the analyses in the manuscript, perform meta-analyses, or support further modelling of cassava food safety and agronomic performance. &nbsp;
"

  uri <- "doi:10.5281/zenodo.17862666"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "UGRE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;root_HCN", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-19",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "Driver_of_cassava_hcn_field.xlsx"]
	f2 <- ff[basename(ff) == "Weather_driver_cassava_hcn_field.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

#### process
	
	d1 <- data.frame(
	  location = r1$Location,
	  MAP = r1$MAP,  # month After planting
	  rep = as.integer(r1$REP),
	  variety = r1$Cultivar,
	  LAI = r1$LAI,
	  plant_height = r1$Height,
	  yield_moisture = r1$DM,
	  yield = r1$Yield*1000,
	  root_HCN = r1$HCN, # hydrogen cyanide
	  soil_pH = r1$pH30cm,
	  soil_P = r1$`P(ug/g)30cm`,
	  soil_K_exch = r1$`K(Cmol/Kg)30cm`,
	  soil_Ca_exch = r1$`Ca(Cmol/Kg)30cm`,
	  soil_Mg_exch = r1$`Mg(Cmol/Kg)30cm`,
	  soil_Fe = r1$`Fe(ug/g)30cm`,
	  soil_Mn = r1$`Mn(ug/g)30cm`,
	  soil_Zn = r1$`Zn(ug/g)30cm`,
	  soil_texture = r1$Texture_30cm,
	  soil_SOC = r1$`%OC30cm`,
	  soil_SOM = r1$`%OM30cm`,
	  soil_N = r1$`%N30cm`*10000,
	  soil_clay = r1$`%Clay30cm`,
	  soil_silt = r1$`%Silt30cm`,
	  trial_id = paste(r1$Location, r1$REP, sep = "-")
	)

	d2 <- data.frame(
		location = r2$Location,
		MAP = r2$MAP,
		variety = r2$Cultivar,
		rain = r2$Total_rainfall,
		tmax = r2$Tmax,
		tmin = r2$Tmin,
		srad = r2$Solar_radiation
	)
	
	
	d <- merge(d1, d2, by= c("location", "MAP", "variety"), all.x = TRUE)

	### Fixing soil texture 
	P <- carobiner::fix_name(d$soil_texture)
	P <- gsub("loamy sandy", "loamy sand", P)
	P <- gsub("Sand clay", "sandy clay", P)
	P <- gsub("sanday clay", "sandy clay", P)
	P <- gsub("Sandy clay loam", "sandy clay loam", P)
	P <- gsub("sandyloam", "sandy loam", P)
	d$soil_texture <- P
	
	#### Adding lon and lat coordinate 
	
	geo <- data.frame(
	  location = c("Chitala", "Chitedze", "Makoka", "Mkondezi"),
	  longitude =  c(33.2713, 33.6373, 35.3788, 34.2475),
	  latitude = c(-9.696, -13.9813, -14.762, -11.6201),
	  geo_from_source = FALSE,
	  geo_source = "Google Maps"
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	d$crop <- "cassava"
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_part <- "roots"
	d$country <- "Malawi"
	d$irrigated <- NA
	d$planting_date <- NA
	d$harvest_date <- NA

	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}

