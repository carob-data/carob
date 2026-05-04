# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Replication Data for: Food and nutrition security merits of intercropping maize, sorghum and millet with legumes on small farms in Southern Africa

This database comprises grain and nutritional yield data from on-farm trials implemented in the Dedza and Mzimba districts of Malawi over three growing seasons (2017/18 to 2019/20). The study was designed to evaluate yields resulting from treatments involving different cereals and legumes within intercropping systems. The complete dataset supported the findings published in: 'Small grains for small farms? Food and nutrition security from maize, sorghum, and millet cropping systems in Southern Africa.
"

	uri <- "doi:10.71682/10549345"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;HEBAU",
		publication = "doi:10.1007/s12571-025-01556-2",
		project = NA,
		carob_date = "2026-05-04",
		design = NA,
		data_type = "experiment",
		treatment_vars = "intercrops",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Grain and nutritional yield data - Malawi 2017 to 2020.xlsx"]
	f2 <- ff[basename(ff) == "Grain and nutritional yield data - Malawi 2017 to 2020.rds"]

	r1 <- carobiner::read.excel(f1, sheet="legend")
	r2 <- carobiner::read.excel(f1, sheet="crop_data")
	#r3 <- readRDS(f2)


	d1 <- data.frame(
		adm2 = carobiner::fix_name(r2$district, "title"),
		location = r2$village,
		trial_id = paste(r2$farm_id, r2$site_name, sep = "-"),
		planting_date = substr(r2$year, 1, 4),
		cropland_owned = r2$land_size*10000,#m2
		rain = r2$rainfall,
		is_survey = r2$plot_type != "demo_plot", 
		land_prep_method = gsub("no-till", "none", r2$land_preparation),
		intercrops_c = r2$legume,
		intercrops_l = r2$cereal,
		crop_c = r2$cereal,
		crop_l = r2$legume,
		fwy_residue_c = r2$cereal_biom,
		fwy_residue_l = r2$legume_biom,
		fwy_total_c = r2$cereal_totbiom,
		yield_c = r2$cereal_yield,
		fwy_total_l = r2$legume_totbiom,
		yield_l= r2$legume_yield,
		#grain_protein = r2$protein, given in kg/ha instead of % or related unit
		country = "Malawi",
		on_farm = TRUE, 
		yield_part = "grain", 
		yield_moisture = as.numeric(NA), 
		irrigated = NA, 
		geo_from_source = FALSE,
		yield_isfresh = TRUE	
	)

	d <- reshape(d1, varying = list(c("yield_c", "yield_l"), c("fwy_residue_c", "fwy_residue_l"), c("fwy_total_c", "fwy_total_l"), c("crop_c", "crop_l"), c("intercrops_c", "intercrops_l")), 
	    v.names = c("yield", "fwy_residue", "fwy_total", "crop", "intercrops"), 
		direction = "long")
	
	d$id <- d$time <- NULL
	
	d <- d[!is.na(d$yield),]
	### fixing crop and intercrops names
	
	d$intercrops <- gsub("bambara nut", "bambara groundnut", d$intercrops)
	d$intercrops <- gsub("French bean", "common bean", d$intercrops)
	d$crop <- gsub("French bean", "common bean", d$crop)
	d$crop <- gsub("bambara nut", "bambara groundnut", d$crop)
	
	###  Adding Lon and lat coordinate
	
	geo <- data.frame(
	   location = c("fifteen", "kamenya", "maya", "chikoya", "kuwusa", "m'banya", "mateyo singini", "msekanawana mithi", "pwele singini", "reuben mithi", "simon guza", "tondoli gama", "tukwawire changaya", "zimema"),
	   longitude = c(35.3395, 34.4508, 34.10505, 33.46472, 33.77007,35.2550, 33.4598, 33.6844, 33.639, 34.0623, 33.737, 33.775, 33.8541, 34.0084),
	   latitude = c(-15.372, -14.371, -12.484, -13.0257, -13.9790, -16.9293, -11.674, -12.751, -10.352, -12.364, -13.432, -14.02586, -14.0732, -11.4387)
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	geo1 <- data.frame(
	   adm2 = c("Dedza", "Mzimba"),
	   lon= c(34.285, 33.5951),
	   lat = c(-14.0405, -11.549)
	)
	
	d <- merge(d, geo1, by= "adm2", all.x = TRUE)
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	
	d$lat <- d$lon <- NULL 
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)  

	
	carobiner::write_files(path, meta, d)
}


