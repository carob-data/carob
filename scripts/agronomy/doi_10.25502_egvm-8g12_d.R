# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Climate-smart cropping arrangements, systems and integrated soil fertility management technologies for crop yield, soil health and income gain

The smallholder farmers’ agri-food system in Malawi faces persistent and new biophysical and socio-economic constraints resulting in low productivity and high production risks, endangering the food and nutrition security and income of smallholder farmers. This situation is aggravated by climate change and strategies, based on science-based evidence, to increase resilience against climatic shocks are urgently needed. Specific challenges include soil degradation, unpredictable rainfall characterized by droughts and floods (for instance the devastating floods in the Shire Valley in March 2019), limited access to agricultural inputs, increasing pressure from pests and diseases and limited skills and knowledge among the farming communities. The proposed Action seeks to address these challenges by developing climate-smart, integrated technology options adapted to local conditions and the farmers’ realities, accompanied by a thorough understanding of socio-economic conditions that drive or hamper adoption through a cross-Center, multidisciplinary research effort. Emphasis will be placed on technologies that increase resilience to climate change with specific attention to sustainable soil and water conservation and integrated pest and disease management. Technological innovations will be designed and fine-tuned together with stakeholders and end-users in a participatory process involving innovation platforms, selection of technology options for on-farm testing by farmers, and farmer-managed on-farm experimentation and evaluation. Integrated action through collaboration among the diverse research partners and end-users will enable a holistic approach that considers the various aspects and dimensions of smallholders’ livelihoods.
"

	uri <- "doi:10.25502/egvm-8g12/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		carob_date = "2025-10-13",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;intercrops",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "desira_23_24_ckan.csv"]
	#f1 <- ff[basename(ff) == "data_dictionary.csv"]
	
	r <- read.csv(f)
	
	d <- data.frame(
		season = r$season,
		rep = r$rep,
		# adm2 = tolower(r$site), EPAs are not ADMins
		location = carobiner::fix_name(r$site, "title"), # these are the EPAs
		site = carobiner::fix_name(r$epa, "title"),
		treatment = r$trt_descrp,
		yield_maize = r$maizeGY,
		yield_cowpea = r$CowpeaGY,
		country = "Malawi",
		yield_part = "grain",
		row_spacing = 75,
		plant_spacing = 20,
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		trial_id = paste(r$site, r$ID, sep = "-"),
		irrigated = NA,
		yield_moisture = as.numeric(NA),
		intercrop_type = "strip"
	)

	d <- reshape(d, varying = c("yield_maize", "yield_cowpea"),
              v.names = "yield",
              times = c("maize", "cowpea"),
              timevar = "crop",
              direction = "long")	

	d <- d[d$yield > 0,]

 
	d$intercrops <- ifelse(grepl("Cowpea", d$treatment) & grepl("maize", d$crop), "cowpea",
                ifelse(grepl("Maize", d$treatment) & grepl("cowpea", d$crop), "maize", "none"))

	d$inoculated <- grepl("inoculation", d$treatment) & grepl("cowpea", d$crop)


	d$N_fertilizer <- ifelse(grepl("maize", d$crop) & grepl("Maize \\+ 100 N", d$treatment), 100,
                  ifelse(grepl("maize", d$crop) & grepl("Maize \\+ 200 N", d$treatment), 200,
                  ifelse(grepl("cowpea", d$crop) & grepl("Cowpea \\+ 30 N", d$treatment), 30, 0)))

## how do we know this?
	d$P_fertilizer <- 30
	d$K_fertilizer <- 60

	d$planting_date <- substr(d$season, 1, 4) 
	d$id <- d$season <- NULL


	geo <- data.frame(
	   site = c("Chinguluwe", "Chipoka", "Thumbwe", "Mombezi", "Dzaone", "Thondwe", "Lisasadzi", "Mtunthama", "Thuchila", "Boma", "Chilipa", "Namkumba", "Zidyana", "Linga"),
	   longitude = c(34.384, 34.511, 35.293, 35.1380, 34.322, 35.446, 33.7540, 33.759, 35.357, 34.334, 34.881, 34.824, 33.681, 33.776),
	   latitude = c(-13.667, -13.993, -15.751, -15.737, -14.3793, -15.5001, -13.8838, -13.982, -15.9143, -14.368, -14.944, -14.365, -14.023, -13.965)
	)

	d <- merge(d, geo, by= "site", all.x = TRUE)

	carobiner::write_files(path, meta, d)

}


