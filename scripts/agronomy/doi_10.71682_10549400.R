# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Agronomic data of on-farm trials on yield response to lime rates  in Tanzania

This dataset was generated as part of the Guiding Acid Soil Management Investments in Africa (GAIA) project, which aims to support evidence-based investments in acid soil management across sub-Saharan Africa. Within this project, researcher-managed on-farm trials were established to quantify crop yield responses to increasing lime application rates. The experimental design followed a randomized complete block design (RCBD) in which each participating farm represented one replication (block). A total of 78 farms were included, and four lime application rates were tested within each farm. Lime was applied at the onset of the trial (Year 1) and subsequently (Year 2). This dataset contains plot-level yield measurements for maize, wheat, faba bean, and soybean collected during the first and second years after trial establishment in Tanzania. Predicted soil properties associated with the experimental sites are available from external datasets and can be linked through the soil sample unique identifier (SSN). These soil datasets are available at:  https://hdl.handle.net/11529/10549139 and https://doi.org/10.71682/10549330.

To facilitate interoperability and harmonization of agronomic terminology, a lookup table is provided that maps variable names and terms used in this dataset to the terminology defined in the terminag GitHub repository. (https://github.com/reagro/terminag/)
"


	uri <- "doi:10.71682/10549400"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	jmeta <- paste0(yuri::simpleURI(uri), ".json")
	json_paths <- ff[grepl("\\.json$", ff, ignore.case=TRUE)]
	json_paths <- json_paths[!tolower(basename(json_paths)) %in% tolower(c(jmeta, "metadata.json"))]
	json_list <- if (length(json_paths) > 0) {
		stats::setNames(lapply(json_paths, jsonlite::fromJSON), basename(json_paths))
	} else {
		list()
	}

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;CIRAD; TARI;ICRAF",
		publication = NA,
		project = "GAIA",
		carob_date = "2026-04-27",
		design = "unitOfAnalysis",
		data_type = "experiment",
		treatment_vars = "lime",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "GAIA_on_farm_trials_agronomy_variable_description_2026-03-13_v0.1.csv"]
	f2 <- ff[basename(ff) == "GAIA_Tza_on_farm_trials_yield_yrs1_2_2026-03-13_v0.1.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	
	#### process
	
	d <- data.frame(
	   trial_id = paste(r2$fid,r2$trial_type, sep = "-"),
	   country = r2$country,
	   adm1 = carobiner::fix_name(r2$adm1, "title"),
	   adm2 = carobiner::fix_name(r2$adm2, "title"),
	   adm3 = carobiner::fix_name(r2$adm3, "title"),
	   longitude = r2$longitude,
	   latitude = r2$latitude,
	   previous_crop = gsub("beans", "common bean", tolower(r2$previous_crop)),
	   crop = gsub("beans","common bean", tolower(r2$crop)),
	   treatment = r2$treatment,
	   lime = r2$lime_tha*1000,
	   planting_date = r2$planting_date,
	   harvest_date = r2$harvest_date,
	   on_farm = r2$is_on_farm,
	   irrigated = r2$is_irrigated,
	   yield =  r2$yield_tha*1000,
	   is_survey = FALSE, 
	   yield_part = "grain", 
	   yield_moisture = as.numeric(NA),
	   yield_isfresh = NA,
	   geo_from_source = TRUE
	)
	
	### fixing lon and lat coordinate 
	
	geo <- data.frame(
	   adm3 = c("Iponya","Nyamalimbe","Nyarugusu"),
	   lon = c(32.109, 32.3676, 32.2373),
	   lat = c(-3.7041, -3.0805, -3.1130),
	   geo_from = FALSE
	)
	
	d  <- merge(d, geo, by= "adm3", all.x = TRUE)
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
	d$lat <- d$lon <- d$geo_from <- NULL 
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	
	carobiner::write_files(path, meta, d)
}


