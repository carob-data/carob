# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Agronomic data of on-farm trials on yield response to lime rates  in Ethiopia

This dataset was generated as part of the Guiding Acid Soil Management Investments in Africa (GAIA) project, which aims to support evidence-based investments in acid soil management across sub-Saharan Africa. Within this project, researcher-managed on-farm trials were established to quantify crop yield responses to increasing lime application rates. The experimental design followed a randomized complete block design (RCBD) in which each participating farm represented one replication (block). A total of 80 farms were included, and four lime application rates were tested within each farm. Lime was applied at the onset of the trial (Year 1). Crop yields measured during the first year capture the direct response to lime application, while yields measured in the second year allow evaluation of the residual effects of the initial lime treatment. This dataset contains plot-level yield measurements for maize, wheat, faba bean, and soybean collected during the first and second years after trial establishment in Ethiopia. Predicted soil properties associated with the experimental sites are available from external datasets and can be linked through the soil sample unique identifier (SSN). These soil datasets are available at: - Predicted Soil Properties Dataset (hdl.handle.net/11529/10549136) - Predicted Soil Properties Dataset (doi:10.71682/10549328)

To facilitate interoperability and harmonization of agronomic terminology, a lookup table is provided that maps variable names and terms used in this dataset to the terminology defined in the terminag GitHub repository. (https://github.com/reagro/terminag/)
"

	uri <- "doi:10.71682/10549401"
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
		data_organization = "CIMMYT; CIRAD; EIAR",
		publication = NA,
		project = "GAIA",
		carob_date = "2026-04-28",
		design = "unitOfAnalysis",
		data_type = "experiment",
   	treatment_vars = "lime",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "GAIA_Eth_on_farm_trials_yield_yrs1_2_2026-03-13_v0.1.csv"]
	f2 <- ff[basename(ff) == "GAIA_on_farm_trials_agronomy_variable_description_2026-03-13_v0.1.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	d <- data.frame(
	   trial_id = paste(r1$fid,r1$trial_type, sep = "-"),
	   country = r1$country,
	   adm1 = carobiner::fix_name(r1$adm1, "title"),
	   adm2 = carobiner::fix_name(r1$adm2, "title"),
	   adm3 = carobiner::fix_name(r1$adm3, "title"),
	   longitude = r1$longitude,
	   latitude = r1$latitude,
	   previous_crop = gsub("fababean", "faba bean", tolower(r1$previous_crop)),
	   crop = gsub("fababean", "faba bean", tolower(r1$crop)),
	   treatment = r1$treatment,
	   lime = r1$lime_tha *1000,
	   planting_date = "2022",
	   harvest_date = as.character(r1$harvest_year),
	   on_farm = r1$is_on_farm,
	   irrigated = r1$is_irrigated,
	   yield =  r1$yield_tha*1000,
	   is_survey = FALSE, 
	   yield_part = "grain", 
	   yield_moisture = as.numeric(NA),
	   yield_isfresh = NA,
	   geo_from_source = TRUE
	)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	### remove rows with missing yield 
	
	d <- d[!is.na(d$yield),]
	

	carobiner::write_files(path, meta, d)
}


