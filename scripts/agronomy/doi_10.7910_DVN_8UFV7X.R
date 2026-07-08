#
# --- carobiner::draft() wrote this under scripts/_AI/_rejected/ (review here, then move manually) ---
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# After review: move to scripts/<group>/ if you complete it, or to scripts/_rejected/ if permanently dropped.
#
# carob_script() ends with return(FALSE): nothing left to auto-generate after data reads.
#
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# data_organization: fixed TARI → SARI (Savanna Agricultural Research Institute) — verified against the actual author/producer fields in the dataset JSON.
# data_type = "on-farm experiment" is infered from "communities" in the abstract
# SPRYR Readme table lists 'Thrice' with the same code (2) as 'Twice'; assumed code 3 = Thrice.
# GRAIN has no documented unit in the Readme, and its magnitude (13.3-93.3) is far too low for terminag's kg/ha "yield"/"yield_marketable" fields, and too high for "seed_weight" (1000-seed weight, ~2-4g for sesame). It is kept as a new term "yield_per_plant" field (unit unconfirmed, likely g/plant or per small subplot). Contacts for clarification: B.Kotu@cgiar.org, A.Nurudeen@cgiar.org.
# PLNTING (planting time) is only given as "mid July"/"late July"/"mid August" with no exact day, so planting_date was estimated as day 15 for "mid" and day 25 for "late".
# SPRYR has no terminag equivalent (no generic spray-count term exists, only product-specific herbicide_times/insecticide_times/fungicide_times) and neither the Readme nor any related publication states what was sprayed, so it is kept as a non-standard "spray_times" field instead of guessing the product.
# Location: Dataset-level metadata (Dataverse geographicCoverage) lists 8 communities in Ghana (Airport, Bonia, Nyangua, Zanko, Guo, Passe, Goli, Nata-Douri), but 001_maizeSesameIntercropping.csv has no location/community column, so rows cannot be linked to a specific site. longitude/latitude left NA; only country="Ghana" is set.

carob_script <- function(path) {

"
Crop Diversification in Maize-Based Cropping System: Maize-Sesame Intercropping

This data study explores raising and sustaining productivity in cereal-legume cropping systems in northern Ghana About the project  Project title: 	AfricaRISING - Sustainable intensification of cereal-based farming systems in the Guinea-Sudan-Savanna of Ghana and Mali  Project abstract  	Mono-cropping of sesame(Sesame indicum L.) is being promoted for cash to diversify smallholder income in the northern Ghana. The objective of this activity is to evaluate agronomic options for integrating sesame into maize, sorghum, and millet cropping systems. A split-plot design replicated in 3-4 communities per region will be used within row spacing 1 and 2 weeks after planting maize. For the trials, data will be collected on growth of maize and sesame, light interception/leaf area index, weed diversity and biomass, grain yield of maize and sesame, soil temperature, soil moisture, and plant pest and diseases. Output and input will be collected for cost benefit analysis. Project website:  https://africa-rising.net/  Project start date: 01/06/2014  Project end date : 01/10/2014
"

	uri <- "doi:10.7910/DVN/8UFV7X"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

## Non-metadata .json files in ff (e.g. nested Dataverse Dataset/*.json). Parsed with jsonlite::fromJSON.
## Optional string snapshots: lapply(json_list, function(x) jsonlite::toJSON(x, pretty=TRUE, auto_unbox=TRUE))
	jmeta <- paste0(yuri::simpleURI(uri), ".json")
	json_paths <- ff[grepl("\\.json$", ff, ignore.case=TRUE)]
	json_paths <- json_paths[!tolower(basename(json_paths)) %in% tolower(c(jmeta, "metadata.json"))]
	json_list <- if (length(json_paths) > 0) {
		stats::setNames(lapply(json_paths, jsonlite::fromJSON), basename(json_paths))
	} else {
		list()
	}

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "IITA; SARI",
		publication = NA,
		project = "AfricaRISING",
		carob_date = "2026-07-08",
		design = "split plot",
		data_type = "on-farm experiment",
		treatment_vars = "planting_date;spray_times",
		response_vars = "plant_height;yield_per_plant;n_capsules;n_branches",
		carob_contributor = "Oscar Bautista",
		carob_completion = 100,
		carob_effort = 2,
		notes = ""
	)
	

	f1 <- ff[basename(ff) == "001_maizeSesameIntercropping.csv"]
	#f2 <- ff[basename(ff) == ""]

	r1 <- read.csv(f1)
	#r2 <- read.???(f2)

	d <- data.frame(rep = as.integer(r1$REP))
	d$crop <- "sesame"
	d$plant_height <- r1$PLNTH
	d$planting_date <- as.character(as.Date(c("2014-07-15", "2014-07-25", "2014-08-15")[r1$PLNTING]))
	d$yield_per_plant <- r1$GRAIN
	d$yield_part <- "grain"
	d$n_capsules <- r1$NCAPS
	d$n_branches <- r1$NBRNCH
	d$spray_times <- as.integer(r1$SPRYR)

	d$yield <- as.numeric(NA)
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	d$N_fertilizer <- as.numeric(NA)
	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)

	d$trial_id <- "1"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$country <- "Ghana"
	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$geo_from_source <- FALSE

	carobiner::write_files (path=path, meta, d)
	
}
