# R script for "carob"
# license: GPL (>=3)

## ISSUES
# the discovery column in data_dictionary.csv lists the coverage as "Kano" but the locations listed in maize-planting-date-experiment.csv are "Iburu" and "Zaria", both located in Kaduna state and not Kano
# no information is given on fertiliser, irrigation, crop-rotation, soil info in either .csv
# coordinates for Iburu were estimated based on location name due to absence in geodata. 
# uncertainty and source where both set as 'NA' due to absence in geodata

carob_script <- function(path) {

"
Agronomy Data, Kano, Nigeria, Under TAMASA project.

TAMASA is a 4-year project seeking to improve productivity and profitability for small-scale maize farmers in Ethiopia, Nigeria and Tanzania.
"
	uri <- "doi:10.25502/sssg-0b75/d"
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

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ABU;BUK",
		publication = NA,
		project = "Taking Maize Agronomy to Scale in Africa (TAMASA)",
		design = "randomised complete block design, with 3 replications, 6 planting dates for 3 differnt varieties, across two different sites (Zaria and Iburu)",
		data_type = "on-farm experiment",
		treatment_vars = "variety;planting_date;rep",
		response_vars = "yield;silking_days;tassling_days;asi;maturity_days;plant_height;ear_height;harvest_index", 
		notes = "this trial was conducted in 2 different locations (Iburu and Zaria)",
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-07-15",
		carob_completion = 52,	
		carob_effort = 8
	)
	
	f2 <- ff[basename(ff) == "maize-planting-date-experiment.csv"]

	r2 <- read.csv(f2)

	d <- data.frame(
		plot_id = as.character(r2[["Plotno"]]),
		location = r2[["Loc"]],
		year = r2[["Year"]], # could not find equivalent in terminag
		variety = r2[["Variety"]],
		planting_date = format(as.Date(r2[["PlantingDate"]], format = "%m/%d/%Y"), "%Y-%m-%d"),
		rep = r2[["Rep"]],
		tassling_days = r2[["DFFL"]],
		silking_days = r2[["DYSK50"]],
		asi = r2[["ASI"]],
		maturity_days = r2[["Maturity"]],
		plant_height = r2[["PLHT"]],
		ear_height = r2[["E_HT"]],
		harvest_index = r2[["hi"]],
		yield = r2[["Gyieldtha"]],
		topwt = r2[["Topwtha"]] # could not find equivalent in terminag
	)
	
	d$country <- "Nigeria" # from coverage.country column in data_dictionary.csv

	d$trial_id <- as.character(as.integer(as.factor(paste(d$location, d$year))))
	
	d$on_farm <- TRUE # from https://hdl.handle.net/10883/19588 - confirm source!! 
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
    d$crop <- "maize"
	d$crop_rotation <- NA 

	# georeferencing SEE NOTES
	d$longitude <- ifelse(d$location == "Zaria", 7.7143, 7.78896)
	d$latitude <- ifelse(d$location == "Zaria", 11.0231, 10.26858)
	d$geo_uncertainty <- ifelse(d$location == "Zaria", 16966, NA)
	d$geo_source <- ifelse(d$location == "Zaria", "GADM 4.1, adm2", NA)

	d$geo_from_source <- FALSE # estimated from location given

	d$harvest_date  <- NA

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- NA
   d$K_fertilizer <- NA
   d$N_fertilizer <- NA
   d$S_fertilizer <- NA
   d$lime <- NA

   d$fertilizer_type <- NA
   
### Yield
	d$yield_part <- "grain"
	d$yield_moisture <- NA # not given
	d$yield_isfresh <- NA # not stated

	d$fwy_storage <- d$yield
	d$dmy_storage <- NA # moisture content not given so cannot be derived
	d$fwy_residue <- d$topwt - d$yield
	
	carobiner::write_files(path, meta, d)
}