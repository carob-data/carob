# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Description and data_dictionary.csv has coverage as "Kano" but the locations are "Iburu" and "Zaria", both in Kaduna state, and not Kano

carob_script <- function(path) {

"
Agronomy Data, Kano, Nigeria, Under TAMASA project.

TAMASA is a 4-year project seeking to improve productivity and profitability for small-scale maize farmers in Ethiopia, Nigeria and Tanzania.
"

	uri <- "doi:10.25502/sssg-0b75/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ABU;BUK",
		publication = NA,
		project = "Taking Maize Agronomy to Scale in Africa (TAMASA)",
		design = "RCBD",
		data_type = "on-farm experiment",
		treatment_vars = "variety;planting_date;rep",
		response_vars = "yield;silking_days;tassling_days;asi;maturity_days;plant_height;ear_height;harvest_index", 
		notes = "this trial was conducted in 2 different locations (Iburu and Zaria)",
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-07-15",
		carob_completion = 52,	
		carob_effort = 8
	)
	
	f <- ff[basename(ff) == "maize-planting-date-experiment.csv"]
	r <- read.csv(f)

	d <- data.frame(
		plot_id = as.character(r[["Plotno"]]),
		location = r[["Loc"]],
		variety = r[["Variety"]],
		planting_date = format(as.Date(r[["PlantingDate"]], format = "%m/%d/%Y"), "%Y-%m-%d"),
		rep = r[["Rep"]],
		tassling_days = r[["DFFL"]],
		silking_days = r[["DYSK50"]],
		asi = r[["ASI"]],
		maturity_days = r[["Maturity"]],
		plant_height = r[["PLHT"]],
		ear_height = r[["E_HT"]],
		harvest_index = r[["hi"]],
		yield = r[["Gyieldtha"]],
		topwt = r[["Topwtha"]] # could not find equivalent in terminag
	)
	
	d$country <- "Nigeria" # from coverage.country column in data_dictionary.csv
	d$trial_id <- paste(d$location, d$year)
	
	d$on_farm <- TRUE # from https://hdl.handle.net/10883/19588 - confirm source!! 
	d$is_survey <- FALSE
	d$irrigated <- FALSE

    d$crop <- "maize"

	# georeferencing SEE NOTES
	d$longitude <- ifelse(d$location == "Zaria", 7.7143, 7.78896)
	d$latitude <- ifelse(d$location == "Zaria", 11.0231, 10.26858)
	d$geo_uncertainty <- ifelse(d$location == "Zaria", 16966, NA)
	d$geo_source <- ifelse(d$location == "Zaria", "GADM 4.1, adm2", NA)
	d$geo_from_source <- d$location == "Zaria"
	d$harvest_date  <- NA
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
   
### Yield
	d$yield_part <- "grain"
	d$yield_moisture <- NA # not given
	d$yield_isfresh <- NA # not stated
	d$fwy_residue <- d$topwt - d$yield
	d$topw <- NULL
	
	carobiner::write_files(path, meta, d)
}