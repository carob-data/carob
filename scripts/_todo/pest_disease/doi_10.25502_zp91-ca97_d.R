# R script for "carob"
# license: GPL (>=3)

## NOTES
# banana-weevil-dynamic-_psmnr-project.csv: adult weevils per pheromone trap per visit, plantain farms, South-West Cameroon, 2015-2017.
# The column named Lat holds about 9.1-9.3 and Long holds about 4.1-4.4. Those match longitude and latitude of the Buea area, so the two columns are swapped when stored.
# Season Rainy is stored as wet. Trap number is the replicate (dictionary: replication of the pheromone trap).
# adm1 Sud-Ouest is the GADM name for the South West region named in the dataset title.

## ISSUES
# Farmer name (Name) is empty on every row, so hhid is village plus trap, not a household code.
# Six Woteva visits have blank Long, Lat, and Altitude. Those coordinates stay missing.
# No variety, even though the abstract mentions improved plantain varieties. No yield.
# The abstract says the lure was cosmolure; the table does not name the lure.
# No fertilizer, irrigation, or planting date.
# "banana weevil" is not yet in the pest vocabulary.
# write_files: missing yield, yield_moisture, irrigated, N/P/K, planting_date, harvest_date; NA latitude and longitude on the 6 Woteva rows; pest name not in the vocabulary; carob_contributor empty. Confirm the Lat/Long swap against a map.

## NOT PROCESSED
# banana-weevil-dynamic-_psmnr-project.csv: ID. Row number.
# banana-weevil-dynamic-_psmnr-project.csv: Name. Farmer name, entirely blank.
# banana-weevil-dynamic-_psmnr-project.csv: Month and Year. The visit date is already stored.

## NEW VARIABLES
# cluster_: Buea or Bomboko (dataset: clusters of Mount Cameroon). Not an administrative unit.


carob_script <- function(path) {

"
Dynamic of banana weevil in plantain farms in South West region of Cameroon

The overall objective of the project was to transfer and promote improved plantain production technologies to enhance and profitability of plantain production in PSMNR target villages around the MCNP, thereby improving the livelihood of involved farmers. A planning. The experiments were set up to evaluate in participatory manner the performance of cosmolure trap against banana weevils in the demonstration plots planted with improved varieties of plantain from IITA and FHIA.
"

	uri <- "doi:10.25502/zp91-ca97/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "pheromone-trap counts on plantain farms",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "banana-weevil-dynamic-_psmnr-project.csv"]
	r <- read.csv(f, check.names=FALSE)
	names(r) <- trimws(names(r))

	season <- tolower(trimws(r$Season))
	season[season == "rainy"] <- "wet"
	season[season == "dry"] <- "dry"

	d <- data.frame(
		country = "Cameroon",
		adm1 = "Sud-Ouest",
		location = carobiner::fix_name(trimws(r$Village), "title"),
		cluster_ = carobiner::fix_name(trimws(r$Cluster), "title"),
		# Lat column is easting (~9.2) and Long column is northing (~4.2). See NOTES.
		latitude = as.numeric(r$Long),
		longitude = as.numeric(r$Lat),
		elevation = as.numeric(r$Altitude),
		geo_from_source = !is.na(as.numeric(r$Long)),
		date = as.character(as.Date(r$Evaluation_Date, format="%d/%m/%Y")),
		season = season,
		rep = as.integer(r$Trap),
		crop = "plantain",
		yield_part = "fruit",
		pest_species = "banana weevil",
		pest_incidence = as.integer(r$weevil_month),
		stress = "pests"
	)

	d$hhid <- paste(d$location, d$rep, sep="_trap")
	d$trial_id <- paste(d$location, r$Year, sep="_")
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
