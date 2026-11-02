# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily pheromone-trap counts for three maize stem-borer/armyworm species at
# Muhaka (Msambweni). Field Hobo climate aggregated to daily tmin/tmax/temp,
# rhum, dewp. Lab and Raspberry Pi indoor climate files not used.

## ISSUES
# Trap records start 2021-11-04; field climate starts 2022-10-19 so weather
# does not cover the first year of trapping. Pest scientific names are not
# in values_pest.csv. Source bf file has 3 duplicate dates. write_files:
# missing yield, yield_moisture, yield_part, N/P/K_fertilizer, planting_date,
# harvest_date; invalid pest_species terms.


carob_script <- function(path) {

"
Corn pest and climate monitoring dataset from Msambweni, Kwale, Kenya, 2022-2024

This dataset corresponds to climate data and maize pests monitoring of Busseola fusca, Chilo partellus, and Spodoptera frugiperda between 2022 and 2024 at Msambweni (Kwale), Kenya (4°20’38”S, 39°29’21”E, altitude 20 masl). Trapping data include site name, survey date, species, number of individuals, pheromone change dates, and any notes. Climatic data are composed of three files: (i) bdd_kenya_muhaka_field_climate.csv with temperature, relative humidity and dew point from a Hobo MX2301A sensor (ONSET(r)) close to the traps ; (ii) bdd_kenya_muhaka_lab_climate.csv with temperature, relative humidity and dew point from a Hobo MX2301A sensor (ONSET(r)) from a nearby building ; (iii) bdd_kenya_muhaka_rpi_climate.csv with temperature from a BME680 sensor, temperature from a DS18B20 sensor, relative humidity and atmospheric pressure from a BME680 sensor, and visible, IR and UV light from a SI1145 sensor acquired using a Raspberry Pi 3A+ single-board computer in a nearby building. All date and time data are in UTC.
"

	uri <- "doi:10.23708/SM7CWG"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRD",
		publication = NA,
		project = NA,
		design = "pheromone trap monitoring",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 75,
		carob_effort = 0.4
	)

	f1 <- ff[basename(ff) == "bdd_kenya_muhaka_bf.csv"]
	f2 <- ff[basename(ff) == "bdd_kenya_muhaka_cp.csv"]
	f3 <- ff[basename(ff) == "bdd_kenya_muhaka_sf.csv"]
	f4 <- ff[basename(ff) == "bdd_kenya_muhaka_field_climate.csv"]

	# one row per trap-night and species
	make_pest <- function(f) {
		r <- read.csv(f)
		data.frame(
			location = r$site,
			date = as.character(as.Date(r$date)),
			pest_species = r$species,
			pest_incidence = as.integer(r$numberOfIndividuals)
		)
	}
	d <- rbind(make_pest(f1), make_pest(f2), make_pest(f3))

	d$country <- "Kenya"
	d$adm1 <- "Kwale"
	d$location <- "Muhaka"
	# coordinates from the dataset abstract
	d$latitude <- -4.343889
	d$longitude <- 39.489167
	d$elevation <- 20
	d$geo_from_source <- TRUE
	d$crop <- "maize"
	d$trial_id <- "Muhaka"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- NA
	d$stress <- "pests"

	# field sensor near the traps; indoor lab/rpi files omitted
	w <- read.csv(f4)
	w$date <- as.Date(w$dateUTC)
	wmean <- aggregate(w[, c("temperature", "rh", "dewpoint")], w["date"], mean)
	names(wmean) <- c("date", "temp", "rhum", "dewp")
	wmin <- aggregate(w["temperature"], w["date"], min)
	names(wmin) <- c("date", "tmin")
	wmax <- aggregate(w["temperature"], w["date"], max)
	names(wmax) <- c("date", "tmax")
	weather <- merge(merge(wmean, wmin, by="date"), wmax, by="date")
	weather$date <- as.character(weather$date)
	weather$country <- "Kenya"
	weather$location <- "Muhaka"
	weather$latitude <- -4.343889
	weather$longitude <- 39.489167
	weather$geo_from_source <- TRUE

	carobiner::write_files(path, meta, d, wth=weather)
}
