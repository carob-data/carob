# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily sheet BaseDeDatos. Dictionary: max_temp and min_temp are °C, precip is mm, radiation is W m^-2.
# The dictionary "Other name" cells for rain and radiation are swapped relative to the variable-name and unit cells. Columns are taken from the data header and the unit on the matching variable name.
# Place is La Molina, Lima, Peru, from the dataset geographic coverage. No coordinates are published.

## ISSUES
# Radiation values are about 20-30. That is low for a daily mean irradiance in W m^-2. The dictionary unit is still W m^-2, so the numbers are not rescaled.
# La Molina needs georeferencing. No longitude or latitude in the file or the geographic coverage.
# write_files: missing variables: carob_contributor; geo_from_source, latitude, longitude.

## NOT PROCESSED
# Year, Month, Day: used only to confirm measure_date.

carob_script <- function(path) {

"
Dataset for: Meteorological data at La Molina CIP experimental station from 1975 to 2001

In the present dataset, daily weather data at La Molina CIP Experimental Station from January 1975 to September 2001 is available. The meteorological variables are the following: Maximum and minimum Air temperature (°C), solar radiation (W m^-2) and rain (mm).
"

	uri <- "doi:10.21223/EQAC70"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "01_LaMolina_Lima.xls"]
	r <- carobiner::read.excel(f, sheet="BaseDeDatos")

	wth <- data.frame(
		station_id = as.character(r$station_code),
		date = as.character(as.Date(r$measure_date)),
		tmax = as.numeric(r$max_temp),
		tmin = as.numeric(r$min_temp),
		prec = as.numeric(r$precip),
		irad = as.numeric(r$radiation)
	)
	wth <- wth[!is.na(wth$date), ]

	d <- data.frame(
		station_id = "CIPLM",
		station_name = "La Molina CIP experimental station",
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
