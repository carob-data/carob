# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily sheet BaseDeDatos. Same dictionary layout as the La Molina 1975-2001 file: temperature °C, rain mm, radiation W m^-2.
# Place is Chanchamayo - San Ramon, Junin, Peru, from the dataset geographic coverage. No coordinates are published.

## ISSUES
# Radiation values follow the dictionary unit W m^-2. Daily numbers near 20-30 are low for a mean irradiance and are not rescaled.
# San Ramon needs georeferencing.
# write_files: missing variables: carob_contributor; geo_from_source, latitude, longitude.

## NOT PROCESSED
# Year, Month, Day: used only to confirm measure_date.

carob_script <- function(path) {

"
Dataset for: Meteorological data at San Ramon CIP experimental station from 1974 to 2002

In the present dataset, daily weather data at San Ramon CIP Experimental Station from January 1974 to December 2002 is available. The meteorological variables are the following: Maximum and minimum Air temperature (°C), solar radiation (W m^-2) and rain (mm).
"

	uri <- "doi:10.21223/PD9BAY"
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
		carob_effort = 0.25
	)

	f <- ff[basename(ff) == "01_SanRamon_Junin.xls"]
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
		station_id = "CIPSR",
		station_name = "San Ramon CIP experimental station",
		country = "Peru",
		adm1 = "Junin",
		location = "San Ramon",
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
