# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily weather at CIP La Victoria (station code CIPHYO), Junin, Peru, January 1976 to December 2000.
# The description states maximum and minimum temperature in C, solar radiation in W/m2, and rain in mm.
# The dictionary row for radiation uses other-name "precip", and the rain row uses other-name "radiation". The data column names are used: precip is rain, radiation is the radiation column.

## ISSUES
# No coordinates in the file or the bounding box. The station still needs a georeference.
# Radiation values are about 2 to 33. That is low for a daily mean in W/m2 and closer to MJ/m2/day. The description says W/m2, so the numbers are stored as irad without conversion.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude.

## NOT PROCESSED
# id, Year, Month, Day: the same day as measure_date.


carob_script <- function(path) {

"
Dataset for: Meteorological data at La Victoria CIP experimental station from 1976 to 2000

In the present dataset, daily weather data at La Victoria Experimental Station from January 1976 to December 2000 is available. The meteorological variables are the following: Maximum and minimum Air temperature (°C), solar radiation (W m^-2) and rain (mm).
"

	uri <- "doi:10.21223/LLG1HO"
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
		carob_completion = 70,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "01_LaVictoria_Junin.xls"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		station_id = "CIPHYO",
		station_name = "La Victoria",
		country = "Peru",
		adm1 = "Junin",
		location = "La Victoria",
		is_survey = FALSE,
		on_farm = FALSE,
		stringsAsFactors = FALSE
	)

	wth <- data.frame(
		station_id = "CIPHYO",
		date = as.character(as.Date(r$measure_date)),
		tmax = as.numeric(r$max_temp),
		tmin = as.numeric(r$min_temp),
		prec = as.numeric(r$precip),
		irad = as.numeric(r$radiation),
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
