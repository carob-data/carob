# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily weather at CIP La Victoria (station code CIPHYO), Junin, Peru, January 1976 to December 2000.
# The description states maximum and minimum temperature in C and rain in mm.
# Daily solar radiation values (2.1-32.8, median 21.8) are clearly MJ/m2/day despite the dictionary's "W/m2" label; stored as srad in kJ/m2 (* 1000).
# The dictionary row for radiation uses other-name "precip", and the rain row uses other-name "radiation". The data column names are used: precip is rain, radiation is the radiation column.
# Coordinates and elevation for La Victoria CIP station are taken from sister workbook doi:10.21223/BJOIJL (geo_from_source = FALSE).

## ISSUES
# Dictionary mislabels daily radiation (MJ/m2/day) as W m^-2; converted to srad (kJ/m2).

## NOT PROCESSED
# id, Year, Month, Day: the same day as measure_date.
# 02_Data_dictionary.xlsx: documentation or a codebook, not an observation table.


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
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-11-26",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 90,
		carob_effort = 1/12
	)

	f <- ff[basename(ff) == "01_LaVictoria_Junin.xls"]
	r <- carobiner::read.excel(f, na=c("NULL", "null", "NA", ""))

	d <- data.frame(
		station_id = "CIPHYO",
		station_name = "La Victoria",
		country = "Peru",
		adm1 = "Junin",
		adm2 = "Huancayo",
		adm3 = "San Agustin",
		location = "La Victoria",
		latitude = -12.02929,
		longitude = -75.24441,
		elevation = 3210,
		geo_from_source = FALSE,
		geo_source = "CIP La Victoria station workbook (doi:10.21223/BJOIJL)",
		is_survey = FALSE,
		on_farm = FALSE
	)

	wth <- data.frame(
		station_id = "CIPHYO",
		date = as.character(as.Date(r$measure_date)),
		tmax = as.numeric(r$max_temp),
		tmin = as.numeric(r$min_temp),
		prec = as.numeric(r$precip),
		# Daily values (2.1-32.8, median 21.8) are MJ/m2/day -> kJ/m2
		srad = as.numeric(r$radiation) * 1000
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
