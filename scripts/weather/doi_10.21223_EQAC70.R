# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily sheet BaseDeDatos. Dictionary: max_temp and min_temp are °C, precip is mm.
# Daily radiation values (1.5-34, median 19.8) are clearly MJ/m2/day despite the dictionary's "W m^-2" label; stored as srad in kJ/m2 (* 1000).
# The dictionary "Other name" cells for rain and radiation are swapped relative to the variable-name and unit cells. Columns are taken from the data header.
# Coordinates and elevation for La Molina CIP station are taken from sister workbook doi:10.21223/0X6T54 (geo_from_source = FALSE).

## ISSUES
# Dictionary mislabels daily radiation (MJ/m2/day) as W m^-2; converted to srad (kJ/m2).

## NOT PROCESSED
# Year, Month, Day: used only to confirm measure_date.
# 02_Data_dictionary.xlsx: documentation or a codebook, not an observation table.

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
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-11-26",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 90,
		carob_effort = 1/12
	)

	f <- ff[basename(ff) == "01_LaMolina_Lima.xls"]
	r <- carobiner::read.excel(f, sheet="BaseDeDatos", na=c("NULL", "null", "NA", ""))

	wth <- data.frame(
		station_id = as.character(r$station_code),
		date = as.character(as.Date(r$measure_date)),
		tmax = as.numeric(r$max_temp),
		tmin = as.numeric(r$min_temp),
		prec = as.numeric(r$precip),
		# Daily values (1.5-34, median 19.8) are MJ/m2/day -> kJ/m2
		srad = as.numeric(r$radiation) * 1000
	)
	wth <- wth[!is.na(wth$date), ]

	d <- data.frame(
		station_id = "CIPLM",
		station_name = "La Molina CIP experimental station",
		country = "Peru",
		adm1 = "Lima",
		adm2 = "Lima",
		adm3 = "La Molina",
		location = "La Molina",
		latitude = -12.07776,
		longitude = -76.94875,
		elevation = 239,
		geo_from_source = FALSE,
		geo_source = "CIP La Molina station workbook (doi:10.21223/0X6T54)",
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
