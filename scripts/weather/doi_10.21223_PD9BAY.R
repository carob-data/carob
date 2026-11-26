# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily sheet BaseDeDatos. Same dictionary layout as the La Molina 1975-2001 file: temperature °C, rain mm.
# Daily radiation values (2.1-33.7, median 19.2) are clearly MJ/m2/day despite the dictionary's "W m^-2" label; stored as srad in kJ/m2 (* 1000).
# Coordinates and elevation for San Ramon CIP station are taken from sister workbook doi:10.21223/USHWLU (geo_from_source = FALSE).

## ISSUES
# Dictionary mislabels daily radiation (MJ/m2/day) as W m^-2; converted to srad (kJ/m2).

## NOT PROCESSED
# Year, Month, Day: used only to confirm measure_date.
# 02_Data_dictionary.xlsx: documentation or a codebook, not an observation table.

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
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-11-26",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 90,
		carob_effort = 1/12
	)

	f <- ff[basename(ff) == "01_SanRamon_Junin.xls"]
	r <- carobiner::read.excel(f, sheet="BaseDeDatos", na=c("NULL", "null", "NA", ""))

	wth <- data.frame(
		station_id = as.character(r$station_code),
		date = as.character(as.Date(r$measure_date)),
		tmax = as.numeric(r$max_temp),
		tmin = as.numeric(r$min_temp),
		prec = as.numeric(r$precip),
		# Daily values (2.1-33.7, median 19.2) are MJ/m2/day -> kJ/m2
		srad = as.numeric(r$radiation) * 1000
	)
	wth <- wth[!is.na(wth$date), ]

	d <- data.frame(
		station_id = "CIPSR",
		station_name = "San Ramon CIP experimental station",
		country = "Peru",
		adm1 = "Junin",
		adm2 = "Chanchamayo",
		adm3 = "San Ramon",
		location = "San Ramon",
		latitude = -11.1275,
		longitude = -75.3601,
		elevation = 847,
		geo_from_source = FALSE,
		geo_source = "CIP San Ramon station workbook (doi:10.21223/USHWLU)",
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
