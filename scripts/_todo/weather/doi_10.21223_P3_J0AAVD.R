# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily maximum and minimum temperature at weather stations in the Mantaro valley, Junín, Peru, 2011 to 2015.
# On every day that both values are present, the column labeled MaxTemp is colder than the column labeled MinTemp. The labels are reversed here: the cold column is tmin and the warm column is tmax.
# 9999 is missing.
# Coordinates, elevation, and locality names are on the installation sheet. The dataset spells ElTambo and the installation sheet spells El Tambo. The match ignores spaces.
# Farmer names are not stored.

## ISSUES
# No rainfall.

## NOT PROCESSED
# Name of farmer.
# UniDate. It is the same day as Year, Month, and Day (40691 is 2011-05-28).
# DataDictionary.txt: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Weather stations in the Mantaro valley, Peru
"

	uri <- "doi:10.21223/P3/J0AAVD"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "WeatherStationsInMantaro(Instalation).xlsx"]
	r <- carobiner::read.excel(f, sheet="Coordinates", col_types="text")
	loc <- tolower(gsub(" ", "", trimws(as.character(r$Locality))))
	d <- data.frame(
		station_id = trimws(as.character(r$Code)),
		station_name = trimws(as.character(r$Locality)),
		country = "Peru",
		adm1 = "Junín",
		location = trimws(as.character(r$Locality)),
		latitude = as.numeric(r$Latitud),
		longitude = as.numeric(r$Longitud),
		elevation = as.numeric(r$Altitud),
		geo_from_source = TRUE,
		on_farm = TRUE,
		is_survey = FALSE
	)

	f1 <- ff[basename(ff) == "WeatherStationsInMantaro(Dataset).xlsx"]
	r2 <- carobiner::read.excel(f1, col_types="text")
	# The sheet column MaxTemp is the cold value and MinTemp is the warm value.
	cold <- as.numeric(r2$MaxTemp)
	warm <- as.numeric(r2$MinTemp)
	cold[cold == 9999] <- NA_real_
	warm[warm == 9999] <- NA_real_
	key <- tolower(gsub(" ", "", trimws(as.character(r2$Locality))))
	wth <- data.frame(
		station_id = d$station_id[match(key, loc)],
		date = sprintf("%04d-%02d-%02d", as.integer(r2$Year), as.integer(r2$Month), as.integer(r2$Day)),
		tmin = cold,
		tmax = warm
	)
	wth <- wth[!is.na(wth$station_id) & !is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
