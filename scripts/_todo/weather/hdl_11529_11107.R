# R script for "carob"
# license: GPL (>=3)

## NOTES
# Daily weather at the CENEB station, CIMMYT Obregon, during two wheat seasons. Geographic coverage is Sonora, Mexico.
# The column is labeled Evap (mm). The dataset description calls it evapotranspiration, so it is stored as ETo.
# Radiation is stored as irad from the W/m2 column. The MJ/m2 column is stored as srad in kJ/m2.
# One day in 2016-17 has no W/m2 value and MJ/m2 stored as 0. That pair is missing.

## ISSUES
# No coordinates in the files or the geographic coverage.
# write_files: missing variables: latitude, longitude.

## NOT PROCESSED
# None. Both seasons are included.


carob_script <- function(path) {

"
Climatic data reported during the wheat cycle in Obregon (IWYP Hub) in 2015-2016 and 2016-2017. Variables reported: average, maximum and minimum daily temperature (°C), precipitation (mm/day), daily relative humidity (%), evapotranspiration (mm/day) and average solar radiation per day (W/m2 and MJ/m2). The data presented in this study was collected from the meteorological station in the 910 section of the CIMMYT experimental fields in Obregon, Sonora (Mexico). This station is run by the Geographical Information Systems Unit at CIMMYT, lead by Kai Sonder.
"

	uri <- "hdl:11529/11107"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "IWYP",
		design = NA,
		data_type = "compilation",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 90,
		carob_effort = 0.3
	)

	read_cycle <- function(f, sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, skip=3)
		year <- as.integer(r$Year)
		month <- as.integer(r$Month)
		day <- as.integer(r$Day)
		irad <- as.numeric(r$`Radiation  (W/m2)`)
		srad <- as.numeric(r$`MJ/m2`) * 1000
		srad[is.na(irad)] <- NA_real_
		data.frame(
			station_id = "CENEB",
			date = sprintf("%04d-%02d-%02d", year, month, day),
			temp = as.numeric(r$`AvgTemp (°C)`),
			tmax = as.numeric(r$`MaxTemp (°C)`),
			tmin = as.numeric(r$`MinTemp(°C)`),
			prec = as.numeric(r$`Rain (mm)`),
			rhum = as.numeric(r$`RH (%)`),
			ETo = as.numeric(r$`Evap (mm)`),
			irad = irad,
			srad = srad
		)
	}

	f1 <- ff[basename(ff) == "Climatic Data Y15-16.xlsx"]
	f2 <- ff[basename(ff) == "Climatic Data Y16-17.xlsx"]
	w1 <- read_cycle(f1, "Climatic Data")
	w2 <- read_cycle(f2, "Y16-17")
	wth <- carobiner::bindr(w1, w2)
	wth <- wth[!is.na(wth$date) & nzchar(wth$date), ]
	wth <- wth[order(wth$date), ]

	d <- data.frame(
		station_id = "CENEB",
		station_name = "CENEB",
		country = "Mexico",
		adm1 = "Sonora",
		location = "Obregon",
		geo_from_source = FALSE,
		is_survey = FALSE,
		on_farm = FALSE
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
