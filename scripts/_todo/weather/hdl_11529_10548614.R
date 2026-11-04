# R script for "carob"
# license: GPL (>=3)

## NOTES
# District wheat yield for the irrigated Cajeme district, Sonora, and daily weather at the CENEB (formerly CIANO) station in the Yaqui Valley.
# Country and state are the geographic coverage. The title names the Yaqui Valley and says the wheat was irrigated and on-farm. The yield file names Cajeme District.
# Yield is kg/ha of grain received by the district authority. Harvest area is stored in hectares (the file is in 1000 ha).
# The yield sheet also has 1951-1959. Those years are included. The average row and the note row are not.
# Daily radiation is MJ/m2/day and is stored as srad in kJ/m2. Day of year is converted with the calendar year, including day 366 in leap years.
# One day in 2013 has solar radiation stored as question marks. That value is missing. Temperature for that day is kept.

## ISSUES
# write_files: missing variables: latitude, longitude. unknown variables: harvest_year_, district_harvest_area_ha_. contributor:

## NEW VARIABLES
# harvest_year_: harvest year of the district wheat record.
# district_harvest_area_ha_: harvested wheat area of the Cajeme district, in hectares.

## NOT PROCESSED
# The monthly workbook. It repeats the daily series as November-April means and is the only source for November 1959 to April 1967.
# Frost days, January-March averages, and the text columns that name the weather source.


carob_script <- function(path) {

"
(prepared R.A.(Tony) Fischer 14 August 2021)
This dataset contains three data files that were obtained and/or constructed an analysis of 60 years' yield progress with irrigated wheat yield in northwest Mexico:
1. Wheat harvested area and average yield each harvest year for the irrigated Cajeme District of Sonora State, Mexico, from 1960 to 2019.
2. Monthly average Tmax, Tmin, precipitation total and frost days from November 1959 to April 2019.
3. Daily solar radiation (Rs), Tmax and Tmin from November 1967 to April 2019.

The wheat area and yield data were collated by the Cajeme District grain receival authority in Cd Obregon. The weather data come from the CIMMYT station at CIANO/CENEB, centrally located in the Yaqui Valley, with earlier and gap-filled values from other Sonora sources named in the files.
"

	uri <- "hdl:11529/10548614"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = "doi:10.1016/j.fcr.2021.108331",
		project = NA,
		design = NA,
		data_type = "compilation",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.5
	)

	num <- function(z) {
		z <- trimws(as.character(z))
		z[z %in% c("", "na", "NA", "n/a")] <- NA_character_
		z[!is.na(z) & grepl("[[:alpha:]?]", z)] <- NA_character_
		as.numeric(z)
	}

	f1 <- ff[grepl("^1\\.", basename(ff))]
	r <- carobiner::read.excel(f1, sheet="Sheet1", col_names=FALSE)
	year <- num(r[[4]])
	keep <- !is.na(year) & year >= 1900 & year <= 2100
	d <- data.frame(
		trial_id = paste0("Cajeme-", year[keep]),
		country = "Mexico",
		adm1 = "Sonora",
		location = "Cajeme",
		crop = "wheat",
		yield = num(r[[5]])[keep],
		yield_part = "grain",
		harvest_year_ = year[keep],
		district_harvest_area_ha_ = num(r[[6]])[keep] * 1000,
		irrigated = TRUE,
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	f3 <- ff[grepl("^3\\.", basename(ff))]
	w <- carobiner::read.excel(f3, sheet="Sheet1", col_names=FALSE)
	yr <- num(w[[6]])
	day <- num(w[[7]])
	ok <- !is.na(yr) & !is.na(day)
	when <- as.Date(sprintf("%d %d", yr[ok], day[ok]), format="%Y %j")
	wth <- data.frame(
		station_name = "CENEB",
		date = as.character(when),
		tmax = num(w[[9]])[ok],
		tmin = num(w[[10]])[ok],
		srad = num(w[[8]])[ok] * 1000
	)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
