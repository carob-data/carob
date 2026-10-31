# R script for "carob"
# license: GPL (>=3)

## NOTES
# Phenology trial of maize varieties at stations in Tanzania. There is no grain yield in the file.
# Flowering is one date labeled as tasseling and silking, stored as flowering_days.
# Some flowering and maturity values are fractional Excel serials. Days are the serial difference, rounded to the nearest day.
# Seven rows have country or variety equal to 0 and are not plots.
# The description mentions a daily temperature file. The download contains only this workbook.

## ISSUES
# write_files: missing variables: yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variable: sowing_order_ (order?). out of bounds: emergence_days (-11, 190). NA detected: latitude, longitude.
# One Monduli juu emergence date is before planting. Miwaleni emergence is 128 to 190 days. Those values are kept.
# Coordinate cells equal to 0 are missing. Several stations have no coordinates.

## NEW VARIABLES
# sowing_order_: first, second, or third planting date at a station.

## NOT PROCESSED
# Experiment zone.
# Plot size.
# Data-history sheet.


carob_script <- function(path) {

"
Tanzania VT TAMASA MetaData

Experiments at multiple locations in Tanzania (800 to 1500m) to calibrate development or phenology of maize varieties. Locations are Igeri, Minja, Mitalula, Mlaaleni, Monduli, Selian, Seatondale, Suluti, Uyole, W. Kilimanjaro. Observations include dates of emergence, tassel, silking and maturity.
"

	uri <- "hdl:11529/10548241"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;sowing_order_",
		response_vars = "emergence_days;flowering_days;maturity_days",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[grepl("\\.xlsx$", basename(ff))]
	r <- carobiner::read.excel(f, sheet="Raw data", col_types="text")
	variety <- trimws(as.character(r[[10]]))
	country <- trimws(as.character(r[[2]]))
	sow <- trimws(as.character(r[[11]]))
	keep <- !is.na(variety) & !variety %in% c("", "0", "NA")
	keep <- keep & !is.na(country) & !country %in% c("", "0", "NA")
	r <- r[keep, ]
	variety <- variety[keep]
	sow <- sow[keep]
	sow[sow == "First PLNdat"] <- "first"
	sow[sow == "Second PLNdat"] <- "second"
	sow[sow == "Third PLNdat"] <- "third"
	sow[sow %in% c("", "0", "NA")] <- NA_character_

	num <- function(x) {
		y <- trimws(as.character(x))
		y[is.na(y) | y %in% c("", "NA", "0")] <- NA_character_
		as.numeric(y)
	}
	day <- function(x, origin) {
		d <- num(x) - origin
		round(d)
	}
	origin <- num(r[[12]])
	station <- trimws(as.character(r[[6]]))
	ok <- !is.na(station) & nzchar(station)
	station[ok] <- carobiner::fix_name(station[ok], "title")

	d <- data.frame(
		country = "Tanzania",
		adm1 = carobiner::fix_name(as.character(r[[4]]), "title"),
		adm2 = carobiner::fix_name(as.character(r[[5]]), "title"),
		location = station,
		latitude = num(r[[7]]),
		longitude = num(r[[8]]),
		elevation = num(r[[9]]),
		trial_id = paste(station, "2015"),
		plot_id = trimws(as.character(r[[14]])),
		rep = as.integer(num(r[[13]])),
		crop = "maize",
		variety = variety,
		treatment = paste(variety, sow),
		sowing_order_ = sow,
		planting_date = as.character(as.Date(origin, origin="1899-12-30")),
		harvest_date = as.character(as.Date(num(r[[18]]), origin="1899-12-30")),
		emergence_days = day(r[[16]], origin),
		flowering_days = day(r[[17]], origin),
		maturity_days = day(r[[18]], origin),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = TRUE
	)
	# A zero in the source is used as a missing flag for coordinates and elevation.
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)

	carobiner::write_files(path, meta, d)
}
