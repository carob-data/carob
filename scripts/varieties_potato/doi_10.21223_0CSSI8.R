# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The description says 16 clones; the Fieldbook has 21 (63 plots).
# 2. The Minimal sheet "End date" (2011-01-12) is the date of vine killing; harvest was on 2011-01-27 (Crop_management).

## NOTES
# 1. All plants of each plot (20 planted, 2.4 m2) were harvested (NPH = 20); yield is TTWP / 2.4 m2.
# 2. Daily weather (rainfall, minimum and maximum temperature and humidity) from the "Weather_data" sheet.
# 3. No fertilizer or irrigation information (winter crop in Gujarat; probably irrigated, not stated).
#    Soil_analysis is an empty template. Collaborator: Potato Research Station, Deesa.
# 4. Sheets "2010-11" and "serial wise clone name" are summaries and are not used.

carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in India (2010)

Trials featuring heat-tolerant potato varieties were conducted in South West Central Asia (SWCA),  in Ladol, India, during the year 2010. In this study, 16 potato clones were evaluated using a randomized complete block design (RCBD) with three repetitions
"

	uri <- "doi:10.21223/0CSSI8"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = "Enhanced food and income security in SWCA through potato varieties with improved tolerance to abiotic stress (07.7860.5-001.00)",
		# design as reported in the "Installation" sheet
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Cursor Grok 4.7; Claude Opus 5.5",
		carob_date = "2026-11-09",
		carob_completion = 85,
		carob_effort = 1
	)

	## standard CIP fieldbook (sheets "Minimal", "Installation" and "Fieldbook"),
	## processed with the shared function; variety names and parents come from
	## the material list (text file)
	process <- carobiner::get_function("process_cip_lbvars", path, group)

	f <- ff[basename(ff) == "PTYL201010_LADOL.xls"]
	d <- process(f)
	## harvest date from the Crop_management sheet (ISSUES 2)
	d$harvest_date <- "2011-01-27"
	d$plot_area <- 2.4
	d$adm3 <- NULL

	## the material list has Windows-1252 characters (e.g. "Craig's Defiance",
	## non-breaking spaces); convert to UTF-8
	i <- sapply(d, is.character)
	d[i] <- lapply(d[i], function(x) {
		x <- iconv(x, "CP1252", "UTF-8")
		x <- gsub("\u00a0", " ", x)
		trimws(gsub("\u2019", "'", x))
	})

	## variety names in upper case in the material list
	d$variety[d$variety == "REICHE"] <- "Reiche"
	d$variety[d$variety == "BASADRE"] <- "Basadre"

	## irrigation and fertilizer not reported in the fieldbook; no dry matter.
	## The shared function adds empty columns for these; they are removed
	d$irrigated <- NA
	d$yield_moisture <- d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- NULL

	## daily weather at the site (NOTES 2)
	w <- carobiner::read.excel(f, sheet="Weather_data")
	wth <- data.frame(
		country = d$country[1],
		location = d$location[1],
		latitude = d$latitude[1],
		longitude = d$longitude[1],
		date = as.character(as.Date(w[["Date of weather observation"]])),
		prec = as.numeric(w[["Rainfall (mm)"]]),
		tmin = as.numeric(w[[grep("^Minimum temperature", names(w))]]),
		tmax = as.numeric(w[[grep("^Maximum temperature", names(w))]]),
		rhmn = as.numeric(w[[grep("^Humidity \\(%\\) Min", names(w))]]),
		rhmx = as.numeric(w[[grep("^Humidity \\(%\\) Max", names(w))]])
	)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
