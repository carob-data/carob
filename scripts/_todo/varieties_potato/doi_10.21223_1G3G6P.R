# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot rows are the Fieldbook sheets of PTYL200606_MPA.xls (Mparambo) and PTYL200607_MUG.xls (Mugerero).
# TTYNA and MTYNA are t/ha in 04_Data_dictionary.xlsx (TbYldNAj_Cp_tha, MkYldNAj_Cp_tha). Stored as kg/ha.
# Site, coordinates, elevation, begin date, end date, design, and planting density are the Minimal and Installation sheets.
# Daily weather is Weather_daily (TMIN, TMAX, TMEAN). No rain column in that sheet.

## ISSUES
# The abstract says "Completely Randomized Block Design". The Installation sheet says "Randomized Complete Block Design (RCBD)". The sheet text is used.
# Mparambo Crop_management planting and harvest dates repeat the Mugerero dates (2006-07-11 and 2006-10-17). Mparambo Minimal begin and end are 2006-06-29 and 2006-10-11. Minimal dates are used.
# Mugerero Weather_daily includes 2005 as well as 2006. Rows are kept as recorded.
# No fertilizer or irrigation in Crop_management. Sites are not labeled on-farm or station.
# Mugerero longitude is published as 29.3 (one decimal).
# write_files: ; yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer.

## NOT PROCESSED
# 01_data_yield_Mparambo_2006.xlsx and 02_data_yield_Mugerero_2006.xlsx: flat exports of the same Fieldbook rows.
# Fieldbook NTP, NPE, NPH, PPH, NNoMTP, NMTP, TNTP, NoMTWP, TTWP, TTWPL, MTWP, MTWPL, ATW, ATMW, TTYA: per-plot counts or weights, or the stand-adjusted yield. Plot area is not in the file. Trial planting density is kept.
# AVDM: not defined in the dictionary sheet.
# Soil_analysis, Hobo_data, Weather_data, Weather_Original, Weather_monthly, Summary by clone, Charts, Ranks by Clone: summaries, logger dumps, or soil tables not mapped here.
# Weather_daily n_sample and TSD: sample size and temperature standard deviation, not a weather variable in the vocabulary.
# Material List Control, pedigree columns used only when a parent code is present are otherwise blank.

## NEW VARIABLES
# none

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones for Performance in Burundi, 2006

An evaluation of 11 potato clones was carried out in the cities of Mugerero and Mparambo, Burundi, in 2006. The study utilized a Completely Randomized Block Design to assess the clones' performance under local conditions.
"

	uri <- "doi:10.21223/1G3G6P"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Randomized complete block design",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.5
	)

	kv <- function(f, sheet) {
		x <- carobiner::read.excel(f, sheet=sheet)
		stats::setNames(as.character(x$Value), trimws(as.character(x$Factor)))
	}

	one_site <- function(f) {
		info <- kv(f, "Minimal")
		inst <- kv(f, "Installation")
		r <- carobiner::read.excel(f, sheet="Fieldbook")
		ml <- carobiner::read.excel(f, sheet="Material List")
		name <- trimws(as.character(ml[["Clone or variety name"]]))
		code <- trimws(as.character(ml[["Institutional number"]]))
		name[!nzchar(name) | is.na(name)] <- NA_character_
		m <- match(trimws(as.character(r$INSTN)), code)
		variety <- trimws(as.character(r$INSTN))
		ok <- !is.na(m) & !is.na(name[m])
		variety[ok] <- name[m[ok]]
		female <- rep(NA_character_, nrow(r))
		male <- female
		if (!is.na(m[1]) || any(!is.na(m))) {
			female[!is.na(m)] <- trimws(as.character(ml[["Female Institutional number"]][m[!is.na(m)]]))
			male[!is.na(m)] <- trimws(as.character(ml[["Male Institutional number"]][m[!is.na(m)]]))
		}
		female[!nzchar(female)] <- NA_character_
		male[!nzchar(male)] <- NA_character_
		loc <- info[["Locality"]]
		d <- data.frame(
			plot_id = as.character(r$PLOT),
			rep = as.integer(r$REP),
			variety_code = trimws(as.character(r$INSTN)),
			variety = variety,
			treatment = variety,
			female_parent = female,
			male_parent = male,
			yield = as.numeric(r$TTYNA) * 1000,
			yield_marketable = as.numeric(r$MTYNA) * 1000,
			country = info[["Country"]],
			adm1 = info[["Admin1"]],
			adm2 = info[["Admin2"]],
			location = loc,
			elevation = as.numeric(info[["Elevation"]]),
			latitude = as.numeric(info[["Latitude"]]),
			longitude = as.numeric(info[["Longitude"]]),
			planting_date = as.character(as.Date(info[["Begin date"]])),
			harvest_date = as.character(as.Date(info[["End date"]])),
			plant_density = as.numeric(inst[["Planting density (plants/Ha)"]]),
			trial_id = paste(loc, substr(info[["Begin date"]], 1, 4))
		)
		w <- carobiner::read.excel(f, sheet="Weather_daily")
		wth <- data.frame(
			country = as.character(w$Country),
			adm1 = as.character(w$Department),
			location = as.character(w$Locality),
			date = sprintf("%04d-%02d-%02d", as.integer(w$Year), as.integer(w$Month), as.integer(w$Day)),
			tmin = as.numeric(w$TMIN),
			tmax = as.numeric(w$TMAX),
			temp = as.numeric(w$TMEAN),
			latitude = d$latitude[1],
			longitude = d$longitude[1],
			geo_from_source = TRUE
		)
		list(d=d, wth=wth)
	}

	f1 <- ff[basename(ff) == "PTYL200606_MPA.xls"]
	f2 <- ff[basename(ff) == "PTYL200607_MUG.xls"]
	a <- one_site(f1)
	b <- one_site(f2)
	d <- carobiner::bindr(a$d, b$d)
	wth <- carobiner::bindr(a$wth, b$wth)

	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- TRUE
	# Published coordinates: Mparambo to 0.0001 degree, Mugerero longitude to 0.1 degree.
	d$geo_uncertainty <- ifelse(d$location == "Mugerero", 12000, 20)
	d$geo_source <- "HIDAP Minimal sheet"

	wth <- wth[!is.na(wth$date), ]
	wth <- unique(wth)

	carobiner::write_files(path, meta, d, wth=wth)
}
