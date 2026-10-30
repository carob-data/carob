# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot-level fieldbooks for five Rwanda sites. Bugeshi and Muhoza keep site details on a sheet named Minimal. Burera, Gihonga, and Nyabihu keep the same key-value block on a Summary sheet.
# TTWP is stored as yield. The fieldbook does not state the unit, and the values are about 0 to 13, so they were not multiplied by 1000.
# The summary workbook has site means of several hundred. That is a different scale, so those means were not put in yield.
# Coordinates in the site sheets are degrees and decimal minutes, with S and E. South is stored as a negative latitude. Elevation text such as 2159m is stored as meters.

## ISSUES
# Bugeshi has no latitude or longitude. That site still needs georeferencing.
# Muhoza Admin3 is written MuhozaSector in the sheet.
# write_files: missing carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. low yield (tons not kg?): potato. NA detected: latitude, longitude (Bugeshi). TTWP was not multiplied because the fieldbook does not state the unit.

## NOT PROCESSED
# 4034_Summary_yield2014.xlsx: site means on a different numeric scale from fieldbook TTWP, and the unit is not stated.
# 4034_Summary_minerals2013_2014.xlsx: dry-matter and Fe and Zn means. The unit columns are not a clean unit label.
# Map sheets and the two dictionaries.

## NEW VARIABLES
# None beyond yield.


carob_script <- function(path) {

"
Dataset for: Late blight resistant varieties adapted to humid highland sub-tropical agro-ecologies in SSA

Yield data of nine selected clones in Rwanda 2014B-2016A
"

	uri <- "doi:10.21223/B2TD15"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.4
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	as_date <- function(x) {
		x <- chr(x)
		if (is.na(x)) return(NA_character_)
		if (grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$", x)) {
			return(as.character(as.Date(x, "%d-%m-%Y")))
		}
		if (grepl("^[0-9]+$", x)) {
			return(as.character(as.Date(as.numeric(x), origin="1899-12-30")))
		}
		NA_character_
	}
	dms <- function(x) {
		x <- chr(x)
		if (is.na(x)) return(NA_real_)
		x <- gsub("°", " ", x, fixed=TRUE)
		x <- gsub("'", "", x, fixed=TRUE)
		p <- strsplit(trimws(x), "\\s+")[[1]]
		hem <- substr(p[1], 1, 1)
		if (grepl("^[NSEW][0-9]", p[1])) {
			deg <- as.numeric(substr(p[1], 2, nchar(p[1])))
			minutes <- as.numeric(p[2])
		} else {
			deg <- as.numeric(p[2])
			minutes <- as.numeric(p[3])
		}
		v <- deg + minutes / 60
		if (hem %in% c("S", "W")) v <- -v
		v
	}
	elev <- function(x) {
		x <- chr(x)
		if (is.na(x)) return(NA_real_)
		x <- trimws(sub("[mM].*$", "", x))
		if (!grepl("^[0-9.]+$", x)) return(NA_real_)
		as.numeric(x)
	}
	books <- data.frame(
		file = c("4034_Bugeshi.xlsx", "4034_Burera.xlsx", "4034_Gihonga.xlsx", "4034_Muhoza.xlsx", "4034_Nyabihu.xlsx"),
		meta = c("Minimal", "Burera Summary", "Gihonga Summary", "Minimal", "Nyabihu Summary"),
		fieldbook = c("Bugeshi Fieldbook", "Burera Fieldbook", "Gihonga Fieldbook", "Muhoza Fieldbook", "Nyabihu Fieldbook")
	)
	parts <- lapply(seq_len(nrow(books)), function(i) {
		f <- ff[basename(ff) == books$file[i]]
		mn <- carobiner::read.excel(f, sheet=books$meta[i])
		val <- function(key) {
			hit <- which(mn[[1]] == key)
			if (!length(hit)) NA_character_ else chr(mn[[2]][hit[1]])
		}
		r <- carobiner::read.excel(f, sheet=books$fieldbook[i])
		data.frame(
			variety = chr(r$INSTN),
			treatment = chr(r$INSTN),
			rep = as.integer(num(r$REP)),
			yield = num(r$TTWP),
			country = val("Country"),
			adm1 = val("Admin1"),
			adm2 = val("Admin2"),
			adm3 = val("Admin3"),
			location = val("Locality"),
			elevation = elev(val("Elevation")),
			latitude = dms(val("Latitude")),
			longitude = dms(val("Longitude")),
			planting_date = as_date(val("Planting_date")),
			harvest_date = as_date(val("Harvest date"))
		)
	})
	d <- do.call(rbind, parts)
	d <- d[!is.na(d$variety), ]
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$trial_id <- paste(d$location, d$planting_date)
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)

	carobiner::write_files(path, meta, d)
}
