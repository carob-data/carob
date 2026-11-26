# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot-level fieldbooks for five Rwanda sites. Bugeshi and Muhoza keep site details on a sheet named Minimal. Burera, Gihonga, and Nyabihu keep the same key-value block on a Summary sheet.
# 4034_DataDictionary defines TTWP as total tuber weight per plot (kg/plot). Four sites report Plot size on Minimal/Summary (Burera 9.6 m2, Gihonga 8.4 m2, Muhoza 8.4 m2, Nyabihu 2.1 m2), so yield (kg/ha) = TTWP * 10000 / plot_area. Bugeshi has no plot size on Minimal, so yield is NA there and raw kg/plot is kept in ttwp_.
# Coordinates in the site sheets are degrees and decimal minutes, with S and E. South is stored as a negative latitude. Elevation text such as 2159m is stored as meters. Bugeshi coordinates (Rubavu district) are added from gazetteer (geo_from_source = FALSE).

## ISSUES
# Nyabihu Summary lists Plot size = 2.1, number row per plot = 3, number tuber per plot = 30. Confirm whether 2.1 m2 is the full plot area or one row (3 * 2.1 = 6.3 m2); using 2.1 m2 yields up to 87 t/ha.
# Bugeshi has no Plot size in Minimal, so yield is NA for Bugeshi (raw kg/plot stored in ttwp_).
# No fertilizer rates, irrigation, or yield moisture reported.

## NOT PROCESSED
# 4034_Summary_yield2014.xlsx: site means on a different numeric scale from fieldbook TTWP, and the unit is not stated.
# 4034_Summary_minerals2013_2014.xlsx: dry-matter and Fe and Zn means. The unit columns are not a clean unit label.
# Map sheets and the two dictionaries.
# 4034_DataDictionary_ElementDescription.xlsx: documentation or a codebook, not an observation table.
# 4034_DataDictionary_UniqueIdentifier.xlsx: documentation or a codebook, not an observation table.

## NEW VARIABLES
# ttwp_: total tuber weight per plot (kg/plot) as recorded in the fieldbook.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 60,
		carob_effort = 0
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
		r1 <- carobiner::read.excel(f, sheet=books$meta[i])
		val <- function(key) {
			hit <- which(r1[[1]] == key)
			if (!length(hit)) NA_character_ else chr(r1[[2]][hit[1]])
		}
		r <- carobiner::read.excel(f, sheet=books$fieldbook[i])
		ps <- num(val("Plot size"))
		ttwp <- num(r$TTWP)
		data.frame(
			variety = chr(r$INSTN),
			treatment = chr(r$INSTN),
			rep = as.integer(num(r$REP)),
			plot_area = ps,
			ttwp_ = ttwp,
			yield = if (!is.na(ps) && ps > 0) ttwp * 10000 / ps else NA_real_,
			country = val("Country"),
			adm1 = val("Admin1"),
			adm2 = trimws(sub("\\s+District$", "", val("Admin2"))),
			adm3 = trimws(sub("Sector$", "", val("Admin3"))),
			location = trimws(sub("\\s+village$", "", val("Locality"), ignore.case=TRUE)),
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
	d$yield_isfresh <- TRUE
	d$trial_id <- paste(d$location, d$planting_date)
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	# Bugeshi sector, Rubavu District, Rwanda
	bug <- is.na(d$latitude) & grepl("Bugeshi", d$adm3, ignore.case=TRUE)
	d$latitude[bug] <- -1.583
	d$longitude[bug] <- 29.400

	carobiner::write_files(path, meta, d)
}
