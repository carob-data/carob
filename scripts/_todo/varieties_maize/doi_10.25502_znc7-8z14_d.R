# R script for "carob"
# license: GPL (>=3)

## NOTES
# Fall armyworm screening of early maize at Ife in 2021 and 2022 (AGG). Four files: opv.csv, landrace-trial.csv, inbred-trial.csv, hybrid-trial.csv. Same columns. YIELD is kg/ha.
# The dictionary says yield was adjusted to 80% shelling and corrected to 15% moisture. yield_moisture is 15 from that statement. Shelling percentage was not stored as a per-plot measurement.
# Env codes Ife2021 and Ife2022 supply the location (Ife) and the year. FAW6, FAW8 and FAW10 are 1-9 plot scores at 6, 8 and 10 weeks after planting (DAP 42, 56 and 70).
# ED is an ear score (1 = tidy full ears, 9 = poor ears) and was mapped to ear_aspect.
# Coordinates are the dataset coverage fields, converted from degrees-minutes-seconds: coverage_x 7° 27' 59.99" N, coverage_y 4° 33' 59.99" E. Every row is an Ife environment.
# Four hybrid rows (HYB37) had invalid bytes 8B A8 AB after the cross "x" in Pedigree. Those bytes were removed, leaving "TZEI 129 x TZEI 16".

## ISSUES
# Confirm that Env "Ife" is Ile-Ife and that the single coverage coordinate applies to both years.
# No fertilizer rate, irrigation, or planting day (only the year). The 80% shelling figure is a yield-formula constant.
# "Spodoptera frugiperda" is not in the pest vocabulary.
# No paper DOI in the dataset metadata.
# write_files: missing harvest_date, N/P/K, irrigated. Invalid pest term Spodoptera frugiperda. Unknown variable ears_per_plant_. Some yield values are NA or 0.

## NOT PROCESSED
# All four trial files, ID: row number.
# All four trial files, Entry: field entry number, one per Name within a file. Name is stored as variety_code.

## NEW VARIABLES
# ears_per_plant_: ears per plant (EPP). Unit: ears/plant.


carob_script <- function(path) {

"
Screening for fall armyworm Spodoptera frugiperda (J. E. Smith) resistance in early maturing maize

Stress Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/znc7-8z14/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Accelerated Genetic Gains (AGG) Project",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;ear_aspect;plant_height;anthesis_days",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 76,
		carob_effort = 0.7
	)

	clean_chr <- function(x) {
		x <- as.character(x)
		for (i in seq_along(x)) {
			if (is.na(x[i])) next
			ok <- tryCatch(validUTF8(x[i]), error = function(e) FALSE)
			if (!ok) {
				x[i] <- iconv(x[i], from = "UTF-8", to = "UTF-8", sub = "")
			}
		}
		z <- trimws(x)
		z[is.na(z) | z %in% c("", "NA")] <- NA_character_
		z
	}

	read_piece <- function(fname, vtype) {
		f <- ff[basename(ff) == fname]
		r <- read.csv(f)
		variety <- clean_chr(r$Pedigree)
		env <- trimws(r$Env)
		data.frame(
			trial_id = paste(vtype, env),
			location = "Ife",
			planting_date = sub("^Ife", "", env),
			plot_id = as.character(r$Plot),
			rep = as.integer(r$Rep),
			block_id = as.character(r$Blk),
			variety = variety,
			variety_pedigree = variety,
			variety_code = trimws(r$Name),
			variety_type = vtype,
			treatment = variety,
			stress = "pests",
			yield = r$YIELD,
			# dictionary: yield corrected to 15% moisture
			yield_moisture = 15,
			yield_isfresh = FALSE,
			anthesis_days = r$ANTHESIS,
			silking_days = r$DYSK50,
			asi = r$ASI,
			plant_height = r$PLHT,
			ear_height = r$EHT,
			ear_aspect = r$ED,
			ears_per_plant_ = r$EPP,
			faw6 = r$FAW6,
			faw8 = r$FAW8,
			faw10 = r$FAW10,
			crop = "maize",
			yield_part = "grain",
			country = "Nigeria",
			on_farm = FALSE,
			is_survey = FALSE,
			# CKAN coverage_x = 7° 27' 59.99" N, coverage_y = 4° 33' 59.99" E
			latitude = 7 + 27 / 60 + 59.99 / 3600,
			longitude = 4 + 33 / 60 + 59.99 / 3600,
			geo_from_source = TRUE
		)
	}

	d <- rbind(
		read_piece("opv.csv", "early-maturing OPV"),
		read_piece("landrace-trial.csv", "landrace"),
		read_piece("inbred-trial.csv", "early-maturing inbred"),
		read_piece("hybrid-trial.csv", "early-maturing hybrid")
	)
	d$record_id <- as.integer(seq_len(nrow(d)))

	long <- NULL
	add <- function(value, dap, ord) {
		keep <- !is.na(value)
		if (!any(keep)) return(invisible(NULL))
		n <- sum(keep)
		x <- data.frame(
			record_id = d$record_id[keep],
			disease = rep(NA_character_, n),
			disease_severity = rep(NA_character_, n),
			disease_incidence = rep(NA_character_, n),
			pest_species = "Spodoptera frugiperda",
			pest_severity = as.character(value[keep]),
			pest_incidence = rep(NA_integer_, n),
			# column is weeks after planting
			DAP = rep(as.integer(dap), n),
			order = rep(as.integer(ord), n)
		)
		if (is.null(long)) long <<- x else long <<- rbind(long, x)
	}
	add(d$faw6, 42, 1)
	add(d$faw8, 56, 2)
	add(d$faw10, 70, 3)
	d[c("faw6", "faw8", "faw10")] <- NULL

	carobiner::write_files(path, meta, d, long = long)
}
