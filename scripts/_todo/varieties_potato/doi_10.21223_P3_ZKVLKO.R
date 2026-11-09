# R script for "carob"
# license: GPL (>=3)

## NOTES
# Three late-blight field trials of potato germplasm in Oxapampa, Peru, in 2018, 2019, and 2020. The title names the place and the years. The files do not give coordinates or a planting day, so the planting date is the year.
# LB1 to LB7 are the percentage of foliage affected. Those scores are in the long table, one row per score, in the order of the columns.
# AUDPC on the sheet was computed from those percentages. The vocabulary AUDPC is the sum of the infected fraction, so the stored value is the sheet value divided by 100.
# rAUDPC is already on a 0 to 1 scale and is stored as read.
# The material list has no variety names, and many species fields say "in revision", so the variety is the institutional number.

## ISSUES
# No coordinates, fertilizer rate, irrigation statement, or yield.
# Oxapampa as a province should be checked against GADM. It is stored as the location, with adm1 Pasco.
# The score sequence has no date, so the long table uses order rather than a day count.
# write_files: missing yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variable late_blight_pct_. The long table has no date or depth column.

## NOT PROCESSED
# 04_BLUEs_by_env.csv. Those are model estimates, not the plot measurements.
# Row and column coordinates of the field plan, beyond using them as plot_id.
# Material-list genus, species, and ploidy. The species column is incomplete.
# 05_Material_list.csv: not read. columns: INSTN, genus, species, subtaxa, acronym, ploidy, pop, notes. Observation records are missing from d until this file is standardized.
# 06_Variables.csv: not read. columns: Abrev, Variable. Observation records are missing from d until this file is standardized.


## NEW VARIABLES
# late_blight_pct_: percent of foliage affected by late blight at one score in the trial sequence.


carob_script <- function(path) {

"
Dataset for: Raw and analyzed late blight resistance data from three field trials in potato germplasm in Oxapampa, Peru (2018–2020)
"

	uri <- "doi:10.21223/P3/ZKVLKO"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "rAUDPC;AUDPC",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.55
	)

	one <- function(f, year) {
		r <- read.csv(f)
		variety <- trimws(as.character(r$INSTN))
		d <- data.frame(
			trial_id = paste0("OXA", year),
			crop = "potato",
			variety = variety,
			treatment = variety,
			rep = as.integer(r$REP),
			plot_id = paste(r$ROW, r$COL, sep="-"),
			rAUDPC = as.numeric(r$rAUDPC),
			AUDPC = as.numeric(r$AUDPC) / 100,
			country = "Peru",
			adm1 = "Pasco",
			location = "Oxapampa",
			planting_date = year,
			stress = "disease",
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
		lbs <- grep("^LB[0-9]+$", names(r), value=TRUE)
		long <- lapply(lbs, function(nm) {
			data.frame(
				trial_id = d$trial_id,
				plot_id = d$plot_id,
				rep = d$rep,
				variety = d$variety,
				order = as.integer(sub("^LB", "", nm)),
				late_blight_pct_ = as.numeric(r[[nm]])
			)
		})
		long <- do.call(rbind, long)
		list(d=d, long=long)
	}

	files <- c("2018" = "01_Rawdata_OXA2018.csv", "2019" = "02_Rawdata_OXA2019.csv", "2020" = "03_Rawdata_OXA2020.csv")
	parts <- lapply(names(files), function(year) one(ff[basename(ff) == files[[year]]], year))
	d <- do.call(rbind, lapply(parts, function(z) z$d))
	long <- do.call(rbind, lapply(parts, function(z) z$long))
	rownames(d) <- NULL
	rownames(long) <- NULL
	d$record_id <- seq_len(nrow(d))
	key <- paste(d$trial_id, d$plot_id, d$rep, d$variety, sep="|")
	long$record_id <- d$record_id[match(paste(long$trial_id, long$plot_id, long$rep, long$variety, sep="|"), key)]
	long$trial_id <- NULL
	long$plot_id <- NULL
	long$rep <- NULL
	long$variety <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
