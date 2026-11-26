# R script for "carob"
# license: GPL (>=3)

## NOTES
# Three late-blight field trials of potato germplasm in Oxapampa, Peru, in 2018, 2019, and 2020. The title names the place and the years. The files do not give coordinates or a planting day, so the planting date is the year.
# LB1 to LB7 are the percentage of foliage affected. Those scores are in the long table, one row per score, in the order of the columns.
# AUDPC on the sheet was computed from those percentages. The vocabulary AUDPC is the sum of the infected fraction, so the stored value is the sheet value divided by 100.
# rAUDPC is already on a 0 to 1 scale and is stored as read.
# The material list has no variety names, and many species fields say "in revision", so the variety is the institutional number.

## ISSUES
# No fertilizer rate, irrigation statement, calendar dates (year only), or yield.
# Oxapampa, Pasco, Peru coordinates (-10.5775, -75.4017) added from gazetteer (geo_from_source = FALSE).
# The score sequence has no date, so the long table uses order (1..7).

## NOT PROCESSED
# 04_BLUEs_by_env.csv. Those are model estimates, not the plot measurements.
# Row and column coordinates of the field plan, beyond using them as plot_id.
# Material-list genus, species, and ploidy. The species column is incomplete.
# 05_Material_list.csv: not read. columns: INSTN, genus, species, subtaxa, acronym, ploidy, pop, notes.
# 06_Variables.csv: not read. columns: Abrev, Variable.


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
		response_vars = "rAUDPC;AUDPC;disease_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 60,
		carob_effort = 0
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
			pathogen = "Phytophthora infestans",
			country = "Peru",
			adm1 = "Pasco",
			location = "Oxapampa",
			latitude = -10.5775,
			longitude = -75.4017,
			planting_date = year,
			stress = "disease",
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
		lbs <- grep("^LB[0-9]+$", names(r), value=TRUE)
		long <- lapply(lbs, function(nm) {
			sev <- as.numeric(r[[nm]])
			keep <- !is.na(sev)
			data.frame(
				trial_id = d$trial_id[keep],
				plot_id = d$plot_id[keep],
				rep = d$rep[keep],
				variety = d$variety[keep],
				order = as.integer(sub("^LB", "", nm)),
				disease = "potato late blight",
				disease_severity = as.character(sev[keep]),
				severity_scale = "0-100"
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
