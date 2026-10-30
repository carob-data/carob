# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2015 elite yam clones scored for virus and anthracnose. Files are split by species (D. alata, D. rotundata) and sett portion (head, middle, bottom).
# D. alata -> purple yam. D. rotundata -> yam (white yam in terminag).
# Loc is Ibadan_CN1, Ibadan_B21 or Ibadan_B23. The location used is Ibadan; the field code is field_code_.
# Planting dates are month/day/year. 8/21/2015 (rotundata head) cannot be day-first, so 6/1/2015 is 1 June 2015.
# NG and JG in the score columns are kept. The dictionary says NG = no growth and JG = just growing.
# Monthly scores are in the long table (2015-09, 2015-10, 2015-11). Mean score columns were not used.
# There is no yield in these files. This is an agronomy trial because sett portion (head, middle, bottom) is a treatment together with clone.

## ISSUES
# Filenames containing "ikenne" have Loc = Ibadan_B23, the same location as the non-ikenne alata files. Ikenne was not used. The two bottom (and two middle) alata files are different accessions, not duplicates (only 32 of 367 keys overlap), and were both kept.
# The virus is not named. disease is "virus". Anthracnose was stored as "anthracnose".
# No coordinates. Georeference Ibadan fields CN1, B21 and B23, Nigeria.
# No yield, fertilizer or irrigation.
# Right-hand or trailing empty columns and mean scores were ignored.
# write_files: missing yield, longitude, latitude, N/P/K, irrigated. invalid disease terms are likely.

## NOT PROCESSED
# Mean_virus_sev, Mean_Anthrc_sev: means of the monthly scores.
# ID: row number.
# data-dictionary.csv: the examples in it (Loc CN1, D. rotundata, Head) match the column meanings used here.
# Files named ikenne were not given a separate location (see ISSUES).

## NEW VARIABLES
# sett_portion_: head, middle or bottom of the seed sett.
# field_code_: CN1, B21 or B23 from Loc.
# plant_number_: Plant_NO within the peg.
# tissue_: not used. Observation month is `date` on the long table.


carob_script <- function(path) {

"
2015 Elite clones Head, Middle, Bottom

Enhanced breeding for increased productivity and improved quality in West Africa
"

	uri <- "doi:10.25502/rq3j-c772/d"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;sett_portion_",
		response_vars = "disease_severity",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.8,
		notes = NA
	)

	fs <- ff[grepl("alata|rotundata", basename(ff), ignore.case=TRUE)]
	rows <- list()
	for (f in fs) {
		r <- read.csv(f, stringsAsFactors=FALSE, na.strings=c("", "NA"), check.names=FALSE)
		r <- r[!is.na(r$AccessionNo) & trimws(r$AccessionNo) != "", ]
		anth <- if ("Anthrc_Sev_Sep" %in% names(r)) r$Anthrc_Sev_Sep else r$Anthrc_Sev_Sept
		sp <- gsub("[^a-z]", "", tolower(r$Specie))
		crop <- ifelse(grepl("alata", sp), "purple yam", ifelse(grepl("rotundata", sp), "yam", NA))
		loc <- trimws(r$Loc)
		field <- sub("^[^_]+_?", "", loc)
		rows[[length(rows) + 1]] <- data.frame(
			location = "Ibadan",
			field_code_ = field,
			country = "Nigeria",
			crop = crop,
			planting_date = as.character(as.Date(trimws(r$planting_date), "%m/%d/%Y")),
			plot_id = as.character(r$PEG_NO),
			variety = trimws(r$AccessionNo),
			sett_portion_ = tolower(trimws(r$Portion)),
			plant_number_ = trimws(r$Plant_NO),
			virus_sep = trimws(as.character(r$Virus_Sev_Sep)),
			virus_oct = trimws(as.character(r$Virus_Sev_Oct)),
			virus_nov = trimws(as.character(r$Virus_Sev_Nov)),
			anth_sep = trimws(as.character(anth)),
			anth_oct = trimws(as.character(r$Anthrc_Sev_Oct)),
			anth_nov = trimws(as.character(r$Anthrc_Sev_Nov)),
			stringsAsFactors = FALSE
		)
	}
	x <- do.call(rbind, rows)
	x$trial_id <- paste(x$field_code_, x$crop, x$planting_date, sep="_")
	x$treatment <- paste(x$variety, x$sett_portion_)
	x$record_id <- seq_len(nrow(x))
	x$on_farm <- FALSE
	x$is_survey <- FALSE
	x$geo_from_source <- FALSE
	x$yield_part <- "tubers"

	blank <- function(z) {
		z <- trimws(z)
		z[z %in% c("", "NA")] <- NA
		z
	}
	one <- function(val, disease, date) {
		data.frame(
			record_id = x$record_id,
			disease = disease,
			disease_severity = blank(val),
			date = date,
			stringsAsFactors = FALSE
		)
	}
	long <- rbind(
		one(x$virus_sep, "virus", "2015-09"),
		one(x$virus_oct, "virus", "2015-10"),
		one(x$virus_nov, "virus", "2015-11"),
		one(x$anth_sep, "anthracnose", "2015-09"),
		one(x$anth_oct, "anthracnose", "2015-10"),
		one(x$anth_nov, "anthracnose", "2015-11")
	)
	long <- long[!is.na(long$disease_severity), ]

	d <- x[, c("record_id", "trial_id", "country", "location", "field_code_", "crop", "planting_date",
		"plot_id", "variety", "sett_portion_", "treatment", "plant_number_", "yield_part",
		"on_farm", "is_survey", "geo_from_source")]

	carobiner::write_files(path, meta, d, long=long)
}
