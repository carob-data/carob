# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2015 elite yam clones scored for virus and anthracnose. Files are split by species (D. alata, D. rotundata) and sett portion (head, middle, bottom).
# D. alata -> purple yam. D. rotundata -> yam (white yam in terminag).
# Loc is Ibadan_CN1, Ibadan_B21 or Ibadan_B23. The location used is Ibadan; the field code is field_code_.
# Planting dates are month/day/year. 8/21/2015 (rotundata head) cannot be day-first, so 6/1/2015 is 1 June 2015.
# NG and JG in the score columns are kept. The dictionary says NG = no growth and JG = just growing. Plants on a peg are averaged. A mix of a numeric score and NG or JG keeps the mean of the numbers and the code.
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

	f1 <- ff[basename(ff) %in% c(
		"d-alata-bottom-ikenne.csv",
		"d.-alata-bottom.csv",
		"d.-alata-head-ikenne-.csv",
		"d.-alata-head.csv",
		"d.-alata-middle-.csv",
		"d.-alata-middle-ikenne.csv",
		"d.-rotundata-bottom-.csv",
		"d.-rotundata-head-.csv",
		"d.-rotundata-middle.csv"
	)]
	rows <- list()
	for (f in f1) {
		r <- read.csv(f, na.strings=c("", "NA"), check.names=FALSE)
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
			anth_nov = trimws(as.character(r$Anthrc_Sev_Nov))
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
			date = date
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

	# Average one row per plant to one row per group in `by`.
	# Numbers are means. A yes/no column stays yes/no when the plants agree, otherwise it is the proportion of plants.
	# Text that is all numbers is averaged. Other text that differs is joined with "; ". Full dates are averaged.
	mean_by <- function(x, by) {
		if (!nrow(x)) return(x)
		if ("plant_number_" %in% names(x)) x$plant_number_ <- NULL
		if ("record_id" %in% names(x)) x$record_id <- NULL
		by <- intersect(by, names(x))
		key <- do.call(paste, c(lapply(by, function(v) ifelse(is.na(x[[v]]), "", as.character(x[[v]]))), list(sep = "\r")))
		mean_na <- function(z) {
			z <- as.numeric(z)
			if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
		}
		keep <- !duplicated(key)
		out <- x[keep, , drop = FALSE]
		num <- vapply(x, is.numeric, TRUE)
		logi <- vapply(x, is.logical, TRUE)
		num[names(num) %in% by] <- FALSE
		logi[names(logi) %in% by] <- FALSE
		for (v in names(x)[num]) {
			m <- tapply(x[[v]], key, mean_na)
			m <- unname(m[key[keep]])
			if (is.integer(x[[v]]) && all(is.na(m) | abs(m - round(m)) < 1e-8)) {
				m <- as.integer(round(m))
			}
			out[[v]] <- m
		}
		for (v in names(x)[logi]) {
			same <- tapply(x[[v]], key, function(z) {
				z <- z[!is.na(z)]
				!length(z) || all(z == z[1])
			})
			if (isTRUE(all(same))) {
				m <- tapply(x[[v]], key, function(z) {
					z <- z[!is.na(z)]
					if (!length(z)) NA else z[1]
				})
				out[[v]] <- unname(m[key[keep]])
			} else {
				m <- tapply(as.numeric(x[[v]]), key, mean_na)
				out[[v]] <- unname(m[key[keep]])
			}
		}
		for (v in names(x)) {
			if (v %in% by || num[v] || logi[v]) next
			u <- tapply(as.character(x[[v]]), key, function(z) {
				z <- z[!is.na(z)]
				z <- trimws(z)
				z <- z[nzchar(z)]
				if (!length(z)) return(NA_character_)
				if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", z))) {
					return(as.character(mean(as.Date(z))))
				}
				isn <- grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
				parts <- character()
				if (any(isn)) parts <- c(parts, as.character(mean(as.numeric(z[isn]))))
				codes <- unique(z[!isn])
				parts <- c(parts, codes)
				if (length(parts) == 1) parts else paste(parts, collapse = "; ")
			})
			out[[v]] <- unname(u[key[keep]])
		}
		rownames(out) <- NULL
		out
	}

	# The long table is linked by record_id. After plants are averaged, that link is rebuilt.
	# long_by keeps species, date, and stage apart. It does not identify the plant.
	collapse_plants <- function(d, by, long, long_by) {
		key_of <- function(x) {
			do.call(paste, c(lapply(by, function(v) ifelse(is.na(x[[v]]), "", as.character(x[[v]]))), list(sep = "\r")))
		}
		long$plot_key <- key_of(d)[match(long$record_id, d$record_id)]
		d <- mean_by(d, by)
		d$plot_key <- key_of(d)
		d$record_id <- seq_len(nrow(d))
		long <- mean_by(long, c("plot_key", long_by))
		long$record_id <- d$record_id[match(long$plot_key, d$plot_key)]
		stopifnot(!any(is.na(long$record_id)))
		long$plot_key <- NULL
		d$plot_key <- NULL
		list(d = d, long = long)
	}

	cl <- collapse_plants(
		d,
		c("trial_id", "plot_id", "variety", "sett_portion_", "treatment", "planting_date", "field_code_", "crop", "location"),
		long,
		c("disease", "date")
	)
	d <- cl$d
	long <- cl$long

	carobiner::write_files(path, meta, d, long=long)
}
