# R script for "carob"
# license: GPL (>=3)

## NOTES
# Yields are from the barley, triticale, and wheat sheets in t/ha.
# Geographic coverage is Mexico, Hidalgo, Progreso de Obregón. The record has no coordinates.
# The title says the trial was irrigated. Emergence dates are written as day/Spanish-month/year.
# The file name says OI16-17. The emergence dates are in December 2017.

## ISSUES
# No coordinates.
# write_files: missing variables: yield_moisture, latitude, longitude, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# ADT_VA workbook: entry list and kilograms of seed, with no area.
# Observations (rust notes). The sheets do not give a rust score.


carob_script <- function(path) {

"
Evaluation of different varieties of barley, triticale and wheat under irrigation conditions in Valle del Mezquital, Hidalgo, México.

Different varieties of barley (4 varieties), triticale (4 varieties) and wheat (8 varieties) where evaluated under irrigation conditions in Valle del Mezquital, Hidalgo, México, during 2016 and 2017.
"

	uri <- "hdl:11529/10548673"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	to_date <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		mon <- c(ene="01", feb="02", mar="03", abr="04", may="05", jun="06", jul="07", ago="08", sep="09", oct="10", nov="11", dic="12")
		parts <- strsplit(x, "/", fixed=TRUE)
		out <- rep(NA_character_, length(x))
		for (i in seq_along(parts)) {
			p <- parts[[i]]
			if (is.na(x[i]) || length(p) != 3) next
			mm <- unname(mon[tolower(p[2])])
			if (is.na(mm) || !nzchar(mm)) next
			day <- as.integer(p[1])
			out[i] <- paste0(p[3], "-", mm, "-", sprintf("%02d", day))
		}
		out
	}

	read_crop <- function(f, sheet, crop) {
		r <- carobiner::read.excel(f, sheet=sheet, col_types="text")
		hdr <- which(apply(r, 1, function(z) any(z == "Variedad", na.rm=TRUE)))[1]
		nms <- as.character(unlist(r[hdr, ]))
		nms[is.na(nms) | !nzchar(nms)] <- paste0("X", seq_along(nms))[is.na(nms) | !nzchar(nms)]
		x <- r[(hdr + 1):nrow(r), ]
		names(x) <- make.names(nms, unique=TRUE)
		col <- function(pattern) x[[grep(pattern, names(x))[1]]]
		variety <- trimws(col("Variedad"))
		keep <- !is.na(variety) & nzchar(variety)
		data.frame(
			crop = crop,
			variety = variety[keep],
			treatment = variety[keep],
			emergence_date = to_date(col("emergencia"))[keep],
			flowering_days = as.numeric(col("flor"))[keep],
			maturity_days = as.numeric(col("madurez"))[keep],
			yield = as.numeric(col("Rendimiento"))[keep] * 1000
		)
	}

	f <- ff[grepl("Resultados", basename(ff))]
	d <- rbind(
		read_crop(f, "Cebada - Barley", "barley"),
		read_crop(f, "Triticale", "triticale"),
		read_crop(f, "Trigo - Wheat", "wheat")
	)
	d$country <- "Mexico"
	d$adm1 <- "Hidalgo"
	d$location <- "Progreso de Obregón"
	d$trial_id <- as.character(as.integer(as.factor(d$crop)))
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- FALSE
	d$yield_part <- "grain"

	carobiner::write_files(path, meta, d)
}
