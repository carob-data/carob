# R script for "carob"
# license: GPL (>=3)

## NOTES
# Six wheat yield trials at CENEB Obregon in the 2024-25 season. The description names the station. geographicCoverage is empty, so country is not set.
# Yield is already kg/ha. A second sheet in experiments 1 to 3 repeats the same yield under PESO and is not used.
# Chromosome segment is the field-book code (IL, NS.3BL, H, recurrent parent).

## ISSUES
# No coordinates and no country column.
# write_files: missing variables: yield_moisture, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: chromosome_segment_. NA detected: yield.

## NEW VARIABLES
# chromosome_segment_: chromosome segment named in the trial (IL, NS.3BL, H, or the recurrent parent).

## NOT PROCESSED
# PESO sheets. They repeat Yield.


carob_script <- function(path) {

"
Preliminary yield and agronomic data for CIMMYT elite spring wheat lines carrying the N and H chromosome segments from Leymus racemosus and the I chromosome segments from Leymus mollis.

Preliminary yield and agronomic trial data collected from 6 different trials carried out in the 2024/2025 season at CENEB Obregon field station.
"

	uri <- "hdl:11529/10549342"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Leymus introgression lines, CENEB 2024-25",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	col_by <- function(nms, body, pat) {
		j <- grep(pat, nms, ignore.case=TRUE)
		if (!length(j)) return(rep(NA_character_, nrow(body)))
		trimws(as.character(body[[j[1]]]))
	}
	as_num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		as.numeric(x)
	}

	one <- function(f) {
		r <- carobiner::read.excel(f, sheet="Sheet1", col_names=FALSE)
		lab <- trimws(as.character(r[[1]]))
		hdr <- which(toupper(lab) == "PLOT CODE")[1]
		nms <- trimws(as.character(unlist(r[hdr, ])))
		code_row <- which(toupper(lab) == "EXP CODE")[1]
		code <- as.character(r[[1]][code_row + 1])
		body <- r[(hdr + 1):nrow(r), ]
		variety <- col_by(nms, body, "GERMPLASM NAME")
		ok <- !is.na(variety) & nzchar(variety)
		body <- body[ok, ]
		variety <- variety[ok]
		data.frame(
			trial_id = code,
			plot_id = col_by(nms, body, "PLOT CODE"),
			variety = variety,
			variety_pedigree = col_by(nms, body, "PARENTAGE"),
			variety_code = col_by(nms, body, "GERMPLASM CODE"),
			chromosome_segment_ = col_by(nms, body, "Chrom"),
			rep = as.integer(as_num(col_by(nms, body, "^REP$"))),
			yield = as_num(col_by(nms, body, "^Yield")),
			plant_height = as_num(col_by(nms, body, "height")),
			heading_days = as_num(col_by(nms, body, "heading")),
			flowering_days = as_num(col_by(nms, body, "flowering")),
			maturity_days = as_num(col_by(nms, body, "maturity"))
		)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	d <- do.call(rbind, lapply(ff, one))
	d$treatment <- d$variety
	d$crop <- "wheat"
	d$yield_part <- "grain"
	d$location <- "Obregon"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	d$chromosome_segment_[!nzchar(d$chromosome_segment_)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
