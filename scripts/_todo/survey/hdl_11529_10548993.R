# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household crop production in northern, western, and southern Bangladesh, collected from December 2022 to June 2023. The combined workbook is used. The regional workbooks repeat those households.
# The codebook states that *_kg is production in kilograms and *_ta is area in hectares. Yield is kilograms divided by hectares.
# Country is Bangladesh in geographicCoverage. District and village are the district and village columns. Names stored in all capitals were changed to title case. Household coordinates are in the file. geo_from_source is TRUE.
# A production of 0 is kept. Rows with no area are left out.
# Jute and gourds are left out. Jute has no yield part in the vocabulary, and gourds are not a crop name in the vocabulary.

## ISSUES
# unknown variable: season_name_ (season?)
# out of bounds: yield (0, 1126400)
# high crop yield: groundnut: 66000, maize: 44000, potato: 1126400, rice: 396000

## NOT PROCESSED
# Sales, income, labor, and the gender modules.
# Jute and gourds.
# The separate North, South, and West workbooks.


carob_script <- function(path) {

"
Analysis of household farm characteristics and resource allocation in three regions of Bangladesh, 2023

Household crop area and production for aman, boro, and aus.
"

	uri <- "hdl:11529/10548993"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f <- ff[grepl("^BD Bangladesh Houshold", basename(ff))]
	r <- as.data.frame(carobiner::read.excel(f, sheet="DB", col_types="text"))

	num <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | !nzchar(x) | x %in% c("NA", "-")] <- NA_character_
		as.numeric(x)
	}
	blank <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | !nzchar(x) | x == "NA"] <- NA_character_
		x
	}

	specs <- data.frame(
		crop = c("rice", "maize", "wheat", "lentil", "mung bean", "tomato", "potato", "onion", "garlic", "turmeric", "ginger", "cabbage", "coriander", "chili pepper", "mustard", "groundnut", "sweetpotato", "sunflower", "cumin"),
		part = c("grain", "grain", "grain", "seed", "seed", "fruit", "tubers", "tubers", "tubers", "rhizome", "rhizome", "leaves", "leaves", "fruit", "seed", "seed", "roots", "seed", "seed"),
		stub = c("rice", "maize", "wheat", "lentil", "mungbean", "tomato", "potato", "onion", "garlic", "tumeric", "ginger", "cabbage", "coriander", "chili", "mustard", "groundnut", "sweetpotato", "sunflower", "cumin")
	)
	seasons <- c("aman", "boro", "aus")
	nice <- function(x) {
		x <- blank(x)
		up <- !is.na(x) & x == toupper(x)
		cap <- function(s) {
			w <- strsplit(s, " ", fixed=TRUE)[[1]]
			paste(toupper(substring(w, 1, 1)), substring(w, 2), sep="", collapse=" ")
		}
		x[up] <- vapply(tolower(x[up]), cap, character(1))
		x
	}
	hhid <- blank(r$ID_typ_orig)
	miss <- is.na(hhid)
	hhid[miss] <- paste0("row", which(miss))

	parts <- list()
	for (s in seasons) {
		for (i in seq_len(nrow(specs))) {
			stub <- specs$stub[i]
			kgc <- paste0(s, "_", stub, "_kg")
			tac <- paste0(s, "_", stub, "_ta")
			if (stub == "lentil") tac <- paste0(s, "_lentils_ta")
			if (stub == "sunflower" && !tac %in% names(r)) tac <- paste0(s, "_sunfloer_ta")
			if (!kgc %in% names(r) || !tac %in% names(r)) next
			kg <- num(r[[kgc]])
			ha <- num(r[[tac]])
			ok <- !is.na(kg) & !is.na(ha) & ha > 0
			if (!any(ok)) next
			parts[[length(parts) + 1]] <- data.frame(
				country = "Bangladesh",
				adm2 = nice(r$district)[ok],
				location = blank(r$village)[ok],
				latitude = num(r$`_geopoint_latitude`)[ok],
				longitude = num(r$`_geopoint_longitude`)[ok],
				hhid = hhid[ok],
				trial_id = paste(hhid[ok], s, specs$crop[i]),
				crop = specs$crop[i],
				yield_part = specs$part[i],
				yield = kg[ok] / ha[ok],
				field_size = ha[ok],
				season_name_ = s,
				on_farm = TRUE,
				is_survey = TRUE,
				geo_from_source = TRUE
			)
		}
	}
	d <- do.call(rbind, parts)
	carobiner::write_files(path, meta, d)
}
