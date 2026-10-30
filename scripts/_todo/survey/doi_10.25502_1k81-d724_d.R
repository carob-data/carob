# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIMMYT / IRRI processed household survey, western Bangladesh (Nawabganj, Rajshahi), collected December 2022-June 2023. One row per household in bd-west-bangladesh-houshold-processed-survey-2023-cimmyt.csv (n=432). There is no ID column; hhid is ID_typ_orig.
# Same layout as the northern Bangladesh file. Household x crop x season (aman, boro, aus) where area (ha) or production (kg) is above 0. yield is production_kg / area_ha.
# variety_type is the codebook index: 1 hybrid, 0.5 modern variety, 0.1 traditional variety.
# OM_used is household-level sys_manure, repeated on every crop row.
# adm1 is the division of the corrected district. The file's region is the study region West (study_region_).

## ISSUES
# The file's *_yield columns (Mg/ha) do not match kg/ha (aman rice correlation about 0.17). yield is kg/area. The file's value is yield_mg_ha_ where that column exists.
# No N/P/K rates. Irrigation is a season average across crops, so irrigated was not set.
# aus, and yield_part fiber (jute) and bulb (onion), are not in the vocabulary. Potato, garlic, and ginger use yield_part tubers. distric_cor "Nawabganj" is "CHAPAI NABABGANJ" in the raw district column; confirm the GADM name Chapai Nawabganj.
# A few latitudes reach about 25.9, north of Rajshahi division. They were kept as recorded.
# OM_used is household-level. yield is NA where kilograms are missing but area is above 0.
# write_files: missing carob_contributor; invalid season aus; invalid yield_part bulb and fiber; NA yield.

## NOT PROCESSED
# Annual *_prod / *_surf / *_yi totals (seasonal kg and ha are used).
# Fodder, livestock, labor, gender income-control, food security.
# Season irrigation shares and fertilizers_u (a type count, not a rate).

## NEW VARIABLES
# yield_mg_ha_: the file's yield column where present (Mg/ha); not used as yield
# crop_sold_kg_: kilograms sold, as recorded
# crop_income_thousand_taka_: crop sales income; dictionary unit is "Tk k"
# study_region_: study region in the file (West)
# union_: union name
# gps_from_: where the GPS point was taken (hh, field, other)


carob_script <- function(path) {

"
BD West-Bangladesh Household processed survey 2023 CIMMYT

Dataset processed of a household level survey to describe the main farm characteritics, production and resources allocation in three regions of country in West (Nawabganj, Rajshashi), collected on December 2022 -June 2023.
"

	uri <- "doi:10.25502/1k81-d724/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; CIMMYT; IRRI",
		publication = NA,
		project = "Transforming Agrifood Systems in South Asia",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "bd-west-bangladesh-houshold-processed-survey-2023-cimmyt.csv"]
	r <- read.csv(f, check.names=FALSE, stringsAsFactors=FALSE)

	place <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		carobiner::fix_name(x, "title")
	}

	vtype <- function(x) {
		x <- as.numeric(x)
		out <- rep(NA_character_, length(x))
		out[abs(x - 1) < 1e-6] <- "hybrid"
		out[abs(x - 0.5) < 1e-6] <- "modern variety"
		out[abs(x - 0.1) < 1e-6] <- "traditional variety"
		other <- !is.na(x) & is.na(out)
		out[other] <- as.character(x[other])
		out
	}

	spec <- data.frame(
		stem = c("rice", "maize", "jute", "wheat", "lentils", "mungbean", "tomato", "potato", "onion", "garlic", "tumeric", "ginger", "cabbage", "coriander", "chili", "mustard", "groundnut", "sweetpotato", "sunflower", "cumin", "gourds"),
		crop = c("rice", "maize", "jute", "wheat", "lentil", "mung bean", "tomato", "potato", "onion", "garlic", "turmeric", "ginger", "cabbage", "coriander", "chili pepper", "mustard", "groundnut", "sweetpotato", "sunflower", "cumin", "gourd"),
		yield_part = c("grain", "grain", "fiber", "grain", "seed", "seed", "fruit", "tubers", "bulb", "tubers", "rhizome", "tubers", "leaves", "leaves", "fruit", "seed", "seed", "roots", "seed", "seed", "fruit"),
		stringsAsFactors = FALSE
	)

	kg_stem <- function(stem) if (stem == "lentils") "lentil" else stem
	ta_col <- function(s, stem) if (s == "aman" && stem == "sunflower") "aman_sunfloer_ta" else paste0(s, "_", stem, "_ta")
	inc_col <- function(s, stem) if (stem == "tomato") paste0(s, "_tomaton_income") else paste0(s, "_", kg_stem(stem), "_income")
	vt_col <- function(s, stem) if (stem == "tomato") paste0(s, "_tomate_variety_type") else paste0(s, "_", stem, "_variety_type")
	y_col <- function(s, stem) paste0(s, "_", if (stem == "jute") "yute" else stem, "_yield")

	div <- c(Dinajpur="Rangpur", Nilphamari="Rangpur", Rangpur="Rangpur", Nawabganj="Rajshahi", Rajshahi="Rajshahi")
	adm2 <- place(r$distric_cor)
	adm1 <- unname(div[adm2])
	om <- rep(NA, nrow(r))
	manure <- as.numeric(r$sys_manure)
	om[!is.na(manure) & manure == 1] <- TRUE
	om[!is.na(manure) & manure == 0] <- FALSE

	loc <- place(r$village)
	union <- place(r$union)
	loc[is.na(loc)] <- union[is.na(loc)]
	gps <- trimws(tolower(as.character(r$geoplace)))
	gps[gps == ""] <- NA_character_

	parts <- list()
	for (s in c("aman", "boro", "aus")) {
		for (i in seq_len(nrow(spec))) {
			stem <- spec$stem[i]
			area <- as.numeric(r[[ta_col(s, stem)]])
			prod <- as.numeric(r[[paste0(s, "_", kg_stem(stem), "_kg")]])
			keep <- (!is.na(area) & area > 0) | (!is.na(prod) & prod > 0)
			if (!any(keep)) next
			yld <- rep(NA_real_, length(area))
			ok <- !is.na(area) & area > 0 & !is.na(prod)
			yld[ok] <- prod[ok] / area[ok]
			yc <- y_col(s, stem)
			ymg <- if (yc %in% names(r)) as.numeric(r[[yc]]) else rep(NA_real_, nrow(r))
			parts[[length(parts) + 1]] <- data.frame(
				hhid = as.character(r$ID_typ_orig)[keep],
				country = "Bangladesh",
				adm1 = adm1[keep],
				adm2 = adm2[keep],
				location = loc[keep],
				union_ = union[keep],
				study_region_ = place(r$region)[keep],
				latitude = as.numeric(r$lat)[keep],
				longitude = as.numeric(r$long)[keep],
				gps_from_ = gps[keep],
				crop = spec$crop[i],
				season = s,
				variety_type = vtype(r[[vt_col(s, stem)]])[keep],
				field_size = area[keep],
				yield = yld[keep],
				yield_part = spec$yield_part[i],
				yield_mg_ha_ = ymg[keep],
				crop_sold_kg_ = as.numeric(r[[paste0(s, "_", kg_stem(stem), "_sell")]])[keep],
				crop_income_thousand_taka_ = as.numeric(r[[inc_col(s, stem)]])[keep],
				OM_used = om[keep],
				stringsAsFactors = FALSE
			)
		}
	}

	d <- do.call(rbind, parts)
	rownames(d) <- NULL
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- TRUE
	d$trial_id <- as.character(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
