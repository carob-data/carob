# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIMMYT / IWMI processed household survey filed as Surkhet-Nepal. File bd-surkhet-nepal-houshold-processed-survey-2023-cimmyt.csv. Coordinates are latitud / longitud.
# The title also names Khotang, but every non-empty row is Surkhet. 390 rows are blank (no id, place, coordinates, or crops) and were dropped.
# Same crop columns as doi:10.25502/m8bd-1x28/d. One row per household x crop where annual area (ha) or yield (Mg/ha) is above 0. yield = Mg/ha * 1000.
# irrigated is TRUE when irri_* is above 0. OM_used is household manure_sys. chemical_fertilizer_k_ is chemfert_inp with dictionary unit "k".

## ISSUES
# No Khotang rows in this file. Surkhet hh_id2 values may overlap doi:10.25502/m8bd-1x28/d. Compare ids before using both.
# Yield is not by season. Seasonal area columns were not expanded.
# masyang is not in the crop vocabulary. chemical_fertilizer_k_ is not elemental N.
# OM_used and fertilizer amount are household-level. adm3 is the municipality; confirm GADM.
# write_files: missing carob_contributor; high crop yield for lentil and rice. No masyang rows had area or yield above 0.

## NOT PROCESSED
# Seasonal areas mon_*, sum_*, win_*. su_other. Land shares s_*.
# Homestead, fodder, livestock, TAPE, food security, labor, and the seasons diversity index.

## NEW VARIABLES
# irrigation_pct_: percent of the crop reported as irrigated
# chemical_fertilizer_k_: household chemical fertilizer amount; dictionary unit "k"
# ward_: ward number. adm3 is the municipality


carob_script <- function(path) {

"
BD Surkhet-Nepal Household processed survey 2023 CIMMYT

Dataset processed of a household level survey to describe the main farm characteritics, production and resources allocation in Surkhet (Gurbhakot) and Khotang (Tuwachung) districts, collected between March -June 2023.
"

	uri <- "doi:10.25502/xmy5-mx73/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; CIMMYT; IWMI",
		publication = NA,
		project = "Transforming Agrifood Systems in South Asia",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "bd-surkhet-nepal-houshold-processed-survey-2023-cimmyt.csv"]
	r <- read.csv(f, check.names=FALSE)
	hh <- trimws(as.character(r$hh_id2))
	r <- r[!is.na(hh) & hh != "", ]
	rownames(r) <- NULL

	place <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		carobiner::fix_name(x, "title")
	}

	spec <- data.frame(
		crop = c("banana", "lentil", "maize", "mango", "masyang", "millet", "potato", "rice", "wheat"),
		area = c("su_banana", "su_lentil", "su_maize", "su_mango", "su_masayang", "su_millet", "su_potato", "su_riceasha", "su_wheat"),
		yield = c("yield_banana", "yield_lentil", "yield_maize", "yield_mango", "yield_masayng", "yield_millet", "yield_potato", "yield_rice_as", "yield_wheat"),
		irri = c("irri_banana", "irri_lentil", "irri_maize", "irri_mango", "irri_masayng", "irri_millet", "irri_potato", "irri_rice_as", "irri_wheat"),
		yield_part = c("fruit", "seed", "grain", "fruit", NA_character_, "grain", "tubers", "grain", "grain")
	)

	muni <- place(r$municipality)
	loc <- place(r$tole_ham)
	loc[is.na(loc)] <- muni[is.na(loc)]
	om <- rep(NA, nrow(r))
	manure <- as.numeric(r$manure_sys)
	om[!is.na(manure) & manure == 1] <- TRUE
	om[!is.na(manure) & manure == 0] <- FALSE

	parts <- list()
	for (i in seq_len(nrow(spec))) {
		area <- as.numeric(r[[spec$area[i]]])
		ymg <- as.numeric(r[[spec$yield[i]]])
		keep <- (!is.na(area) & area > 0) | (!is.na(ymg) & ymg > 0)
		if (!any(keep)) next
		pct <- as.numeric(r[[spec$irri[i]]])
		irr <- rep(NA, length(pct))
		irr[!is.na(pct) & pct > 0] <- TRUE
		irr[!is.na(pct) & pct == 0] <- FALSE
		parts[[length(parts) + 1]] <- data.frame(
			hhid = trimws(as.character(r$hh_id2))[keep],
			country = "Nepal",
			adm2 = place(r$district)[keep],
			adm3 = muni[keep],
			location = loc[keep],
			ward_ = trimws(as.character(r$ward))[keep],
			latitude = as.numeric(r$latitud)[keep],
			longitude = as.numeric(r$longitud)[keep],
			elevation = as.numeric(r$altitude)[keep],
			crop = spec$crop[i],
			field_size = area[keep],
			yield = ymg[keep] * 1000,
			yield_part = spec$yield_part[i],
			irrigated = irr[keep],
			irrigation_pct_ = pct[keep],
			OM_used = om[keep],
			chemical_fertilizer_k_ = as.numeric(r$chemfert_inp)[keep]
		)
	}

	d <- do.call(rbind, parts)
	rownames(d) <- NULL
	d$ward_[d$ward_ %in% c("", "NA")] <- NA
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- TRUE
	d$trial_id <- as.character(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
