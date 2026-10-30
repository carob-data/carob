# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIMMYT / IWMI processed household survey, Khotang (Halesi Tuwachung) and Surkhet (Gurbhakot), Nepal, March-June 2023. File bd-nepal-houshold-processed-survey-2023-cimmyt.csv.
# 390 rows have no household id, district, coordinates, or crop values and were dropped. Remaining rows are one household (hh_id2).
# One row per household x crop where annual area (su_*, ha) or yield (yield_*, Mg/ha) is above 0. yield is Mg/ha * 1000 = kg/ha. field_size is the annual area.
# irrigated is TRUE when the crop irrigation share (irri_*) is above 0. The share is kept as irrigation_pct_.
# OM_used is household-level manure_sys. chemical_fertilizer_k_ is chemfert_inp; the dictionary unit is "k", so it was not converted to N/P/K.
# Rice columns are named ashare rice (yield_rice_as, su_riceasha). Mapped as crop rice. masayng / su_masayang is kept as crop masyang.

## ISSUES
# Yield is a single household figure, not by season. Seasonal areas (mon_*, sum_*, win_*) were not expanded, because attaching the same yield to each season would double-count. A human should decide whether to split them.
# masyang is not in the crop vocabulary (local name; not mapped to a species). millet is the vocabulary name, not finger millet or another species.
# chemical_fertilizer_k_ is not elemental N. Unit "k" is whatever the dictionary means by k.
# OM_used and the fertilizer amount are household-level, repeated on every crop row.
# adm3 is the municipality (Gurbhakot, Halesi Tuwachung). Confirm GADM. Ward is not an adm level.
# doi:10.25502/xmy5-mx73/d is Surkhet only and may overlap these Surkhet households. Compare hh_id2 before using both.
# write_files: missing carob_contributor; invalid crop masyang; NA yield_part for masyang; out of bounds yield (mango up to about 629000 kg/ha); high crop yield for banana, lentil, mango, rice.

## NOT PROCESSED
# Seasonal areas mon_maize, mon_masayng, mon_millet, mon_potato, mon_rice, sum_maize, sum_potato, win_lentil, win_potato, win_wheat.
# su_other (unspecified crop). Land shares s_* (the ha columns were used).
# Homestead production, fodder, livestock (milk, eggs), TAPE, food-security months, and labor/gender columns.
# seasons (an index of season diversity, not a season name).

## NEW VARIABLES
# irrigation_pct_: percent of the crop reported as irrigated
# chemical_fertilizer_k_: household chemical fertilizer amount; dictionary unit "k"
# ward_: ward number. adm3 is the municipality (Gurbhakot or Halesi Tuwachung)


carob_script <- function(path) {

"
BD Nepal Household processed survey 2023 CIMMYT

Dataset processed of a household level survey to describe the main farm characteritics, production and resources allocation in Surkhet (Gurbhakot) and Khotang (Tuwachung) districts, collected between March -June 2023.
"

	uri <- "doi:10.25502/m8bd-1x28/d"
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
		carob_effort = 1
	)

	f <- ff[basename(ff) == "bd-nepal-houshold-processed-survey-2023-cimmyt.csv"]
	r <- read.csv(f, check.names=FALSE)
	hh <- trimws(as.character(r$hh_id2))
	# 390 rows are blank on id, place, coordinates, and crop columns
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
			latitude = as.numeric(r$lat)[keep],
			longitude = as.numeric(r$long)[keep],
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
