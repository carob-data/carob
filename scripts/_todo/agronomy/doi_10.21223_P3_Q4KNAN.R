# R script for "carob"
# license: GPL (>=3)

## NOTES
# UNICA potato irrigation experiment. Geographic coverage is Peru, Lima, La Molina. The published bounding box gives only the south and west corner, so that single corner is used for this one site with geo_uncertainty 2000.
# The dictionary expands DR to drip irrigation and FU to furrow irrigation. Treatment 1 is full irrigation, 2 is a stomatal-conductance threshold of 0.15, and 3 is a threshold of 0.05. Those thresholds are not a fertilizer rate.
# Harvest workbook weights are fresh and dry mass of one plant (leaf, stem, root, tuber), averaged across plants harvested on the same day. Sequential harvests stay separate. The dictionary shipped with the files does not state the unit, so they are not converted to kg/ha.

## ISSUES
# Plant mass has no stated unit, so there is no yield.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: fresh_leaf_, fresh_stem_, fresh_root_, fresh_tuber_, dry_leaf_, dry_stem_, dry_root_, dry_tuber_.

## NEW VARIABLES
# fresh_leaf_, fresh_stem_, fresh_root_, fresh_tuber_: mean fresh mass of the plants harvested that day, unit not stated.
# dry_leaf_, dry_stem_, dry_root_, dry_tuber_: mean dry mass of the plants harvested that day, unit not stated.

## NOT PROCESSED
# Canopy cover, photosynthesis, isotopes, irrigation volume, and PRI workbooks. Irrigation volume is not joined because this table is the sequential harvest, and the volume unit is not applied here.
# CANOPY COVER-EX17-I.xlsx: not read. sheet hoja 1: ID, DAP, irrigation, treatment, plot, CC. Observation records are missing from d until this file is standardized.
# Data_Dictionary.xlsx: documentation or a codebook, not an observation table.
# Documentation_v2.xlsx: not read. sheet Ecophysiology (of 8): Variable name, Abbreviation, Unit, Description, Method, Formula, Reference. Observation records are missing from d until this file is standardized.
# ECOPHYSIOLOGY-EX17-I.xlsx: not read. sheet Hoja1: ID, date, dap, irrigation, treatment, plot, plant, A, gs. Observation records are missing from d until this file is standardized.
# HARVEST ISOTOPES-EX17-I.xlsx: not read. sheet Hoja4: ID, harvest, dap, irrigation, treatment, plot, tm, dw, D13. Observation records are missing from d until this file is standardized.
# IRRIGATION VOLUME-EX17-I.xlsx: not read. sheet Hoja2: ID, date, dap, irrigation, treatment, plot, IV. Observation records are missing from d until this file is standardized.
# Photosynthesis recovery-2017_v2.xlsx: not read. sheet Ecophysiology (of 8): id, date, dap, irrigation, treatment, plot, plant, Pn, gs_max, Fv'/Fm'. Observation records are missing from d until this file is standardized.
# PRI EX17-I.xlsx: not read. sheet hoja 1: ID, dap, irrigation, treatment, plot, plant, PRI. Observation records are missing from d until this file is standardized.
# RECOVERY ISOTOPES-EX17-I.xlsx: not read. sheet hoja 1: ID, dap, ms, irrigation, treatment, plot, tm, dw, D13. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Dataset for: The photosynthesis recovery, a key trait as indicator of an appropriate irrigation criteria in potato crop

Field experiment to compare physiological performance and crop yield among different water stress levels before and after rehidratation periods in UNICA potato variety, to test the reliability of photosynthesis recovery as key trait for indicate an appropriate irrigation criteria, under two irrigation types.
"

	uri <- "doi:10.21223/P3/Q4KNAN"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;irrigation_method",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "HARVEST-EX17-I.xlsx"]
	r <- carobiner::read.excel(f)
	num <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z) | z == "NA"] <- NA_character_
		ok <- is.na(z) | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}
	irr <- trimws(as.character(r$irrigation))
	method <- ifelse(irr == "DR", "drip", ifelse(irr == "FU", "furrow", NA_character_))
	d <- data.frame(
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = TRUE,
		geo_uncertainty = 2000,
		geo_source = "CIP Dataverse bounding box (south and west only)",
		crop = "potato",
		variety = "UNICA",
		treatment = trimws(as.character(r$treatment)),
		irrigation_method = method,
		irrigated = TRUE,
		DAP = as.integer(num(r$dap)),
		plot_id = trimws(as.character(r$plot)),
		plant_number_ = as.integer(num(r$plant)),
		fresh_leaf_ = num(r[["fw-leaf"]]),
		fresh_stem_ = num(r[["fw-stem"]]),
		fresh_root_ = num(r[["fw- root"]]),
		fresh_tuber_ = num(r[["fw-tuber"]]),
		dry_leaf_ = num(r[["dw-leaf"]]),
		dry_stem_ = num(r[["dw-stem"]]),
		dry_root_ = num(r[["dw-root"]]),
		dry_tuber_ = num(r[["dw-tuber"]]),
		trial_id = "LaMolina_UNICA_irrigation",
		on_farm = FALSE,
		is_survey = FALSE
	)
	rownames(d) <- NULL

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

	d <- mean_by(d, c("plot_id", "DAP", "treatment", "irrigation_method", "trial_id", "variety", "crop", "location"))

	carobiner::write_files(path, meta, d)
}
