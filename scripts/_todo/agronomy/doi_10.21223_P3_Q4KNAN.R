# R script for "carob"
# license: GPL (>=3)

## NOTES
# UNICA potato irrigation experiment. Geographic coverage is Peru, Lima, La Molina. The published bounding box gives only the south and west corner, so that single corner is used for this one site with geo_uncertainty 2000.
# The dictionary expands DR to drip irrigation and FU to furrow irrigation. Treatment 1 is full irrigation, 2 is a stomatal-conductance threshold of 0.15, and 3 is a threshold of 0.05. Those thresholds are not a fertilizer rate.
# Harvest workbook weights are fresh and dry mass of one plant (leaf, stem, root, tuber). The dictionary shipped with the files does not state the unit, so they are not converted to kg/ha.

## ISSUES
# Plant mass has no stated unit, so there is no yield.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: plant_number_, fresh_leaf_, fresh_stem_, fresh_root_, fresh_tuber_, dry_leaf_, dry_stem_, dry_root_, dry_tuber_.

## NEW VARIABLES
# fresh_leaf_, fresh_stem_, fresh_root_, fresh_tuber_: fresh mass of one plant, unit not stated.
# dry_leaf_, dry_stem_, dry_root_, dry_tuber_: dry mass of one plant, unit not stated.
# plant_number_: plant index within the plot.

## NOT PROCESSED
# Canopy cover, photosynthesis, isotopes, irrigation volume, and PRI workbooks. Irrigation volume is not joined because this table is the sequential harvest, and the volume unit is not applied here.


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

	carobiner::write_files(path, meta, d)
}
