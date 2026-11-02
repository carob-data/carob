# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are the Boiling sheet. Cooking time is minutes.
# Flavor AVG and texture AVG are kept under tuber_flavor and tuber_texture when they are numeric. The vocabulary scale is 1-5.
# Country Peru is the dataset geographic coverage. The sheet has no location, so the La Molina and Huancayo boxes are not assigned to rows.

## ISSUES
# No yield, fertilizer, or dates. The metadata spans 2000-2016 and is not one planting date.
# Sites need georeferencing. Two places are named in the metadata and are not linked to rows.
# Rows with a blank clone were removed.
# write_files: ; yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: cooking_time_, cooking_type_, cooking_darkening_, flesh_color_cooked_, cooking_class_.

## NEW VARIABLES
# cooking_time_: minutes
# cooking_type_: cooking type or quality text
# cooking_darkening_: after-cooking darkening text
# flesh_color_cooked_: flesh color after cooking
# cooking_class_: classification text

carob_script <- function(path) {

"
Dataset for:  Standard evaluation trial  for cooking quality documentation of CIP advanced breeding clones

The primary objective of evaluating post-harvest traits is obtain information about the potential or aptitude of  advanced clones for diverse end-uses, ranging from fresh consumption to processed products. The interpretation of these evaluations provides important information to guide potato breeding and selection programs, as well as for the recommendation of varieties for specific uses. Therefore, CIP has adopted standard procedure for determining cooking quality for evaluation of advanced clonal selections. Variables such as  cooking time, cooking type/quality, darkening and flesh color were evaluated for each clone. In addition if a taste panel is available taste and texture is are assessed. The clones are classified as: slightly soggy, moderately soggy  and very mealy.
"

	uri <- "doi:10.21223/P3/RM1D5W"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "cooking_time_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Standard Evaluation Trial for cooking quality Data.xls"]
	r <- carobiner::read.excel(f, sheet="Boiling")
	txt <- function(x) {
		y <- trimws(as.character(x))
		y[!nzchar(y) | y %in% c("NA")] <- NA_character_
		y
	}

	d <- data.frame(
		crop = "potato",
		variety = txt(r$Clone),
		treatment = txt(r$Clone),
		cooking_time_ = {
			y <- txt(r[["Cooking Time (minutes)"]])
			y[y %in% c("NA", ".", "-")] <- NA_character_
			as.numeric(y)
		},
		cooking_type_ = txt(r[["Cooking Type/Quality"]]),
		cooking_darkening_ = txt(r[["After cooking darkening"]]),
		flesh_color_cooked_ = txt(r[["Flesh Color after cooking"]]),
		tuber_flavor = {
			y <- txt(r[["Flavor AVG"]])
			y[y %in% c("NA", ".", "-")] <- NA_character_
			as.numeric(y)
		},
		tuber_texture = {
			y <- txt(r[["Texture of tuber after cooking AVG"]])
			y[y %in% c("NA", ".", "-")] <- NA_character_
			as.numeric(y)
		},
		cooking_class_ = txt(r$Classification),
		country = "Peru",
		trial_id = txt(r$Set),
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA
	)
	d$plot_id <- as.character(seq_len(nrow(d)))
	d <- d[!is.na(d$variety), ]

	carobiner::write_files(path, meta, d)
}
