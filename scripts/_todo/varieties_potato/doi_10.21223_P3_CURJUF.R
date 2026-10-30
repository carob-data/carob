# R script for "carob"
# license: GPL (>=3)

## NOTES
# Chipping color, reducing sugar (mg/dL), and oil absorption (%) for advanced clones. The sheet has two header rows under those traits: Lowland Tropic (winter, spring) and Highland Tropic (dry, rain). Average columns were not copied.
# Country Peru is the Dataverse geographic coverage. The coverage also names La Molina and San Agustin / La Victoria and gives two bounding-box points. The chip sheet does not say which point belongs to Lowland Tropic or Highland Tropic, so those coordinates were not attached to rows.

## ISSUES
# Sites need georeferencing once the lowland and highland labels are tied to La Molina and Junin.
# No yield and no fertilizer.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: environment_, chipping_color_, reducing_sugar_mg_dl_, oil_absorption_pct_.

## NOT PROCESSED
# Pigment and the AVG columns in Standard Evaluation Trial for chipping quality Data.xls.
# The dictionary workbook.

## NEW VARIABLES
# environment_: lowland winter, lowland spring, highland dry, or highland rain.
# chipping_color_: score as recorded.
# reducing_sugar_mg_dl_: mg/dL.
# oil_absorption_pct_: percent.


carob_script <- function(path) {

"
Dataset for: Standard evaluation trial for chipping quality documentation of CIP advanced breeding clones

The primary objective of evaluating post-harvest traits is obtain information about the potential or aptitude of advanced clones for diverse end-uses, ranging from fresh consumption to processed products. The interpretation of these evaluations provides important information to guide potato breeding and selection programs, as well as for the recommendation of varieties for specific uses. Procedures for determining chipping quality, reducing sugar content, and percentage of oil content have been standardized to document processing quality of advanced clones.
"

	uri <- "doi:10.21223/P3/CURJUF"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "chipping_color_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	f <- ff[grepl("chipping quality Data", basename(ff))]
	x <- readxl::read_excel(f, col_names=FALSE, .name_repair="minimal")
	x <- x[-c(1, 2, 3), ]
	num <- function(v) {
		v <- trimws(as.character(v))
		ok <- grepl("^-?[0-9.]+$", v)
		v[!ok] <- NA_character_
		as.numeric(v)
	}
	variety <- trimws(as.character(x[[2]]))
	piece <- function(env, color, sugar, oil) {
		data.frame(
			variety = variety,
			treatment = variety,
			environment_ = env,
			chipping_color_ = num(x[[color]]),
			reducing_sugar_mg_dl_ = if (is.na(sugar)) NA_real_ else num(x[[sugar]]),
			oil_absorption_pct_ = num(x[[oil]])
		)
	}
	d <- rbind(
		piece("lowland winter", 4, NA, 16),
		piece("lowland spring", 5, NA, 17),
		piece("highland dry", 7, 12, 19),
		piece("highland rain", 8, 13, 20)
	)
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]
	d <- d[!(is.na(d$chipping_color_) & is.na(d$reducing_sugar_mg_dl_) & is.na(d$oil_absorption_pct_)), ]
	d$crop <- "potato"
	d$country <- "Peru"
	d$trial_id <- paste("chipping", d$variety, d$environment_, seq_len(nrow(d)), sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
