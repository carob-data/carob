# R script for "carob"
# license: GPL (>=3)

## NOTES
# Clone means of tuber dry matter and specific gravity. read.excel.hdr (skip 2, hdr 2) gives one row per clone. The header rows are the trait, then Lowland Tropic or Highland Tropic, then winter, spring, dry, or rain.
# Dry matter is a percent. yield_moisture is 100 minus that percent. Specific gravity is weight in air divided by (weight in air minus weight in water).
# Average columns and the final Dry Matter % column are summaries of those seasons, so they are not copied.
# Rows kept are clone codes that start with CIP. That is the 319-clone block.
# Pend in the highland dry-matter columns means the value was not recorded.

## ISSUES
# The files and the title do not name a country or a site. Lowland Tropic and Highland Tropic are environment labels, not coordinates.
# The description says the design is an RCBD and that a plot is 2 rows of 10 plants. The sheet has clone means, not plots, and it does not give a row spacing, so plot area is not filled.
# No fertilizer, irrigation, planting date, harvest date, or yield.
# write_files: missing carob_contributor, yield, yield_part, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: environment_, specific_gravity_.

## NOT PROCESSED
# Primary Trait codes (TPS, LB, and the other dictionary labels).
# AVG columns and the closing Dry Matter % column.
# The control block after the second header (Set, Control, CIP Number, Cultivar Name). Those columns do not line up with the clone table: High and Low rows put dry matter in the specific-gravity columns.
# The dictionary workbook.
# Data_Dictionary.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# environment_: lowland winter, lowland spring, highland dry, or highland rain.
# specific_gravity_: tuber specific gravity, unitless.


carob_script <- function(path) {

"
Dataset for: Standard Evaluation Trial  to Assessing solid content of tuber

The primary objective of evaluating post-harvest traits is to obtain information about the potential or aptitude of intermediate and advanced clones for diverse end-uses, ranging from fresh consumption to processed products. The interpretation of these evaluations provides important information to guide potato breeding and selection programs, as well as for the recommendation of varieties for specific uses. Therefore, CIP has adopted standard procedures for determining: specific gravity and dry matter content.
These procedures serve for the evaluation of advanced clonal selections and may be adapted for the determination of parental value for post-harvest parameters. The experimental design is an RCBD with at least two replications. Each plot consists of 2 rows with 10 plants/row. Cultural management of the experimental area should be like that practiced locally for commercial potato production and/or as required for each experimental site. Assessing the solid content of tuber were conducted for 319 clones in 6 sets. The dry matter content and specific gravity were evaluated in 2 different environments: Lowland tropic (winter or spring) and highland tropic (dry or rain).
"

	uri <- "doi:10.21223/P3/H3BHDR"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield_moisture;specific_gravity_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Solid.xls"]
	# skip 2 and hdr 2: trait row, environment row, and the season row used as the readxl name row.
	r <- carobiner::read.excel.hdr(f, sheet="Solid", skip=2, hdr=2, fix_names=FALSE)
	clone <- trimws(as.character(r[[2]]))
	ok <- grepl("^CIP[0-9]", clone)
	r <- r[ok, ]
	clone <- clone[ok]
	set <- as.integer(r[[1]])
	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("Pend", "")] <- NA_character_
		as.numeric(x)
	}
	piece <- function(env, dm, sg) {
		data.frame(
			trial_id = paste0("set", set, "_", gsub(" ", "_", env)),
			crop = "potato",
			variety = clone,
			treatment = clone,
			environment_ = env,
			yield_moisture = 100 - num(dm),
			specific_gravity_ = num(sg),
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}
	# Columns: 4 winter DM, 5 spring DM, 7 highland dry DM, 8 highland rain DM.
	# 11-12 and 14-15 are the specific gravities in that same season order. 6, 9, 10, 13, 16, 17, 18 are averages.
	d <- rbind(
		piece("lowland winter", r[[4]], r[[11]]),
		piece("lowland spring", r[[5]], r[[12]]),
		piece("highland dry", r[[7]], r[[14]]),
		piece("highland rain", r[[8]], r[[15]])
	)
	d <- d[!(is.na(d$yield_moisture) & is.na(d$specific_gravity_)), ]
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
