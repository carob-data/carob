# R script for "carob"
# license: GPL (>=3)

## NOTES
# Crossing block of Ethiopian sorghum landraces and elite lines. The title places the work in Ethiopia. Sheet2 is the list that is read.
# Name is the variety. Alternate name is stored only when it differs from Name. Region, zone, and woreda are the collection origin. Rows with no origin are kept.
# Diversity group and sterility group are the codes in the sheet (sterility is B or R). The sheet does not say which rows are landraces and which are elite lines.
# There are no yield, date, or coordinate columns.

## ISSUES
# write_files: missing variables: yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: diversity_group_, sterility_group_. duplicate coordinates: fewer coordinates than locations. NA detected: latitude, longitude.

## NEW VARIABLES
# diversity_group_: diversity group code on the crossing-block sheet.
# sterility_group_: sterility group, B or R.

## NOT PROCESSED
# Sheet1. Sterility reaction with tester TX623A, source institute, and a second copy of the name list laid out beside it.
# Sheet3 is empty.


carob_script <- function(path) {

"
A crossing block for determining heterotic pools among Ethiopian sorghum landraces

List of selected Ethiopian sorghum landraces and elite lines assembled as a crossing block to determine heterotic pools in the SMIL Phase II project. The dataset records the landrace names, their collection origin (region, zone, woreda) and diversity/sterility group; it does not contain agronomic measurements.
"

	uri <- "doi:10.7910/DVN/DG0EQ2"
	group <- "genotypes"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = "SMIL",
		design = NA,
		data_type = "compilation",
		treatment_vars = "none",
		response_vars = "none",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Crossing block for Ethiopian sorghum landraces.xlsx"]
	r <- carobiner::read.excel(f, sheet="Sheet2", skip=3)

	blank <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		x
	}
	variety <- blank(r$Name)
	alt <- blank(r[["Alternate name"]])
	same <- !is.na(alt) & !is.na(variety) & alt == variety
	alt[same] <- NA_character_

	d <- data.frame(
		country = "Ethiopia",
		adm1 = blank(r$Region),
		adm2 = blank(r$Zone),
		adm3 = blank(r[["Woreda/District"]]),
		latitude = NA_real_,
		longitude = NA_real_,
		geo_from_source = FALSE,
		trial_id = "SMIL_sorghum_crossing_block",
		variety = variety,
		variety_alt = alt,
		diversity_group_ = blank(r[["Diversity group"]]),
		sterility_group_ = blank(r[["Sterility group"]]),
		crop = "sorghum",
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA
	)
	d <- d[!is.na(d$variety), ]
	d$plot_id <- as.character(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
