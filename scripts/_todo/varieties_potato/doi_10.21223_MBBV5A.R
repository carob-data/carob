# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are the 2015 summary in 4030_Data.xlsx. The header is two rows. Accession_Number, FeDw, ZnDW, DM, TTWPL, NMTP.
# TTWPL in CIP field books is kilograms per plot. It is not converted to kg/ha. Plot area is not in this summary.
# FeDw, ZnDW, and DM are stored as recorded. The summary header does not print the unit.
# Country Rwanda is in the dataset title. No coordinates are published.

## ISSUES
# Sites need georeferencing.
# No fertilizer or dates. The 2016 workbooks are planting lists without yield.
# write_files: ; yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: fe_recorded_, zn_recorded_, dm_recorded_, ttwpl_kg_.

## NOT PROCESSED
# 4030_Kinigi_2016_.xlsx and 4030_Sigira_2016.xlsx: block, plot, and clone lists without a yield.
# Summary sheet "Summary 3 sites 2015" if it repeats the first sheet.
# NMTP: tuber count, not a density.

## NEW VARIABLES
# fe_recorded_: FeDw as recorded
# zn_recorded_: ZnDW as recorded
# dm_recorded_: DM as recorded
# ttwpl_kg_: TTWPL, treated as kg/plot from the CIP field-book name, not converted

carob_script <- function(path) {

"
Dataset for: 100 promising diploid clones from biofortification cycle III selected and assessed in Rwanda

Summary of mineral content for Iron, Zinc and yield parameters over 3 sites in Rwanda, 2015.
"

	uri <- "doi:10.21223/MBBV5A"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "fe_recorded_;ttwpl_kg_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "4030_Data.xlsx"]
	x <- as.data.frame(carobiner::read.excel(f, sheet=1, col_names=FALSE))
	lab <- trimws(as.character(x[[2]]))
	i <- which(lab == "Accession_Number")[1]
	y <- x[(i + 1):nrow(x), ]
	keep <- !is.na(y[[2]]) & nzchar(trimws(as.character(y[[2]])))
	y <- y[keep, ]
	num <- function(v) as.numeric(trimws(as.character(v)))
	d <- data.frame(
		crop = "potato",
		variety = trimws(as.character(y[[2]])),
		fe_recorded_ = num(y[[3]]),
		zn_recorded_ = num(y[[4]]),
		dm_recorded_ = num(y[[5]]),
		ttwpl_kg_ = num(y[[6]]),
		country = "Rwanda",
		trial_id = "Rwanda_2015",
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA
	)
	d$treatment <- d$variety
	d$plot_id <- as.character(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
