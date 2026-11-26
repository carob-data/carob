# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are the 2015 3-site summary in 4030_Data.xlsx (Accession_Number, FeDw, ZnDW, DM, TTWPL, NMTP).
# TTWPL in CIP field books is total tuber weight per plant (kg/plant); kept in ttwpl_ since no plot area or density is reported.
# DM (%) is mapped to yield_moisture = 100 - DM; FeDw and ZnDW are stored in tuber_Fe_dw_ and tuber_Zn_dw_.
# Country Rwanda is in the dataset title. Multi-site summary across 3 sites with no coordinates.

## ISSUES
# Multi-site summary across 3 unnamed sites in Rwanda; no coordinates, fertilizer, irrigation, or exact planting/harvest dates (year 2015 only).
# The 2016 workbooks are planting lists without yield.

## NOT PROCESSED
# 4030_Kinigi_2016_.xlsx and 4030_Sigira_2016.xlsx: block, plot, and clone lists without a yield.
# Summary sheet "Summary 3 sites 2015" if it repeats the first sheet.
# NMTP: tuber count, not a density.
# 4030_DataDictionary_ElementDescription.xlsx: documentation or a codebook, not an observation table.
# 4030_DataDictionary_UniqueIdentifier.xlsx: documentation or a codebook, not an observation table.

## NEW VARIABLES
# tuber_Fe_dw_: FeDw as recorded (mg/kg dry weight, unit not printed on summary header).
# tuber_Zn_dw_: ZnDW as recorded (mg/kg dry weight, unit not printed on summary header).
# ttwpl_: TTWPL as recorded (kg/plant).

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
		response_vars = "tuber_Fe_dw_;tuber_Zn_dw_;yield_moisture;ttwpl_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 45,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "4030_Data.xlsx"]
	r <- as.data.frame(carobiner::read.excel(f, sheet=1, col_names=FALSE))
	lab <- trimws(as.character(r[[2]]))
	i <- which(lab == "Accession_Number")[1]
	y <- r[(i + 1):nrow(r), ]
	keep <- !is.na(y[[2]]) & nzchar(trimws(as.character(y[[2]])))
	y <- y[keep, ]
	num <- function(v) as.numeric(trimws(as.character(v)))
	d <- data.frame(
		crop = "potato",
		variety = trimws(as.character(y[[2]])),
		tuber_Fe_dw_ = num(y[[3]]),
		tuber_Zn_dw_ = num(y[[4]]),
		yield_moisture = 100 - num(y[[5]]),
		ttwpl_ = num(y[[6]]),
		country = "Rwanda",
		planting_date = "2015",
		trial_id = "Rwanda_2015",
		on_farm = NA,
		is_survey = FALSE,
		geo_from_source = FALSE,
		irrigated = NA
	)
	d$treatment <- d$variety
	d$plot_id <- as.character(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
