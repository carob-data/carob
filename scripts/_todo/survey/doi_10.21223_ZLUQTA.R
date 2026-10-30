# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot rows are 04_data_yield_F4_ Harvest_Mother.xlsx and 05_data_yield_F5_harvest_Baby.xlsx.
# TTYNA and MTYNA are the CIP fieldbook names for unadjusted total and marketable tuber yield in t/ha. Stored as kg/ha.
# Country Nepal, state Mid-Western, and place Jumla, Chandannath are the geographic coverage.
# Latitude 28.466667 and longitude 81.466667 are the south and west edges of the bounding box. The north and east edges are empty, so this is that corner, not a measured plot coordinate.

## ISSUES
# The dataset is a variety-selection trial filed under the survey group, so a household id is still required and is not in the yield sheets.
# No fertilizer and no planting or harvest date in the yield sheets.
# The bounding box has only one corner.
# write_files: missing variables: carob_contributor; hhid.
# write_files: unknown variables: tuber_class_.

## NOT PROCESSED
# Selection, flowering, dormancy, and organoleptic workbooks.
# Per-plot tuber counts and weights (NTP, NPH, TTWP, and the stand-adjusted TTYA). Plot area is not in the yield sheet.
# PTPV201203_JUMLA.xls if present: the same trial in HIDAP form was not required once the yield sheets had one row per plot.

## NEW VARIABLES
# tuber_class_: mother or baby harvest, as named by the file

carob_script <- function(path) {

"
Dataset for: Development of LB-Resistant Potato Varieties Through Participatory Selection in Jumla, 2013

New LB-resistant potato varieties with improved yield, resistance, and nutritional traits identified and proposed for release through multistakeholder participatory selection (Participatory variety selection of varieties)
"

	uri <- "doi:10.21223/ZLUQTA"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.35
	)

	one <- function(f, tuber_class_) {
		r <- carobiner::read.excel(f)
		data.frame(
			plot_id = paste(tuber_class_, r$PLOT, r$REP, sep="_"),
			rep = as.integer(r$REP),
			variety_code = trimws(as.character(r$INSTN)),
			variety = trimws(as.character(r$INSTN)),
			treatment = trimws(as.character(r$INSTN)),
			tuber_class_ = tuber_class_,
			yield = as.numeric(r$TTYNA) * 1000,
			yield_marketable = as.numeric(r$MTYNA) * 1000,
			stringsAsFactors = FALSE
		)
	}

	f4 <- ff[grepl("F4_", basename(ff)) & grepl("xlsx$", basename(ff))]
	f5 <- ff[grepl("F5_", basename(ff)) & grepl("xlsx$", basename(ff))]
	d <- carobiner::bindr(one(f4, "mother"), one(f5, "baby"))
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	d$country <- "Nepal"
	d$adm1 <- "Mid-Western"
	d$location <- "Jumla"
	d$site <- "Chandannath"
	d$latitude <- 28.466667
	d$longitude <- 81.466667
	d$geo_from_source <- TRUE
	d$geo_uncertainty <- 20000
	d$geo_source <- "Dataverse bounding box, south and west edges only"
	d$trial_id <- "Jumla_2013"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA

	carobiner::write_files(path, meta, d)
}
