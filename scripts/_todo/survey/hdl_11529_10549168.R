# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey of wheat plots in Ethiopia, meher season 2020/21. The title and geographic coverage are Ethiopia.
# Plot_GrainYield_Qnt is kilograms from the plot. The variable description says the value is the expected yield when the plot was not harvested. Plot area is recorded in an undefined local unit (LU), so yield is not converted to kg/ha.
# Region, zone, and village are taken from the household sheet and joined on household id.
# The rainfall workbook is a CHIRPS station-distance table, not a weather-station series, and is not processed.

## ISSUES
# Plot area is in local units. kg/ha is not computed.
# The grain quantity mixes harvested and expected yield. The file does not flag which rows are expected.
# write_files: missing variables: latitude, longitude
# write_files: unknown variables: grain_yield_kg_

## NEW VARIABLES
# grain_yield_kg_: grain quantity from the plot, kg, as recorded. This includes expected yield where the plot was not harvested.

## NOT PROCESSED
# Seed, fertilizer, herbicide, labour, prices, and the social-network modules.
# Annual rainfall 2001_2021_Ethiopia_AGG_Revised.xlsx. It records distance from the household to a CHIRPS rainfall pixel, not a station series.
# Plot_Area, because the unit is an undefined local unit.


carob_script <- function(path) {

"
Gender Intentional Wheat Seed Delivery Pathways

Household survey of wheat plots in Ethiopia, with grain quantity recorded in kilograms per plot.
"

	uri <- "hdl:11529/10549168"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "variety",
		response_vars = "grain_yield_kg_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "AGG_HH_Wheat_Survey_2021_Ethiopia.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="2.2.a")
	r2 <- carobiner::read.excel(f, sheet="1")
	key <- match(as.character(r1$HH_Id), as.character(r2$HH_Id))
	variety <- trimws(as.character(r1$Plot_Wheat_Var))
	variety[!nzchar(variety)] <- NA_character_
	variety <- carobiner::fix_name(variety, "title")

	d <- data.frame(
		country = "Ethiopia",
		adm1 = carobiner::fix_name(as.character(r2$Region)[key], "title"),
		adm2 = carobiner::fix_name(as.character(r2$Zone)[key], "title"),
		location = carobiner::fix_name(as.character(r2$Village)[key], "title"),
		trial_id = as.character(r1$HH_Id),
		hhid = as.character(r1$HH_Id),
		plot_id = as.character(r1$Plot_Id),
		crop = "wheat",
		variety = variety,
		treatment = variety,
		yield_part = "seed",
		grain_yield_kg_ = as.numeric(r1$Plot_GrainYield_Qnt),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d <- d[!is.na(d$variety), ]

	carobiner::write_files(path, meta, d)
}
