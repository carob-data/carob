# R script for "carob"
# license: GPL (>=3)

## NOTES
# Postharvest survey of small-scale farmers in Ecuador, 2021-2022. Country is Ecuador in the title and in geographic coverage. Province, canton, parish, and locality are columns.
# The crop_name values are maize variety names, and the loss columns are maize insect and rodent losses. crop is maize and crop_name is stored as variety.
# insect_loss_percentage and rodent_loss_percentage are the legend's loss percentages.
# The legend does not give a unit for land_area, crop_area, or altitude. Altitude values are stored as elevation because they fall in the meter range. Area is not stored as hectares.

## ISSUES
# No yield and no coordinates.
# Area units are not stated.
# Loss percentages in the file already run from 0 to 100.
# write_files: missing variables: latitude, longitude. unknown variable: crop_area_recorded_ (crop?). unknown variables: insect_loss_percent_, rodent_loss_percent_, land_area_recorded_.

## NEW VARIABLES
# insect_loss_percent_: percentage of losses caused by insects.
# rodent_loss_percent_: percentage of losses caused by rodents.
# crop_area_recorded_: area planted with the crop, unit not stated in the legend.
# land_area_recorded_: total land area managed, unit not stated in the legend.

## NOT PROCESSED
# Gender of tasks, storage construction, grain use, and seed storage.
# The legend's coded loss-source options.


carob_script <- function(path) {

"
Data for: Postharvest grain system in Ecuador and small-scale farmers' perceptions of gender roles and postharvest loss

INIAP (Instituto Nacional de Investigaciones Agropecuarias) and CIMMYT implemented a postharvest diagnostic in Ecuador to document grain production and postharvest practices among small-scale farmers in Ecuador. The survey was conducted between 2021 and 2022, to characterize production systems, identify determinants of postharvest loss, and understand gendered participation across the agricultural value chain.
"

	uri <- "doi:10.71682/10549370"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		response_vars = "insect_loss_percent_;rodent_loss_percent_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.4
	)

	f <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	r <- carobiner::read.excel(f, sheet="Data")
	num <- function(x) {
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		as.numeric(x)
	}
	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c("NA")] <- NA_character_
		x
	}
	loss_i <- num(r$insect_loss_percentage)
	loss_r <- num(r$rodent_loss_percentage)

	d <- data.frame(
		country = "Ecuador",
		adm1 = chr(r$province),
		adm2 = chr(r$canton),
		adm3 = chr(r$parish),
		location = chr(r$locality),
		hhid = chr(r$ID),
		trial_id = chr(r$ID),
		crop = "maize",
		variety = chr(r$crop_name),
		elevation = num(r$altitude),
		insect_loss_percent_ = loss_i,
		rodent_loss_percent_ = loss_r,
		crop_area_recorded_ = num(r$crop_area),
		land_area_recorded_ = num(r$land_area),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$adm2 <- carobiner::fix_name(d$adm2, "title")
	d$adm3 <- carobiner::fix_name(d$adm3, "title")
	d$location <- carobiner::fix_name(d$location, "title")
	d$variety <- carobiner::fix_name(d$variety, "title")

	carobiner::write_files(path, meta, d)
}
