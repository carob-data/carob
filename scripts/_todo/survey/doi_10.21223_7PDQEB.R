# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are sheet 5-Productivity. Country Peru and department La Libertad are in the title.
# previous_year_yield is kept as recorded. The sheet does not give the unit, so it is not converted to kg/ha.
# crop_name papa or potato is stored as potato. Other names are kept.

## ISSUES
# No coordinates.
# The yield unit is not stated.
# write_files: ; geo_from_source, latitude, longitude.
# write_files: unknown variable: previous_yield_.

## NOT PROCESSED
# Farmer, sowing, visit, harvest, manual-activity, and soil workbooks in this dataset. The productivity sheet is the yield record.
# factor_difference and the environmental and management factor columns: coded reasons, not a rate.

## NEW VARIABLES
# previous_yield_: previous year yield as recorded

carob_script <- function(path) {

"
Dataset for: Agronomic log of small potato farmers during the 2021-2022 campaign in the districts of Chugay, Pataz, Pías y Tayabamba in the department of La Libertad - Peru.

This dataset corresponds to the records of small potato farmers carried out during the 2021-2022 campaign in the districts of Chugay, Pataz, Pías, and Tayabamba, in the department of La Libertad - Peru. Corresponding to the different activities in their agricultural plots, such as field preparation, planting, agronomic management, pest and disease management, and harvest. This dataset has been developed considering the e-Agrology platform and using the modules: Farmers-crops-site (n=759), sowing (n=684), visit (n=794), harvest (n=592), productivity (n=592), and soil analysis (n=204).
"

	uri <- "doi:10.21223/7PDQEB"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "previous_yield_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.3
	)

	f <- ff[grepl("Productivity", basename(ff))]
	r <- carobiner::read.excel(f, sheet="5-Productivity")
	crop <- tolower(trimws(as.character(r$crop_name)))
	crop[crop %in% c("papa", "potato", "patata")] <- "potato"

	d <- data.frame(
		hhid = trimws(as.character(r$ID)),
		location = tools::toTitleCase(tolower(trimws(as.character(r$Site)))),
		plot_id = trimws(as.character(r$plot_name)),
		crop = crop,
		previous_yield_ = {
			y <- trimws(as.character(r$previous_year_yield))
			y[!nzchar(y) | y %in% c("NA", ".")] <- NA_character_
			as.numeric(y)
		},
		country = "Peru",
		adm1 = "La Libertad",
		trial_id = "LaLibertad_2021_2022",
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA
	)

	carobiner::write_files(path, meta, d)
}
