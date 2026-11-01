# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household rows are Module A - Household Identication.dta.
# country_code is a labelled code. carobiner::read.dta turns 21 into Ghana.
# The file has no column named hhid. The household key is a01-a06 from that module, concatenated.
# Sweetpotato production modules are coded (i07, i10a, and so on) and are not mapped without a codebook.

## ISSUES
# No coordinates.
# No yield. The production modules were not decoded.
# write_files: ; geo_from_source, latitude, longitude.

## NOT PROCESSED
# Modules B through T (rosters, diet, sweetpotato production, sales, assets, and food security). The column names are questionnaire codes.

carob_script <- function(path) {

"
Dataset for: Jump-starting Orange-Fleshed Sweetpotato In West Africa Through Diversified Markets

The Jump-starting project had the goal of sustainable and inclusive market-driven approaches for orange-fleshed sweetpotato (OFSP) to increase incomes, and improve health through consumption of vitamin A rich OFSP, especially in women and children. The project started in 2014 and was implemented in 3 West African countries, namely Burkina Faso, Ghana and Nigeria. This baseline survey data is from Ghana and was conducted with the aim of characterizing the households targeted. It specifically focused on, among others; household demographics, asset ownership, nutrition knowledge and dietary diversity, crops produced, knowledge, attitudes and practices and sources of vines
"

	uri <- "doi:10.21223/HISNEE"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 15,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Module A - Household Identication.dta"]
	r <- carobiner::read.dta(f)
	key <- function(col) trimws(as.character(r[[col]]))
	d <- data.frame(
		hhid = paste(key("a01"), key("a02"), key("a03"), key("a04"), key("a05"), key("a06"), sep="-"),
		country = trimws(as.character(r$country_code)),
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA,
		trial_id = "Ghana_OFSP_baseline"
	)
	carobiner::write_files(path, meta, d)
}
