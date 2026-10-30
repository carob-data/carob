# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has a dictionary and the dataset metadata. The measurement workbook is marked restricted and was not extracted.
# Files on disk: Data_dictionary_organoleptic.xls, doi_10.21223_F5JJZZ.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Potential new bio-fortified potato varieties high in iron and zinc to contribute to food and nutrition security in Apurimac, Peru (Organoleptic data).

A select group of 30 tetraploid biofortified clones from cycle I will be evaluated using a farmer participatory variety selection (PVS) in the highland of Apurimac during the campaign 2018-2019; yield components, micronutrients content (Fe & Zn) and post-harvest traits will be determinated, with the main purpose to improve the food security and nutrition of families in situation of vulnerability through the identification of 3-5 potential new biofortified potatoes varieties with 25% of micronutrient concentration (Fe) above the local varieties.  Additionally, an organoleptic evaluation was assessed at harvesting to evaluate darkness, flavor, strange flavor and texture after boiling. The number of panelists included men and women for the organoleptic evaluation.
"

	uri <- "doi:10.21223/F5JJZZ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.1
	)

	message("No measurement table in the local raw files for ", uri)
	invisible(meta)
}
