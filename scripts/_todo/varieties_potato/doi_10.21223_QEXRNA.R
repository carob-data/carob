# R script for "carob"
# license: GPL (>=3)

## NOTES
# The measurement files for this record are restricted on Dataverse. The raw folder only has the unrestricted documentation (dictionary and, in some cases, a questionnaire). There are no observation rows to standardize.

## ISSUES
# No data table. The restricted measurement files were not downloaded.
# The script stops before write_files because there is no table.

## NOT PROCESSED
# The dictionary or questionnaire that is on disk describes columns that are not in the downloaded files.


carob_script <- function(path) {

"
Dataset for: Potential new bio-fortified potato varieties high in iron and zinc to contribute to food and nutrition security in Cajamarca, Peru (Organoleptic data).

A select group of 30 tetraploid biofortified clones from cycle I will be evaluated using a farmer participatory variety selection (PVS) in the highland of Cajamarca during the campaign 2018-2019; yield components, micronutrients content (Fe & Zn) and post-harvest traits will be determinated, with the main purpose to improve the food security and nutrition of families in situation of vulnerability through the identification of 3-5 potential new biofortified potatoes varieties with 25% of micronutrient concentration (Fe) above the local varieties.  Additionally, an organoleptic evaluation was assessed at harvesting to evaluate darkness, flavor, strange flavor and texture after boiling. The number of panelists included men and women for the organoleptic evaluation.
"

	uri <- "doi:10.21223/QEXRNA"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	stop("Measurement files are restricted and are not in the raw folder. Only documentation was available, so no data table was built.")
}

