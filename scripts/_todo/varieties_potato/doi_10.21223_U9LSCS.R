# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has a data dictionary only. The thesis tables were not extracted.
# Files on disk: Data_dictionary.xls, doi_10.21223_U9LSCS.zip.
# No data table was written.

carob_script <- function(path) {

"
Replication Data for: Genetic Gains in the content of 'Fe' and 'Zn” diploid potato of three cycles recurrent selection

The purpose of this study was to estimate the genetic gains of three cycles of recurrent selection for the Zn and Fe content in potato diploids, and to determine the heritability and relationships with variables of agronomical importance. The field evaluation was conducted in Acco, Paucartambo, Junin, Peru in a Randomized Complete Block Design, with four repetitions, evaluating 64 genotypes (17=cycle0, 4=cycle1, 21=cycle2, 22=cycle3).
"

	uri <- "doi:10.21223/U9LSCS"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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
