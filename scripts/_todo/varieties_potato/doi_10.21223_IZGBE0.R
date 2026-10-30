# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has a unique-identifier dictionary only. No plot measurements were extracted.
# Files on disk: DataDictionary_UniqueIdentifier.xls, doi_10.21223_IZGBE0.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Presence of potato psilids and phytoplasms in potato farmers' fields in Ecuador 2018 and 2019

In Ecuador, the emergence of two new pests is causing economical problems and yield reductions. The first one is the potato psilid (Bactericera cockerelli) and the second one is the phytoplasm Candidatus Phytoplasma aurantifolia belonging to the group 16SrII. Due to the fact that it is necessary to design an integrated pest management for both of these pests, the aim of this map was to diagnose the presence of both pests in the country. To do this, we used the information provided by  AGROCALIDAD (Ecuadorian gobernmental agency for control quality) which collected the samples and analyzed them in their laboratories.   This interactive map shows the presence of B.cockerelli and Candidatus phytoplasma aurentifolia  group 16SrII in Ecuador during 2018 and 2019.  We expect that this map could be used to improve our understanding about emerging pests in Ecuador and possible risks to other countries. We kindly acknowledge the support of Agrocalidad who provided the information to map the presence of the pests.
"

	uri <- "doi:10.21223/IZGBE0"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
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
