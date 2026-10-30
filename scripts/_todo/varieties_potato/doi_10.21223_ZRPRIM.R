# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_PTPVS112018_APURIMAC_ALLAHUA.tab; 01_PTPVS112018_APURIMAC_SACCHUILCA.tab.

carob_script <- function(path) {

"
Dataset for: Potential new bio-fortified potato varieties  high in iron and zinc to contribute to food and nutrition security in Apurimac, Peru (Participatory Varietal Selection).

A select group of 30 tetraploid biofortified clones from cycle I will be evaluated using a farmer participatory variety selection (PVS) in the highland of Apurimac during the campaign 2018-2019; yield components, micronutrients content (Fe & Zn) and post-harvest traits will be determinated, with the main purpose to improve the food security and nutrition of families in situation of vulnerability through the identification of 3-5 potential new biofortified potatoes varieties with 25% of micronutrient concentration (Fe) above the local varieties
"

	uri <- "doi:10.21223/ZRPRIM"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	# No measurement table is available in the raw folder.
	return(FALSE)
}

