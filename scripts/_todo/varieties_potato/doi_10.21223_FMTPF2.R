# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_data_Tintec.tab; 03_Field_management_Tintec.xlsx; 04_Field sketch of a plot Tintec.tab.

carob_script <- function(path) {

"
Dataset for: Evaluation of the Morphological Characterization of Native Potato Varieties in Tintec, Paucartambo - Cusco, 2024

In the Tintec field, situated in the province of Paucartambo, Cusco, 112 native potato varieties were cultivated under zero-tillage farming systems. The agronomic management activities included Yapuy (maintenance), Jaimay (first earthing-up), and Qasuy (final maintenance), incorporating traditional farming knowledge to promote sustainability. The experiment evaluated the morphological traits of native potato varieties, focusing on tuber characteristics (size, shape, and skin color) and flower features (petal color and structure). This study emphasizes assessing the agronomic performance of these varieties while conserving genetic diversity and improving agricultural systems in the Paucartambo region.
"

	uri <- "doi:10.21223/FMTPF2"
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

