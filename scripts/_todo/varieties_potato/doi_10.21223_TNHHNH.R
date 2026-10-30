# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_data_Totorani.tab; 03_Field_management_Totorani.tab; 04_Field sketch of a plot Totorani.tab.

carob_script <- function(path) {

"
Dataset for: Evaluation of the Morphological Characterization of Native Potato Varieties in Totorani, Paucartambo - Cusco, 2024

In the Totorani field, located in the province of Paucartambo, Cusco (Lat: -13.34718°, Lon: -71.49765°), 96 native potato varieties were planted using sustainable zero-tillage practices. Agronomic activities included Yapuy (maintenance), Jaimay (first earthing-up), and Qasuy (final maintenance), maintaining traditional agricultural techniques. This experiment focused on evaluating the morphological characteristics of native potatoes, specifically tuber traits (size, shape, skin color) and flower attributes (petal color and morphology). The study highlights the agronomic performance of these varieties and their role in conserving genetic diversity. By improving sustainable agricultural practices, the research aims to support local communities in the Paucartambo region.
"

	uri <- "doi:10.21223/TNHHNH"
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

