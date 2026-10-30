# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: 02_dictionary_5Cell_Colombia.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Agrobiodiversity Conservation in Colombia: Utilizing the Five-Cell Method and local farmer

In the Colombian study, specific crop varieties (potatoes, ocas, mashua, and ulluco) were classified and documented in the rural areas of Boyera, Cuaical, Cuetial, Cuaspud, Guan, and Quilismal. The dataset encompasses information from 138 participants, with a participation rate of 55% for women and 44.9% for men. Participants were tasked with assigning local names to these crop varieties. Additionally, farmers were asked to identify and name varieties they cultivated but that were absent from the sample set. These varieties were recorded on cards and integrated into the study. A four-quadrant matrix was laid out on the ground, positioned at the center of all participants, and an extra cell was added next to the matrix for lost/extinct varieties. The significance of each cell was explained, and a shared understanding of the terms 'a lot' and 'little' was established within each community. Subsequently, the varieties were placed into the following cells: A) Many households in large areas, B) Many households in small areas, C) Few households in large areas, D) Few households in small areas, E) Lost varieties. The study delved into the primary reasons and uses of conserving these varieties, along with determining agrobiodiversity. Furthermore, the major pests and threats in these rural areas were identified.
"

	uri <- "doi:10.21223/VYGCHT"
	group <- "survey"
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
		carob_effort = 0.15
	)

	# The Dataverse file list marks the observation workbooks restricted.
	# The raw folder only has documentation, so there is no table to standardize.
	stop("No observation file in data/raw/survey/doi_10.21223_VYGCHT. Files present: 02_dictionary_5Cell_Colombia.xlsx")
}

