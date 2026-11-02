# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Tabular survey file Data_SCASI_ContinuousLearning_2022 (Blurred).xlsx is restricted. Only codebook, questionnaire, and report PDFs downloaded.


carob_script <- function(path) {

"
Scaling Conservation Agriculture-based Sustainable Intensification in Ethiopia (SCASI): Continuous Learning Survey Dataset (2022)

This dataset contains data from the SCASI Continuous Learning Survey conducted in Ethiopia in October 2022 with 281 participating farmers across 40 kebeles in eight districts and four regions. The survey was designed to obtain direct feedback on the opportunities, challenges, and lessons learned from implementing Conservation Agriculture-based Sustainable Intensification (CASI) practices on demonstration plots. It includes information on crop production, intercropping, plot characteristics, mulching practices, land preparation, planting, crop establishment, weeding, fertilizer management, disease and insect pest control, farmers' perceptions of CASI performance, and qualitative feedback from farmers and enumerators.
"

	uri <- "doi:10.71682/10549471"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=0, minor=0,
		publication = NA,
		project = "SCASI",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 0,
		carob_effort = 0.1
	)

	return(FALSE)
}
