# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Soil sampling, lab results, and merged data xlsx files are restricted. Only the codebook and baseline report downloaded.


carob_script <- function(path) {

"
Scaling Conservation Agriculture-based Sustainable Intensification in Ethiopia (SCASI): Baseline Soil Sampling and Soil Health Dataset (2022)

This dataset contains baseline soil sampling survey data and laboratory soil analysis results from 288 soil samples collected under the Scaling Conservation Agriculture-based Sustainable Intensification (SCASI) project in Ethiopia during 2022. It combines plot-level observations, soil sample metadata, and laboratory measurements, including soil pH, Soil Organic Carbon (OC%), and Soil Organic Matter (OM%). The dataset provides baseline soil-health information for monitoring and evaluating the impacts of Conservation Agriculture-based Sustainable Intensification (CASI) interventions over time.
"

	uri <- "doi:10.71682/10549480"
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
