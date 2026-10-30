# R script for "carob"
# license: GPL (>=3)

## ISSUES
# All data files are restricted on CIMMYT Dataverse (endline lab analysis, soil sampling xlsx, and the impact report). No tabular files downloaded.


carob_script <- function(path) {

"
Scaling Conservation Agriculture-based Sustainable Intensification in Ethiopia (SCASI): Endline Soil Sampling and Soil Health Dataset (2025)

This dataset combines soil sampling survey data and laboratory soil analysis results collected from 256 plots in Ethiopia under the Scaling Conservation Agriculture-based Sustainable Intensification in Ethiopia (SCASI) project. It includes plot characteristics, cropping history, crop-cut yield data, CASI adoption and management practices, geospatial information, farmers’ perceptions of implementation benefits and challenges, and laboratory measurements of soil pH, Soil Organic Carbon (SOC), Soil Organic Matter (SOM), and Bulk Density (BD). The soil sampling survey was conducted during February–March 2025, followed by laboratory analysis in April 2025. The two components are linked through common soil sample identifiers.
"

	uri <- "doi:10.71682/10549470"
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
