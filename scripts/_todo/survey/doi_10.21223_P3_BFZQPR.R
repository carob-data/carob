# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: GCBC_Midline_data_dictionary.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: GCBC midline household survey on native potato and mashua diversity, climate events, food security and social capital in Peru and Ecuador (2026)

This dataset contains the midline household survey of the project 'Andean Crop Diversity for Climate Change' (GCBC), carried out in 2026 with 110 farming households that grow native potato and/or mashua in three Andean sites: Cotopaxi and Chimborazo, Ecuador (n = 47); Quilcas, Junín, Peru (n = 37); and the Potato Park, Pisac, Cusco, Peru (n = 26). Households are classified by their level of involvement with the project: P, households conducting participatory trial plots; M, households taking part in evaluations, field days, meetings or seed exchanges; and N, non-participant households of the same or neighbouring communities (P 21, M 44, N 45). The questionnaire covers household composition and income sources, land and fields planted with potato and mashua (area, tenure, irrigation), livestock (Peru only), the native potato and mashua varieties named by the household (up to five per crop), eight climate events in the last 12 months with their severity and impact on crops, the coping strategies used, household dietary diversity (HDDS, 10 food groups with frequency and source), months of food scarcity, membership in organizations, community activities and support networks, and training demand. The data are provided in three linked files: households (one row per household, 207 variables), varieties (one row per variety named) and fields (one row per field), linked by Household_ID. Two questionnaire families were used (EC-June in Ecuador and Quilcas; Peru-v2 in Cusco and five Quilcas households) and were harmonized to a common core; derived indices (HDDS score, IRA_10 adaptive response index, organizational social capital, tropical livestock units) are recalculated with the same rule for all sites. Missing values are not imputed; the data dictionary explains, for each variable and site, why data are missing (for example, HDDS in Cusco is pending field recapture and some local land units have no conversion factor to hectares). The sample is a non-probability (convenience, snowball) sample and is not statistically representative of the communities; the data are intended for descriptive and between-group comparisons and as the midline of a planned endline round. Personal identifiers (names, GPS coordinates) have been removed.
"

	uri <- "doi:10.21223/P3/BFZQPR"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
	stop("No observation file in data/raw/survey/doi_10.21223_P3_BFZQPR. Files present: GCBC_Midline_data_dictionary.xlsx")
}

