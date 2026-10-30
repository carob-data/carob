# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has the household dictionary only. The Vietnam homegarden modules are marked restricted and were not extracted.
# Files on disk: Dictionary_vn_households.xlsx, doi_10.21223_WSNXID.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Household and agrobiodiversity characterization in Son La and Lao Cai, Vietnam

Agrobiodiversity faces a current decline influenced by diverse factors, including climate change and global market dynamics. Recognizing the imperative to monitor agrobiodiversity for conservation, this study sought to characterize 198 households and the agrobiodiversity within farmers' fields. Utilizing ARCGIS Survey 123, surveys were conducted, specifically targeting farmers with homegardens, as they were expected to showcase the highest agrobiodiversity. The resulting comprehensive database encompasses modules on household characteristics (Module A), homegardens (Module B), annual (Module C) and perennial crops (Module D), agrobiodiversity and seed networks (Module E1), agrobiodiversity trends and seed fairs (Module E2), livestock (Module F), food gathering (Module G), local nature-based solutions (Module H), crises, and climate change (Module I). Each module has been individualized, anonymized, and standardized using the 'Global ID' for streamlined aggregation at the household level. The integration of these modules through the 'Global ID' facilitates a holistic understanding. The dataset derived from this survey is anticipated to play a pivotal role in designing more effective conservation initiatives in Son La and Lao Cai, Vietnam.
"

	uri <- "doi:10.21223/WSNXID"
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
		carob_effort = 0.1
	)

	message("No measurement table in the local raw files for ", uri)
	invisible(meta)
}
