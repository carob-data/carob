# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Rejected: no farm, field or plot records.
# "Top 75% varieties per study year, Central Luzon.xlsx" has variety-level aggregates per survey year (and season): rice area, number of farmers, % of area. These are derived from the Central Luzon Loop Survey (farm surveys 1966-2012), so they duplicate (in aggregated form) the Loop survey data. The ave_yield column and the trait columns come from the varietal release database, not from the farms.
# "Varietal release database, Philippines (1961-2013).xlsx" lists released varieties with traits (maturity, plant height, amylose, milling recovery, grain length, chalkiness, pest and disease reactions) and an average yield from the National Cooperative Trials, i.e. one mean value per variety without trial locations or years.
# A human could decide to use the release database as variety metadata (e.g. variety release year and MV group), but it does not fit the carob record structure.


carob_script <- function(path) {

"
Farmer's preference for rice traits: Insights from farm surveys in Central Luzon, Philippines, 1966-2012

Many modern rice varieties (MVs) have been released but only a few have been widely adopted by farmers. To understand farmers' preferences, we characterized MVs released in the Philippines from 1966 to 2013 and identified important characteristics of the varieties that were widely adopted in Central Luzon using farm surveys conducted in 1966–2012. We found that farmers adopt MVs that are high yielding, mature faster, and have long and slender grains, high milling recovery, and intermediate amylose content. The amylose content of adopted varieties has been declining, suggesting value in developing softer rice. To have a high potential for adoption, new MVs should have characteristics within the ranges of values observed for the adopted MVs. In addition, new MVs should have higher head rice recovery, less chalky grains, and better resistance to pests and diseases. Most MVs released in 2005–2013 compared poorly in these three traits. To reduce the risk of severe outbreaks, broad spectrum resistance should be incorporated into new MVs. This analysis of five decades of farm surveys provides insights into the varietal characteristics preferred by farmers which could contribute to the establishment of a product profile for developing improved MVs that are more targeted and, hence, would have high potential for adoption by farmers in Central Luzon and similar areas. We recommend a similar analysis be done in other major rice growing regions to aid the development of MVs that are more responsive to farmers' needs and preferences.
"

	uri <- "doi:10.7910/DVN/OA6XHO"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "IRRI",
		publication = NA,
		project = "Central Luzon Loop Survey",
		design = NA,
		data_type = "compilation",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 0,
		carob_effort = 0.2
	)

	NULL
}
