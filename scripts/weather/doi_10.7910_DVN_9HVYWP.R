# R script for "carob"
# license: GPL (>=3)

# Notes, issues, and columns not processed are in africa_rice_weather() in scripts/weather/_functions.R.

carob_script <- function(path) {

"
Daily weather data for Fanaye (Senegal) from 2013 to 2018

Daily weather data owned or distributed by AfricaRice and available on http://www.mlaxmel.com/weatherdata/query.php
"

	uri <- "doi:10.7910/DVN/9HVYWP"
	group <- "weather"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "AfricaRice",
		carob_contributor = "Robert Hijmans",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.4
	)

	africewth <- carobiner::get_function("africa_rice_weather", path, group)
	out <- africewth(ff)
	carobiner::write_files(path, meta, out$d, wth=out$wth)
}
