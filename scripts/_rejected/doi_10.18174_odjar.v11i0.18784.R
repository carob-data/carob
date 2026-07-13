# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {


"This paper presents data from a two-year study with sugar beet in Germany (2000 and 2001). A total of 27 field trials were conducted in a wide range of environmental conditions including trials with and without irrigation. Sequential harvests were made every 14-28 days between May and October. Root yield and quality, leaf yield, leaf area index and soil water content were determined in four replicates at each harvest date. Soil characteristics were assessed in the field and daily weather data were collected for each trial site. The dataset is suitable for validating sugar beet growth models."

	uri <- "doi:10.18174/odjar.v11i0.18784"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group, 
			files="https://odjar.org/article/view/18784/18279/Dataset.zip")

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "Institute of Sugar Beet Research",
		publication = "doi:10.18174/odjar.v11i0.18784",
		project = NA,
		data_type = "experiment",
		treatment_vars = "irrigation",
		response_vars = "yield", 
		carob_completion = 0,
		carob_effort = 0,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-07-13",
		notes = NA, 
		design = NA
	)
	carobiner::write_files(path, meta, d)
}