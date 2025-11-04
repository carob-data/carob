# R script for "carob"

carob_script <- function(path) {

"This study contains grain yield data collected from IRRI's long term continuous cropping experiment, year"

	uri <- "doi:10.7910/DVN/GX0J15"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "IRRI",
		publication = NA,
		project = "IRRI LTCCE",
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "season;variety_code;N_fertilizer",
		carob_contributor = "Robert Hijmans",
		carob_date = "2024-07-13"
	)
	

	irri_lte <- carobiner::get_function("IRRI_LTE", path, group)
	d <- irri_lte(ff)

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}
