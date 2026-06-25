# R script for "carob"

# RH send inquiry about data to authors. Awaiting answer.

carob_script <- function(path) {

"The integrated BEM and e-Agrology dataset comprises historical data from 2012 to 2022, specifically collected from the Bajío region in Mexico, which includes the states of Guanajuato, Jalisco, Michoacan, and Queretaro. This dataset offers comprehensive information on farmers' field activities, plot details, and specific characteristics of various crops, organized into nearly five hundred variables that cover all stages of the agronomic cycle. By making this data available to the community, it provides valuable insights that can enhance knowledge dissemination and support farmers in refining their production practices."

	uri <- "hdl:11529/10549106"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = NA,
		carob_contributor = "Robert Hijmans",
		carob_effort = NA,
		carob_date = "2024-09-2024"
	)
	
	f <- ff[basename(ff) == "2.-Sowing_harvest_yields_2012-2022_BAJIO.xlsx"]
	r <- carobiner::read.excel(f, col_types="text", fix_names=T)
	names(r) <- tolower(names(r))
	
return(TRUE)
	carobiner::write_files(path, meta, d)
}
