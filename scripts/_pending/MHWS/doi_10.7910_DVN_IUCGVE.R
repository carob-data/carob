# R script for "carob"

carob_script <- function(path) {

"The Myanmar Agricultural Performance Survey (MAPS) is a nationwide phone panel consisting of approximately 5,500 households. The objective of the survey is to collect data on farm characteristics and agricultural assets, area and crops planted, access to inputs, crop marketing, and constraints in agricultural activities. The respondents interviewed are a sub-sample of the Myanmar Household Welfare Survey. A novel sampling strategy in combination with the development of household and population weights allows for estimates that are nationally, regionally, and urban/rural representative. MAPS Round 2 was carried out during the Dry (pre/post monsoon) Season of 2022 that includes farming households from the Myanmar Household Welfare Survey (MHWS)."

	uri <- "doi:10.7910/DVN/IUCGVE"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	return(TRUE)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "IFPRI",
		publication = NA,
		project = "MHWS",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Robert Hijmans",
		carob_date = "2025-04-07"
	)
	
	
	carobiner::write_files(path, meta, d)
}



