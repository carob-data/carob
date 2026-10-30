# R script for "carob"


carob_script <- function(path) {

"The sixth round of the Myanmar Household Welfare Survey (MHWS)–a nationwide phone panel consisting of 12,898 households–was implemented between August, 2023 and November, 2023. The objective of the survey was to collect data on a wide range of household and individual welfare indicators–including wealth, livelihoods, unemployment, food insecurity, diet quality, health shocks, and coping strategies–in a country exceptionally hard hit by conflict, severe economic collapse, and several damaging waves of COVID-19. The respondents interviewed in the MHWS were purposely selected from a large phone database aimed at being representative at the region/state level and urban/rural level in Myanmar. A novel sampling strategy in combination with the development of household and population weights allows for estimates that are nationally, regionally, and urban/rural representative."

	uri <- "doi:10.7910/DVN/CVKOPF"
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



