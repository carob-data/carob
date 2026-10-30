# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Potato native Miskipapa market survey in 2022

The native potato varieties have been successfully marketed to consumers in Lima - 2022 who wish to support the small farmer and the conservation of biodiversity.  The survey was carried out on 100 consumers of native potatoes of the Miskipapa brand from different districts of Lima.  This dataset included information about the demand of Miskipapa Consumers by gender, Product demand analysis, Percentage of delivery and pickup method, Percentage of distribution by districts, Major districts of distributions in Lima, Number of kilos of native potatoes that consumers buy, and Frequency of purchase of native potatoes.
"

	uri <- "doi:10.21223/CACKFD"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP; Yanapai Group",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: One Consumer of native potatoes of the Miskipapa brand; universe: 100 consumers of native potatoes of the Miskipapa brand from different districts of Lima; dataCollector: Yanapai and International Potato Center; targetSampleSize: list(targetSampleActualSize = list(typeName = "targetSampleActualSize", multiple = FALSE, typeClass = "primitive", value = "100"))",
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Your Name",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "01_Encuesta_sobre_demanda_Miskipapa_2022.xlsx"]
	f2 <- ff[basename(ff) == "02_Diccionario_Encuesta_sobre_demanda_Miskipapa_2022.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
