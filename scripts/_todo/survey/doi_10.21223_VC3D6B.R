# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Utilization of Participatory Mapping Tools for the Assessment of Crop Diversity in Cumbal, Colombia

A total of 206 farmers in Cumbal, Colombia, were surveyed utilizing ArcGIS Survey 123. Each farmer's field was physically visited, and the 'Participatory Mapping of Farmers' Fields' tool was employed to delineate polygons for each 'shagra', accurately determining their respective areas. This process captured the characteristics of 557 mapped plots across 206 farmers' households. The resultant comprehensive dataset comprises five distinct databases: A) Preliminary Information, B) Shagra Information, C) Land Use Over the Years, and D) Variety Names. These datasets have been individualized and standardized using the common identifiers 'GlobalID Household' and 'GlobalID Shagra'.
"

	uri <- "doi:10.21223/VC3D6B"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP; Agrosavia",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Household; universe: Farmers living in the resguardo Cumbal in the following veredas: Boyera, Cuaical, Cuaspud, Cuetial, Guan, Llano de Piedras, Quilismal, and Tasmag; timeMethod: cross-sectional; dataCollector: International Potato Center (CIP) and Agrosavia; samplingProcedure: We followed a snowball sampling approach. For this, we contacted representatives of the shagreros groups to create a list of people with shagras that we could interview. After we interviewed each people we asked the interviewees whether they knew about other shagreros in the area. We performed the same exercise until we reached 206 people; targetSampleSize: list(targetSampleActualSize = list(typeName = "targetSampleActualSize", multiple = FALSE, typeClass = "primitive", value = "206")); collectionMode: Draw the polygons for each 'shagra' using the Participatory Mapping of Farmers Fields tool through a table with the ArcGIS Survey123 program",
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
	

	f1 <- ff[basename(ff) == "Dictionary PGIS Colombia.xlsx"]

	r1 <- carobiner::read.excel(f1)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
