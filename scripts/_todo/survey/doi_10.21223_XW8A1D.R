# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Utilization of Participatory Mapping Tools for the Assessment of Crop Diversity in Vietnam

A total of 200 farmers in Vietnam, were surveyed utilizing ArcGIS Survey 123. Each farmer's field was physically visited, and the 'Participatory Mapping of Farmers' Fields' tool was employed to delineate polygons for each 'parcel', accurately determining their respective areas. This process captured the characteristics of 773 mapped plots across 200 farmers' households. The resultant comprehensive dataset comprises five distinct databases: A) Preliminary Information, B) Plot Information, C) Plot and Land Use, D) Crop Information, E) Variety Names. These datasets have been individualized and standardized using the common identifiers 'GlobalID Household' and 'GlobalID Plot'.
"

	uri <- "doi:10.21223/XW8A1D"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP; Plant Resources Center",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Household; universe: Farming households in Sa Pa (Son La province) and Mai Son (Lao Cai province); dataCollector: Cross-sectional; samplingProcedure: The survey was conducted in the four communes in the northwest mountainous region of Vietnam targeted by the CGIAR Nature Positive Solutions initiative: Mường Hoa and Thanh Bình (Sa Pa district, Lao Cai province) and Cò Nòi and Chiềng Lương (Mai Son district, Son La province). Two villages were purposively selected in each of the target communes (totaling eight villages: Thao Hong Den, Hoa Su Pan 1, Lech Dao, Kim, Tan Thao, Mon, Y Luong, and Mon 2). These villages were identified in consultation with local authorities as the ones hosting high level of biological diversity, especially for the four crops prioritized by the initiative: banana, taro, pumpkin and H’Mong mustard. In each of the eight target villages, twenty-five households were randomly selected from available lists (total target sample: 200 HHs) and interviewed in local languages.; targetSampleSize: list(targetSampleActualSize = list(typeName = "targetSampleActualSize", multiple = FALSE, typeClass = "primitive", value = "200")); collectionMode: Surveys",
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
	

	f1 <- ff[basename(ff) == "Dictionary PGIS Vietnam VNM1.xlsx"]

	r1 <- carobiner::read.excel(f1)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
