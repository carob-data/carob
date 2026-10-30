# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Household and agrobiodiversity characterization in Son La and Lao Cai, Vietnam

Agrobiodiversity faces a current decline influenced by diverse factors, including climate change and global market dynamics. Recognizing the imperative to monitor agrobiodiversity for conservation, this study sought to characterize 198 households and the agrobiodiversity within farmers' fields. Utilizing ARCGIS Survey 123, surveys were conducted, specifically targeting farmers with homegardens, as they were expected to showcase the highest agrobiodiversity. The resulting comprehensive database encompasses modules on household characteristics (Module A), homegardens (Module B), annual (Module C) and perennial crops (Module D), agrobiodiversity and seed networks (Module E1), agrobiodiversity trends and seed fairs (Module E2), livestock (Module F), food gathering (Module G), local nature-based solutions (Module H), crises, and climate change (Module I). Each module has been individualized, anonymized, and standardized using the 'Global ID' for streamlined aggregation at the household level. The integration of these modules through the 'Global ID' facilitates a holistic understanding. The dataset derived from this survey is anticipated to play a pivotal role in designing more effective conservation initiatives in Son La and Lao Cai, Vietnam.
"

	uri <- "doi:10.21223/WSNXID"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP; Plant Resources Center",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Household; universe: Farmers living in Son La and Lao Cai in the following villages: Mòn, Hòa_Sử_Pán, Kim, Lếch_Dao, Mờn_2, Tân_Thảo, Thào Hồng Dến, and Ý_Lường; timeMethod: Cross-sectional; dataCollector: International Potato Center (CIP) and Agrosavia; samplingProcedure: We employed a random sampling approach by collaborating with representatives from each village. Together, we compiled a list of individuals maintaining homegardens. Subsequently, from this list, we randomly selected potential interviewees. The interviews were conducted in the local language.; targetSampleSize: list(targetSampleActualSize = list(typeName = "targetSampleActualSize", multiple = FALSE, typeClass = "primitive", value = "198")); collectionMode: Surveys",
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
	

	f1 <- ff[basename(ff) == "Dictionary_vn_households.xlsx"]

	r1 <- carobiner::read.excel(f1)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
