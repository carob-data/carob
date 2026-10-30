# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Household and agrobiodiversity characterization in Cumbal Colombia

Agrobiodiversity is currently on the decline, attributed to various factors such as climate change and the global market. Recognizing the critical need to monitor agrobiodiversity status for effective conservation strategies, this study aimed to characterize households and agrobiodiversity within farmers' fields. A total of 206 farmers in Cumbal, Colombia, were surveyed using ARCGIS Survey 123. The surveyed farmers were specifically those with shagras (homegardens), as they were anticipated to exhibit the highest agrobiodiversity. Additionally, seed custodians, individuals acknowledged by communities for possessing extensive diversity and providing seeds for variety recovery, were included in the survey. As a result, we obtained a comprehensive database covering the following modules: household characteristics (Module A), shagras (Module B), annual (Module C) and perennial crops (Module D), agrobiodiversity and seed exchange (Module E1), conservation trends (Module E2), livestock (Module F), food collection and hunting (Module G), local nature-based solutions (Module H), crises and climate change (Module I), soil (Module J) and water management (Module K), air quality and noise contamination (Module L), landscape value (Module M), and agricultural residues (Module N). These modules have been individualized, anonymized, and standardized using the same 'Global ID' to facilitate the aggregation of the different modules at a household level. We also include here two exploratory databases about the management of the shagras and perennial crops that are called 'Datos_exploratorios_shagras' and 'Datos_exploratorios_diversidad_pastos_cultivados' respectively. These could be integrated with the modules using 'Global ID'. The dataset resulting from this survey is expected to be instrumental in designing more effective conservation initiatives in Cumbal, Colombia.
"

	uri <- "doi:10.21223/GIRW3W"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP; Agrosavia",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Household; universe: Farmers living in the resguardo Cumbal in the following veredas: Boyera, Cuaical, Cuaspud, Cuetial, Guan, Llano de Piedras, Quilismal, and Tasmag; timeMethod: cross-sectional; dataCollector: International Potato Center (CIP) and Agrosavia; samplingProcedure: We followed a snowball sampling approach. For this, we contacted representatives of the shagreros groups to create a list of people with shagras that we could interview.  After we interviewed each people we asked the interviewees whether they knew about other shagreros in the area. We performed the same exercise until we reached 206 people.; targetSampleSize: list(targetSampleActualSize = list(typeName = "targetSampleActualSize", multiple = FALSE, typeClass = "primitive", value = "206")); collectionMode: Use the survey123",
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
	

	f1 <- ff[basename(ff) == "Dictionary.xlsx"]

	r1 <- carobiner::read.excel(f1)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
