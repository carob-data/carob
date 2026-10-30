# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Health crisis and quarantine measures in Peru: Effects on livelihoods of potato farmers: Stakeholders survey

International Potato Center (CIP) and partners collaborated to develop strategies to improve welfare and nutrition of Andean potato producers in La Libertad, Juinin and Huancavelica. COVID-19 pandemic affected those interventions and there was a need to understand the project implication of COVID-19 measurements in production, consumption and commercialization of potatoes in those settings. A stakeholder survey from Andean potato producing areas was conducted from June to July 2020. The sample size includes 44 stakeholders located in Andean communities from 8 provinces and 3 Departments. The survey covered the four major domains: food security, labor availability, farm system resilience, and agricultural systems connectivity.
"

	uri <- "doi:10.21223/46KVFG"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Potato producing households; universe: Potato households in  CIP intervention areas in Peru; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: We identified 44 stakeholders in the three regions where the International Potato Center (CIP) and local partners were conducting interventions before the pandemic. We used the snowball technique to identified key stakeholders to provide information to the effects of the pandemic. We include stakeholders from local government, ministry of agriculture, social programs, community organizations, and health centers.; collectionMode: The survey was primarily conducted by telephone to reduce risk of infection to stakeholders and enumerators.; researchInstrument: Structured household survey questionnaire",
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
	

	f1 <- ff[basename(ff) == "Diccionario_Encuesta_Autoridades.xlsx"]
	f2 <- ff[basename(ff) == "Encuesta Autoridades - La Libertad - Junin - Huancavelica - COVID-19.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="Autoridades")
	r2b <- carobiner::read.excel(f2, sheet="Dictionary")
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
