# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Impacto Productivo y Social del Covid-19 en Productores de papa de la Sierra Norte del Perú en 2021

During 2020, the challenges created by the COVID-19 health crisis were critical and household livelihood were affected and created challenges for everybody. In order to understand the extent of the crisis, challenges and measures taken to overcome the crisis, we conducted a mid-term survey to understand how COVID-19 has affected rural families. The survey corresponds to the following sections: (1) General information, (2) Informed consent, (3) Crop production, (4) COVID-19 knowledge, (5) mitigation measures, (6) migration and health services, (7) General comments, (8) Farewell.
"

	uri <- "doi:10.21223/PREYUF"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP; Pataz Association -AP",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Smallholder potato producing households; universe: Potato producing households in Julcán, Pataz and Sánchez Carrión; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: We purposively selected three provinces in Julcán, Pataz and Sanchez Carrión, in northern Peru. In each district, we also purposively selected 74 smallholder potato producing households that participated in the first COVID-19 survey; collectionMode: Enumerators visited selected random households in each village and collected the information required in the 9-page questionnaire and 8 sections including a informed consent using Microsoft Form survey template.; researchInstrument: Structured household survey questionnaire",
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
	

	f1 <- ff[basename(ff) == "01_DataSet_COVID-19_La Libertad_2021.xlsx"]
	f2 <- ff[basename(ff) == "02_Datadictionary_Segunda Encuesta COVID La Libertad_2021.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
