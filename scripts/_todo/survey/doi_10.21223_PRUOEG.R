# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Monitoreo del piloto de intervención Papas “Puño de Hierro” Distribución y uso de semilla

Through a gender-responsive pilot intervention, the project Anemia Zero is contributing to improve food security, nutrition, gender equality, and empowerment in potato-based production systems in high Andean regions of Peru (Curgos and Julcan Districts, Province Sanchez Carrion, Department of La Libertad) and to scale agricultural and nutrition innovations in coordination with the social protection programs (SPP) Juntos and Cuna Mas (both overseen by the Ministry of Development and Social Inclusion (MIDIS), as well as with other stakeholders. This survey aims to measure the perception and preliminary adoption of biofortified potato clones and varieties with iron. The survey corresponds to the following sections: (1) General information, (2) Informed consent, (3) potato planting information, (4) potato harvesting information, (5) biofortified seed distribution, (6) biofortified potato consumption, (7) general comments.
"

	uri <- "doi:10.21223/PRUOEG"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIP; Pataz Association - AP",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Smallholder potato producing households; universe: Potato producing households in Sánchez Carrión; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: We purposively selected three provinces in  Sanchez Carrión, in northern Peru. We randomly selected 46 smallholder potato-producing households from the 142 households in the intervention area of Sánchez Carrión that received biofortified clones and varieties.; collectionMode: Enumerators visited selected random households in each village and collected the information required in the 5-page questionnaire with 40 questions in 7 sections including an informed consent using Microsoft Form survey template.; researchInstrument: Structured household survey questionnaire",
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
	

	f1 <- ff[basename(ff) == "01_DataSet_Papas Puño de Hierro La Libertad_2021.xlsx"]
	f2 <- ff[basename(ff) == "02_Datadictionary_PPH La Libertad_2021.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="PPH La Libertad")
	r2b <- carobiner::read.excel(f2, sheet="Hoja1")
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
