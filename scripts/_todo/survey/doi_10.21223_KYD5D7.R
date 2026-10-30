# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Linea de salida de Conocimientos, Actitudes y Prácticas (CAP) sobre Deficiencia de Hierro (Anemia) en familias productoras de papa en la Sierra de La Libertad (September - October)

Through a gender-responsive pilot intervention, the project Anemia Zero is contributing to improve food security, nutrition, gender equality, and empowerment in potato-based production systems in high Andean regions of Peru (Curgos and Julcan Districts, Province Sanchez Carrion, Department of La Libertad) and to scale agricultural and nutrition innovations in coordination with the social protection programs (SPP) Juntos and Cuna Mas (both overseen by the Ministry of Development and Social Inclusion (MIDIS), as well as with other stakeholders. This survey was conducted between September 28th and October 21st, 2021 to evaluate changes in knowledge, attitude and practices corresponding to Anemia among 242 potato producing households (142 households from the intervention group and 100 households from the control group). The survey  presented 75 questions divided in the following sections: (1) Informed consent (2 questions), (A) Socioeconomic information from both parents and children (50 questions), (B) knowledge, attitude and practices corresponding to Anemia (31 questions), (C) crop production aspects (48 questions).
"

	uri <- "doi:10.21223/KYD5D7"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP; Instituto de Investigación Nutricional - IIN; PATAZ",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Smallholder potato producing households; universe: Potato producing households in Julcán, and Sánchez Carrión; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: We purposively selected two provinces in Julcán, and Sanchez Carrión, in northern Peru. In each district, we also purposively selected 242 smallholder potato producing households, 142 households in the intervention area of Sánchez Carrión and 100 households in the control group in Julcan. This is the endline survey, and farmers that participated in the baseline survey were interview in this survey.; collectionMode: Enumerators visited selected random households in each village and collected the information required in the 15-page questionnaire with 133 questions in 4 sections including a informed consent using Microsoft Form survey template.; researchInstrument: Structured household survey questionnaire",
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
	

	f1 <- ff[basename(ff) == "01_DataSet_CAP Anemia - La Libertad 2021 - Linea de Salida v2.xlsx"]
	f2 <- ff[basename(ff) == "02_DataDictionary_CAP Anemia - La Libertad 2021 - Linea de Salida v2.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
