# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication data for: Adoption and diffusion of potato varieties in Peru

For more than 40 years CIP has worked with the potato breeding program of the National Institute of Agriculture Innovation (INIA) in Peru to develop new genetic material. More than 34 improved varieties have been released in the country under this collaborative framework. Canchan (released in 1990) and Amarilis (1993) are amongst the most popular. Recently released varieties such as Unica (1998) and Serranita (2006) seem to be increasing their adoption rates. Besides meeting market needs, these varieties have higher yields and reduce production costs, increasing farmers' income and reducing the environmental footprint through lower pesticide use. Estimates of adoption of these varieties have been obtained in the past from expert opinions, or are only partial approximations based on local level studies. Lack of a nationally representative sample has constrained the validity of these estimations and the ability of drawing rigorous conclusions about farmers' drivers of adoption. These estimates and conclusions are a crucial feedback to adjust breeding strategies while developing new varieties. This database collected during the 2013 growing season were conducted from a nationally representative sample of 1098 potato farmers in 120 communities. The objectives of these database were to quantify adoption and diffusion of improved potato varieties in the most representative production regions of Peru; and to identify adoption determinants of improved potato varieties in Peru and estimate their impacts.
"

	uri <- "doi:10.21223/P3/MDNLI6"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Potato producing households; universe: Potato households in Peru; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: The estimation of sample size, the selection of the random sample, and the data cleaning and analysis followed a novel fifth-step optimization method that considered prior information regarding perceived adoption levels, heterogeneity of the sampled population, the desired level of precision in the estimation and the study budget constraints. The data collected in 11 different Departments represents 86% (275,000 hectares) of the total potato area of Peru in 2013.; collectionMode: Six teams of six enumerators and one supervisor per team were formed. Enumerators were undergraduate agricultural science students or recently graduated agronomist or related disciplines. Supervisors were agronomist with survey experience in previous CIP studies in the Andes. Enumerators and supervisors underwent a three-day training on survey enumeration. During training, the objectives of the project, the methodology, and the data collection techniques were covered. In the field, supervisors organized the logistics to enter into communities if needed as well as transportation, and lodging of the team, while the enumerators administered the household surveys. Data collection occurred from late November 2013 to mid July 2013. A hundred and twenty villages were visited in 45 provinces in Peru. In each village, the supervisor would organize the random selection of households. Enumerators would then conduct the survey with the household member who was most knowledgeable about potato production. Each household interview took approximately 40 to 60 minutes to complete.; researchInstrument: Structured household survey. The survey had eight modules. Module 1 described the composition and characteristics of the household. Module 2 gathered information on household social capital and networks. Module 3 asked about land tenancy, land use, crops under production and potato varieties knowledge and information sources, including to rate the importance of different potato traits of the main varieties planted. Module 4 asked respondents to detail their potato production (at varietal level) from planting to harvesting during the 2012-2013 main cropping season as well as constraints faced in potato production. Module 5 details the marketing of the potato at varietal level. Module 6 include questions about housing characteristics, while Module 7 recorded questions about household and farm assets. Finally, Module 8 asked questions about access to agricultural training, access to credits, production constraints.",
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
	

	f1 <- ff[basename(ff) == "Data_dictionary_STC_Survey.xls"]
	f2 <- ff[basename(ff) == "STC_Survey_2017_Febrero09-2018.accdb"]

	r1 <- carobiner::read.excel(f1)
	#r2 <- read.???(f2)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
