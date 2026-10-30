# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: GCBC midline household survey on native potato and mashua diversity, climate events, food security and social capital in Peru and Ecuador (2026)

This dataset contains the midline household survey of the project 'Andean Crop Diversity for Climate Change' (GCBC), carried out in 2026 with 110 farming households that grow native potato and/or mashua in three Andean sites: Cotopaxi and Chimborazo, Ecuador (n = 47); Quilcas, Junín, Peru (n = 37); and the Potato Park, Pisac, Cusco, Peru (n = 26). Households are classified by their level of involvement with the project: P, households conducting participatory trial plots; M, households taking part in evaluations, field days, meetings or seed exchanges; and N, non-participant households of the same or neighbouring communities (P 21, M 44, N 45). The questionnaire covers household composition and income sources, land and fields planted with potato and mashua (area, tenure, irrigation), livestock (Peru only), the native potato and mashua varieties named by the household (up to five per crop), eight climate events in the last 12 months with their severity and impact on crops, the coping strategies used, household dietary diversity (HDDS, 10 food groups with frequency and source), months of food scarcity, membership in organizations, community activities and support networks, and training demand. The data are provided in three linked files: households (one row per household, 207 variables), varieties (one row per variety named) and fields (one row per field), linked by Household_ID. Two questionnaire families were used (EC-June in Ecuador and Quilcas; Peru-v2 in Cusco and five Quilcas households) and were harmonized to a common core; derived indices (HDDS score, IRA_10 adaptive response index, organizational social capital, tropical livestock units) are recalculated with the same rule for all sites. Missing values are not imputed; the data dictionary explains, for each variable and site, why data are missing (for example, HDDS in Cusco is pending field recapture and some local land units have no conversion factor to hectares). The sample is a non-probability (convenience, snowball) sample and is not statistically representative of the communities; the data are intended for descriptive and between-group comparisons and as the midline of a planned endline round. Personal identifiers (names, GPS coordinates) have been removed.
"

	uri <- "doi:10.21223/P3/BFZQPR"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP; Yanapai; Asociación ANDES; Fundación EkoRural",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Household; universe: Farming households growing native potato and/or mashua in the project communities of each site, classified by their level of participation in project activities (P, M, N).; timeMethod: Cross-sectional (midline round of a pre/post design; endline planned); dataCollector: See Contributor (role Data Collector) and Data Collection Situation; samplingProcedure: Non-probability sample. In all three sites, households were selected by convenience through snowball referral, within the logistical limits of the project, following the unit-of-analysis criteria of the field protocol (FEB2026): farming households growing native potato and/or mashua in the project communities, classified by participation level. Participation group was assigned mainly from the local partner's participation records (trial registry and activity attendance) and cross-checked with the respondent's answer in the interview. P = households conducting project trial plots; M = households attending at least one participatory project activity (evaluations, field days, seed exchanges) in the last 12 months; N = farming households of the same or adjacent communities with no participation in project activities. Respondent: household head or spouse, preferably the person deciding on agricultural production and variety management; no respondents under 18. The sample is not statistically representative of the communities; results support descriptive and between-group comparisons, not population inference. Ecuador: 47 (P 7, M 24, N 16) Quilcas: 37 (P 6, M 11, N 20), including 5 P households interviewed in April 2026 Cusco: 26 (P 8, M 9, N 9); collectionMode: Face-to-face interview in Spanish, with oral clarification in Quechua/Kichwa by the enumerator when needed; tablet (KoboToolbox) in Ecuador and Quilcas, paper in Cusco; dataCollectionSituation: Ecuador: Survey led by CIP (I. Navarrete) with Fundación EkoRural (local coordination: R. Borja, P. Oyarzún); interviews conducted by paid enumerators. Quilcas: Survey coordinated by CIP with Grupo Yanapai, the local partner that has worked with these communities since earlier participatory research phases; interviews conducted face to face with KoboToolbox by a paid enumerator from Quilcas recruited through Yanapai. The five April-2026 households were interviewed by a CIP intern with an earlier version of the questionnaire (Peru-v2 instrument family). Cusco: Survey coordinated by CIP with Asociación ANDES; interviews conducted by arariwas (community technicians of the Potato Park). Some arariwas are themselves members of participant (P) households, so interviewers and respondents belong to the same community networks.",
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
	

	f1 <- ff[basename(ff) == "GCBC_Midline_data_dictionary.xlsx"]

	r1 <- carobiner::read.excel(f1)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
