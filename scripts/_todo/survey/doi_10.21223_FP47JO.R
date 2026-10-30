# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Agricultural Resilience through roots and tubers in typhoon prone regions in Philippines

Root and tuber crops (RTC) play an important role in diverse Asian agri-food systems and contribute to the household resilience which is the capacity to recover from shocks and stressors, including the ones related to climate change and increasingly frequent extreme whether events like typhoons. The dataset was collected with the objective to perform a quantitative assessment of the contribution of RTCs for enhanced resilience and disaster recovery after typhoons. Data collection was implemented after the super typhoon Ompong hit the Philippines in September 2018. The information collected was divided in seven sections: (0) Location information and consent, (B) Household roster: socio-demograpghic characteristics, (C) Household characteristics and economic activity, (D) Farm characteristics, (E) Typhoon characteristics, (F) Typhoon response: inmediate resilience strategies, and (G) Typhoon response: General strategies.
"

	uri <- "doi:10.21223/FP47JO"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP; Department of Environment and Natural Resources, Philippines",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Roots and tubers producing households; universe: Households participating in INREMP activities in Apayao and Kalinga provinces, in Luzon region.; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: In CAR region of Luzon, Philippines, we selected two provinces, Apayao and Kalinga. In both provinces, we further purposively selected purposively five municipalities on the basis of their involvement in an IFAD investment project entitled “Integrated Natural Resources and Environmental Management Project” (INREMP) which was implemented by the Philippine Department of Environment and Natural Resources (DENR). In Apayao, the municipalities of Conner and Kabugao were selected; in Kalinga, Pinukpuk, Tabuk, and Tanudan were selected.
Within the sampled municipalities, 22 barangays – the smallest administrative division in the Philippines – were randomly selected from barangay lists with INREMP activities. From existing household lists in each barangay, 20 households per barangay were randomly selected. Overall, a total of 423 households were included in the study.; collectionMode: Paper-based; researchInstrument: Structured household survey questionnaire",
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
	

	f1 <- ff[basename(ff) == "DataDictionary_Typhoon Philippines.xlsx"]
	f2 <- ff[basename(ff) == "DataSet_Typhoon Philippines.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Section 0")
	r1b <- carobiner::read.excel(f1, sheet="Section B")
	r1c <- carobiner::read.excel(f1, sheet="Section C")
	r1d <- carobiner::read.excel(f1, sheet="Section D")
	r1e <- carobiner::read.excel(f1, sheet="Section E")
	r1f <- carobiner::read.excel(f1, sheet="Section F")
	r1g <- carobiner::read.excel(f1, sheet="Section G")
	r2a <- carobiner::read.excel(f2, sheet="Codes")
	r2b <- carobiner::read.excel(f2, sheet="Section 0")
	r2c <- carobiner::read.excel(f2, sheet="Section B")
	r2d <- carobiner::read.excel(f2, sheet="Section C")
	r2e <- carobiner::read.excel(f2, sheet="Section D")
	r2f <- carobiner::read.excel(f2, sheet="Section E")
	r2g <- carobiner::read.excel(f2, sheet="Section F")
	r2h <- carobiner::read.excel(f2, sheet="Section G")
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
