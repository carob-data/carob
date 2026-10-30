# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Screenhouse SPVD evaluations of 53 unique accessions of sweetpotato crop wild relative's species

A total of 53 accessions were evaluated for sweetpotato virus disease resistance. Virus carriers with SPVD were grafted on plants of parental accessions used for the crossing study. After four weeks of growth, plants were cut below the grafted node removing the virus carrier. Subsequently, plants were left to re-grow for another four weeks. On completion of the re-growth period plant samples were subjected to ELISA screening. Genotypes that were negative (no virus detected) for SPCSV or both virus were re-grafted with infected material and tested again for resistance. A negative control consisting of five I. trifida plants were grafted with healthy I. batatas (cv. Beauregard) scions which were tested negative. Scions of plants of those accessions that remained resistant were grafted on I. setosa as indicator plant to validate results.
"

	uri <- "doi:10.21223/MCB28H"
	group <- "varieties_other"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIP",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = NA,
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
	

	f1 <- ff[basename(ff) == "CIPVIR_2018_2020.csv"]
	f2 <- ff[basename(ff) == "DataDictionary_ElementDescription.csv"]
	f3 <- ff[basename(ff) == "DataDictionary_Introduction.csv"]
	f4 <- ff[basename(ff) == "DataDictionary_UniqueIdentifier.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
