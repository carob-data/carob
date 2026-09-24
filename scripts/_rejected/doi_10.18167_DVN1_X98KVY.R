# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Weed surveys in food crops and irrigated rice in the Zonmon region, Benin (2013)

Characterization of weed communities of food crops (groundnut, maize, sweet potato, cassava, fallow land) and irrigated rice in the Zonmon region, Benin in 2013. Phytoecological study with abundance scale 1-9. This dataset covers 26 plant surveys for 86 weed species.
"

	uri <- "doi:10.18167/DVN1/X98KVY"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIRAD, UR (AIDA), Réunion; CIRAD, UMR (AMAP), France; CIRAD, UR (AIDA), Réunion / Université Grenoble Alpes (Master MIASHS/SSD)",
		publication = NA,
		project = NA,
		carob_date = "2026-09-24",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Blessing Dzuda",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "BEN-MAR-2013-DIV-AD.xlsx"]
	f2 <- ff[basename(ff) == "BEN-MAR-2013-DIV-AD-DOC.txt"]
	f3 <- ff[basename(ff) == "BEN-MAR-2013-DIV-AD-FLO.txt"]
	f4 <- ff[basename(ff) == "BEN-MAR-2013-DIV-FAC.txt"]
	f5 <- ff[basename(ff) == "BEN-MAR-2013-DIV-PA-FLO.txt"]

	r1a <- carobiner::read.excel(f1, sheet="Méta données")
	r1b <- carobiner::read.excel(f1, sheet="Facteurs")
	r1c <- carobiner::read.excel(f1, sheet="Floristique")
	r1d <- carobiner::read.excel(f1, sheet="parcelles chem p100")
	#r2 <- read.???(f2)
	#r3 <- read.???(f3)
	#r4 <- read.???(f4)
	#r5 <- read.???(f5)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
