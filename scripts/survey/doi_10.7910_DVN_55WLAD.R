# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication Data for: Increasing paddy yields and improving farm management: results from participatory experiments with good agricultural practices (GAP) in Tanzania

Rice is an increasingly important commodity in sub-Saharan Africa. In Tanzania, the rice yield gap is as high as 87%, due to a combination of production constraints and sub-optimal crop management. Reducing this yield gap may be partly achieved through the introduction and dissemination of good agricultural practices (GAP). We conducted 18 farmer-managed on-farm trials in Tanzania, to test a set of GAP components against conventional farmers' practices (FP) for two consecutive growing seasons in 2013 and 2014. The objectives were: (1) to understand farmers' capabilities in implementing GAP; (2) to acquire better insights into the merits, relevance and suitability of individual GAP components; and (3) to provide a case study showing that exposure to good practices combined with the farmers' own experimentations can serve to improve and, trigger a positive change in the participating farmers' crop management. Compared to the farmers' own practices, average yield increases of 1 t paddy ha−1 in 2013 and 2.7 t ha−1 in 2014 were achieved when following GAP. These yield advantages were mainly obtained by a higher panicle number, improved harvest index and improved weed control. Farmers experienced difficulties with land levelling, planting or sowing in lines and using rotary weeders, but they were convinced that these technologies are important to boost their rice yields. The case of Tanzania shows that paddy yields can be substantially improved by GAP and that adoption of GAP by smallholder rice farmers can be triggered by stimulating experimentations with such practices on their own farms.
"

	uri <- "doi:10.7910/DVN/55WLAD"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = "",
		project = NA,
		carob_date = "2026-10-24",
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
	

	f1 <- ff[basename(ff) == "Dictionary.xls"]
	f2 <- ff[basename(ff) == "Questionnaire1_2013.xls"]
	f3 <- ff[basename(ff) == "Questionnaire2_2013.xls"]
	f4 <- ff[basename(ff) == "Questionnaire3_2014.xls"]
	f5 <- ff[basename(ff) == "Questionnaire4_2014.xls"]
	f6 <- ff[basename(ff) == "Questionnaire5_2014.xls"]
	f7 <- ff[basename(ff) == "Questionnaire6_2013-2014.xls"]
	f8 <- ff[basename(ff) == "Trial1_2013.xls"]
	f9 <- ff[basename(ff) == "Trial2_2014.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Dictionary Trials")
	r1b <- carobiner::read.excel(f1, sheet="Dictionary Questionnaire1&3")
	r1c <- carobiner::read.excel(f1, sheet="Dictionary Questionnaire2&4")
	r1d <- carobiner::read.excel(f1, sheet="Dictionary Questionnaire5")
	r1e <- carobiner::read.excel(f1, sheet="Dictionary Questionnaire6")
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)
	r8 <- carobiner::read.excel(f8)
	r9 <- carobiner::read.excel(f9)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
