# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Only 48 of the 223 plots were harvested (one plot each of 48 new clones; in the "original" sheet
#    31 clones are marked "Select"). The other 175 plots, including all 47 plots of the checks
#    (CIP388611.22 Reiche, CIP392797.22 Unica, CIP388676.1), have only plant vigor, flowering and
#    senescence scores and no yield. Because these scores are not processed (NOTES 4), those plots
#    have no other information and are not included.
# 2. The "Installation" sheet says RCBD with 2 replications, but there is one plot per new clone and
#    the checks are repeated (13 to 18 plots): an unreplicated observation trial with repeated checks.

## NOTES
# 1. CIP experiment station in La Molina, Lima (coordinates and elevation from the "Minimal" sheet).
#    Irrigated (CIP La Molina, confirmed by the contributor).
# 2. Plots of 10 plants in one row, 0.3 m between plants and 0.9 m between rows (2.7 m2).
#    Yield is the non-adjusted total tuber yield (TTWP / plot size); marketable yield is not reported.
# 4. Not processed: plant vigor, flowering, senescence (1-9 scales) and chipping color.

carob_script <- function(path) {

"
Dataset for:  Evaluation of New Clones in Spring, La Molina - Lima, 2005

A screening of 180 potato clones was conducted to determine yield and dry matter content characteristics. The screening took place at La Molina Experimental Station, Lima, during 2005.
"

	uri <- "doi:10.21223/JKTLQF"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		# one plot per new clone, repeated checks (ISSUES 2)
		design = "unreplicated",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-06",
		carob_completion = 80,
		carob_effort = 1
	)

	## the fieldbook has the standard CIP sheets ("Minimal", "Installation", "Fieldbook")
	process <- carobiner::get_function("process_cip_lbvars", path, group)
	f <- ff[basename(ff) == "PTYL200512_CIPHQ_new_clones_LMspr.xls"]
	# this fieldbook has no MTWP, TTYA or PPH
	d <- process(f, addvars=c("mtwp", "ttya"))

	d$on_farm <- FALSE
	d$irrigated <- TRUE

	## only the harvested plots (ISSUES 1)
	d <- d[!is.na(d$yield), ]

	carobiner::write_files(path, meta, d)
}
