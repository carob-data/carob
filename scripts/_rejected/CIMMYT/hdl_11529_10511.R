# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The Results sheets list line names and pedigrees. fFieldWtGms and GrainYieldTons_FieldWt hold fieldbook limits, not entry yields.
# Site means and locations are PDFs.

carob_script <- function(path) {

	uri <- "hdl:11529/10511"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
