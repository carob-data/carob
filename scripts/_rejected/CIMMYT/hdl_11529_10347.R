# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The Results sheets list hybrid names and pedigrees. GrainYieldTons_FieldWt and fFieldWtGms hold fieldbook limits and column labels, not entry yields.
# Site means, the combined analysis, and locations are PDFs.

carob_script <- function(path) {

	uri <- "hdl:11529/10347"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
