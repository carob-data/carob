# R script for "carob"
# license: GPL (>=3)

## REJECTED
# International Late White Hybrid trial ILWH1401 (14TTWCWL). The Results sheet lists 15 entries and their pedigrees, including the two local checks.
# The grain-yield cells for those entries are empty. The numbers in that column are the fieldbook lower limit (0), upper limit (15), and selection weight (10), not measured yields.
# Site means and locations are in the PDF files.

carob_script <- function(path) {

	uri <- "hdl:11529/10236"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
