# R script for "carob"
# license: GPL (>=3)

## REJECTED
# Evaluation of maize landraces for drought tolerance in 2014. The trial workbooks are still restricted.
# get_data warned that access to some files is restricted and returned only Drought_2014_Germplasm_DOIs.csv.
# That table is germplasm DOIs. It has no plot measurements.

carob_script <- function(path) {

	uri <- "hdl:11529/10832"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
