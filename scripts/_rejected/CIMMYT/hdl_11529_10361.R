# R script for "carob"
# license: GPL (>=3)

## REJECTED
# 12EVT215.xls and 12EVT2124.xls are Fieldbook exports. The Results sheet lists the entries, but the grain-yield cells for those entries are empty. The per-entry means and the location coordinates are only in the PDF files. There is no readable table of results.

carob_script <- function(path) {

	uri <- "hdl:11529/10361"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
