# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The 10LETTLW site workbooks are Fieldbook exports. The Results sheet lists the entries, but the grain-yield cells for those entries are empty. The per-entry means and the locations are only in the PDF files. There is no readable table of results.

carob_script <- function(path) {

	uri <- "hdl:11529/10509"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
