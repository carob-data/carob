# R script for "carob"
# license: GPL (>=3)

## REJECTED
# 14CHTPROA15.xls is a Fieldbook export. The Results sheet lists the entries, but the grain-yield cells for those entries are empty. The per-entry means and the location are only in the PDF files. There is no readable table of results.

carob_script <- function(path) {

	uri <- "hdl:11529/10239"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
