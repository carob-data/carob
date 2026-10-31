# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The 08TTWCWL site workbooks are Fieldbook exports. The Results sheet lists the entries, but the grain-yield cells for those entries are empty. Trial-level mean and LSD rows are filled. The per-entry means and the locations are only in the PDF files. There is no readable table of results.

carob_script <- function(path) {

	uri <- "hdl:11529/10530"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
