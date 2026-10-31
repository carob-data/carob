# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The Results and Fieldbook sheets list the hybrid names and the trait headers, and the yield cells are empty.
# The trial reports are PDFs. There is no table of measurements a script can read.

carob_script <- function(path) {

	uri <- "hdl:11529/10498"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
