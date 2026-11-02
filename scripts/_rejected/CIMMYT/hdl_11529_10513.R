# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The Results sheets list hybrid names and pedigrees. Entry grain-yield cells are empty.
# Site 11 stores only the trial mean, LSD, and related summary lines. The yield reports and locations are PDFs.

carob_script <- function(path) {

	uri <- "hdl:11529/10513"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
