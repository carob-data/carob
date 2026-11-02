# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The Results sheets list entry names and trait headers. The grain-yield column has no trial values, only the fieldbook limits.
# Locations and the yield reports are PDFs. There is no table of measurements a script can read.

carob_script <- function(path) {

	uri <- "hdl:11529/10533"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
