# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The site workbooks are empty Fieldbook templates. Grain yield, moisture, and the other result columns are blank.
# Country, location, and dates are also blank in the Results sheet.
# The trial results and the location list are only in PDF reports, which are not read.

carob_script <- function(path) {

	uri <- "hdl:11529/10536"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
