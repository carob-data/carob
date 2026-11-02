# R script for "carob"
# license: GPL (>=3)

## REJECTED
# International Late Yellow Variety trial ILYV1002. The xls workbooks are empty CIMMYT field-book templates.
# One workbook has a handful of numeric cells and no yield values. The location list and the trial results are PDF reports, which are not read.

carob_script <- function(path) {

	uri <- "hdl:11529/10503"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
