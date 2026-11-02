# R script for "carob"
# license: GPL (>=3)

## REJECTED
# International Late White Variety trial ILWV0801. The xls workbooks are empty CIMMYT field-book templates.
# Yield, pedigree, moisture, and location columns contain labels only. The location file is PDF, so there is no readable results table.

carob_script <- function(path) {

	uri <- "hdl:11529/10526"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
