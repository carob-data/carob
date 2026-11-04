# R script for "carob"
# license: GPL (>=3)

## REJECTED
# International Late White Hybrid trial ILWH1301. The xls workbooks are empty CIMMYT field-book templates.
# Yield, pedigree, and location columns contain labels only. The location list and the trial results are PDF reports, which are not read.

carob_script <- function(path) {

	uri <- "hdl:11529/10335"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

}
