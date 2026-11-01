# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The workbook is figure means of arthropod counts per hectare for conservation agriculture versus conventional tillage. It has no plot records, location, or yield column. The PDF is the paper, not a data table.

carob_script <- function(path) {

	uri <- "hdl:11529/10548763"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

}
