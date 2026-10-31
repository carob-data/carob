# R script for "carob"
# license: GPL (>=3)

## REJECTED
# PHALARIS SCREENING 2016-final.xls contains a protocol, an herbicide dose table, and a plot-layout map. It has no measured response such as biomass, mortality, or control. The other file is a photograph of the sampling map.

carob_script <- function(path) {

	uri <- "hdl:11529/10548672"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

}
