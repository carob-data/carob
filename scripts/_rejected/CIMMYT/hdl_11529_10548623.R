# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The record is code and inputs for a terrain-model paper on simulated discharge and soil loss.
# It is not a table of field measurements.

carob_script <- function(path) {

	uri <- "hdl:11529/10548623"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

}
