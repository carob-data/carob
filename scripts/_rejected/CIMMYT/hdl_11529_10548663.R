# R script for "carob"
# license: GPL (>=3)

## REJECTED
# Data are included in doi:10.71682/10549439

carob_script <- function(path) {

	uri <- "hdl:11529/10548663"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

}
