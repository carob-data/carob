# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The xlsx is a multi-header laboratory matrix of soil-carbon fractions by scenario, depth, and year (Rep and Treat, then blocks for 2010, 2014, and 2019).
# The workbook does not name a site or country, does not give units, and does not contain the weed counts described in the title.
# It cannot be standardized as a plot or soil-sample table.

carob_script <- function(path) {

	uri <- "hdl:11529/10548760"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

}
