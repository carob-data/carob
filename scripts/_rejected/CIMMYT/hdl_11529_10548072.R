# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The four Rajshahi mung bean long-term trial workbooks are listed, but Dataverse returns HTTP 404 for each file id (failed to locate the physical file). There is no readable table.

carob_script <- function(path) {

	uri <- "hdl:11529/10548072"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

}
