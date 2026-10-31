# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The two Sunsari long-term trial workbooks are listed, but Dataverse returns HTTP 404 for both (datafile 7405 and 7407). The saved response is "Failed to locate and/or open physical file." There is no readable table.

carob_script <- function(path) {

	uri <- "hdl:11529/10548075"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

}
