# R script for "carob"
# license: GPL (>=3)

## ISSUES
## Rejected : Missing fertilizer
#            : Missing response variable

carob_script <- function(path) {

"
2006 Fertilizer Experiment with 18 clones at Ikenne

2006 Fertilizer Experiment with 18 clones at Ikenne
"

	uri <- "doi:10.25502/jn5z-rd08"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "none",
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-10-08",
		carob_completion = 0,	
		carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "2021-11-24t035555phenotype_download.csv"]
	f2 <- ff[basename(ff) == "metadata.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

####
	d1 <- data.frame(
	  year = r1$studyYear,
	  plot_width = r1$plotWidth,
	  planting_date = r1$plantingDate,
	  harvest_date = r1$harvestDate,
	  location_id = r1$locationDbId,
	  rep = r1$replicate,
	  plot_length	= r1$plotLength,
	  field_size = r1$fieldSize,
	  location = r1$locationName,
	  variety = r1$germplasmName,
	  trial_id = r1$observationUnitName,
	  bloc_id = r1$blockNumber,
	  plot_id = r1$plotNumber
	)
	


	carobiner::write_files(path, meta, d)
}

