# R script for "carob"

carob_script <- function(path) {

	burl <- "https://asi.ucdavis.edu/sites/g/files/dgvnsk5751/files/inline-files/"
	f <- c("Sheet%201.zip", "File%202.zip", "File%203.zip", "Sheet%204.zip", "Sheet%205.zip", "CenturyExperiment_metadata.pdf")
	urls <- paste0(burl, f)

	uri <- "UCD_century_LTE"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group, files=urls)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "UCD",
		publication=NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Robert Hijmans",
		carob_effort = NA,
		carob_date="2024-07-15",
		response_vars = "yield",
		treatment_vars = ""
	)
	
	f <- ff[basename(ff) == ""]

	  
	carobiner::write_files(meta, d, path=path)
}

