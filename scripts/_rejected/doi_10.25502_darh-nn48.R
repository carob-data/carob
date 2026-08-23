# R script for "carob"
# license: GPL (>=3)

## ISSUES
## NO response data

carob_script <- function(path) {

"
Clonal Evaluation trial (231 clones) in Chitedze, Malawi  2012-13 Breeding Season.

Clonal Evaluation trial (231 clones) in Chitedze, Malawi  2012-13 Breeding Season.
"

	uri <- "doi:10.25502/darh-nn48"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
  	data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "variety",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-22",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "2020-03-24t035443phenotype_download.csv"]
	f2 <- ff[basename(ff) == "metadata.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	d1 <- data.frame(
	  year = r1$studyYear,
	  planting_date = as.Date(carobiner::eng_months_to_nr(r1$plantingDate), "%Y-%m-%d") ,
	  harvest_date =  as.Date(carobiner::eng_months_to_nr(r1$harvestDate), "%Y-%m-%d"),
	  location = r1$locationName,
	  variety = r1$germplasmName,
	  trial_id = r1$observationUnitName,
	  rep = r1$replicate,
	  plot_id = r1$plotNumber,
	  longitude = 33.6371 ,
	  latitude = -13.981 ,
	  geo_source = "Google Maps",
	  country = "Malawi",
	  geo_from_source = FALSE,
	  yield = NA,
	  crop = "cassava"
	)

	carobiner::write_files(path, meta, d)
}


