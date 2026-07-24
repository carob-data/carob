# R script for "carob"
# license: GPL (>=3)

## ISSUES
# in 2021-11-24t035520phenotype_download.csv "harvestDate" is an empty column
# in 2021-11-24t035520phenotype_download.csv "locationName" lists Ibadan as the location but "studyDescription" and the datasets title place trial in Lanlate
# the dataset is described as a "Fertilizer experiment" in both the description and title but NO fertiliser information is given, so all fertiliser vars are set to NA and treatment_var in metadata is left empty
# variables dry_matter_content, dry_yield, fresh_yield, fwy_leaves, harvest_index, top_yield all caused issues processing due to discrepancies in column names in csv files downloaded and files loaded by carobiner E.g. "top yield|CO_334:0000017" was only accepted by the script as "top.yield.CO_334.0000017"
# longitude and latitude derived from experiment title. NOTE: it was given on IITA webpage for this dataset, but the coordinates pointed to a Mosque [Aribi-Desi Mosque]
# subject in metadata.csv is listed as Maize when trial was done on cassava.
# "dmy_total" is presented as "dmy_totat" in draft
# "fertilizer_type" is presented as "fertlizer_type" in draft


carob_script <- function(path) {

"
2006 Fertilizer experiment at Ekha Agro farm LANLATE

2006 Fertilizer experiment at Ekha Agro farm LANLATE
"

	uri <- "https://doi.org/10.25502/6ana-1536"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "NextGen Cassava",
		design = "RCBD",
		data_type = "experiment", # on-farm/site not specified
		treatment_vars = "", # not provided in dataset
		response_vars = "yield;dry_matter_content;fwy_storage;harvest_index;", 
		notes = "3 seperate yields were recorded in this dataset, so 3 seperate variable names were used instead of the terminags 'yield'",
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-07-22",
		carob_completion = 30,	
		carob_effort = 7
	)

	f1 <- ff[basename(ff) == "2021-11-24t035520phenotype_download.csv"]

	r1 <- read.csv(f1)

	d <- data.frame(
		year = as.character(r1[["studyYear"]]), # could not find equivalent in terminag
		plot_id = as.character(r1[["plotNumber"]]),
		rep = as.integer(r1[["replicate"]]),
		planting_date = r1[["plantingDate"]],
		# harvest_date = r1[["harvestDate"]],
		location = r1[["locationDbId"]],
		variety = r1[["germplasmName"]],
		dry_matter_content = r1[["dry.matter.content.percentage.CO_334.0000092"]], # could not find equivalent in terminag
		dry_yield = r1[["dry.yield.CO_334.0000014"]], # see notes
		fresh_yield = r1[["fresh.root.yield.CO_334.0000013"]],
	 	fwy_leaves = r1[["fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016"]],
		harvest_index = r1[["harvest.index.variable.CO_334.0000015"]],
		top_yield = r1[["top.yield.CO_334.0000017"]]		
	)

	d$country <- "Nigeria"
	d$trial_id <- "1" # there is only trial, one location, and one design
	
## about the data (TRUE/FALSE)
	d$on_farm <- TRUE # sumised from experiment name "Ekha Agro farm"
	d$is_survey <- FALSE
	d$irrigated <- NA # no irrigation methods documented

    d$crop_rotation <- NA # no information provided
	d$crop <- "cassava"

# could not find lanlate in geodata so use of just adm1 was most accurate
	d$adm1 <- "Oyo" 
	d$location <- "Lanlate"
	d$longitude <- 3.6188
	d$latitude <- 8.1437 
	d$geo_uncertainty <- 124749
	d$geo_source <- "GADM 4.1, adm1"

	d$geo_from_source <- FALSE

	d$planting_date <- as.character(as.Date(d$planting_date, format= "%Y-%m-%d"  ))
	d$harvest_date  <- NA # see issues

### Fertilizers 
# see issues
   d$P_fertilizer <- NA 
   d$K_fertilizer <- NA
   d$N_fertilizer <- NA
   d$S_fertilizer <- NA
   d$lime <- NA

   d$fertilizer_type <- NA

### Yield
	d$yield <- d$fresh_yield * 1000
	d$yield_part <- "tubers" # cassava is classified as a tuberous root
	d$yield_moisture <- 100 - d$dry_matter_content #dmc is given in %

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
	d$fwy_storage <- d$fresh_yield * 1000
	d$dmy_storage <- d$dry_yield * 1000
	d$dmy_total <- d$top_yield * 1000
	
	carobiner::write_files(path, meta, d)
}