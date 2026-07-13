# R script for "carob"
# license: GPL (>=3)

## ISSUES
# REJECTED - the dataset's only file ("Grain Yield Performance Saito et al Crop Sci2010.xls")
# does not contain actual tabular data. Table 1 and Table 2 (referenced in the sheet as
# screenshots of the data from the published paper) were never pasted in as real cell values:
# Sheet1 has only the two table captions as text, and Sheet2/Sheet3 are completely empty.
# There is no numeric yield/genotype data to extract from this file.


carob_script <- function(path) {

"
Grain Yield Performance of Selected Lowland NERICA and Modern Asian Rice Genotypes in West Africa

Six lowland experiments were conducted over three years in southern Benin to assess yield differences in 28 rice (Oryzaspp.) genotypes grown under nonfertilized and fertilized conditions. These included the interspecifi c hybrids, the lowland New Rice for Africa (NERICA) genotypes developed from crossing O. sativa and O. glaberrima. Fertilizer rates were 70–86N: 30–37P2O5: 30–37K2O kg ha–1. Fertilizer applicationincreased average grain yield across all genotypes and experiments by 39% (154 g m–2increase). Considerable genotypic differences existed in grain yields under both nonfertilized and fertilized conditions, and in yield response to fertilizer application. Two lowland NERICA genotypes ('NERICA-L-6' and '-54') outyielded'IR 72' and 'WITA4' (standard checks) acrossnonfertilized and fertilized conditions in fourexperiments with favorable water availability inwet seasons (651 vs. 575 g m–2). The high grainyields resulted from large spikelet number m–2and biomass accumulation. In contrast, threeindicagenotypes from Asia ('B 6144F-MR-6-0-0', 'IR 70181-32-PMI-1-1-5-1', and 'PSBRc 80')outperformed the checks in two experiments,one straddling wet and dry seasons and theother with no standing water during most of theusual wet rice-growing season. These resultsindicate that while specifi c adaptations arelikely to provide significant yield advantage inparticular environments, interspecifi c breedingstill offers an effective approach to improvinglowland rice productivity.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.7910/DVN/1MFGTJ"
	group <- "draft"
	ff  <- carobiner::get_data(uri, path, group)

## Non-metadata .json files in ff (e.g. nested Dataverse Dataset/*.json). Parsed with jsonlite::fromJSON.
## Optional string snapshots: lapply(json_list, function(x) jsonlite::toJSON(x, pretty=TRUE, auto_unbox=TRUE))
	jmeta <- paste0(yuri::simpleURI(uri), ".json")
	json_paths <- ff[grepl("\\.json$", ff, ignore.case=TRUE)]
	json_paths <- json_paths[!tolower(basename(json_paths)) %in% tolower(c(jmeta, "metadata.json"))]
	json_list <- if (length(json_paths) > 0) {
		stats::setNames(lapply(json_paths, jsonlite::fromJSON), basename(json_paths))
	} else {
		list()
	}

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "Africa Rice Center (WARDA), 01 BP 2031, Cotonou, Benin; frica Rice Center (WARDA), 01 BP 2031, Cotonou, Benin; Rice Center (WARDA), 01 BP 2031, Cotonou, Benin",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = NA,
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-07-11",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "Grain Yield Performance Saito et al Crop Sci2010.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Sheet1")
	r1b <- carobiner::read.excel(f1, sheet="Sheet2")
	r1c <- carobiner::read.excel(f1, sheet="Sheet3")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		yield = r1a[["Table 1. Grain yield, yield components, total biomass, harvest index, plant height and days to heading of 28 rice genotypes"]]
	)


	d1b <- data.frame()


	d1c <- data.frame()


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

