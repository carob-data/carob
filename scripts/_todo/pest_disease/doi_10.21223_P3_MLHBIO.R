# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication data for: Leafminer fly (Diptera: Agromyzidae) Occurrence, Distribution and Parasitoid Associations in Field and Vegetable Crops along the Peruvian Coast

Surveys to study the occurrence and distribution of leafminer fly and their associated parasitoids were conducted in the winter, from July to August 2003, and autumn cropping season, from April to May 2004, in ten agricultural production regions along the Peruvian coast from Tumbes (0º01´11' S) in the north of Peru to Tacna (18°21'05' S) in the south in a total of 29 provinces. In the different regions the production of field and vegetable crops as host plants of leafminer flies are not equally important, hence the sample size varied in each region. The survey did not take into account the different plant-protection measures and pesticide applications by farmers and therefore does not reflect undisturbed herbivore-parasitoid relationships. On average, 30 leafminer fly-infested leaves were randomly collected from each field sampled, covering a total of 459 fields, with 243 and 216 fields in the winter and autumn cropping season, respectively. During the daily surveys, the leaf samples collected were immediately stored in paper bags and afterwards transferred onto damp paper towels into plastic containers (15x25x10 cm) to avoid drying of the leaves. Leaf layers were separated with dry paper towels and all towels were changed at 2-day intervals for the following 6 days. Boxes were closed with a lid containing muslin windows (20x12 cm) for ventilation and kept at temperatures of 20ºC. After 7 to 10 days pupae were collected from the boxes and stored in plastic Petri dishes until adults of either leafminer flies or endoparasitoids hatched. Leaves were returned to the container to identify the ectoparastoids. Specimens were identified using conventional taxonomic keys and reference collections. Infested fields were registered based on visual assessment of presence or ausence of leafminer foliar damage. In each field, the infestation intensity was estimated by using a rating scale of four levels of leaf damage: no infestation (0%), low (1-25%), middle (26-50%) and high (> 50%) leaf infestation. These parameters were calculated in 30 plants randomly selected per field.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/MLHBIO"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
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
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "1.1_dictionary.xls"]
	f2 <- ff[basename(ff) == "subchapter 1.1_Survey dataset.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Num", "Trait", "Unit", "Variable Label", "Trait description", "Category 1", "Category 2", "Category 3", "Category 4"


	d2 <- data.frame(
		adm1 = carobiner::fix_name(r2[["Region"]], "title"),
		latitude = r2[["Latitude (DMS)"]],
		longitude = r2[["Longitude (DMS)"]],
		elevation = r2[["Altitude (m)"]],
		season = r2[["Season"]],
		crop = tolower(r2[["Crop (common name)"]])
	)
##r2: "Tmax", "Tmin", "Crop (Family plant)", "Foliar damage (low = 1-25%; middle=26-50%; high>50%)", "Foliar damage (no damage=1;low = 2; middle=3; high=4)", "leafminer fly adults", "Parasitoid adults", "Pupae (without emerging adults)", "Total (LMF + Par + Pupae)", "Larva/ leaflet", "L. sativae", "L. huidobrensis", "Braconidae sp. A", "Braconidae sp. B", "Braconidae sp. C", "Braconidae sp. D", "Braconidae sp. E", "Braconidae sp. F", "Braconidae sp. G", "Braconidae sp. H", "Braconidae sp. I", "Braconidae sp. J", "Braconidae sp. K", "Eulophidae sp. 9", "Eulophidae sp. 10", "Eulophidae sp. 11", "Eulophidae sp. 12", "Eulophidae sp. 13", "Eulophidae sp. 14", "Chrysocharis brethesi", "Chrysocharis caribea", "Chrysocharis flacilla", "Chrysocharis phytomyzae", "Chrysocharis sp. A", "Chrysocharis sp. B", "Chrysocharis sp. C", "Chrysocharis sp. D", "Chrysonotomyia sp. 1", "Chrysonotomyia sp. 2", "Chrysonotomyia sp. 3", "Chrysonotomyia sp. 4", "Chrysonotomyia sp. 5", "Chrysonotomyia thysanoides", "Closterocerus sp. 1", "Closterocerus sp. 2", "Diaulinopsis callichroma", "Diaulinopsis sp.1", "Diaulinopsis sp.3", "Diglyphus begini", "Diglyphus websteri", "Eulophidae sp. 1", "Eulophidae sp. 2", "Eulophidae sp. 3", "Eulophidae sp. 4", "Eulophidae sp. 5", "Eulophidae sp. 7", "Eulophidae sp.6", "Eulophidae sp.8", "Ganaspidium sp. 1", "Halticoptera arduine", "Halticoptera sp. A", "Halticoptera sp. B", "Halticoptera  sp. D", "Halticoptera  sp. E", "Halticoptera sp. G", "Halticoptera sp. H", "Halticoptera I", "Zagrammosoma sp.1", "Zagrammosoma sp.2", "Zagrammosoma sp.3", "Zagrammosoma sp.4", "Zagrammosoma sp.5", "Zagrammosoma sp.6"


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

