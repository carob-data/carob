# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Vitamin C Analysis in Promising Potato Clones from Junin, Peru (2007)

An analysis of vitamin C concentration was performed on 29 promising potato clones in the Victoria region of Junin in 2007. The study employed a completely randomized design (CRD) with two replications. These clones were part of Set 5, identified as promising clones for further agricultural development.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/0XGN5Q"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
	

	f1 <- ff[basename(ff) == "01_Data_VitaminC_Huancayo_Set5_2007.xlsx"]
	f2 <- ff[basename(ff) == "02_Material_list_VitaminC_Huancayo_Set5_2007.xlsx"]
	f3 <- ff[basename(ff) == "03_Data_dictionary.xlsx"]
	f4 <- ff[basename(ff) == "TVIT200712_LAVICT_Set_5.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4a <- carobiner::read.excel(f4, sheet="Minimal")
	r4b <- carobiner::read.excel(f4, sheet="Identification")
	r4c <- carobiner::read.excel(f4, sheet="Installation")
	r4d <- carobiner::read.excel(f4, sheet="Field")
	r4e <- carobiner::read.excel(f4, sheet="Crop_management")
	r4f <- carobiner::read.excel(f4, sheet="Material List")
	r4g <- carobiner::read.excel(f4, sheet="VitaminC")
	r4h <- carobiner::read.excel(f4, sheet="Summary by clone")
	r4i <- carobiner::read.excel(f4, sheet="Lists")
	r4j <- carobiner::read.excel(f4, sheet="Notes")
	r4k <- carobiner::read.excel(f4, sheet="2008_Sets 5_Hyo")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		rep = r1[["REP"]],
		year = r1[["Year"]],
		country = r1[["Country"]],
		adm2 = carobiner::fix_name(r1[["Province"]], "title")
	)
##r1: "Numeration", "INSTN", "Breeder Code or Collecting Number", "Accession name", "Code laboratory", "Departament", "District", "Locality", "ASC_DW", "ASC_FW", "PDM"


	d2 <- data.frame()
##r2: "Numeration", "Institutionalnumber", "Breeder Code or Collecting Number"


	d3 <- data.frame(
		crop = tolower(r3[["Crop"]])
	)
##r3: "numeration", "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref"


	d4a <- data.frame()
##r4a: "Factor", "Value"


	d4b <- data.frame()
##r4b: "Factor", "Value"


	d4c <- data.frame()
##r4c: "Factor", "Value", "ID1", "ID2", "ID3"


	d4d <- data.frame()
##r4d: "Factor", "Value"


	d4e <- data.frame()
##r4e: "Intervention category", "Intervention type", "Date of intervention", "Name of product *", "Product concentration", "Dose of application", "Observations"


	d4f <- data.frame(
		variety = r4f[["Clone/variety name"]]
	)
##r4f: "Numeration", "Control", "CIP number", "Code", "Pedigree", "Species", "Origin", "Seed source1", "References to silmultaneous trials", "References to previous trials", "Breeder Code or Collecting Number", "Accession name"


	d4g <- data.frame()
##r4g: "EVALUATION OF VITAMIN C", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15"


	d4h <- data.frame(
		N_fertilizer = r4h[["n"]],
		year = r4h[["Year"]],
		country = r4h[["Country"]],
		adm2 = carobiner::fix_name(r4h[["Province"]], "title")
	)
##r4h: "INSTN", "Breeder Code or Collecting Number", "Accession name", "Departament", "District", "Locality", "Ascorbic acid (mg/100 g, DW) _mean", "Ascorbic acid (mg/100 g, DW) _sd", "Ascorbic acid (mg/100g,FW) _mean", "Ascorbic acid (mg/100g,FW) _sd", "Percentage Dry matter(%)_mean", "Percentage Dry matter(%)_sd"


	d4i <- data.frame()
##r4i: "DES...1", "ID...2", "...3", "DES...4", "ID...5", "...6", "DES...7", "ID...8", "...9", "DES...10", "ID...11", "...12", "DES...13", "ID...14", "...15", "DES...16", "ID...17", "...18", "DES...19", "ID...20", "...21", "DES...22", "ID...23", "ID2", "ID3", "ID4", "ID5", "...28", "DES...29", "ID...30", "...31", "LIST1_EVALUATIONS", "...33", "LIST_DESIGN", "ID_DESIGN", "...36", "LIST_SOIL", "...38", "LIST_METO", "...40", "LIST_DEPA", "...42", "LIST_PRO", "...44", "LIST_DIST"


	d4j <- data.frame()
##r4j: "type", "desc_type"


	d4k <- data.frame(
		plot_id = r4k[["Plot"]],
		treatment = r4k[["Treatment Factor"]],
		year = r4k[["Year"]],
		country = r4k[["Country"]],
		adm1 = carobiner::fix_name(r4k[["Region"]], "title")
	)
##r4k: "Block-", "Breeder Code", "Code laboratory", "Locality", "Ascorbic acid(mg / 100 g, DW)", "Ascorbic acid.(mg / 100 g, FW)", "Percentage Dry matter(%)"


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

