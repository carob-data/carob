# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Analysis of Minerals and Vitamin C in virus-resistant clones in Muqui, Junin

An analysis of micronutrient concentration, including minerals and vitamin C, was conducted using 10 virus-resistant clones in the Muqui area of Junin during 2004, following a completely randomized design (CRD) with 3 replications.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/OZYBNZ"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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
	

	f1 <- ff[basename(ff) == "TMIN200312_MUQUI_Virus.xls"]
	f2 <- ff[basename(ff) == "TVIT200312_MUQUI_Virus.xls"]
	f3 <- ff[basename(ff) == "01_Fieldbook_TMIN200312_MUQUI_Virus.txt"]
	f4 <- ff[basename(ff) == "02_Fieldbook_TVIT200312_MUQUI_Virus.txt"]
	f5 <- ff[basename(ff) == "03_Material_list.txt"]
	f6 <- ff[basename(ff) == "04_Datadictionary.txt"]

	r1a <- carobiner::read.excel(f1, sheet="Minimal")
	r1b <- carobiner::read.excel(f1, sheet="Identification")
	r1c <- carobiner::read.excel(f1, sheet="Installation")
	r1d <- carobiner::read.excel(f1, sheet="Field")
	r1e <- carobiner::read.excel(f1, sheet="Crop_management")
	r1f <- carobiner::read.excel(f1, sheet="Material List")
	r1g <- carobiner::read.excel(f1, sheet="Minerals")
	r1h <- carobiner::read.excel(f1, sheet="Summary by clone")
	r1i <- carobiner::read.excel(f1, sheet="Lists")
	r1j <- carobiner::read.excel(f1, sheet="Notes")
	r1k <- carobiner::read.excel(f1, sheet="ICPAES Results")
	r2a <- carobiner::read.excel(f2, sheet="Minimal")
	r2b <- carobiner::read.excel(f2, sheet="Identification")
	r2c <- carobiner::read.excel(f2, sheet="Installation")
	r2d <- carobiner::read.excel(f2, sheet="Field")
	r2e <- carobiner::read.excel(f2, sheet="Crop_management")
	r2f <- carobiner::read.excel(f2, sheet="Material List")
	r2g <- carobiner::read.excel(f2, sheet="VitaminC")
	r2h <- carobiner::read.excel(f2, sheet="Summary by clone")
	r2i <- carobiner::read.excel(f2, sheet="Muqui-2004 Virus_VitaminC")
	r2j <- carobiner::read.excel(f2, sheet="Lists")
	r2k <- carobiner::read.excel(f2, sheet="Notes")
	r2l <- carobiner::read.excel(f2, sheet="ICPAES Results")
	#r3 <- read.???(f3)
	#r4 <- read.???(f4)
	#r5 <- read.???(f5)
	#r6 <- read.???(f6)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame()
##r1a: "Factor", "Value", "ID", "ID2"


	d1b <- data.frame()
##r1b: "Factor", "Value"


	d1c <- data.frame()
##r1c: "Factor", "Value", "ID1", "ID2", "ID3"


	d1d <- data.frame()
##r1d: "Factor", "Value"


	d1e <- data.frame()
##r1e: "Intervention category", "Intervention type", "Date of intervention", "Name of product *", "Product concentration", "Dose of application", "Observations"


	d1f <- data.frame(
		variety = r1f[["Clone/variety name"]]
	)
##r1f: "Numeration", "Control", "CIP number", "Code", "Pedigree", "Species", "Origin", "Seed source1", "References to silmultaneous trials", "References to previous trials", "Breeder Code or Collecting Number", "Accession name"


	d1g <- data.frame(
		plot_id = r1g[["#Plot"]]
	)
##r1g: "1", "30", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48"


	d1h <- data.frame(
		N_fertilizer = r1h[["n"]],
		year = r1h[["Year"]],
		country = r1h[["Country"]],
		adm2 = carobiner::fix_name(r1h[["Province"]], "title")
	)
##r1h: "INSTN", "Breeder Code or Collecting Number", "Accession name", "Departament", "District", "Locality", "Percentage Dry matter(%) _mean", "Percentage Dry matter(%) _sd", "Fe (mg/kg,DW) _mean", "Fe (mg/kg,DW) _sd", "Fe  (mg/100g,FW) _mean", "Fe  (mg/100g,FW) _sd", "Mn (mg/kg,DW) _mean", "Mn (mg/kg,DW) _sd", "Mn (mg/100g,FW) _mean", "Mn (mg/100g,FW) _sd", "B (mg/kg,DW) _mean", "B (mg/kg,DW) _sd", "B (mg/100g,FW) _mean", "B (mg/100g,FW) _sd", "Cu (mg/kg,DW) _mean", "Cu (mg/kg,DW) _sd", "Cu (mg/100g,FW) _mean", "Cu (mg/100g,FW) _sd", "Zn (mg/kg,DW) _mean", "Zn (mg/kg,DW) _sd", "Zn (mg/100g,FW) _mean", "Zn (mg/100g,FW) _sd", "Ca (mg/kg,DW) _mean", "Ca (mg/kg,DW) _sd", "Ca (mg/100g,FW) _mean", "Ca (mg/100g,FW) _sd", "Mg (mg/kg,DW) _mean", "Mg (mg/kg,DW) _sd", "Mg (mg/100g,FW) _mean", "Mg (mg/100g,FW) _sd", "K (mg/kg,DW) _mean", "K (mg/kg,DW) _sd", "K (mg/100g,FW) _mean", "K (mg/100g,FW) _sd", "P (mg/kg,DW) _mean", "P (mg/kg,DW) _sd", "P (mg/100g,FW) _mean", "P (mg/100g,FW) _sd", "S (mg/kg,DW) _mean", "S (mg/kg,DW) _sd", "S (mg/100g,FW) _mean", "S (mg/100g,FW) _sd", "Na (mg/kg,DW) _mean", "Na (mg/kg,DW) _sd", "Na (mg/100g,FW) _mean", "Na (mg/100g,FW) _sd"


	d1i <- data.frame()
##r1i: "DES...1", "ID...2", "...3", "DES...4", "ID...5", "...6", "DES...7", "ID...8", "...9", "DES...10", "ID...11", "...12", "DES...13", "ID...14", "...15", "DES...16", "ID...17", "...18", "DES...19", "ID...20", "...21", "DES...22", "ID...23", "ID2", "ID3", "ID4", "ID5", "...28", "DES...29", "ID...30", "...31", "LIST1_EVALUATIONS", "...33", "LIST_DESIGN", "ID_DESIGN", "...36", "LIST_SOIL", "...38", "LIST_METO", "...40", "LIST_DEPA", "...42", "LIST_PRO", "...44", "LIST_DIST"


	d1j <- data.frame()
##r1j: "type", "desc_type"


	d1k <- data.frame(
		rep = r1k[["Final Report 1"]]
	)
##r1k: "...1", "...2", "Waite Ref. : ipc403", "...4", "Order No. n.a.", "...6", "...7", "...8", "...10", "...11", "...12", "...13", "...14", "Date Reported :", "38189.383069097225", "...17", "...18", "...19", "...20", "...21"


	d2a <- data.frame()
##r2a: "Factor", "Value"


	d2b <- data.frame()
##r2b: "Factor", "Value"


	d2c <- data.frame()
##r2c: "Factor", "Value"


	d2d <- data.frame()
##r2d: "Factor", "Value"


	d2e <- data.frame()
##r2e: "Intervention category", "Intervention type", "Date of intervention", "Name of product *", "Product concentration", "Dose of application", "Observations"


	d2f <- data.frame(
		variety = r2f[["Clone/variety name"]]
	)
##r2f: "Numeration", "Control", "CIP number", "Code", "Pedigree", "Species", "Origin", "Seed source1", "References to silmultaneous trials", "References to previous trials", "Breeder Code or Collecting Number", "Accession name"


	d2g <- data.frame()
##r2g: "EVALUATION OF VITAMIN C", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15"


	d2h <- data.frame(
		N_fertilizer = r2h[["n"]],
		year = r2h[["Year"]],
		country = r2h[["Country"]],
		adm2 = carobiner::fix_name(r2h[["Province"]], "title")
	)
##r2h: "INSTN", "Breeder Code or Collecting number", "Accession name", "Departament", "District", "Locality", "Ascorbic acid (mg/100 g, DW) _mean", "Ascorbic acid (mg/100 g, DW) _sd", "Ascorbic acid (mg/100g,FW) _mean", "Ascorbic acid (mg/100g,FW) _sd", "Percentage Dry matter(%)_mean", "Percentage Dry matter(%)_sd"


	d2i <- data.frame(
		plot_id = r2i[["Plot"]],
		rep = r2i[["Repetitions"]],
		year = r2i[["Year"]]
	)
##r2i: "Genotype", "Breeder Code", "Code laboratory", "Locality", "Ascorbic acid(mg / 100 g, DW)", "Ascorbic acid(mg / 100 g, FW)", "Percentage Dry matter(%)"


	d2j <- data.frame()
##r2j: "DES...1", "ID...2", "...3", "DES...4", "ID...5", "...6", "DES...7", "ID...8", "...9", "DES...10", "ID...11", "...12", "DES...13", "ID...14", "...15", "DES...16", "ID...17", "...18", "DES...19", "ID...20", "...21", "DES...22", "ID...23", "ID2", "ID3", "ID4", "ID5", "...28", "DES...29", "ID...30", "...31", "LIST1_EVALUATIONS", "...33", "LIST_DESIGN", "ID_DESIGN", "...36", "LIST_SOIL", "...38", "LIST_METO", "...40", "LIST_DEPA", "...42", "LIST_PRO", "...44", "LIST_DIST"


	d2k <- data.frame()
##r2k: "type", "desc_type"


	d2l <- data.frame(
		rep = r2l[["Final Report 1"]]
	)
##r2l: "...1", "...2", "Waite Ref. : ipc403", "...4", "Order No. n.a.", "...6", "...7", "...8", "...10", "...11", "...12", "...13", "...14", "Date Reported :", "38189.383069097225", "...17", "...18", "...19", "...20", "...21"


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

