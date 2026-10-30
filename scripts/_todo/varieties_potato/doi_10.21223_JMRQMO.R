# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Field Evaluation and Nutritional Analysis of Phureja Collection Native Potatoes in Comas - Junin, 2006

An evaluation was carried out on 192 varieties from the Phureja potato collection. The assessment took place in the Comas locality in Junin during the 2006 growing season. Additionally, nutritional analyses were conducted for micronutrients, vitamin C, carotenoids, polyphenols, and a morphological characterization.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/JMRQMO"
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
	

	f1 <- ff[basename(ff) == "TMIN200605_COMAS_Phureja_Collection_Field.xls"]
	f2 <- ff[basename(ff) == "TMORP200605_COMAS_Phureja_Collection.xls"]
	f3 <- ff[basename(ff) == "TPHEN200605_COMAS_Phureja_Collection_Field.xls"]
	f4 <- ff[basename(ff) == "TVIT200605_COMAS_Phureja_Collection_Field.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Minimal")
	r1b <- carobiner::read.excel(f1, sheet="Identification")
	r1c <- carobiner::read.excel(f1, sheet="Installation")
	r1d <- carobiner::read.excel(f1, sheet="Field")
	r1e <- carobiner::read.excel(f1, sheet="Crop_management")
	r1f <- carobiner::read.excel(f1, sheet="Material List")
	r1g <- carobiner::read.excel(f1, sheet="Minerals")
	r1h <- carobiner::read.excel(f1, sheet="Summary by clone")
	r1i <- carobiner::read.excel(f1, sheet="Phureja-Comas-2006-Minerals")
	r1j <- carobiner::read.excel(f1, sheet="Lists")
	r1k <- carobiner::read.excel(f1, sheet="Notes")
	r1l <- carobiner::read.excel(f1, sheet="ICPAES Results ")
	r2a <- carobiner::read.excel(f2, sheet="Minimal")
	r2b <- carobiner::read.excel(f2, sheet="Installation")
	r2c <- carobiner::read.excel(f2, sheet="Material List")
	r2d <- carobiner::read.excel(f2, sheet="Soil_analysis")
	r2e <- carobiner::read.excel(f2, sheet="Hobo_data")
	r2f <- carobiner::read.excel(f2, sheet="Weather_data")
	r2g <- carobiner::read.excel(f2, sheet="Crop_management")
	r2h <- carobiner::read.excel(f2, sheet="Var List")
	r2i <- carobiner::read.excel(f2, sheet="Fieldbook")
	r2j <- carobiner::read.excel(f2, sheet="Comas_original")
	r2k <- carobiner::read.excel(f2, sheet="Sheet2")
	r3a <- carobiner::read.excel(f3, sheet="Minimal")
	r3b <- carobiner::read.excel(f3, sheet="Identification")
	r3c <- carobiner::read.excel(f3, sheet="Installation")
	r3d <- carobiner::read.excel(f3, sheet="Field")
	r3e <- carobiner::read.excel(f3, sheet="Crop_management")
	r3f <- carobiner::read.excel(f3, sheet="Material List")
	r3g <- carobiner::read.excel(f3, sheet="Polyphenols")
	r3h <- carobiner::read.excel(f3, sheet="Summary by clone")
	r3i <- carobiner::read.excel(f3, sheet="Original")
	r3j <- carobiner::read.excel(f3, sheet="2006-Comas-Phenolic")
	r3k <- carobiner::read.excel(f3, sheet="Lists")
	r3l <- carobiner::read.excel(f3, sheet="Notes")
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
	r4k <- carobiner::read.excel(f4, sheet="2006-Comas_Phurejas_VitaminC")

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
		plot_id = r1g[["# Plot"]]
	)
##r1g: "2", "499", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48"


	d1h <- data.frame(
		N_fertilizer = r1h[["n"]],
		year = r1h[["Year"]],
		country = r1h[["Country"]],
		adm2 = carobiner::fix_name(r1h[["Province"]], "title")
	)
##r1h: "INSTN", "Breeder Code or Collecting Number", "Accession name", "Departament", "District", "Locality", "Percentage Dry matter(%) _mean", "Percentage Dry matter(%) _sd", "Fe (mg/kg,DW) _mean", "Fe (mg/kg,DW) _sd", "Fe  (mg/100g,FW) _mean", "Fe  (mg/100g,FW) _sd", "Mn (mg/kg,DW) _mean", "Mn (mg/kg,DW) _sd", "Mn (mg/100g,FW) _mean", "Mn (mg/100g,FW) _sd", "B (mg/kg,DW) _mean", "B (mg/kg,DW) _sd", "B (mg/100g,FW) _mean", "B (mg/100g,FW) _sd", "Cu (mg/kg,DW) _mean", "Cu (mg/kg,DW) _sd", "Cu (mg/100g,FW) _mean", "Cu (mg/100g,FW) _sd", "Zn (mg/kg,DW) _mean", "Zn (mg/kg,DW) _sd", "Zn (mg/100g,FW) _mean", "Zn (mg/100g,FW) _sd", "Ca (mg/kg,DW) _mean", "Ca (mg/kg,DW) _sd", "Ca (mg/100g,FW) _mean", "Ca (mg/100g,FW) _sd", "Mg (mg/kg,DW) _mean", "Mg (mg/kg,DW) _sd", "Mg (mg/100g,FW) _mean", "Mg (mg/100g,FW) _sd", "K (mg/kg,DW) _mean", "K (mg/kg,DW) _sd", "K (mg/100g,FW) _mean", "K (mg/100g,FW) _sd", "P (mg/kg,DW) _mean", "P (mg/kg,DW) _sd", "P (mg/100g,FW) _mean", "P (mg/100g,FW) _sd", "S (mg/kg,DW) _mean", "S (mg/kg,DW) _sd", "S (mg/100g,FW) _mean", "S (mg/100g,FW) _sd", "Na (mg/kg,DW) _mean", "Na (mg/kg,DW) _sd", "Na (mg/100g,FW) _mean", "Na (mg/100g,FW) _sd", "Al (mg/kg,DW) _mean", "Al (mg/kg,DW) _sd", "Al (mg/100g,FW) _mean", "Al (mg/100g,FW) _sd"


	d1i <- data.frame(
		plot_id = r1i[["Plot"]],
		year = r1i[["Year ()"]]
	)
##r1i: "Block", "Genotipe", "Code laboratory", "Locality", "Breeder Code", "Percentage Dry matter(%)", "Fe(mg/kg,DW)", "Fe.(mg / 100g , FW)", "Mn.(mg/kg,DW)", "Mn(mg / 100 g, FW)", "B(mg/kg,DW)", "B.(mg / 100 g, FW)", "Cu(mg/kg,DW)", "Cu.(mg / 100 g, FW)", "Zn(mg/kg,DW)", "Zn.(mg / 100g , FW)", "Ca(mg/kg,DW)", "Ca.(mg / 100 g, FW)", "Mg(mg/kg,DW)", "Mg.(mg / 100g , FW)", "K(mg/kg,DW)", "K.(mg / 100 g, FW)", "P(mg/kg,DW)", "P.(mg / 100g , FW)", "S(mg/kg,DW)", "S.(mg / 100 g, FW)", "Al(mg/kg,DW)", "Na(mg/kg,DW)", "Ti(mg/kg,DW)", "Cr(mg/kg,DW)", "Se(mg/kg,DW)", "Pb(mg/kg,DW)", "Cd(mg/kg,DW)", "Mo(mg/kg,DW)", "Co(mg/kg,DW)", "Ni(mg/kg,DW)"


	d1j <- data.frame()
##r1j: "DES...1", "ID...2", "...3", "DES...4", "ID...5", "...6", "DES...7", "ID...8", "...9", "DES...10", "ID...11", "...12", "DES...13", "ID...14", "...15", "DES...16", "ID...17", "...18", "DES...19", "ID...20", "...21", "DES...22", "ID...23", "ID2", "ID3", "ID4", "ID5", "...28", "DES...29", "ID...30", "...31", "LIST1_EVALUATIONS", "...33", "LIST_DESIGN", "ID_DESIGN", "...36", "LIST_SOIL", "...38", "LIST_METO", "...40", "LIST_DEPA", "...42", "LIST_PRO", "...44", "LIST_DIST"


	d1k <- data.frame()
##r1k: "type", "desc_type"


	d1l <- data.frame(
		rep = r1l[["Final Report 1"]]
	)
##r1l: "...1", "...2", "Waite Ref. : ipc634", "...4", "Order No. n.a.", "...6", "...7", "...8", "...10", "...11", "...12", "...13", "...14", "Date Reported :", "39122.556129166667", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25"


	d2a <- data.frame()
##r2a: "Factor", "Value"


	d2b <- data.frame()
##r2b: "Factor", "Value"


	d2c <- data.frame(
		variety = r2c[["Clone or variety name"]]
	)
##r2c: "Numeration", "Control", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "References to silmultaneous trials", "References to previous trials"


	d2d <- data.frame()
##r2d: "Variables", "Abbreviture", "Unit", "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", "Data8", "Data9", "Data10"


	d2e <- data.frame(
		year = r2e[["Year"]]
	)
##r2e: "Month", "Day", "Time", "Intervale Time", "Temperature °C", "Relative humidity (%)"


	d2f <- data.frame(
		year = r2f[["YEAR"]]
	)
##r2f: "MONTH", "DAY", "DATE", "WEATHERTY", "TIMEINTVL", "HOUR", "TMEAN", "TMAX", "TMIN", "RHMEAN", "RHMAX", "RHMIN", "DEWPT", "RAIN", "SOLRAD", "TSOIL1", "TSOIL2", "TSOIL3", "TSOIL4", "WCONT1", "WCONT2", "WCONT3", "WCONT4"


	d2g <- data.frame()
##r2g: "Intervention category", "Intervention type", "Date", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Uncertainty of Measurement"


	d2h <- data.frame()
##r2h: "Factor.Variables", "Abbreviations", "Summarize", "Analyze", "Selection direction"


	d2i <- data.frame(
		plot_id = r2i[["PLOT"]],
		rep = r2i[["REP"]]
	)
##r2i: "INSTN", "PGH", "LEATY", "NPLL", "INTERJ0", "INTERJ1", "STMPG", "STMWNG", "FLOWERING", "CORSHAP", "MCFAS", "MFCI", "SCFAS", "DISEFLO", "ANTPIG", "PISP", "CALPIG", "PEDPIG", "BERCO", "BERSHA", "TBSKN1", "TBSKN2", "TBSKN3", "Intensity secondary", "TBSKN4", "TBSHP1", "TBSHP2", "TBSHP3", "TBFSH1", "Intensity...32", "TBFSH2", "Intensity...34", "TBFSH3"


	d2j <- data.frame(
		country = r2j[["COUNTRY"]],
		adm1 = carobiner::fix_name(r2j[["STATE"]], "title"),
		adm2 = carobiner::fix_name(r2j[["Distribution of secondary flower color"]], "title")
	)
##r2j: "CIPNUMBER", "CULTVRNAME", "COLNUMBER", "REGION", "LOCALITY", "Growth habit type", "Leaf dissection", "No. of primary lateral leaflets", "No. of interjected leaflets", "No. of interjected leaflets over petiolule", "Stem color", "Stem wing", "Degree of flowering", "Corolla shape", "Predominant flower color", "Intensity of flower color", "Secondary flower color", "Anther pigments", "Pistil pigments", "Calyx color", "Pedicel color", "Fruit color", "Fruit shape", "Tuber skin color", "...28", "...29", "...30", "...31", "Tuber shape", "...33", "...34", "Tuber flesh color", "...36", "...37", "...38", "...39"


	d2k <- data.frame(
		plot_id = r2k[["PLOT"]],
		rep = r2k[["REP"]]
	)
##r2k: "INSTN", "PGH", "LEATY", "NPLL", "INTERJ0", "INTERJ1", "STMPG", "STMWNG", "FLOWERING", "CORSHAP", "MCFAS", "MFCI", "SCFAS", "DISEFLO", "ANTPIG", "PISP", "CALPIG", "PEDPIG", "BERCO", "BERSHA", "TBSKN1", "TBSKN2", "TBSKN3", "Intensity secondary", "TBSKN4", "TBSHP1", "TBSHP2", "TBSHP3", "TBFSH1", "Intensity...32", "TBFSH2", "Intensity...34", "TBFSH3"


	d3a <- data.frame()
##r3a: "Factor", "Value", "ID", "ID2"


	d3b <- data.frame()
##r3b: "Factor", "Value"


	d3c <- data.frame()
##r3c: "Factor", "Value", "ID1", "ID2", "ID3"


	d3d <- data.frame()
##r3d: "Factor", "Value"


	d3e <- data.frame()
##r3e: "Intervention category", "Intervention type", "Date of intervention", "Name of product *", "Product concentration", "Dose of application", "Observations"


	d3f <- data.frame(
		variety = r3f[["Clone/variety name"]]
	)
##r3f: "Numeration", "Control", "CIP number", "Code", "Pedigree", "Species", "Origin", "Seed source1", "References to silmultaneous trials", "References to previous trials", "Breeder Code or Collecting Number", "Accession name"


	d3g <- data.frame()
##r3g: "EVALUATION OF POLYPHENOLS", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15"


	d3h <- data.frame(
		N_fertilizer = r3h[["n"]],
		year = r3h[["Year"]],
		country = r3h[["Country"]],
		adm2 = carobiner::fix_name(r3h[["Province"]], "title")
	)
##r3h: "INSTN", "Breeder Code or Collecting Number", "Accession me", "Departament", "District", "Locality", "Phen comp (Chlorogenic Acid)   (mg/100g,DW) _mean", "Phen comp (Chlorogenic Acid)   (mg/100g,DW) _sd", "Phen comp  (Chlorogenic Acid)  (mg/100g,FW) _mean", "Phen comp  (Chlorogenic Acid)  (mg/100g,FW) _sd", "Percentage Dry matter(%)_mean", "Percentage Dry matter(%)_sd"


	d3i <- data.frame()
##r3i: "...1", "...2", "RESULTADOS - SAS", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11"


	d3j <- data.frame(
		plot_id = r3j[["Plot"]],
		year = r3j[["Year ()"]]
	)
##r3j: "Block", "Genotype", "Breeder Code", "Code laboratory ()", "Locality(F) ()", "Phen comp ( Chlorogenic Acid)(mg / 100 g, DW)", "Phen comp ( Chlorogenic Acid)(mg / 100 g, FW)", "Phen comp (Galic Acid)(mg / 100 g, DW)", "Phen comp (Galic Acid)(mg / 100 g, FW)", "Percentage Dry matter(%)"


	d3k <- data.frame()
##r3k: "DES...1", "ID...2", "...3", "DES...4", "ID...5", "...6", "DES...7", "ID...8", "...9", "DES...10", "ID...11", "...12", "DES...13", "ID...14", "...15", "DES...16", "ID...17", "...18", "DES...19", "ID...20", "...21", "DES...22", "ID...23", "ID2", "ID3", "ID4", "ID5", "...28", "DES...29", "ID...30", "...31", "LIST1_EVALUATIONS", "...33", "LIST_DESIGN", "ID_DESIGN", "...36", "LIST_SOIL", "...38", "LIST_METO", "...40", "LIST_DEPA", "...42", "LIST_PRO", "...44", "LIST_DIST"


	d3l <- data.frame()
##r3l: "type", "desc_type"


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
		year = r4k[["Year ()"]]
	)
##r4k: "Block", "Genotipe", "Locality(F) ()", "Breeder Code", "Ascorbic acid(mg / 100 g, DW)", "Ascorbic acid(mg / 100 g, FW)", "Percentage Dry matter(%)"


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

