# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Continuous Storage Root Formation and Bulking in Sweetpotato

The data set for the 'Continuous Storage Root Formation and Bulking in sweetpotato ' Gates Open Research paper has on-station plant breeding trials conducted at Namulonge and Serere in Wakiso and Serere districts respectively between 2016 and 2017 leading to an original research article. The on-station data are in Excel format for Sweetpotato Breeding Protocol (manual) http://www.sweetpotatoknowledge.org/). The on-station data on Continuous storage root formation and bulking scores, root numbers, vegetative growth data, and root weight from plots over 4 harvesting times used to estimate growth overtime and compute root yield, and biomass yield was the basis for the article. The pdf file has details on background information, site description, materials and methods, analysis, results and discussion, and relevant tables and illustrations. Supporting raw data and analysis tables include spreadsheets for singles harvests (at 90, 120, 150 and 180 Days after planting), the combined data sheet for all seasons and locations, and its respective analysis tables, summary means and breeding value estimations.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/WBC9WC"
	group <- "varieties_other"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "MAK; Institut des Sciences Agronomiques du Burundi; CIP",
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
	

	f1 <- ff[basename(ff) == "Continuous_Storage_Root_Bulking_Data and Analysis.xlsx"]
	f2 <- ff[basename(ff) == "Continuous_Storage_Root_Bulking_Data_Dictionary.xls"]
	f3 <- ff[basename(ff) == "Continuous_Storage_Root_Bulking_Material_list.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="4_90DAP_harvest")
	r1b <- carobiner::read.excel(f1, sheet="5_120DAP_harvest")
	r1c <- carobiner::read.excel(f1, sheet="6_150DAP_harvest")
	r1d <- carobiner::read.excel(f1, sheet="7_180DAP_Harvest")
	r1e <- carobiner::read.excel(f1, sheet="8_Combined harvests")
	r1f <- carobiner::read.excel(f1, sheet="9_ANOVA_90DAP")
	r1g <- carobiner::read.excel(f1, sheet="10_Means_90DAP")
	r1h <- carobiner::read.excel(f1, sheet="11_Means _120DAP")
	r1i <- carobiner::read.excel(f1, sheet="12_ANOVA_120DAP")
	r1j <- carobiner::read.excel(f1, sheet="13_ANOVA_150DAP")
	r1k <- carobiner::read.excel(f1, sheet="14_Means_150DAP")
	r1l <- carobiner::read.excel(f1, sheet="15_ANOVA_180DAP")
	r1m <- carobiner::read.excel(f1, sheet="16_Means_180DAP")
	r1n <- carobiner::read.excel(f1, sheet="17_Combined ANOVA")
	r1o <- carobiner::read.excel(f1, sheet="18_Means Cobined ")
	r1p <- carobiner::read.excel(f1, sheet="19_General means")
	r1q <- carobiner::read.excel(f1, sheet="20_Lin and Quad analysis")
	r1r <- carobiner::read.excel(f1, sheet="Sheet1")
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		plot_id = r1a[["Plot"]],
		rep = r1a[["Rep"]]
	)
##r1a: "Loc", "Seas", "HT", "ENTRY", "NOPH", "SRN", "SRL", "SRD", "NOCR", "NONC", "CRW", "NCRW", "Weevil", "Senc", "VW", "CSRFAB", "SRY", "VY", "HI"


	d1b <- data.frame(
		plot_id = r1b[["Plot"]],
		rep = r1b[["Rep"]]
	)
##r1b: "Loc", "Seas", "ENTRY", "NOPH", "SRN", "SRL", "SRD", "NOCR", "NONC", "CRW", "NCRW", "Weevil", "Senc", "VW", "CSRFAB", "SRY", "VY", "HI"


	d1c <- data.frame(
		plot_id = r1c[["Plot"]],
		rep = r1c[["Rep"]]
	)
##r1c: "Loc", "Seas", "HT", "ENTRY", "NOPH", "SRN", "SRL", "SRD", "NOCR", "NONC", "CRW", "NCRW", "Weevil", "Senc", "VW", "CSRFAB", "SRY", "VY", "HI"


	d1d <- data.frame(
		plot_id = r1d[["Plot"]],
		rep = r1d[["Rep"]]
	)
##r1d: "Loc", "Seas", "HT", "ENTRY", "NOPH", "SRN", "SRL", "SRD", "NOCR", "NONC", "CRW", "NCRW", "Weevil", "Senc", "VW", "CSRFAB", "SRY", "VY", "HI"


	d1e <- data.frame(
		plot_id = r1e[["Plot"]],
		rep = r1e[["Rep"]]
	)
##r1e: "Loc", "Seas", "HT", "ENTRY", "NOPH", "SRN", "SRL", "SRD", "NOCR", "NONC", "CRW", "NCRW", "Weevil", "Senc", "VW", "CSRFAB", "SRY", "VY", "HI"


	d1f <- data.frame()
##r1f: "Variate: SRN...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "Variate: SRN...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19"


	d1g <- data.frame()
##r1g: "Entry", "SRN", "Weevil", "Senc", "CSRFAB", "SRY", "VY", "HI", "...9", "...10", "...11", "PROB", "...13", "...14", "...15", "...16", "...17", "...18"


	d1h <- data.frame()
##r1h: "ENTRY", "SRN", "Weevil", "Senc", "CSRFAB", "SRY", "VY", "HI", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18"


	d1i <- data.frame()
##r1i: "Variate: SRN...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "Variate: SRN...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19"


	d1j <- data.frame()
##r1j: "Variate: SRN...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "Variate: SRN...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21"


	d1k <- data.frame()
##r1k: "ENTRY", "SRN", "Weevil", "Senc", "CSRFAB", "SRY", "VY", "HI", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17"


	d1l <- data.frame()
##r1l: "Variate: SRN...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "Variate: SRN...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20"


	d1m <- data.frame()
##r1m: "ENTRY", "SRN", "Weevil", "Senc", "CSRFAB", "SRY", "VY", "HI", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18"


	d1n <- data.frame()
##r1n: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "Variate: SRN", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19"


	d1o <- data.frame()
##r1o: "ENTRY", "SRN", "Weevil", "Senc", "CSRFAB", "SRY", "VY", "HI", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19"


	d1p <- data.frame()
##r1p: "Entry...1", "SRY_1...2", "SRY_2...3", "SRY_3...4", "SRY_4...5", "Change...6", "...7", "...8", "...9", "CSRFAB_1", "CSRFAB_2", "CSRFAB_3", "CSRFAB_4", "Change...14", "...15", "...16", "...17", "Entry...18", "SRY_1...19", "SRY_2...20", "SRY_3...21", "SRY_4...22", "...23", "Entry...24", "SRY_1...25", "SRY_2...26", "SRY_3...27", "SRY_4...28"


	d1q <- data.frame()
##r1q: "Residual variance model...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "Residual variance model...10", "...11", "...12", "...13", "...14", "...15", "...16", "Term", "Factor", "Model(order)", "Parameter", "Estimate", "s.e.", "...23", "Residual variance model...24", "...25", "...26", "...27", "...28", "...29", "...30", "Residual variance model...31", "...32", "...33", "...34", "...35", "...36", "...37", "Residual variance model...38", "...39", "...40", "...41", "...42", "...43", "...44", "Residual variance model...45", "...46", "...47", "...48", "...49", "...50", "...51", "Residual variance model...52", "...53", "...54", "...55", "...56", "...57", "...58", "Residual variance model...59", "...60", "...61", "...62", "...63", "...64", "...65", "Residual variance model...66", "...67", "...68", "...69", "...70", "...71", "...72", "Residual variance model...73", "...74", "...75", "...76", "...77", "...78"


	d1r <- data.frame()


	d2 <- data.frame(
		crop = tolower(r2[["Crop"]])
	)
##r2: "Order", "Abbreviature_dataset", "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9"


	d3 <- data.frame()
##r3: "Code", "Names"


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

