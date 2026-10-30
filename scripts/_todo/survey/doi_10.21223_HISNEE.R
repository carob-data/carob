# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Jump-starting Orange-Fleshed Sweetpotato In West Africa Through Diversified Markets

The Jump-starting project had the goal of sustainable and inclusive market-driven approaches for orange-fleshed sweetpotato (OFSP) to increase incomes, and improve health through consumption of vitamin A rich OFSP, especially in women and children. The project started in 2014 and was implemented in 3 West African countries, namely Burkina Faso, Ghana and Nigeria. This baseline survey data is from Ghana and was conducted with the aim of characterizing the households targeted. It specifically focused on, among others; household demographics, asset ownership, nutrition knowledge and dietary diversity, crops produced, knowledge, attitudes and practices and sources of vines
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/HISNEE"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Households; universe: This study focused on small-holder farm households with the respondents drawn from communities targeted by the project.; timeMethod: Cross-Sectional; samplingProcedure: This study focused on small-holder farm households. Specifically, the respondents were drawn from communities targeted by the project. In Ghana, 302 farmers drawn from 13 communities participated in the study. Among the communities targeted in Ghana, 8 were located in the Upper East region while 5 were in the Northern region. Non-random sampling procedure was used to obtain the study respondents. The respondents were mainly male heads of the household, although in a few cases in Ghana, female spouses of the male heads were interviewed. Efforts were made to interview as many of the households with children under 5 years of age as possible. Respondents were approached, introduced to the study and consent to voluntarily participate in the study sought. Interviews were only started after consent was obtained.; targetSampleSize: list(targetSampleActualSize = list(typeName = "targetSampleActualSize", multiple = FALSE, typeClass = "primitive", value = "302")); collectionMode: Data was collected by trained enumerators using direct/personal interviews. The data collected included demographic information, crop production and sales, seed systems (especially the sources of vines planted), agronomic and pest management practices, nutrition knowledge attitude and practices, household assets, and food consumption aimed at establishing the quality of diets. Diet quality was estimated using dietary diversity scores for children under 5 years and women of reproductive age. These are the members of household that are most at risk of vitamin A deficiency.; researchInstrument: Structured Questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "2.GH_JP_Base_questionnaire.xls"]
	f2 <- ff[basename(ff) == "Module A - Household Identication.dta"]
	f3 <- ff[basename(ff) == "Module B - Household Roster (Above 5 years).dta"]
	f4 <- ff[basename(ff) == "Module C - Household Roster (5 years and Below).dta"]
	f5 <- ff[basename(ff) == "Module D - Diet Diversity.dta"]
	f6 <- ff[basename(ff) == "Module E - Farmer's Membership to Associations (Main).dta"]
	f7 <- ff[basename(ff) == "Module E02 - Farmer Associations (Details).dta"]
	f8 <- ff[basename(ff) == "Module F1 - Food Crop Production.dta"]
	f9 <- ff[basename(ff) == "Module F2 - Cash Crop Production.dta"]
	f10 <- ff[basename(ff) == "Module G02 - Crop Sales (Roster).dta"]
	f11 <- ff[basename(ff) == "Module H - Road Access.dta"]
	f12 <- ff[basename(ff) == "Module I - SP Production (Preamble)-1.dta"]
	f13 <- ff[basename(ff) == "Module I - SP Production (Roster).dta"]
	f14 <- ff[basename(ff) == "Module I - SP Sales (Roster).dta"]
	f15 <- ff[basename(ff) == "Module J - Labour.dta"]
	f16 <- ff[basename(ff) == "Module K - APPK.dta"]
	f17 <- ff[basename(ff) == "Module L1 - Seed System (Preamble).dta"]
	f18 <- ff[basename(ff) == "Module L2 - Seed System (Vine Dispatch).dta"]
	f19 <- ff[basename(ff) == "Module L3 - Seed System (Vine Receipt).dta"]
	f20 <- ff[basename(ff) == "Module L4 - Seed System (Postscript).dta"]
	f21 <- ff[basename(ff) == "Module M - SP Knowledge & Practices.dta"]
	f22 <- ff[basename(ff) == "Module N - Vitamin A Knowledge.dta"]
	f23 <- ff[basename(ff) == "Module O - Nutrition Knowledge and Habits.dta"]
	f24 <- ff[basename(ff) == "Module P - Vitamin A Rich Foods Consumption.dta"]
	f25 <- ff[basename(ff) == "Module Q - Minimum Meal Frequency.dta"]
	f26 <- ff[basename(ff) == "Module R - Household Food security.dta"]
	f27 <- ff[basename(ff) == "Module S01 - Livestock Status.dta"]
	f28 <- ff[basename(ff) == "Module S02 - Fish Sales.dta"]
	f29 <- ff[basename(ff) == "Module T01 - Household Assets.dta"]
	f30 <- ff[basename(ff) == "Module T02 - Main Housing.dta"]
	f31 <- ff[basename(ff) == "BF_JP_Base_data_dictionary.txt"]
	f32 <- ff[basename(ff) == "Module A - Household Identication.sav"]
	f33 <- ff[basename(ff) == "Module B - Household Roster (Above 5 years).sav"]
	f34 <- ff[basename(ff) == "Module C - Household Roster (5 years and Below).sav"]
	f35 <- ff[basename(ff) == "Module D - Diet Diversity.sav"]
	f36 <- ff[basename(ff) == "Module E - Farmer's Membership to Associations (Main).sav"]
	f37 <- ff[basename(ff) == "Module E02 - Farmer Associations (Details)-1.sav"]
	f38 <- ff[basename(ff) == "Module F1 - Food Crop Production-1.sav"]
	f39 <- ff[basename(ff) == "Module F2 - Cash Crop Production-1.sav"]
	f40 <- ff[basename(ff) == "Module G02 - Crop Sales (Roster).sav"]
	f41 <- ff[basename(ff) == "Module H - Road Access.sav"]
	f42 <- ff[basename(ff) == "Module I - SP Production (Preamble).sav"]
	f43 <- ff[basename(ff) == "Module I - SP Production (Roster).sav"]
	f44 <- ff[basename(ff) == "Module I - SP Sales (Roster).sav"]
	f45 <- ff[basename(ff) == "Module J - Labour.sav"]
	f46 <- ff[basename(ff) == "Module K - APPK.sav"]
	f47 <- ff[basename(ff) == "Module L1 - Seed System (Preamble).sav"]
	f48 <- ff[basename(ff) == "Module L2 - Seed System (Vine Dispatch).sav"]
	f49 <- ff[basename(ff) == "Module L3 - Seed System (Vine Receipt).sav"]
	f50 <- ff[basename(ff) == "Module L4 - Seed System (Postscript).sav"]
	f51 <- ff[basename(ff) == "Module M - SP Knowledge & Practices.sav"]
	f52 <- ff[basename(ff) == "Module N - Vitamin A Knowledge.sav"]
	f53 <- ff[basename(ff) == "Module O - Nutrition Knowledge and Habits.sav"]
	f54 <- ff[basename(ff) == "Module P - Vitamin A Rich Foods Consumption.sav"]
	f55 <- ff[basename(ff) == "Module Q - Minimum Meal Frequency.sav"]
	f56 <- ff[basename(ff) == "Module R - Household Food security.sav"]
	f57 <- ff[basename(ff) == "Module S01 - Livestock Status-1.sav"]
	f58 <- ff[basename(ff) == "Module S02 - Fish Sales-1.sav"]
	f59 <- ff[basename(ff) == "Module T01 - Household Assets-1.sav"]
	f60 <- ff[basename(ff) == "Module T02 - Main Housing.sav"]

	r1a <- carobiner::read.excel(f1, sheet="Cover page")
	r1b <- carobiner::read.excel(f1, sheet="Adult demographics")
	r1c <- carobiner::read.excel(f1, sheet="Children demographics & dietary")
	r1d <- carobiner::read.excel(f1, sheet="Farmer group ass.")
	r1e <- carobiner::read.excel(f1, sheet="Crop production")
	r1f <- carobiner::read.excel(f1, sheet="Crop sales")
	r1g <- carobiner::read.excel(f1, sheet="SP & Yam pdn")
	r1h <- carobiner::read.excel(f1, sheet="Labor")
	r1i <- carobiner::read.excel(f1, sheet="Attitude, P, P & K")
	r1j <- carobiner::read.excel(f1, sheet="Seed system 1")
	r1k <- carobiner::read.excel(f1, sheet="Seed System 2")
	r1l <- carobiner::read.excel(f1, sheet="Seed System 3")
	r1m <- carobiner::read.excel(f1, sheet="SP knowledge & Practice 1")
	r1n <- carobiner::read.excel(f1, sheet="SP knowledge & Practice 2")
	r1o <- carobiner::read.excel(f1, sheet="SP Knowledge & Practice 3")
	r1p <- carobiner::read.excel(f1, sheet="Knowledge VIT A")
	r1q <- carobiner::read.excel(f1, sheet="VIT A food Freq")
	r1r <- carobiner::read.excel(f1, sheet="MIN MEAL FREQ")
	r1s <- carobiner::read.excel(f1, sheet="HH Food Security")
	r1t <- carobiner::read.excel(f1, sheet="Livestock & Economic")
	r1u <- carobiner::read.excel(f1, sheet="HH assets")
	r1v <- carobiner::read.excel(f1, sheet="HH assets 2")
	r1w <- carobiner::read.excel(f1, sheet="Sheet1")
	r2 <- haven::read_dta(f2)
	r3 <- haven::read_dta(f3)
	r4 <- haven::read_dta(f4)
	r5 <- haven::read_dta(f5)
	r6 <- haven::read_dta(f6)
	r7 <- haven::read_dta(f7)
	r8 <- haven::read_dta(f8)
	r9 <- haven::read_dta(f9)
	r10 <- haven::read_dta(f10)
	r11 <- haven::read_dta(f11)
	r12 <- haven::read_dta(f12)
	r13 <- haven::read_dta(f13)
	r14 <- haven::read_dta(f14)
	r15 <- haven::read_dta(f15)
	r16 <- haven::read_dta(f16)
	r17 <- haven::read_dta(f17)
	r18 <- haven::read_dta(f18)
	r19 <- haven::read_dta(f19)
	r20 <- haven::read_dta(f20)
	r21 <- haven::read_dta(f21)
	r22 <- haven::read_dta(f22)
	r23 <- haven::read_dta(f23)
	r24 <- haven::read_dta(f24)
	r25 <- haven::read_dta(f25)
	r26 <- haven::read_dta(f26)
	r27 <- haven::read_dta(f27)
	r28 <- haven::read_dta(f28)
	r29 <- haven::read_dta(f29)
	r30 <- haven::read_dta(f30)
	#r31 <- read.???(f31)
	#r32 <- read.???(f32)
	#r33 <- read.???(f33)
	#r34 <- read.???(f34)
	#r35 <- read.???(f35)
	#r36 <- read.???(f36)
	#r37 <- read.???(f37)
	#r38 <- read.???(f38)
	#r39 <- read.???(f39)
	#r40 <- read.???(f40)
	#r41 <- read.???(f41)
	#r42 <- read.???(f42)
	#r43 <- read.???(f43)
	#r44 <- read.???(f44)
	#r45 <- read.???(f45)
	#r46 <- read.???(f46)
	#r47 <- read.???(f47)
	#r48 <- read.???(f48)
	#r49 <- read.???(f49)
	#r50 <- read.???(f50)
	#r51 <- read.???(f51)
	#r52 <- read.???(f52)
	#r53 <- read.???(f53)
	#r54 <- read.???(f54)
	#r55 <- read.???(f55)
	#r56 <- read.???(f56)
	#r57 <- read.???(f57)
	#r58 <- read.???(f58)
	#r59 <- read.???(f59)
	#r60 <- read.???(f60)

## select the variables of interest and assign them to the correct name

	d1a <- data.frame()
##r1a: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "Pg1", "...87"


	d1b <- data.frame(
		year = r1b[["B. HOUSEHOLD MEMBERS WITH AN AGE ABOVE 60 MONTHS (OLDER THAN 5 YEARS)"]]
	)
##r1b: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80"


	d1c <- data.frame(
		year = r1c[["C. DEMOGRAPHIC OF MEMBERS OF HOUSEHOLD WHO ARE LESS OR EQUAL TO 60 MONTHS OF AGE (5 YEARS OR YOUNGER)"]]
	)
##r1c: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "Pg 3", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95"


	d1d <- data.frame(
		hhid = r1d[["E. Farmer's membership to farmers associations/ other groups"]]
	)
##r1d: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "Group10.sav (key variables: hhid, group)", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99", "...100", "Pg 4"


	d1e <- data.frame(
		crop = tolower(r1e[["F. CROP PRODUCTION"]])
	)
##r1e: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "Pg 5"


	d1f <- data.frame(
		crop = tolower(r1f[["G. Crop sales"]])
	)
##r1f: "...1", "...2", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "Pg 6", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99"


	d1g <- data.frame()
##r1g: "...1", "I. SWEETPOTATO PRODUCTION", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99", "...100", "...101", "...102", "...103", "...104", "...105", "Pg 7"


	d1h <- data.frame()
##r1h: "...1", "J. Labor", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "J09", "...68", "...69", "For the following sweetpotato activities tell me who is most responsible", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99", "...100", "...101", "...102", "...103", "...104", "...105", "...106", "...107", "Pg8"


	d1i <- data.frame()
##r1i: "K. Attitude, Perception, Practice and Knowledge", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "Pg 9"


	d1j <- data.frame(
		hhid = r1j[["L. Seed system: Vine diffusion (seed01.sav key variables: hhid seedcode)"]]
	)
##r1j: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "Pg 10"


	d1k <- data.frame()
##r1k: "...1", "If anyone in your household received any sweetpotato vines from elsewhere apart from your own farm, please fill in the table below", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "Pg 11"


	d1l <- data.frame()
##r1l: "...1", "(continue from previous page)", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "Pg 12"


	d1m <- data.frame()
##r1m: "M. Sweetpotato,  knowledge, and practices", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "Pg 13"


	d1n <- data.frame()
##r1n: "M. Sweetpotato,  knowledge, and knowledge (CONT.)", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "Pg 14", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99", "...100", "...101", "...102", "...103", "...104", "...105", "...106", "...107", "...108", "...109"


	d1o <- data.frame()
##r1o: "M47", "...2", "...3", "I am going to read for you a list of traits about sweetpotato. Please tell me if you like, dislike the characteristics,or  is not important to you at all.", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "Pg 15", "...85", "...86", "...87", "...88", "...89"


	d1p <- data.frame()
##r1p: "N.  KNOWLEDGE ABOUT VITAMIN A", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "Pg 16"


	d1q <- data.frame()
##r1q: "...1", "...2", "P. FREQUENCY OF CONSUMPTION OF VITAMIN A RICH FOODS", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "Pg 17", "...78", "...79", "...80", "...81", "...82", "...83", "...84"


	d1r <- data.frame()
##r1r: "Q. MINIMUM MEAL FREQUENCY OF THE REFERENCE CHILD", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "Pg 18", "...98", "...99", "...100"


	d1s <- data.frame()
##r1s: "R.  HOUSEHOLD FOOD SECURITY", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "Pg 19", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82"


	d1t <- data.frame()
##r1t: "...1", "...2", "S. LIVESTOCK, FISHERY AND ECONOMIC ACTIVITIES", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "Pg 20"


	d1u <- data.frame(
		hhid = r1u[["T. Household Assets : asset10.sav  (Key variables: hhid, item)"]]
	)
##r1u: "...1", "...2", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "Pg 21"


	d1v <- data.frame()
##r1v: "...1", "...2", "...3", "...4", "(Observe or ask about the following)", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "Pg 22"


	d1w <- data.frame()
##r1w: "eduction: 1-no formal, 2=primary (level 1 to 6), 3 =secondary education (GS 1 to SS3)", "...2", "...3", "...4", "...5"


	d1 <- data.frame(
		country = r1[["country_code"]]
	)
##r1: "a01", "a02", "a03", "a04", "a05", "a06a", "a06", "a07", "a08", "a09", "a10", "a12a", "a12", "a13", "a14a", "a14b", "a14c", "a14d", "a14e", "a14f", "a15", "a16", "a17", "a18", "a19", "a20", "a21", "a22", "a23"


	d2 <- data.frame()
##r2: "a01", "a02", "a03", "a04", "a05", "mem", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "b13", "b14", "a15", "doi"


	d3 <- data.frame()
##r3: "a01", "a02", "a03", "a04", "a05", "mem_child", "c01", "c02", "dob_child", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "a15", "doi"


	d4 <- data.frame()
##r4: "a01", "a02", "a03", "a04", "a05", "d01", "d02", "d03a", "d03b", "d04a", "d04b", "d05a", "d05b", "d06a", "d06b", "d07a", "d07b", "d08a", "d08b", "d09a", "d09b", "d10a", "d10b", "d11a", "d11b", "d12a", "d12b", "d13a", "d13b", "d14a", "d14b", "d15a", "d15b", "d16a", "d16b", "d17a", "d17b", "d18a", "d18b", "d19a", "d19b", "d20a", "d20b", "d21a", "d21b", "d22a", "d22b"


	d5 <- data.frame()
##r5: "a01", "a02", "a03", "a04", "a05", "e01", "e03", "e04", "e05", "e06a", "e06b", "e06c", "e07a", "e07b", "e07c", "e08a", "e08b", "e08c", "e09", "e10", "e11", "e12", "e13a", "e13b", "e13c", "e14a", "e14b", "e14c"


	d6 <- data.frame()
##r6: "a01", "a02", "a03", "a04", "a05", "mem_in_groups", "e02a", "e02b", "e02c", "e02d", "e02e", "e02f"


	d7 <- data.frame()
##r7: "a01", "a02", "a03", "a04", "a05", "f02", "f02a", "f03", "f04", "f05"


	d8 <- data.frame()
##r8: "a01", "a02", "a03", "a04", "a05", "f21", "f21a", "f22", "f23", "f24"


	d9 <- data.frame()
##r9: "a01", "a02", "a03", "a04", "a05", "g03", "g04", "g05", "g06", "g07", "g08", "g08b", "g09", "g10", "g11", "g12", "g12a", "g13", "g13a", "g14"


	d10 <- data.frame()
##r10: "a01", "a02", "a03", "a04", "a05", "h01", "h02a", "h02b", "h03a", "h03b", "h04a", "h04b"


	d11 <- data.frame()
##r11: "a01", "a02", "a03", "a04", "a05", "i01", "i02", "i03", "i03a", "i03b", "i03c", "i03d", "i03e", "i04", "i05", "i06"


	d12 <- data.frame()
##r12: "a01", "a02", "a03", "a04", "a05", "i07", "i08", "i09", "i10a", "i10b", "i10c", "i10d", "i10e", "i10f", "i10g", "i10h", "i10i", "i10j", "i10k", "i10l", "i11a", "i11b", "i12a", "i12b", "i13a", "i13b", "i14a", "i14b", "i15a", "i15b", "i16", "i17a", "i17b"


	d13 <- data.frame()
##r13: "a01", "a02", "a03", "a04", "a05", "i18a", "i18b", "i19a", "i20", "i21a", "i21b", "i22a", "i22b", "i23", "i24", "i25a", "i25b", "i25c", "i26"


	d14 <- data.frame()
##r14: "a01", "a02", "a03", "a04", "a05", "j01", "j02a", "j02b", "j03a", "j03b", "j04a", "j04b", "j04c", "j04d", "j04e", "j05", "j06", "j07a", "j07b", "j07c", "j07d", "j07e", "j07f", "j08", "j09a", "j09b", "j09c", "j09d", "j09e", "j09f", "j09g", "j09h", "j09i", "j09j", "j09k", "j09l", "j09m"


	d15 <- data.frame()
##r15: "a01", "a02", "a03", "a04", "a05", "k01", "k02", "k03", "k04", "k05", "k06", "k07", "k08", "k09", "k10", "k11", "k12", "k13", "k14", "k14a", "k15", "k16"


	d16 <- data.frame()
##r16: "a01", "a02", "a03", "a04", "a05", "l01a", "l01b", "l01c", "l01d", "l01e", "l01f", "l01g", "l01h", "l01i", "l01j", "l02", "l03", "l04a", "l04b", "l05", "l06", "l07", "l08", "l09"


	d17 <- data.frame()
##r17: "a01", "a02", "a03", "a04", "a05", "l11", "l12", "l13", "l14", "l15", "l16", "l17", "l18", "l19", "l20", "l21", "l22", "l23"


	d18 <- data.frame()
##r18: "a01", "a02", "a03", "a04", "a05", "l24", "l25", "l26", "l27", "l28", "l29", "l30", "l31", "l32", "l33", "l34", "l35", "l36", "l37"


	d19 <- data.frame()
##r19: "a01", "a02", "a03", "a04", "a05", "l38", "l39", "l40", "l41", "l42", "l43", "l44", "l45", "l46a", "l46b", "l47", "l48a", "l48b", "l49", "l50", "l51", "l52", "l53", "l54", "l55", "l56", "l57", "l57a", "l58", "l58a", "l59"


	d20 <- data.frame()
##r20: "a01", "a02", "a03", "a04", "a05", "m01", "m02", "m03", "m03b", "m04", "m05", "m06", "m07a", "m07b", "m07c", "m07d", "m07e", "m07f", "m07g", "m08", "m09", "m10", "m11", "m12a", "m12b", "m13", "m13a", "m14", "m15", "m15a", "m16", "m17", "m17a", "m18", "m19", "m20", "m20a", "m21a", "m21b", "m21c", "m21d", "m21e", "m22", "m23a2", "m23b2", "m23c2", "m23d2", "m24", "m25", "m26", "m27", "m28", "m29", "m30", "m31", "m32a", "m32b", "m32c", "m32d", "m32e", "m32f", "m32g", "m33", "m34a", "m34b", "m34c", "m34d", "m34e", "m34f", "m34g", "m34h", "m34i", "m35a", "m35b", "m35c", "m35d", "m36", "m37a", "m38_pile", "m38_pile_units", "m37_sack", "m38_sack", "m38_sack_units", "m37_pit", "m38_pit", "m38_pit_units", "m37_wood", "m38_wood", "m38_wood_units", "m37_ash", "m38_ash", "m38_ash_units", "m37_sand", "m38_sand", "m38_sand_units", "m37_soil", "m38_soil", "m38_soil_units", "m37_other", "m38_other", "m38_other_units", "m39", "m40", "m40a", "m41a", "m41b", "m41c", "m42", "m42a", "m43", "m44", "m45", "m46"


	d21 <- data.frame()
##r21: "a01", "a02", "a03", "a04", "a05", "n01", "n02", "n02a", "n02b", "n02c", "n03a", "n03b", "n03c", "n04a", "n04_other_a", "n04b", "n04_other_b", "n04c", "n04_other_c"


	d22 <- data.frame()
##r22: "a01", "a02", "a03", "a04", "a05", "o01", "o02", "o02a", "o03", "o03u", "o04", "o04u", "o05", "o05u", "o06", "o06u", "o07a", "o08", "o09a", "o09b", "o09c", "o09d", "o09e", "o09f", "o09g", "o09h", "o09i", "o09j", "o09k", "o09l", "o09m", "o10", "o11", "o012a", "o012b", "o012c", "o13a", "o13b", "o13c", "o13d", "o13e", "o13f", "o13g", "o014"


	d23 <- data.frame()
##r23: "a01", "a02", "a03", "a04", "a05", "p00", "p01", "p02", "p03", "p04", "p04a", "p05", "p06", "p07a", "p08a", "p07b", "p08b", "p07c", "p08c", "p07d", "p08d", "p07e", "p08e", "p07f", "p08f", "p07g", "p08g", "p07h", "p08h", "p07i", "p08i", "p07j", "p08j", "p07k", "p08k", "p07l", "p08l", "p07m", "p08m", "p07n", "p08n", "p07o", "p08o", "p07p", "p08p", "p07q", "p08q", "p07r", "p08r", "p07s", "p08s", "p07t", "p08t", "p07u", "p08u", "p07v", "p08v", "p07w", "p08w", "p07x", "p08x", "p07y", "p08y", "p07z", "p08z", "p07aa", "p08aa", "p07bb", "p08bb", "p07cc", "p08cc", "p07dd", "p08dd", "p07ee", "p08ee", "p07ff", "p08ff", "p07gg", "p08gg", "p07hh", "p08hh", "p07ii", "p08ii", "p07jj", "p08jj", "p07kk", "p08kk", "p07ll", "p08ll", "p07mm", "p08mm", "p07nn", "p08nn", "p07oo", "p08oo", "p07pp", "p08pp", "p09", "p09a", "p10", "p10a", "p11"


	d24 <- data.frame()
##r24: "a01", "a02", "a03", "a04", "a05", "q01", "q02", "q03", "q04", "q05a", "q05b", "q06", "q07a", "q07b", "q08", "q09", "q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18", "q19", "q20", "q21a", "q21b", "q21c", "q21d", "q21e", "q21f", "q22", "q22a", "q22b", "q22c", "q22d", "q22e", "q22f", "q22g"


	d25 <- data.frame()
##r25: "a01", "a02", "a03", "a04", "a05", "r01a", "r01b", "r02a", "r02b", "r03a", "r03b", "r04a", "r04b", "r05a", "r05b", "r06a", "r06b", "r07a", "r07b", "r08a", "r08b", "r09a", "r09b", "r10a", "r10b", "r10c", "r10d", "r10e", "r10f", "r10g", "r10h", "r10i", "r10j", "r10k", "r10l", "r11a", "r11b", "r11c", "r11d", "r11e", "r11f", "r11g", "r11h", "r11i", "r11j", "r11k", "r11l", "r12a", "r12b", "r12c", "r12d", "r12e", "r12f", "r12g", "r12h", "r12i", "r12j", "r12k", "r12l", "r13", "r14a", "r14b", "r14c", "r15a", "r15b", "r15c"


	d26 <- data.frame()
##r26: "a01", "a02", "a03", "a04", "a05", "s01", "s01a", "s02", "s03", "s04"


	d27 <- data.frame()
##r27: "a01", "a02", "a03", "a04", "a05", "s05", "s06", "s07", "s08", "s09", "s10", "s11", "s12"


	d28 <- data.frame()
##r28: "a01", "a02", "a03", "a04", "a05", "t02", "t02a", "t03", "t04", "t05", "t06"


	d29 <- data.frame()
##r29: "a01", "a02", "a03", "a04", "a05", "t07", "t07a", "t08", "t08a", "t09", "t09b", "t10", "t10a", "t11", "t12", "t12a", "t13", "t13a", "t14a", "t14b", "t15", "t15a", "t16", "t16a", "t17", "t17a", "t18", "t18a", "t19", "t19a", "t20"


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

