# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: The Viable Sweetpotato Technologies in Africa (VISTA)-Mozambique: The Baseline Survey  for Nampula and Zambezia Provinces

The baseline survey for Viable Sweetpotato Technologies in Africa (VISTA) project in Mozambique was conducted in May-June 2015 in four districts of Nampula province (Monapo, Meconta, Nampula-Rapale and Murrupula) and two of Zambezia province (Alto Molocue and Gurue) in Mozambique.  About 640 households were interviewed. Most of the visited households had at least one child less than 5 years old, and 54% of them were female. The average age of the household heads was 39 years, and most of them with fourth grade education level. Overall, about 10% of the household heads were women. Other relevant items included were related to farmer organizations, crop production, Seed system and related activities, farmers' practices, attitude and perception on sweetpotato, Vitamin A and other nutrition knowledge and household and individually dietary diversity.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/NN00OC"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=7,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Household; universe: Children under 5, above 5, adult Women and men; timeMethod: Cross-sectional; dataCollector: Abdul Naico and Higino Andre; samplingProcedure: A total of 640 head of the households were interviewed in the 168 villages visited during the baseline survey. Overall, the number of household interviewed represented a good sample size to enable statistic inferences. This sample size was established taking into account both direct and indirect project beneficiaries in the targeted areas. Because of that, and taking into account that Nampula has potentially 5.3% of the 3,500,000 potential farmers growing sweetpotatoes in the country, while Zambezia 29% (TIA-2012), we come out with our overall population of almost 60,000 farmers growing sweetpotato in the targeted areas. For sake of estimation, we have considered the two district of Zambezia as for Nampula because, in reality, these districts are very similar to the traditional districts of Nampula than Zambezia in terms of sweetpotato production. 
However, for this baseline survey we considered the 157,000 beneficiaries, that is, the 22,500 direct and 135,000 indirect beneficiaries as our overall sweetpotato potential, and using a rule of thumb of 0.7 standard deviation, 10% margin of error, and fixing the critical value from the normal probability distribution (z) at 1.96 corresponding to a 95% confidence level 
From here we obtained an initial sample size of 138 households. Then we adjusted this sample size to the design effect, that is, we had 3 levels randomization in our sampling frame, that is, we randomly selected the localities, villages, and beneficiaries. This 3 level design effect increased the sample size to 564 (3*188). Also we had to adjust this sample size to non-response rate, and we fixed the rate at 10% to come out with the final sample size of 627 households. However, we have also adjusted the possible loss of the questionnaires (man handling error) to a total of 640 households. For this type of study, Feed the Future recommends the adoption of a minimum overall simple size of 525 beneficiaries (Stukel and Friedman, 2016).
The dietary diversity scores were calculated according to FAO (Kennedy, et al., 2011), by summing the number of food groups consumed in the household or by the individual respondent over the 24-hour recall period. In general, there were created groups of foods to match the basics needs for the HDDS (Household Dietary Diversity Scores) and IDDS (Individual Dietary Diversity Scores).; collectionMode: In person interview at in the communities at village level using a anthropometric measures and data on dietary diversity. These households were selected using the combination of the simple and systematic sampling approaches. A questionnaire designed to capture the most important events on the agriculture-nutrition-marketing related activities under VISTA, and the issued of gender and farmer organization was employed.; researchInstrument: Strutured questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "Lista of FILES contained in the folder_DATASET Mozambique_VISTA BASELINE SURVEY 2015.xlsx"]
	f2 <- ff[basename(ff) == "Questionnaires for VISTA Modules of the  Baseline Survey in Portuguese.xls"]
	f3 <- ff[basename(ff) == "BaseLineSurvey_VISTA Baseline Survey Dictionary Nampula_Mozambique_2015"]
	f4 <- ff[basename(ff) == "PAG16_Baseline Survey VISTA_2015_Practices and Preferences.sav"]
	f5 <- ff[basename(ff) == "PAG2B_Baseline Survey VISTA_2015_Household members with age above 60 Months.sav"]
	f6 <- ff[basename(ff) == "PAGE 1_Baseline Survey VISTA_2015_Household identification and statement of agreement.sav"]
	f7 <- ff[basename(ff) == "PAGE 11_Baseline Survey VISTA_2015_Seed System_General.sav"]
	f8 <- ff[basename(ff) == "PAGE 11_Baseline Survey VISTA_2015_Seed System_Vine Dissemination.sav"]
	f9 <- ff[basename(ff) == "PAGE 13_Baseline Survey VISTA_2015_Attitude, Knowledge, practices.sav"]
	f10 <- ff[basename(ff) == "PAGE 17_Baseline Survey VISTA_2015_Women Knowledge on Vitamina A.sav"]
	f11 <- ff[basename(ff) == "PAGE 19_Baseline Survey VISTA_2015_Dietary Diversity for Women and Children.sav"]
	f12 <- ff[basename(ff) == "PAGE 2_Bseline Survey VISTA_2015_Demographic of Members of Household who are less or equal to 60 months of Age (5 years).sav"]
	f13 <- ff[basename(ff) == "PAGE 3_Baseline Survey VISTA_2015_Affiliation to ssociation or group.sav"]
	f14 <- ff[basename(ff) == "PAGE 4_Baseline Survey VISTA_2015 Crop production.sav"]
	f15 <- ff[basename(ff) == "PAGE 5_Baseline Survey VISTA_2015_Crop production_Area and use of fertilizer.sav"]
	f16 <- ff[basename(ff) == "PAGE 6_Baseline Survey VISTA_2015_Sweetpotato production.sav"]
	f17 <- ff[basename(ff) == "PAGE 9_Baseline Survey VISTA_2015_Sweetpotato marketing and selling.sav"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="1-Cover page")
	r2b <- carobiner::read.excel(f2, sheet="2-Demographics")
	r2c <- carobiner::read.excel(f2, sheet="3-Farm group")
	r2d <- carobiner::read.excel(f2, sheet="4-Crop Production 1")
	r2e <- carobiner::read.excel(f2, sheet="5-Crop production area")
	r2f <- carobiner::read.excel(f2, sheet="6-SP Production")
	r2g <- carobiner::read.excel(f2, sheet="7-Marketing crops")
	r2h <- carobiner::read.excel(f2, sheet="8-SP Marketing")
	r2i <- carobiner::read.excel(f2, sheet="9-Trading channels")
	r2j <- carobiner::read.excel(f2, sheet="10-Labor")
	r2k <- carobiner::read.excel(f2, sheet="11-Sistema de Sementes 1")
	r2l <- carobiner::read.excel(f2, sheet="12-Sistema de semente 2")
	r2m <- carobiner::read.excel(f2, sheet="13-Attitude")
	r2n <- carobiner::read.excel(f2, sheet="14-SP Knoledg VISTA")
	r2o <- carobiner::read.excel(f2, sheet="15-Conservacao")
	r2p <- carobiner::read.excel(f2, sheet="16-Variedades de Batata-doce")
	r2q <- carobiner::read.excel(f2, sheet="17-Women knowledge VIT A")
	r2r <- carobiner::read.excel(f2, sheet="18-HH FOOD SECURITY")
	r2s <- carobiner::read.excel(f2, sheet="19-WOMEN & CHILD DIVERSITY")
	r2t <- carobiner::read.excel(f2, sheet="20-VITA A RICH FOOD FREQ")
	r2u <- carobiner::read.excel(f2, sheet="21-lAST HH Assets")
	r2v <- carobiner::read.excel(f2, sheet="Crop codes")
	#r3 <- read.???(f3)
	#r4 <- read.???(f4)
	#r5 <- read.???(f5)
	#r6 <- read.???(f6)
	#r7 <- read.???(f7)
	#r8 <- read.???(f8)
	#r9 <- read.???(f9)
	#r10 <- read.???(f10)
	#r11 <- read.???(f11)
	#r12 <- read.???(f12)
	#r13 <- read.???(f13)
	#r14 <- read.???(f14)
	#r15 <- read.???(f15)
	#r16 <- read.???(f16)
	#r17 <- read.???(f17)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "1. DATA", "...2"


	d2a <- data.frame()
##r2a: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "Pg1", "...87"


	d2b <- data.frame()
##r2b: "B. CARACTERISTICAS DOS ENTREVISTADOS E CRIANCAS MAIS DE 5 ANOS NO AF", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "AFID", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84"


	d2c <- data.frame(
		adm2 = carobiner::fix_name(r2c[["PROVINCIA"]], "title")
	)
##r2c: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "DIST", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "POST ADM", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "ALDEIA", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "AF", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "Pg 3", "...69"


	d2d <- data.frame(
		adm2 = carobiner::fix_name(r2d[["PROVINCIA"]], "title")
	)
##r2d: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "DIST", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "POST ADM", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "ALDEIA", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "AF", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "Pg 4", "...75", "...76", "...77"


	d2e <- data.frame(
		adm2 = carobiner::fix_name(r2e[["PROVINCIA"]], "title")
	)
##r2e: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "DIST", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "POST ADM", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "ALDEIA", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "AF", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99", "...100", "Pg 5", "...102", "...103", "...104"


	d2f <- data.frame(
		adm2 = carobiner::fix_name(r2f[["PROVINCIA"]], "title")
	)
##r2f: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "DIST", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "POST ADM", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "ALDEIA", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "AF", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99", "...100", "...101", "...102", "Pg 6", "...104", "...105"


	d2g <- data.frame(
		adm2 = carobiner::fix_name(r2g[["PROVINCIA"]], "title")
	)
##r2g: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "DIST", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "POST ADM", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "ALDEIA", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "AF", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99"


	d2h <- data.frame(
		adm2 = carobiner::fix_name(r2h[["PROVINCIA"]], "title")
	)
##r2h: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "DIST", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "POST ADM", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "ALDEIA", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "AF", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "Pg 8", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "Pg. 8"


	d2i <- data.frame(
		adm2 = carobiner::fix_name(r2i[["PROVINCIA"]], "title")
	)
##r2i: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "DIST", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "POST ADM", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "ALDEIA", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "AF", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "Pg. 9", "...72"


	d2j <- data.frame(
		adm2 = carobiner::fix_name(r2j[["PROVINCIA"]], "title")
	)
##r2j: "...1", "L. USO DA MAO DE OBRA", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "DIST", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "POST ADM", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "ALDEIA", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "AF", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99", "...100", "...101", "...102", "...103", "...104", "...105", "...106", "...107", "Pg 10", "...109", "...110", "...111", "...112", "...113", "...114", "...115"


	d2k <- data.frame(
		adm2 = carobiner::fix_name(r2k[["PROVINCIA"]], "title")
	)
##r2k: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "DIST", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "POST ADM", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "ALDEIA", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "AF", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "Pg 11"


	d2l <- data.frame(
		adm2 = carobiner::fix_name(r2l[["PROVINCIA"]], "title")
	)
##r2l: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "DIST", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "POST ADM", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "ALDEIA", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "AF", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "Pg 12"


	d2m <- data.frame(
		adm2 = carobiner::fix_name(r2m[["PROVINCIA"]], "title")
	)
##r2m: "...1", "...2", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "DIST", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "POST ADM", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "ALDEIA", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "AF", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "Pg 13"


	d2n <- data.frame(
		adm2 = carobiner::fix_name(r2n[["PROVINCIA"]], "title")
	)
##r2n: "...1", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "DIST", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "POST ADM", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "ALDEIA", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "AF", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "Pg 14"


	d2o <- data.frame(
		adm2 = carobiner::fix_name(r2o[["PROVINCIA"]], "title")
	)
##r2o: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "DIST", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "POST ADM", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "ALDEIA", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "AF", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "Pg 15"


	d2p <- data.frame(
		adm2 = carobiner::fix_name(r2p[["PROVINCIA"]], "title")
	)
##r2p: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "DIST", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "POST ADM", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "ALDEIA", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "AF", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "Pg 16"


	d2q <- data.frame(
		adm2 = carobiner::fix_name(r2q[["PROVINCIA"]], "title")
	)
##r2q: "E.  CONHECIMENTO DA MULHER ACERCA DA VITAMINA  A", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "DIST", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "POST ADM", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "ALDEIA", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "AF", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "Pg 17"


	d2r <- data.frame(
		adm2 = carobiner::fix_name(r2r[["PROVINCIA"]], "title")
	)
##r2r: "H.  SEGURANCA ALIMENTAR NO AGREGADO", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "DIST", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "POST ADM", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "ALDEIA", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "AF", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82"


	d2s <- data.frame(
		adm2 = carobiner::fix_name(r2s[["PROVINCIA"]], "title")
	)
##r2s: "...1", "...2", "...3", "D. DIVERSIDADE DIETARIA NAS MULHERES E CRINCAS", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "DIST", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "POST ADM", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "ALDEIA", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "AF", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97", "...98", "...99", "Pg. 19", "...101"


	d2t <- data.frame()
##r2t: "F. FREQUENCIA DE CONSUMO DE ALIMENTOS RICOS EM VITAMINA A E PRINCIPAIS GRUPOS DE ALIMENTOS  NOS ULTIMOS 7 DIAS", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "AF", "...93", "...94", "...95", "...96", "...97", "...98", "...99", "...100", "...101", "...102", "...103", "...104", "Pg 20"


	d2u <- data.frame(
		adm2 = carobiner::fix_name(r2u[["PROVINCIA"]], "title")
	)
##r2u: "...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "DIST", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "POST ADM", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "ALDEIA", "...55", "...56", "...57", "...58", "...59", "...60", "...61", "...62", "...63", "...64", "AF", "...66", "...67", "...68", "...69", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80", "...81", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94"


	d2v <- data.frame(
		variety = r2v[["Codigo de Variedades"]]
	)
##r2v: "Codogo de Culturas"


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

