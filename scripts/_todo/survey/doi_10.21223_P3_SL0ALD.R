# REJECTED 
# Reason: no .xlsx/.xls/.csv/.dta/.rds files in download (draft cannot auto-map columns)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Nutritious Orange-Fleshed Sweetpotat (OFSP) For Niassa - Baseline Household Survey

Relevant information on the challenges and opportunities of the project were considered for the benefit of the project results. In fact, most of the findings from the study were used as the baseline reference for evaluating the impact of the project's intervention in the project target areas three years later, including activities on seed system, crop sales, marketing, nutrition and food security in general.  This baseline survey was conducted between from July 8 to August 23, 2013, and overall, 396 households were visited in 24 villages distributed across the 8 districts surveyed. Overall, about 90% of the farmers interviewed in the present study had agriculture as the most important activity. Among the farmers who mentioned sweetpotato, only about 12% of the households produced OFSP due to earlier dissemination efforts by IIAM and partners.  However, only 2.3% of households' total landholding was under sweetpotato, while 1.3% was plated with OFSP. Although the high number of farmers producing sweetpotatoes, the area under production is relatively small, with an average of 300 m2 per household. In general, sweetpotato was mostly (79%) produced for consumption, while 21% of the interviewees mentioned the sale.  Overall, farmer-to-farmer exchange of planting material was the most important source of sweetpotato planting material. Most farmers obtain sweetpotato planting material from their own plots (69%). However, some considerable farmers depend on vines from neighbors (23%), and about 7% from relatives. There was not any reference related to the selling of planting material. In seed system, vine conservation is one of the critical activities during the sweetpotato production cycle. Vines must be read for planting even after a long dry season. According to the results from this study, most (65%) of sweetpotato is planted 2-3 after the beginning of the raining season. At this moment, vines must be ready for planting. In general, the majority (94%) of sweetpotato farmers usually conserve their vines. The typical methods of conservation include leave some portions of the plot without harvesting (60%) for later sprouting, establish small fenced plots in lowlands (28%), and conservation in small plots near their houses (15%).   Important to note was the fact that most of the selling of the fresh roots was conducted in local marketplaces (64%) as opposed to the farm gate (6%).  This means that most of the farmers had to transport their products to local markets and other places in the urban (30%) areas to sell their produce.  Vitamin A knowledge, farmers' practices, attitude and perception on sweetpotato was further assessed.  One of the most important objectives of this project is to address vitamin A deficiency through the consumption of OFSP. Overall, both male (70%) and female (66%) have heard about vitamin A. Although not important, in general, the results indicate that there were slightly more men than women informed about vitamin A among the respondents of this study. The most important source vitamin A information for the women was the health unit (50%), while for the men was the radio (44%) program aired in local language. In general, the use of radio with programs in local languages is more effective for men than women, while, most of the women can be effectively reached by using the heath unit.
"

	uri <- "doi:10.21223/P3/SL0ALD"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Households; universe: The study targeted households with mainly children aged 6-23 months and their mothers.; timeMethod: Cross-sectional survey; dataCollector: CIP; collectorTraining: A group of 12 enumerators supervised by the CIP Monitoring and Evaluation Specialist and the IIAM socio-economist based in Niassa were previously trained on interviewing methods using survey and structured questionnaires. A pre-test of the questionnaire was conducted before the survey was administrated in the selected Villages.; samplingProcedure: This baseline survey was conducted between from July 8 to August 23, 2013. The selected households were the ones located in project targeted areas in the three regions of Niassa province. The districts in the survey are all part of the project target area. Additionally, each village in the district was selected according to the agriculture potential, that is, a list of villages with relatively high level of production was provided by the local agriculture authorities, and from that, 3 villages with potential and tradition of sweetpotato production and existence of lowlands were selected for the interview. In total, 24 villages were visited during the survey in 8 districts.  
Apart from being part of a group of vulnerable household, for them to qualify for the interview must at least one child with less than 5 years. The households interviewed were selected using a systematic sampling, that is, upon reach the villages, the local agriculture extension guided the group to the local traditional leader, and from there, usually starting with the leaders, the households were systematically picked up.; collectionMode: The data collection was conducted using a structured questionnaire developed to capture the most important aspects related to sweetpotato production, seed systems, nutrition, and attitudes, knowledge, food diversity and food security, and household assets and other poverty related and local development questions in general.; controlOperations: After data collection, a quick verification and pre-cleaning was conducted prior to data entry to check for logic incoherencies in the questionnaires. Data clerks were trained on how to double enter the data using CSPro software. During and after data entry, consistency checks were conducted by both times in Lichinga and Maputo.",
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Your Name",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "BaseLineSurvey Data Dictionary.txt"]
	f2 <- ff[basename(ff) == "Niass baseline pag_4.sav"]
	f3 <- ff[basename(ff) == "Niassa baseline pag 7.sav"]
	f4 <- ff[basename(ff) == "Niassa baseline pag_1.sav"]
	f5 <- ff[basename(ff) == "Niassa baseline pag_10.sav"]
	f6 <- ff[basename(ff) == "Niassa baseline pag_101.sav"]
	f7 <- ff[basename(ff) == "Niassa baseline pag_11.sav"]
	f8 <- ff[basename(ff) == "Niassa baseline pag_12.sav"]
	f9 <- ff[basename(ff) == "Niassa baseline pag_13.sav"]
	f10 <- ff[basename(ff) == "Niassa baseline pag_14.sav"]
	f11 <- ff[basename(ff) == "Niassa baseline pag_15.sav"]
	f12 <- ff[basename(ff) == "Niassa baseline pag_16.sav"]
	f13 <- ff[basename(ff) == "Niassa baseline pag_17.sav"]
	f14 <- ff[basename(ff) == "Niassa baseline pag_18.sav"]
	f15 <- ff[basename(ff) == "Niassa baseline pag_19.sav"]
	f16 <- ff[basename(ff) == "Niassa baseline pag_2.sav"]
	f17 <- ff[basename(ff) == "Niassa baseline pag_20.sav"]
	f18 <- ff[basename(ff) == "Niassa baseline pag_21.sav"]
	f19 <- ff[basename(ff) == "Niassa baseline pag_22.sav"]
	f20 <- ff[basename(ff) == "Niassa Baseline pag_3.sav"]
	f21 <- ff[basename(ff) == "Niassa baseline pag_32.sav"]
	f22 <- ff[basename(ff) == "Niassa baseline pag_4_new.sav"]
	f23 <- ff[basename(ff) == "Niassa baseline pag_41.sav"]
	f24 <- ff[basename(ff) == "Niassa baseline pag_5.sav"]
	f25 <- ff[basename(ff) == "Niassa baseline pag_51.sav"]
	f26 <- ff[basename(ff) == "Niassa baseline pag_6.sav"]
	f27 <- ff[basename(ff) == "Niassa baseline pag_62.sav"]
	f28 <- ff[basename(ff) == "Niassa baseline pag_63.sav"]
	f29 <- ff[basename(ff) == "Niassa baseline pag_71.sav"]
	f30 <- ff[basename(ff) == "Niassa baseline pag_72.sav"]
	f31 <- ff[basename(ff) == "Niassa baseline pag_8.sav"]
	f32 <- ff[basename(ff) == "Niassa baseline pag_9.sav"]

	#r1 <- read.???(f1)
	#r2 <- read.???(f2)
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
	#r18 <- read.???(f18)
	#r19 <- read.???(f19)
	#r20 <- read.???(f20)
	#r21 <- read.???(f21)
	#r22 <- read.???(f22)
	#r23 <- read.???(f23)
	#r24 <- read.???(f24)
	#r25 <- read.???(f25)
	#r26 <- read.???(f26)
	#r27 <- read.???(f27)
	#r28 <- read.???(f28)
	#r29 <- read.???(f29)
	#r30 <- read.???(f30)
	#r31 <- read.???(f31)
	#r32 <- read.???(f32)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
