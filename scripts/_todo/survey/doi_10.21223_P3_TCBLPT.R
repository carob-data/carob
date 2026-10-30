# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Baseline Survey for Scaling Up Sweetpotato Through Agriculture and Nutrition  (SUSTAIN) Project, Kenya

Scaling Up Sweetpotato Through Agriculture and Nutrition (SUSTAIN) was a 5-year project of the International Potato Center (CIP). The project aimed at improving production and consumption of vitamin A-rich orange-fleshed sweetpotato (OFSP) by households with children under five years of age in Kenya. A study was conducted to provide baseline information about the status of the households in terms of sweetpotato production, consumption and sale of OFSP. It was also intended to obtain information on dietary diversity, feeding practices, household food security status, vitamin A and acceptance of OFSP among the households. Data was collected from 600 households randomly selected from five counties where SUSTAIN project was implemented, namely, Siaya, Kisumu, Homa bay, Nyamira and Migori. In each household, the head responded to the agriculture modules of the tool while the reference woman (defined as a caregiver of child less than 5 years of age or lactating/pregnant woman) responded to the questions on knowledge, household nutrition, food security and feeding practices. Household's diet quality was assessed using the standard child dietary diversity (CDDS) and minimum dietary diversity for women of reproductive age (MDD-W) as well as frequency of consumption of different foods based on 24-hour recall. The vitamin consumption frequency was assessed using scores based on the Helen Keller International (HKI) approach based on 7-day recall.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/TCBLPT"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Households; universe: Children under 5, above 5, adult Women and men; timeMethod: Cross-sectional; samplingProcedure: The study was conducted in all the five counties targeted by SUSTAIN Kenya project, namely, Migori, Nyamira, Homabay, Kisumu and Siaya. A total of 30 Community Units health administrative structures under the Government of Kenya community health outreach system(CUs) were targeted by the project in all the five Counties. 10 CUs were randomly sampled from among the targeted 30 CUs using probability proportionate to size sampling technique. Three CUs were selected in Siaya County, one in Kisumu, one in Nyamira, three in Homabay and two in Migori. Names of all the villages in each of the CU were compiled and 50 villages randomly sampled from the total tally of villages in the 10 CUs using probability proportionate to size sampling technique. In each of the randomly sampled villages, a complete list of all the households with: i) pregnant woman, ii) a children less than 2 year old (i.e., 0-23 months) and, iii) a child 2-5 years old was drawn with the help of community health workers and local leaders.  Respondents were then randomly selected from each of the village lists using probability proportion to size technique. This procedure resulted in 192 households being interviewed in Siaya, 117 in Kisumu, 144 in Homabay, 41 in Nyamira and 106 in Migori.  Overall, 600 households with 83 female headed and 517 male headed households were interviewed.  The respondents however comprised of 41 males and 559 females. The disproportionately high number of female respondents is because the study targeted caregivers of children under 5 or pregnant women in each of study households.; targetSampleSize: list(targetSampleActualSize = list(typeName = "targetSampleActualSize", multiple = FALSE, typeClass = "primitive", value = "600")); researchInstrument: Structured Computer Aided Personal Interview (CAPI)",
		
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
	

	f1 <- ff[basename(ff) == "SUS_BASE_015_SITEID_13.dta"]
	f2 <- ff[basename(ff) == "SUS_BASE_PART2A_sweetpotato knowledge adoption disadoption.dta"]
	f3 <- ff[basename(ff) == "SUS_BASE_PART2A_VARIETIES.dta"]
	f4 <- ff[basename(ff) == "SUS_BASE_PART2B_non_labour input use.dta"]
	f5 <- ff[basename(ff) == "SUS_BASE_PARTB_hhdComposition.dta"]
	f6 <- ff[basename(ff) == "SUSTAIN_BASE_qtycontrol_Consumer_study.dta"]
	f7 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE PART4E_Facilities_amenities.dta"]
	f8 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_farmerId_introduction.dta"]
	f9 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_livestock_Assets.dta"]
	f10 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Other sources income.dta"]
	f11 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_PART2C_labour_use_sweetpotato.dta"]
	f12 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_PART2D_production utilisation.dta"]
	f13 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_PART2D06-D11_production utilisation.dta"]
	f14 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_PART2G_Vine dissemination.dta"]
	f15 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_PART4D_Land_Assets.dta"]
	f16 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_PART4D_Physical assets.dta"]
	f17 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Part5_HFIAS.dta"]
	f18 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Part5C_Dietary diversity.dta"]
	f19 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Part5D_Food frequency.dta"]
	f20 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Part5E_consumption of OFSP.dta"]
	f21 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Part5F_OFSPKnowledge_attitude_perception.dta"]
	f22 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Part5G_IYCF_mareternalKnowledge.dta"]
	f23 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Part5H_Minimum_meal_frequency.dta"]
	f24 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Part5I_WASH.dta"]
	f25 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Part601_personal involvement.dta"]
	f26 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_PARTE_sweetpotato characteristics.dta"]
	f27 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Production sale othercrops.dta"]
	f28 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Qn2_Expected liking.dta"]
	f29 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Qn3_actual  liking.dta"]
	f30 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Qn4_Feelings and moods about OFSP.dta"]
	f31 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Qn5_Trust in information sources.dta"]
	f32 <- ff[basename(ff) == "SUSTAIN_KENYA_BASE_Transaction characteristics.dta"]
	f33 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_Consumer_Part_Quality control.log"]
	f34 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PAcontrolPARRT5H_minimum_meal_frequency.log"]
	f35 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART2A_sweetpotato knowledge adoption disadoption.log"]
	f36 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART2A_varieties..log"]
	f37 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART2B_Non_labor_input_use.log"]
	f38 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART2C_ labor use.log"]
	f39 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART2D_Production and utilization.log"]
	f40 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART2D06-2D011_Production and utilization.log"]
	f41 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART2E_Sweetpotato characteristics.log"]
	f42 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART2E_Sweetpotato transaction characteristics.log"]
	f43 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART2G_vine dissemination.log"]
	f44 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART3A_production sale othercrops.log"]
	f45 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART4A_livestock owned.log"]
	f46 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART4B_income_othersources.log"]
	f47 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART4D_physical_assets.log"]
	f48 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART4E_Land_Assets.log"]
	f49 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART4F_Facilities_Amenities.log"]
	f50 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART5A_HFIAS.log"]
	f51 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART5C_Dietary diversity.log"]
	f52 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART5D_Food frequency.log"]
	f53 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART5E_Consumption of OFSP.log"]
	f54 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART5F_OFPKnowledge.log"]
	f55 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART5G_IYCF_marternalKnowledge.log"]
	f56 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PART601_Personal involvement.log"]
	f57 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_PARTB_hhdComposition.log"]
	f58 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_Question2_Expected liking.log"]
	f59 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_Question3_Actual liking.log"]
	f60 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_Question4_Feelings and moods about OFSP.log"]
	f61 <- ff[basename(ff) == "codebook_SUSTAIN_KENYA_BASE_Question4_Trust in information sources.log"]
	f62 <- ff[basename(ff) == "codebook_SUSTAIN_SUSTAIN_KENYA_BASE_PARTA_015_SITEID.log"]
	f63 <- ff[basename(ff) == "codebook_WASH PRACTICES.log"]

	r1 <- haven::read_dta(f1)
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
	r31 <- haven::read_dta(f31)
	r32 <- haven::read_dta(f32)
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
	#r61 <- read.???(f61)
	#r62 <- read.???(f62)
	#r63 <- read.???(f63)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		treatment = r1[["treatment"]],
		adm3 = carobiner::fix_name(r1[["community_unit"]], "title"),
		location = r1[["village"]]
	)
##r1: "qnno", "dateintr", "startime", "endtime", "Gender_respodent", "hhd_ID", "A02_1", "A08_1", "A09_1", "A10_1", "A11_1", "A16_1", "A17_1", "A18_1", "A19_1", "A20_1", "A21_1", "B01_1", "B02_1", "B04_1", "B05_1", "B06_1", "B07_1", "B08_1", "B09_1", "B10_1", "B11_1"


	d2 <- data.frame()
##r2: "qnno", "A01_2", "A02_2", "A03_2", "A04_2", "A05_2", "A05a_2", "A06_2", "A07_2", "A08_2", "A09_2"


	d3 <- data.frame(
		variety = r3[["variety"]]
	)
##r3: "qnno"


	d4 <- data.frame(
		plot_id = r4[["plotcode"]],
		season = r4[["season"]]
	)
##r4: "qnno", "plotsize", "plotunit", "B01_2", "B02_2", "B03_2", "B04_2", "B05a_2", "B05b_2", "B05c_2", "B06_2", "B07_2", "B08_2", "B09_2"


	d5 <- data.frame()
##r5: "qnno", "memid", "B01_1", "B02_1", "B03_1", "B04_1", "B05_1", "B05a_1", "B06_1", "B07_1", "B08_1", "B09_1", "B10_1", "B11_1"


	d6 <- data.frame(
		treatment = r6[["treatment"]],
		adm3 = carobiner::fix_name(r6[["community_unit"]], "title"),
		location = r6[["village"]]
	)
##r6: "qnno", "dateintr", "hhd_ID", "county"


	d7 <- data.frame()
##r7: "qnno", "F01_4", "F02_4", "F03_4", "F04a_4", "F04b_4", "F05_4", "F06_4"


	d8 <- data.frame(
		treatment = r8[["treatment"]]
	)
##r8: "qnno", "A02_1", "A03_1", "A04_1", "A05_1", "A06_1", "A07_1", "A08_1", "A09_1", "A10_1", "A11_1", "A12_1", "A13_1", "A14_1", "A15_1", "A16_1", "A17_1", "A18_1", "A19_1", "A20_1", "A21_1", "B12_1", "B13_1"


	d9 <- data.frame()
##r9: "qnno", "lvstktype", "A01_4", "A02_4", "A03_4", "A04_4"


	d10 <- data.frame()
##r10: "qnno", "source", "B01_4", "B02a_4", "B02b_4", "B03_4", "B04_4", "B05_4"


	d11 <- data.frame(
		plot_id = r11[["plotcode"]],
		season = r11[["season1"]]
	)
##r11: "qnno", "C01_2", "C02_2", "C03_2", "C04_2", "C05_2", "C06_2", "C07_2", "C08_2", "C09_2"


	d12 <- data.frame(
		plot_id = r12[["plotcode"]],
		season = r12[["season"]]
	)
##r12: "qnno", "D01_2", "D02a_2", "D02b_2", "D03a_2", "D03b_2", "D04_2", "D05_2"


	d13 <- data.frame()
##r13: "qnno", "D06a_2", "D06b_2", "D06c_2", "D06d_2", "D07_2", "D08_2", "D09_2", "D10_2", "D11a_2", "D11b_2", "D11c_2"


	d14 <- data.frame()
##r14: "qnno", "G01_2", "G02a_2", "G02b_2", "G02c_2", "G03a_2", "G03b_2", "G03c_2", "G04_2", "G05a_2", "G05b_2", "G06a_2", "G06b_2", "G07a_2", "G07b_2", "G08_2", "G09_2", "G10_2", "G11a_2", "G11b_2", "G12", "G13", "G14"


	d15 <- data.frame()
##r15: "qnno", "landtype", "E01a_4", "E01b_4", "E02a_4", "E02b_4", "E03a_4", "E03b_4", "E04a_4", "E04b_4"


	d16 <- data.frame()
##r16: "qnno", "assetname", "D01_4", "D02_4", "D03_4"


	d17 <- data.frame()
##r17: "qnno", "A01a_5", "A01b_5", "A02a_5", "A02b_5", "A03a_5", "A03b_5", "A04a_5", "A04b_5", "A05a_5", "A05b_5", "A06a_5", "A06b_5", "A07a_5", "A07b_5", "A08a_5", "A08b_5", "A09a_5", "A09b_5"


	d18 <- data.frame()
##r18: "qnno", "ref_IDchild", "ref_IDwoman", "C01_5child", "C01_5woman", "C02_5child", "C02_5woman", "C03_5child", "C03_5woman", "C04_5child", "C04_5woman", "C05_5child", "C05_5woman", "C06_5child", "C06_5woman", "C07_5child", "C07_5woman", "C08_5child", "C08_5woman", "C09_5child", "C09_5woman", "C10_5child", "C10_5woman", "C11_5child", "C11_5woman", "C12_5child", "C12_5woman", "C13_5child", "C13_5woman", "C14_5child", "C14_5woman", "C15_5child", "C15_5woman", "C16_5child", "C16_5woman", "C17_5child", "C17_5woman", "C18_5child", "C18_5woman"


	d19 <- data.frame()
##r19: "qnno", "ref_IDchild", "ref_IDwoman", "D01_5child", "D01_5woman", "D02_5child", "D02_5woman", "D03_5child", "D03_5woman", "D04_5child", "D04_5woman", "D05_5child", "D05_5woman", "D06_5child", "D06_5woman", "D07_5child", "D07_5woman", "D08_5child", "D08_5woman", "D09_5child", "D09_5woman", "D10_5child", "D10_5woman", "D11_5child", "D11_5woman", "D12_5child", "D12_5woman", "D13_5child", "D13_5woman", "D14_5child", "D14_5woman", "D15_5child", "D15_5woman", "D16_5child", "D16_5woman", "D17_5child", "D17_5woman", "D18_5child", "D18_5woman", "D19_5child", "D19_5woman", "D20_5child", "D20_5woman", "D21_5child", "D21_5woman", "D22_5child", "D22_5woman", "D23_5child", "D23_5woman", "D24_5child", "D24_5woman", "D25_5child", "D25_5woman", "D26_5child", "D26_5woman", "D27_5child", "D27_5woman", "D28_5child", "D28_5woman", "D29_5child", "D29_5woman", "D30_5child", "D30_5woman", "D31_5child", "D31_5woman", "D32_5child", "D32_5woman", "D33_5child", "D33_5woman", "D34_5child", "D34_5woman", "D35_5child", "D35_5woman", "D36_5child", "D36_5woman", "D37_5child", "D37_5woman", "D38_5child", "D38_5woman", "D39_5child", "D39_5woman", "D40_5child", "D40_5woman", "D41_5child", "D41_5woman", "D42_5child", "D42_5woman"


	d20 <- data.frame()
##r20: "qnno", "E01_5child", "E01_5woman", "E02_5child", "E02_5woman", "E03_5child", "E03_5woman", "E04_5child", "E04_5woman", "E05_5child", "E05_5woman", "E06_5child", "E06_5woman", "E07_5child", "E07_5woman", "E08_5child", "E08_5woman", "E09a1_5child", "E09b1_5woman", "E09a2_5child", "E09b2_5woman"


	d21 <- data.frame()
##r21: "qnno", "F01_5", "F02_5", "F03a_5", "F03b_5", "F04a_5", "F04b_5", "F04c_5", "F05a_5", "F05b_5", "F05c_5", "F06_5", "F07_5", "F08_5", "F09_5", "F09a_5", "F10_5", "F11_5", "F12_5", "F13_5", "F14_5", "F15_5", "F16a_5", "F16b_5", "F16c_5", "F17a_5", "F17b_5", "F17c_5", "F17d_5", "F17e_5", "F17f_5", "F17g_5", "F17h_5", "F17i_5", "F17j_5", "F17k_5", "F17l_5", "F17m_5", "F17n_5"


	d22 <- data.frame()
##r22: "qnno", "G02_5", "G03_5", "G04_5", "G05_5", "G06_5", "G07_5", "G08_5", "G09_5", "G10_5", "G11_5", "G12_5", "G13_5", "G14_5", "G15_5", "G16_5", "G17_5", "G18_5", "G19_5", "G20_5", "G21_5", "G22_5", "G23_5", "G24_5", "G25_5", "G26_5", "G27_5", "G28a_5", "G28b_5", "G28c_5", "G28_project", "G28_NGO", "G29_5", "G30_5", "G31_5", "G32_5", "G33_5", "G34_5"


	d23 <- data.frame()
##r23: "qnno", "H00", "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12", "H13", "H14", "H16a", "H16b", "H16c", "H16d", "H16e", "H16f", "H17"


	d24 <- data.frame()
##r24: "qnno", "I01a_5", "I01b_5", "I02a_5", "I02b_5", "I02c_5", "I02d_5", "I02e_5", "I02f_5", "I02g_5", "I03_5"


	d25 <- data.frame(
		treatment = r25[["treatment"]]
	)
##r25: "qnno", "important", "unimportant", "ofnoconcern", "ofconcern", "irrelevant", "relevant", "meansalot", "meansnothing", "useless", "useful", "valuable", "worthless", "trivial", "fundamental", "beneficial", "notbeneficial", "mattertome", "doesntmatter", "uninterested", "interested", "significant", "insignificant", "vital", "superfluous", "boring", "interesting", "unexciting", "exciting", "appealing", "unappealing", "mundane", "fascinating", "essential", "nonessential", "undesirable", "desirable", "wanted", "unwanted", "notneeded", "needed"


	d26 <- data.frame(
		season = r26[["season"]]
	)
##r26: "qnno", "potatovar", "E01_2", "E02_2", "E03_2", "E04_2", "E05_2", "E06_2", "E07_2", "E08_2", "E09_2", "E10_2", "E11_2", "E12_2", "E13_2", "E14_2", "E15_2", "E16_2", "E17_2", "E18_2"


	d27 <- data.frame(
		crop = tolower(r27[["foodcrop1"]])
	)
##r27: "qnno", "A01a_3", "A01b_3", "A02a_3", "A02b_3", "A03a_3", "A03b_3", "A04a_3", "A04b_3", "A05a_3", "A05b_3", "A06a_3", "A06b_3", "A07a_3", "A07b_3", "A08a_3", "A08b_3", "A09a_3", "A09b_3", "A10a_3", "A10b_3", "A11a_3", "A11b_3", "A12a_3", "A12b_3", "A13a_3", "A13b_3", "A14a_3", "A14b_3", "A15a_3", "A15b_3", "A16a_3", "A16b_3", "A17a_3", "A17b_3", "A18a_3", "A18b_3", "A19_a3", "A19_b3", "A01c_3", "A01d_3", "A02c_3", "A02d_3", "A03c_3", "A03d_3", "A04c_3", "A04d_3", "A05c_3", "A05d_3", "A06c_3", "A06d_3", "A07c_3", "A07d_3", "A08c_3", "A08d_3", "A09c_3", "A09d_3", "A10c_3", "A10d_3", "A11c_3", "A11d_3", "A12c_3", "A12d_3", "A13c_3", "A13d_3", "A14c_3", "A14d_3", "A15c_3", "A15d_3", "A16c_3", "A16d_3", "A17c_3", "A17d_3", "A18c_3", "A18d_3", "A19_c3", "A19_d3", "foodcrop2", "foodcrop3", "cashcrop1", "cashcrop2", "cashcrop3"


	d28 <- data.frame(
		treatment = r28[["treatment"]],
		soil_texture = tolower(r28[["texture"]])
	)
##r28: "qnno", "sugary", "nutrition", "smell", "color", "taste", "handle"


	d29 <- data.frame(
		treatment = r29[["treatment"]],
		soil_texture = tolower(r29[["texture"]])
	)
##r29: "qnno", "sugary", "nutrition", "smell", "color", "taste", "handle"


	d30 <- data.frame(
		treatment = r30[["treatment"]]
	)
##r30: "qnno", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12", "s13", "s14", "s15", "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23"


	d31 <- data.frame(
		treatment = r31[["treatment"]],
		hhid = r31[["farmers"]]
	)
##r31: "qnno", "tvdoc", "tvnews", "tvadvert", "newspap", "internet", "radio", "magazine", "traders", "supermkt", "orgshop", "specstor", "transpor", "sellers", "doctor", "univ", "county", "kebs", "politic", "kco"


	d32 <- data.frame(
		season = r32[["season"]]
	)
##r32: "qnno", "F01_2", "F02_2", "F03_2", "F04_2", "F05_2", "F06a_2", "F06b_2", "F06c_2", "F07_2", "F08_2"


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

