# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh in 2018

RTCs play an important role in diverse Asian agri-food systems and contribute to their resilience and capacity to adapt to different challenging environments.The study is a quantitative assessment to better understand the current farming practice including potato and sweetpotato farming in salt prone areas to finally increase the potato and sweetpotato yield in those areas. The information collected was divided in sixteen sections: A) Basic information and consent, (B) Household Roster, (C) Roster of land and pond/water bodies owned or under operation, (D1) Agriculture Plot Utilization, (D2) Irrigation method (in the last 12 months), (D3) Usage of Agricultural Chemicals, Fertilizers and Pesticides (in the last 12 months), (D4) Rental Cost of Tools, Machinery and Draft Animal (in the last 12 months), (D5) Labor Usage by Gender for Crop Plantation and Harvesting, (D6) Post Harvest Labor, Animal and Tools/ Machinery Usage, (E) Household Assets, (F) Household Access to Resources, (G) Household Access to Extension and Credit, (H) Livestock and Aquaculture, (I) Household annual off-farm Income, (J) Household annual Expenditures, (K) Social networks, (L) Migration, (M) Household Food Security, (N) Constraints to POTATO AND SWEETPOTATO production and Marketing, (O) Risk Perception, (P) Nutrition and Hygiene.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/JHMUZN"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Bangladesh Agricultural University, Mymensingh",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Households with different salinity intrusion intensity.; universe: Households with different salinity intrusion intensity in Satkhira and Khulna districts.; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: We purposively selected two districts, Satkhira and Khulna, in the southwest of Bangladesh. In each district, 3 upazilas (the next lowest administrative level) were also purposively selected based on salinity intrusion intensity. Across the 6 upazilas, we randomly selected 19 villages and further randomly sampled 450 households. The minimum number of households selected was 20 for logistical reasons of data collection. The remaining 70 households were added to villages proportionate to their overall village size.; collectionMode: Enumerators visited selected random households in each village and collected the information required in the 16-page questionnaire.; researchInstrument: Structured household survey questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "Database_Salt Tolerant Bangladesh.xlsx"]
	f2 <- ff[basename(ff) == "DataDictionary_Salt Tolerant Bangladesh.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Section A-F-G-K-L-N-O-P")
	r1b <- carobiner::read.excel(f1, sheet="Section B")
	r1c <- carobiner::read.excel(f1, sheet="Section C")
	r1d <- carobiner::read.excel(f1, sheet="Section D1")
	r1e <- carobiner::read.excel(f1, sheet="Section D2")
	r1f <- carobiner::read.excel(f1, sheet="Section D3")
	r1g <- carobiner::read.excel(f1, sheet="Section D4")
	r1h <- carobiner::read.excel(f1, sheet="Section D5")
	r1i <- carobiner::read.excel(f1, sheet="Section D6")
	r1j <- carobiner::read.excel(f1, sheet="Section E")
	r1k <- carobiner::read.excel(f1, sheet="Section H")
	r1l <- carobiner::read.excel(f1, sheet="Section I")
	r1m <- carobiner::read.excel(f1, sheet="Section J")
	r1n <- carobiner::read.excel(f1, sheet="Section M")
	r2a <- carobiner::read.excel(f2, sheet="Dictionary A-F-G-K-L-N-O-P")
	r2b <- carobiner::read.excel(f2, sheet="Dictionary B")
	r2c <- carobiner::read.excel(f2, sheet="Dictionary C")
	r2d <- carobiner::read.excel(f2, sheet="Dictionary D1")
	r2e <- carobiner::read.excel(f2, sheet="Dictionary D2")
	r2f <- carobiner::read.excel(f2, sheet="Dictionary D3")
	r2g <- carobiner::read.excel(f2, sheet="Dictionary D4")
	r2h <- carobiner::read.excel(f2, sheet="Dictionary D5")
	r2i <- carobiner::read.excel(f2, sheet="Dictionary D6")
	r2j <- carobiner::read.excel(f2, sheet="Dictionary E")
	r2k <- carobiner::read.excel(f2, sheet="Dictionary H")
	r2l <- carobiner::read.excel(f2, sheet="Dictionary I")
	r2m <- carobiner::read.excel(f2, sheet="Dictionary J")
	r2n <- carobiner::read.excel(f2, sheet="Dictionary M")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		elevation = r1a[["GPS Altitude"]],
		adm2 = carobiner::fix_name(r1a[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1a[["Union / commune"]], "title"),
		location = r1a[["Village"]],
		plot_id = r1a[["F2. Main plot to house (km)"]],
		crop = tolower(r1a[["G8. Crop/livestock insurance"]]),
		hhid = r1a[["G11.Visited by fellow farmers"]]
	)
##r1a: "Household ID", "Language", "Interview date (DD-MM-YYYY)", "Start time (HH:MM:SS)", "Upazilla / Subdistrict", "Agreement to participate", "F1. Electricity", "F3. Main plot to house (min)", "F4. Potato farm to house (km)", "F5. Potato farm to house (min)", "F6. Sweetpotato farm to house (km)", "F7. Sweetpotato farm to house (min)", "F9. Water source (min)", "F8. Water source (Km)", "F10. Money lender or bank or cooperative (Km)", "F11. Money lender or bank or cooperative (min)", "F12. Extension office (Km)", "F13. Extension office (min)", "F14. School (Km)", "F15. School (min)", "F16. Input shop (Km)", "F17. Input shop (min)", "F18. Trader/collection center/market (Km)", "F19. Trader/collection center/market (min)", "G1. Get credit", "G2. Got credit", "G3.1 Source of credit", "G3.2 Source of credit", "G3.3 Source of credit", "G3. Source of credit (OTHER)", "G4.1 Credit SOURCE 1 (Tk)", "G4.2 Credit SOURCE 2 (Tk)", "G4.3 Credit SOURCE 3 (Tk)", "G5. Number of finance sources", "G6. Organization/ cooperative", "G7. HH members in organization/ cooperative", "G9.1. Saline water intrusion problem", "G9.2.% of land under saline prone areas", "G10. Visited by extension officer", "K1. Community meeting", "K2.Neighbors/friends potato/sweetpotato", "K3. Family members in potato/sweetpotato", "K4. Progressive farmers in potato enterprise", "K5. HH in village in potato production (%)", "K6. HH in  village in sweetpotato production (%)", "L1. Member of HH migrated", "L3.  Number of HH member migrate (2)", "L2.  Number of HH member migrate (1)", "L4. Remittances (Tk)", "N1.Potato/sweetpotato experience", "N2. Constraints in potato/sweetpotato", "N3.1 Constraint 1 in potato/sweetpotato", "N3.2 Constraint 1 in potato/sweetpotato", "N3.3 Constraint 2 in potato/sweetpotato", "N3.4 Constraint 3 in potato/sweetpotato", "N3.5 Constraint 4 in potato/sweetpotato", "N4. Remark constraints", "O1. Risk level", "P1. Exclusively breastfeed age", "P2. Complementary feeding age", "P3. Benefits of complementary feeding", "P4. Portion size for complementary feeding", "P5. Month breastfeeding", "P6.1 Food 1 good for children", "P6.2 Food 2 good for children", "P6.3 Food 3 good for children", "P6.4 Food 4 good for children", "P6.5 Food 5 good for children", "P6.6 Food good for children (OTHER)", "P7.1 Foods bad for child", "P7.2.1 Food 1 bad for  child", "P7.2.2 Food 2 bad for  child", "P7.2.3 Food 3 bad for  child", "P7.2.4 Food 4 bad for  child", "P7.2.5 Food 5 bad for  child", "P7.2.6 Food 6 bad for  child", "P8.1 Foods improve eye sight", "P8.2.1 Food 1 improve eye sight", "P8.2.2 Food 2 improve eye sight", "P8.2.3 Food 3 improve eye sight", "P8.2.4 Food 4 improve eye sight", "P8.2.5 Food 5 improve eye sight", "P8.2.6 Food 6 improve eye sight (Other)", "P9.1 Foods essential blood formation", "P9.2.1 Food 1 essential blood formation", "P9.2.2 Food 2 essential blood formation", "P9.2.3 Food 3 essential blood formation", "P9.2.4 Food 4 essential blood formation", "P9.2.5 Food 5 essential blood formation", "P9.2.6 Food 6 essential blood formation", "P10. Potato in complementary feeding", "P11.1 Problem 1 potato in complementary feeding", "P11.2 Problem 2 potato in complementary feeding", "P11.3 Problem 3 potato in complementary feeding", "P12. Sweetpotato in complementary feeding", "P13.1 Problem 1 sweetpotato in complementary feeding", "P13.2 Problem 2 sweetpotato in complementary feeding", "P13.3 Problem 3 sweetpotato in complementary feeding", "P14.1  Heard OFSP", "P14.2.1 What heard OFSP 1", "P14.2.2 What heard OFSP 2", "P14.2.3 What heard OFSP 3", "P15. Nutritional services", "P16.1 Nutritional service  GO", "P16.2 Specify GO program", "P17.1 Nutritional service  NGO", "P17.2 Specify NGO program", "P18.1 Nutritional service other organization", "P18.2 Specify name other organization", "P18.3 Specify other program", "P19.1 Type of services 1", "P19.2 Type of services 2", "P19.3 Type of services 3", "P19.4 Type of services 4", "P19.5 Type of services 5", "P19.6 Type of services 6", "P20.1 Wash hands before eating", "P20.2 How wash hands before eating", "P21.1 Wash hands after toilet", "P21.2 How wash hands after toilet", "P22.1 Wash hands cleaning babies stool", "P22.2 How wash hands cleaning babies stool", "P23.1 Wash hands giving food to children", "P23.2 How wash hands giving food to children", "Remarks by Enumerator", "End Time"


	d1b <- data.frame(
		adm2 = carobiner::fix_name(r1b[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1b[["Union / commune"]], "title"),
		location = r1b[["Village"]],
		year = r1b[["B3.4 Age (years) of HH Member"]]
	)
##r1b: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "B1. Total members in the households staying in the house (number) during the pas", "B2a. Out of the household members, how many Adults (18yrs and Above) do you have", "B2b. Out of the household members, how many Adolescents (6-17yrs) do you have in", "B2c. Out of the household members, how many Children (3-5yrs) do you have in the", "B2d. Out of the household members, how many Infants (0-2yrs) do you have in the", "B3.1 HH Member ID", "B3.2 Relation with the HH", "B3.2 Relation with the HH (OTHER)", "B3.3 Gender of HH Member", "B3.5 Marital status of HH member", "B3.6 Education (Years of Schooling) of HH member", "B3.7 Main occupation of HH member (>50% of income comes from this activity)", "B3.7 Main occupation of HH member (>50% of income comes from this activity) (OTH", "B3.8 Secondary occupation of HH member", "B3.8 Secondary occupation of HH member (OTHER)", "Remarks by Enumerator"


	d1c <- data.frame(
		adm2 = carobiner::fix_name(r1c[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1c[["Union / commune"]], "title"),
		location = r1c[["Village"]],
		plot_id = r1c[["C1.0: How many plots of land did you or your household own or operated in the la"]],
		crop = tolower(r1c[["C1.4I- From the previous aman to last season before aman in 12 month which crops...13"]]),
		season = r1c[["C1.4I- From the previous aman to last season before aman in 12 month which crops...13"]]
	)
##r1c: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "C1.1 Plot ID for owned or under operation in the last 12 months", "C1.2 Plot Description", "C1.3 Plot type", "C1.3 Plot type (OTHER)", "C1.4 Current Size/Area of the plot", "C1.4I- From the previous aman to last season before aman in 12 month which crops...14", "C1.4I- From the previous aman to last season before aman in 12 month which crops...15", "C1.4IO- From the previous aman to last season before aman in 12 month which crop", "C1.4Ia- Amount produced (kg) Crop 1", "C1.4b. Total value of the produced crop/fish (tk)- Crop 1", "C1.4Ia- Amount produced (kg) Crop 2", "C1.4b. Total value of the produced crop/fish (tk)- Crop 2", "C1.4Ia- Amount produced (kg) Crop 3", "C1.4b. Total value of the produced crop/fish (tk)- Crop 3", "C1.5 Distance of the plot from home", "C1.6 Ownership of the plot (last 12 months)", "C1.6 Ownership of the plot (last 12 months) (OTHER)", "C1.7 Who owns the plot?", "C1.7 Who owns the plot (OTHER)?", "C1.8 Amount (TK) of plot rented/ leased in for cash/crop/labor during the past 1", "C1.9 If plot is rented/leased out for cash/crop/labour, provide amount (TK) duri", "Remarks by Enumerator"


	d1d <- data.frame(
		adm2 = carobiner::fix_name(r1d[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1d[["Union / commune"]], "title"),
		location = r1d[["Village"]],
		crop = tolower(r1d[["D2.2 Crop season during the last 12 months"]]),
		season = r1d[["D2.2 Crop season during the last 12 months"]],
		variety = r1d[["D2.5 Main variety cultivated (name) during this season"]],
		rep = r1d[["D2.7 How often do you replace your seed?"]]
	)
##r1d: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "D2.3 Crop name cultivated during this season", "D2.3O Crop name cultivated during this season (OTHER)", "D2.4 Area  (decimal) cultivated during this season", "D2.5 Main variety cultivated (name) during this season (OTHER)", "D2.6 Source of  seed/seedlings during this season", "D2.6 Source of  seed/seedlings during this season (OTHER)", "D2.8a Amount of seed (Kg.) during this season", "D2.8b Cost of seed (per kg) during this season", "D2.9a Amount of seedling (Kg.) during this season", "D2.9b Cost of seedling (per kg) during this season", "D2.10 Quantity harvested (Kg) during this season", "D2.11 Quantity sold (kg) during this season", "D2.12 Average price (Kg) of the harvested crop during this season", "D2.13a. How much was used for home consumption / shared with others (Kg)", "D2.13B. Total value of the byproduct (tk)", "D2.14 Amount (TK) paid for land obtained through renting/leasing in during this", "Remarks by Enumerator"


	d1e <- data.frame(
		adm2 = carobiner::fix_name(r1e[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1e[["Union / commune"]], "title"),
		location = r1e[["Village"]],
		crop = tolower(r1e[["D2.2 Crop season in the last 12 months"]]),
		season = r1e[["D2.2 Crop season in the last 12 months"]]
	)
##r1e: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "D2.3 Crop irrigated in this season", "D2.3 Crop (OTHER) irrigated in this season", "D2.4 Source of water for irrigation in this season", "D2.4 Source of water for irrigation in this season (OTHER)", "D2.5 Irrigation method used in this season", "D2.5 Irrigation method used in this season (OTHER)", "D2.6 Energy used for Irrigation in this season", "D2.7 Number of times the crop was irrigated in this season", "D2.8 Payment method for Irrigation in this season", "D2.9 Cash cost of Irrigation in this season", "D2.10 What was the cost of irrigation beyond cash expenditure in this season?", "D2.11 Cause of inadequency/insufficiency ot water during this season...19", "D2.11 Cause of inadequency/insufficiency ot water during this season...20", "D2.11 Cause of inadequency/insufficiency ot water during this season...21", "D2.11 Cause of inadequency/insufficiency ot water during this season (OTHER)", "D2.12 In which month was the water insufficiency greatest during this season", "Remarks by Enumerator"


	d1f <- data.frame(
		adm2 = carobiner::fix_name(r1f[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1f[["Union / commune"]], "title"),
		location = r1f[["Village"]],
		crop = tolower(r1f[["D3.1 CROP no."]])
	)
##r1f: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "D3.1 Crop OTHER", "D3.2 Inputs used in the last 12 months", "D3.3 Number of times inputs was used in the last 12 months", "D3.4 Total amount of input used in the last 12 months", "D3.5 Unit of measurement of inputs used in the last 12 months", "D3.6 Costs of inputs used (per QU) in the last 12 months", "Remarks by Enumerator"


	d1g <- data.frame(
		adm2 = carobiner::fix_name(r1g[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1g[["Union / commune"]], "title"),
		location = r1g[["Village"]],
		crop = tolower(r1g[["D4.2.1 Crop ID"]]),
		rep = r1g[["D4.2.2 Have animals been used for land preparation in the last 12 months?"]]
	)
##r1g: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "D4.2.1 Crop (OTHER)", "D4.2.3 Cost (TK) of the animals used for land preparation in the last 12 months", "D4.2.4 Has machinery been used for land preparation in the last 12 months.", "D4.2.5 Which machinery did you use for land preparation in the last 12 months", "D4.2.6 Total costs (TK) for rental/labor/fuels in the last 12 months", "D4.2.7 Has Tools/machinery been used for planting in the last 12 months", "D4.2.8 Cost (TK) of Tools/machinery used for planting in the last 12 months", "D4.2.9 Has Tools/machinery been used for fertilizer application in the last 12 m", "D4.2.10 Cost (TK) of Tools/machinery used for fertilizer application in the last", "D4.2.11 Has Tools/machinery been used for pesticide application in the last 12 m", "D4.2.12 Cost (TK) of Tools/machinery used for pesticide application in the last", "D4.2.13 Has Tools/machinery been used for weeding in the last 12 months", "D4.2.14 Cost (TK) of Tools/machinery used for weeding in the last 12 months", "D4.2.15 Has Tools/machinery been used for harvesting in the last 12 months", "D4.2.16 Cost (TK) of Tools/machinery used for harvesting in the last 12 months", "Remarks by Enumerator"


	d1h <- data.frame(
		adm2 = carobiner::fix_name(r1h[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1h[["Union / commune"]], "title"),
		location = r1h[["Village"]],
		crop = tolower(r1h[["D5.4.1 Crop ID"]]),
		rep = r1h[["D5.4.2 How many days of hired labour (MALE) were hired for land preppartion (Plo"]]
	)
##r1h: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "D5.1 Do you experience difficulties hiring labor?", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...8", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...9", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...10", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...11", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...12", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...13", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...14", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...15", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...16", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...17", "D5.2 If Yes, during which months do you experience difficulties hiring labor?...18", "D5.3 During which months is labor readily available...19", "D5.3 During which months is labor readily available...20", "D5.3 During which months is labor readily available...21", "D5.3 During which months is labor readily available...22", "D5.3 During which months is labor readily available...23", "D5.3 During which months is labor readily available...24", "D5.3 During which months is labor readily available...25", "D5.3 During which months is labor readily available...26", "D5.3 During which months is labor readily available...27", "D5.3 During which months is labor readily available...28", "D5.3 During which months is labor readily available...29", "D5.4.1 Crop (OTHER)", "D5.4.3 How many days of hired labour (FEMALE) were hired for land preppartion (P", "D5.4.4 How many days of Family labour (MALE) were hired for land preppartion (Pl", "D5.4.5 How many days of Family labour (FEMALE) were hired for land preppartion (", "D5.4.6 What was the wage rate (TK/Day) of Hired / Family labour (MALE) were hire", "D5.4.7 What was the wage rate (TK/Day) of Hired / Family labour (FEMALE) were hi", "D5.4.8 How many days of hired labour (MALE) were hired for Planting (Seeding/Tra", "D5.4.9 How many days of hired labour (FEMALE) were hired for Planting (Seeding/T", "D5.4.10 How many days of Family labour (MALE) were hired for Planting (Seeding/T", "D5.4.11 How many days of Family labour (FEMALE) were hired for Planting (Seeding", "D5.4.12 What was the wage rate (TK/Day) of Hired / Family labour (MALE) were hir", "D5.4.13 What was the wage rate (TK/Day) of Hired / Family labour (FEMALE) were h", "D5.4.14 How many days of hired labour (MALE) were hired for Fertilizer applicati", "D5.4.15 How many days of hired labour (FEMALE) were hired for Fertilizer applica", "D5.4.16 How many days of Family labour (MALE) were hired for Fertilizer applicat", "D5.4.17 How many days of Family labour (FEMALE) were hired for Fertilizer applic", "D5.4.18 How many days of hired labour (MALE) were hired for Pesticide applicatio", "D5.4.19 How many days of hired labour (FEMALE) were hired for Pesticide applicat", "D5.4.20 How many days of Family labour (MALE) were hired for Pesticide applicati", "D5.4.21 How many days of Family labour (FEMALE) were hired for Pesticide applica", "D5.4.22 How many days of hired labour (MALE) were hired for Weeding", "D5.4.23 How many days of hired labour (FEMALE) were hired for Weeding", "D5.4.24 How many days of Family labour (MALE) were hired for Weeding", "D5.4.25 How many days of Family labour (FEMALE) were hired for Weeding", "D5.4.26 What was the wage rate (TK/Day) of Hired / Family labour (MALE) were hir", "D5.4.27 What was the wage rate (TK/Day) of Hired / Family labour (FEMALE) were h", "D5.4.28 How many days of hired labour (MALE) were hired for Irrigation (channel", "D5.4.29 How many days of hired labour (FEMALE) were hired for Irrigation (channe", "D5.4.30 How many days of Family labour (MALE) were hired for Irrigation (channel", "D5.4.31 How many days of Family labour (FEMALE) were hired for Irrigation (chann", "D5.4.32 How many days of hired labour (MALE) were hired for Harvest", "D5.4.33 How many days of hired labour (FEMALE) were hired for Harvest", "D5.4.34 How many days of Family labour (MALE) were hired for Harvest", "D5.4.35 How many days of Family labour (FEMALE) were hired for Harvest", "D5.4.36 What was the wage rate (TK/Day) of Hired / Family labour (MALE) for Harv", "D5.4.37 What was the wage rate (TK/Day) of Hired / Family labour (FEMALE) for Ha", "D5.4.38 How many days of hired labour (MALE) were hired for Transportation (i.e.", "D5.4.39 How many days of hired labour (FEMALE) were hired for Transportation (i.", "D5.4.40 How many days of Family labour (MALE) were hired for Transportation (i.e", "D5.4.41 How many days of Family labour (FEMALE) were hired for Transportation (i", "Remarks by Enumerator"


	d1i <- data.frame(
		adm2 = carobiner::fix_name(r1i[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1i[["Union / commune"]], "title"),
		location = r1i[["Village"]],
		crop = tolower(r1i[["D6.2.1 Crop harvested"]])
	)
##r1i: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "D6.2.1 Crop  harvested (OTHER)", "D6.2.2 Total days of Animal used for threshing", "D6.2.3 Total Cost (Tk) of Animal used for threshing", "D6.2.4 Did you use Tools/machinery for Threshing", "D6.2.5 What was the total cost of Tools/machinery used for threshing", "D6.2.6 How many days of hired labour (MALE) were hired for Threshing", "D6.2.7 How many days of hired labour (FEMALE) were hired for Threshing", "D6.2.8 How many days of Family labour (MALE) were hired for Threshing", "D6.2.9 How many days of Family labour (FEMALE) were hired for Threshing", "D6.2.10 What was the wage rate (TK/Day) of Hired / Family labour (MALE) were hir", "D6.2.11 What was the wage rate (TK/Day) of Hired / Family labour (FEMALE) were h", "D6.2.12 How many days of hired labour (MALE) were hired for Drying", "D6.2.13 How many days of hired labour (FEMALE) were hired for Drying", "D6.2.14 How many days of Family labour (MALE) were hired for Drying", "D6.2.15 How many days of Family labour (FEMALE) were hired for Drying", "D6.2.16 What was the wage rate (TK/Day) of Hired / Family labour (MALE) were hir", "D6.2.17 What was the wage rate (TK/Day) of Hired / Family labour (FEMALE) were h", "D6.2.18 How many days of hired labour (MALE) were hired for Sorting/Grading", "D6.2.19 How many days of hired labour (FEMALE) were hired for Sorting/Grading", "D6.2.20 How many days of Family labour (MALE) were hired for Sorting/Grading", "D6.2.21 How many days of Family labour (FEMALE) were hired for Sorting/Grading", "D6.2.22 What was the wage rate (TK/Day) of Hired / Family labour (MALE) were hir", "D6.2.23 What was the wage rate (TK/Day) of Hired / Family labour (FEMALE) were h", "D6.2.24 How many days of hired labour (MALE) were hired for Packaging", "D6.2.25 How many days of hired labour (FEMALE) were hired for Packaging", "D6.2.26 How many days of Family labour (MALE) were hired for Packaging", "D6.2.27 How many days of Family labour (FEMALE) were hired for Packaging", "D6.2.28 What was the wage rate (TK/Day) of Hired / Family labour (MALE) were hir", "D6.2.29 What was the wage rate (TK/Day) of Hired / Family labour (FEMALE) were h", "Remarks by Enumerator"


	d1j <- data.frame(
		adm2 = carobiner::fix_name(r1j[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1j[["Union / commune"]], "title"),
		location = r1j[["Village"]]
	)
##r1j: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "E2. What assets do you own in your household", "E2. What assets do you own in your household (OTHER)", "E2.2 Number of assets owned by your household", "E2.3 What was the purchase price (Tk) of the assets owned", "Remarks by Enumerator"


	d1k <- data.frame(
		adm2 = carobiner::fix_name(r1k[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1k[["Union / commune"]], "title"),
		location = r1k[["Village"]],
		year = r1k[["H1. What kind of farm animals did you keep in the last one year?"]]
	)
##r1k: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "H1. What kind of farm animals did you keep in the last one year (OTHER)?", "H2.1 What was the total Value of all Livestock/Aquaculture in the last one year", "H2.2 What was the Total rearing costs (Tk) farm animals", "H2.3 What was the Total revenue from selling (Tk) farm animals", "H2.4 What was the Total value of household consumption (Tk)", "Remarks by Enumerator"


	d1l <- data.frame(
		adm2 = carobiner::fix_name(r1l[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1l[["Union / commune"]], "title"),
		location = r1l[["Village"]],
		year = r1l[["I3.5 Gross income per year (Tk.) for man/husband"]]
	)
##r1l: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "I.1 Do you or your household have any off-farm income", "I3.1 Source of Income", "I3.3 Gross income per month (Tk.) for man/husband", "I3.4 How many months out of the past 12 months did the man/husband receive incom", "I3.6 percentage contribution to total income for man/husband", "I3.7 Gross income per month (Tk.) for woman/spouse", "I3.8 How many months out of the past 12 months did the woman/spouse receive inco", "I3.9 Gross income per year (Tk.) for woman/spouse", "I3.10 percentage contribution to total income for woman/spouse", "Remarks by Enumerator"


	d1m <- data.frame(
		adm2 = carobiner::fix_name(r1m[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1m[["Union / commune"]], "title"),
		location = r1m[["Village"]]
	)
##r1m: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "J2.1. Which item did you spend on during the last 12 months", "J2.1. Which item did you spend on during the last 12 months (OTHER)", "J2.2. What was the annual expenditure (Tk) on the item in the last 12 months", "Remarks by Enumerator"


	d1n <- data.frame(
		adm2 = carobiner::fix_name(r1n[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1n[["Union / commune"]], "title"),
		location = r1n[["Village"]],
		year = r1n[["M3. How long did the hunger period (Monga) last for last year (months)"]]
	)
##r1n: "Household ID", "Interview date (DD-MM-YYYY)", "Upazilla / Subdistrict", "M2.1 Which food item did your house hold consume during the past 24 hour", "M2. How often did you or your household members consume these items during the p", "M2.2 Who consumed the food item in the past 24 hours...9", "M2.2 Who consumed the food item in the past 24 hours...10", "M2.2 Who consumed the food item in the past 24 hours...11", "M2.2 Who consumed the food item in the past 24 hours...12", "Remarks by Enumerator"


	d2a <- data.frame(
		adm2 = carobiner::fix_name(r2a[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2a: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2b <- data.frame(
		adm2 = carobiner::fix_name(r2b[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2b: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2c <- data.frame(
		adm2 = carobiner::fix_name(r2c[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2c: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2d <- data.frame(
		adm2 = carobiner::fix_name(r2d[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2d: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2e <- data.frame(
		adm2 = carobiner::fix_name(r2e[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2e: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2f <- data.frame(
		adm2 = carobiner::fix_name(r2f[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2f: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2g <- data.frame(
		adm2 = carobiner::fix_name(r2g[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2g: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2h <- data.frame(
		adm2 = carobiner::fix_name(r2h[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2h: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2i <- data.frame(
		adm2 = carobiner::fix_name(r2i[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2i: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2j <- data.frame(
		adm2 = carobiner::fix_name(r2j[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2j: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2k <- data.frame(
		adm2 = carobiner::fix_name(r2k[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2k: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2l <- data.frame(
		adm2 = carobiner::fix_name(r2l[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2l: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2m <- data.frame(
		adm2 = carobiner::fix_name(r2m[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2m: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2n <- data.frame(
		adm2 = carobiner::fix_name(r2n[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)
##r2n: "...2", "...3", "...4", "...5", "...6", "...7", "...8"


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

