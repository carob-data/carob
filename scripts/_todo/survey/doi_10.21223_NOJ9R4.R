# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh in 2020

Roots and tuber crops (RTCs) play an important role in diverse Asian agri-food systems and contribute to their resilience and capacity to adapt to different challenging environments.The study is a quantitative assessment to better understand the current farming practice including potato and sweetpotato farming in salt prone areas to finally increase the potato and sweetpotato yield in those areas. The information collected was divided in sixteen sections: A) Basic information and consent, (B) Household Roster, (C) Roster of land and pond/water bodies owned or under operation, (D1) Agriculture Plot Utilization, (D2) Irrigation method (in the last 12 months), (D3) Usage of Agricultural Chemicals, Fertilizers and Pesticides (in the last 12 months), (D4) Rental Cost of Tools, Machinery and Draft Animal (in the last 12 months), (D5) Labor Usage by Gender for Crop Plantation and Harvesting, (D6) Post Harvest Labor, Animal and Tools/ Machinery Usage, (E) Household Assets, (F) Household Access to Resources, (G) Household Access to Extension and Credit, (H) Livestock and Aquaculture, (I) Household annual off-farm Income, (J) Household annual Expenditures, (K) Social networks, (L) Migration, (M) Household Food Security, (N) Constraints to POTATO AND SWEETPOTATO production and Marketing, (O) Risk Perception, (P) Nutrition and Hygiene, (Q) Knowledge, exposure and behavioral responses to Covid-19, (R) Labour and other inputs use in the last harvest and transplant, (S) Potato and Sweet potato and last cyclone (Amphan) related information.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/NOJ9R4"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Bangladesh Agricultural University, Mymensingh",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Potato and sweetpotato producing households with different salinity intrusion intensity.; universe: Potato and sweetpotato producing households with different salinity intrusion intensity in Satkhira and Khulna districts.; timeMethod: Cross-sectional; collectorTraining: International Potato Center; samplingProcedure: We purposively selected two districts, Satkhira and Khulna, in the southwest of Bangladesh. In each district, 3 upazilas (the next lowest administrative level) were also purposively selected based on salinity intrusion intensity. Across the 6 upazilas, we randomly selected 19 villages and further randomly sampled 444 households.; collectionMode: Enumerators visited selected random households in each village and collected the information required in the 28-page questionnaire using tablets and the CS Pro software.; researchInstrument: Structured household survey questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "01-General Non-Roster Dataset.xlsx"]
	f2 <- ff[basename(ff) == "02-Household Demographics, Roster.xlsx"]
	f3 <- ff[basename(ff) == "03-Land And Water Bodies Owned, Roster.xlsx"]
	f4 <- ff[basename(ff) == "04-Plot Utilization and Irrigation Methods, Roster.xlsx"]
	f5 <- ff[basename(ff) == "05-Inputs, tools-animal rentals and labour costs, Roster.xlsx"]
	f6 <- ff[basename(ff) == "06-Assets, Roster.xlsx"]
	f7 <- ff[basename(ff) == "07-Livestock And Aquaculture, Roster.xlsx"]
	f8 <- ff[basename(ff) == "08-Off Farm Income, Roster.xlsx"]
	f9 <- ff[basename(ff) == "09-Household Expenditures, Roster.xlsx"]
	f10 <- ff[basename(ff) == "10-Household Food Consumption, Roster.xlsx"]
	f11 <- ff[basename(ff) == "11_Datadictionary_Salt Tolerant Bangladesh_2020.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)
	r8 <- carobiner::read.excel(f8)
	r9 <- carobiner::read.excel(f9)
	r10 <- carobiner::read.excel(f10)
	r11a <- carobiner::read.excel(f11, sheet="Sections")
	r11b <- carobiner::read.excel(f11, sheet="01-General")
	r11c <- carobiner::read.excel(f11, sheet="02-Demographics")
	r11d <- carobiner::read.excel(f11, sheet="03-Land_Water")
	r11e <- carobiner::read.excel(f11, sheet="04-Plot")
	r11f <- carobiner::read.excel(f11, sheet="05-Costs")
	r11g <- carobiner::read.excel(f11, sheet="06-Assets")
	r11h <- carobiner::read.excel(f11, sheet="07-Livestock")
	r11i <- carobiner::read.excel(f11, sheet="08-Off_farm")
	r11j <- carobiner::read.excel(f11, sheet="09-Expenditure")
	r11k <- carobiner::read.excel(f11, sheet="10-Food")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm2 = carobiner::fix_name(r1[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r1[["Union / commune"]], "title"),
		location = r1[["Village"]],
		year = r1[["A5b. Year to receive seed"]],
		plot_id = r1[["C1.0: Number of plots"]],
		crop = tolower(r1[["D1.1.1 Crop 1"]]),
		variety = r1[["F20. Cyclone shelter (Km)"]],
		hhid = r1[["G11.Visited by fellow farmers"]],
		elevation = r1[["GPS Altitude"]]
	)
##r1: "Household ID", "Language", "Interview date (DD-MM-YYYY)", "Start time (HH:MM:SS)", "Upazilla / Subdistrict", "Gender", "Agreement to participate", "A5a. CIP Seed source", "A5b. Month to receive the seed?", "A5c. Seed quantity", "A5d. Potato farming kit", "A6a. CIP Sweetpotato vines source", "A6b. Year to receive vines", "A6b. Month to receive vines", "A6c. Vine quantity", "A6c. UNIT of Vine Received", "A6d.Sweetpotato farming kit", "A7a. CIP Training", "A7b. Number of training", "A7c. Last training (Year)", "A7c. Last training (Month)", "A7d. Wife attend", "A7e. Husband attend", "B1. Total households members", "B2a. Adults (18yrs and Above)", "B2b. Adolescents (6-17yrs)", "B2c.  Children (3-5yrs)", "B2d.  Infants (0-2yrs)", "D1.1.2 Crop 2", "D1.1.3 Crop 3", "D1.1.4 Crop 4", "D1.1.5 Crop 5", "D1.1.6 Crop 6", "D1.1.7 Crop 7", "D1.1.8 Crop 8", "D1.1.9 Crop 9", "D1.1.10 Crop 10", "D1.1.11 Crop 11", "D1.1.12 Crop 12", "D1.1.13 Crop 13", "D1.1.14 Crop 14", "D1.1.15 Crop 15", "D1.1.16 Crop 16", "D1.1.17 Crop 17", "D1.1.18 Crop 18", "D1.1.19 Crop 19", "D1.1.20 Crop 20", "D3.2.1 Input 01 used in the last 12 months", "D3.2.2 Input 02 used in the last 12 months", "D3.2.3 Input 03 used in the last 12 months", "D3.2.4 Input 04 used in the last 12 months", "D3.2.5 Input 05 used in the last 12 months", "D3.2.6 Input 06 used in the last 12 months", "D4.1 Rent tools/machinery/animal", "D5.1 Difficulties hiring labor?", "D5.2.1 Month 1 hiring labor difficult", "D5.2.2 Month 2 hiring labor difficult", "D5.2.3 Month 3 hiring labor difficult", "D5.2.4 Month 4 hiring labor difficult", "D5.2.5 Month 5 hiring labor difficult", "D5.2.6 Month 6 hiring labor difficult", "D5.2.7 Month 7 hiring labor difficult", "D5.2.8 Month 8 hiring labor difficult", "D5.2.9 Month 9 hiring labor difficult", "D5.2.10 Month 10 hiring labor difficult", "D5.2.11 Month 11 hiring labor difficult", "D5.3.1 Months 1 labor available", "D5.3.2 Months 2 labor available", "D5.3.3 Months 3 labor available", "D5.3.4 Months 4 labor available", "D5.3.5 Months 5 labor available", "D5.3.6 Months 6 labor available", "D5.3.7 Months 7 labor available", "D5.3.8 Months 8 labor available", "D5.3.9 Months 9 labor available", "D5.3.10 Months 10 labor available", "D5.3.11 Months 11 labor available", "D5.3.12 Months 12 labor available", "D5.3.13 Months 13 labor available", "D5.3.14 Months 14 labor available", "D5.3.15 Months 15 labor available", "D5.3.16 Months 16 labor available", "D5.3.17 Months 17 labor available", "D5.3.18 Months 18 labor available", "D5.3.19 Months 19 labor available", "D5.3.20 Months 20 labor available", "D5.3.21 Months 21 labor available", "D5.3.22 Months 22 labor available", "E1.1 Asset 01", "E1.2 Asset 02", "E1.3 Asset 03", "E1.4 Asset 04", "E1.5 Asset 05", "E1.6 Asset 06", "E1.7 Asset 07", "E1.8 Asset 08", "E1.9 Asset 09", "E1.10 Asset 10", "E1.11 Asset 11", "E1.12 Asset 12", "E1.13 Asset 13", "E1.14 Asset 14", "E1.15 Asset 15", "E1.16 Asset 16", "E1.17 Asset 17", "E1.18 Asset 18", "E1.19 Asset 19", "E1.20 Asset 20", "E1.21 Asset 21", "E1.22 Asset 22", "E1.23 Asset 23", "E1.24 Asset 24", "E1.25 Asset 25", "F1. Electricity", "F2. Main plot to house (km)", "F3. Main plot to house (min)", "F4. Potato farm to house (km)", "F5. Potato farm to house (min)", "F6. Sweetpotato farm to house (km)", "F7. Sweetpotato farm to house (min)", "F9. Water source (min)", "F8. Water source (Km)", "F10. Money lender or bank or cooperative (Km)", "F11. Money lender or bank or cooperative (min)", "F12. Extension office (Km)", "F13. Extension office (min)", "F14. School (Km)", "F15. School (min)", "F16. Input shop (Km)", "F17. Input shop (min)", "F18. Trader/collection center/market (Km)", "F19. Trader/collection center/market (min)", "F20. Cyclone shelter (min)", "F21. Coast (Km)", "F21. Coast (min)", "F22. Embankment (Km)", "F22. Embarkment (min)", "G1. Get credit", "G2. Got credit", "G3.1 Source of credit", "G3.2 Source of credit", "G3.3 Source of credit", "G3.4 Source of credit", "G3.5 Source of credit", "G3.6 Source of credit", "G3. Source of credit (OTHER)", "G4.1 Credit SOURCE 1 (Tk)", "G4.2 Credit SOURCE 2 (Tk)", "G4.3 Credit SOURCE 3 (Tk)", "G5. Number of finance sources", "G6. Organization/ cooperative", "G7. HH members in organization/ cooperative", "G8. Crop/livestock insurance", "G9.1. Saline water intrusion problem", "G9.2.% of land under saline prone areas", "G10. Visited by extension officer", "H1.1 Farm animal 01", "H1.2 Farm animal 02", "H1.3 Farm animal 03", "H1.4 Farm animal 04", "H1.5 Farm animal 05", "H1.6 Farm animal 06", "H1.7 Farm animal 07", "I.1 Off-farm income", "I2.1 Source of Income 01", "I2.2 Source of Income 02", "I2.3 Source of Income 03", "I2.4 Source of Income 04", "I2.5 Source of Income 05", "I2.6 Source of Income 06", "J1.1. Item spent 01", "J1.2. Item spent 02", "J1.3. Item spent 03", "J1.4. Item spent 04", "J1.5. Item spent 05", "J1.6. Item spent 06", "J1.7. Item spent 07", "J1.8. Item spent 08", "J1.9. Item spent 09", "J1.10. Item spent 10", "J1.11. Item spent 11", "J1.12. Item spent 12", "J1.13. Item spent 13", "J1.14. Item spent 14", "J1.15. Item spent 15", "J1.16. Item spent 16", "J1.17. Item spent 17", "J1.18. Item spent 18", "J1.19. Item spent 19", "K1. Community meeting", "K2.Neighbors/friends potato/sweetpotato", "K3. Family members in potato/sweetpotato", "K4. Progressive farmers in potato enterprise", "K5. HH in village in potato production (%)", "K6. HH in  village in sweetpotato production (%)", "L1. Member of HH migrated", "L3.  Number of HH member migrate (2)", "L2.  Number of HH member migrate (1)", "L4. Remittances (Tk)", "M1.1 Food consumed 01", "M1.2 Food consumed 02", "M1.3 Food consumed 03", "M1.4 Food consumed 04", "M1.5 Food consumed 05", "M1.6 Food consumed 06", "M1.7 Food consumed 07", "M1.8 Food consumed 08", "M1.9 Food consumed 09", "M1.10 Food consumed 10", "M1.11 Food consumed 11", "M1.12 Food consumed 12", "M1.13 Food consumed 13", "M1.14 Food consumed 14", "M1.15 Food consumed 15", "M1.16 Food consumed 16", "M1.17 Food consumed 17", "M1.18 Food consumed 18", "M1.19 Food consumed 19", "M1.20 Food consumed 20", "M1.21 Food consumed 21", "M1.22 Food consumed 22", "M1.23 Food consumed 23", "M1.24 Food consumed 24", "M1.25 Food consumed 25", "M1.12 Food consumed 26", "N1.Potato/sweetpotato experience", "N2. Constraints in potato/sweetpotato", "N3.1 Constraint 1 in potato/sweetpotato", "N3.2 Constraint 2 in potato/sweetpotato", "N3.3 Constraint 3 in potato/sweetpotato", "N3.4 Constraint 4 in potato/sweetpotato", "N3.5 Constraint 5 in potato/sweetpotato", "N3.6 Constraint 6 in potato/sweetpotato", "N3.7 Constraint 7 in potato/sweetpotato", "N3.8 Constraint 8 in potato/sweetpotato", "N3.9 Constraint 9 in potato/sweetpotato", "N3.10 Constraint 10 in potato/sweetpotato", "N3.11 Constraint 11 in potato/sweetpotato", "N3.12 Constraint 12 in potato/sweetpotato", "N3.13 Constraint 13 in potato/sweetpotato", "N3.14 Constraint 14 in potato/sweetpotato", "N3.15 Constraint 15 in potato/sweetpotato", "N3.16 Constraint 16 in potato/sweetpotato", "N3.17 Constraint 17 in potato/sweetpotato", "N3.18 Constraint 18 in potato/sweetpotato", "N3.19 Constraint 19 in potato/sweetpotato", "N3.20 Constraint 20 in potato/sweetpotato", "N3.21 Constraint 21 in potato/sweetpotato", "N3.22 Constraint 22 in potato/sweetpotato", "N3.23 Constraint 23 in potato/sweetpotato", "N3.24 Constraint 24 in potato/sweetpotato", "N3.25 Constraint 25 in potato/sweetpotato", "N3.26 Constraint 26 in potato/sweetpotato", "N3.27 Constraint 27 in potato/sweetpotato", "N3.28 Constraint 28 in potato/sweetpotato", "N3.29 Constraint 29 in potato/sweetpotato", "N3.30 Constraint 30 in potato/sweetpotato", "N3.31 Constraint 31 in potato/sweetpotato", "N3.32 Constraint 32 in potato/sweetpotato", "N3.33 Constraint 33 in potato/sweetpotato", "N3.34 Constraint 34 in potato/sweetpotato", "N4. Remark constraints", "O1. Risk level", "O2. Take risks", "O3. Give up something", "O4. money won invested", "P1c. Exclusively breastfeed age", "P2. Complementary feeding age", "P3. Benefits of complementary feeding", "P4. Portion size for complementary feeding", "P5. Month breastfeeding", "P6.1 Food 1 good for children", "P6.2 Food 2 good for children", "P6.3 Food 3 good for children", "P6.4 Food 4 good for children", "P6.5 Food 5 good for children", "P6.6 Food 6 good for children", "P6.7 Food 7 good for children", "P6.8 Food 8 good for children", "P6.9 Food 9 good for children", "P6.6 Food good for children (OTHER)", "P7.1 Foods bad for child", "P7.2.1 Food 1 bad for  child", "P7.2.2 Food 2 bad for  child", "P7.2.3 Food 3 bad for  child", "P7.2.4 Food 4 bad for  child", "P7.2.5 Food 5 bad for  child", "P7.2.6 Food 6 bad for  child", "P7.2.7 Food 7 bad for  child", "P7.2.8 Food 8 bad for  child", "P7.2.9 Food 9 bad for  child", "P7.2.10 Food 10 bad for  child", "P7.2.11 Food 11 bad for  child", "P7.2.12 Food 12 bad for  child", "P8.1 Foods improve eye sight", "P8.2.1 Food 1 improve eye sight", "P8.2.2 Food 2 improve eye sight", "P8.2.3 Food 3 improve eye sight", "P8.2.4 Food 4 improve eye sight", "P8.2.5 Food 5 improve eye sight", "P8.2.6 Food 6 improve eye sight", "P8.2.7 Food 7 improve eye sight", "P8.2.8 Food 8 improve eye sight", "P8.2.9 Food 9 improve eye sight", "P8.2.10 Food 6 improve eye sight (Other)", "P9.1 Foods essential blood formation", "P9.2.1 Food 1 essential blood formation", "P9.2.2 Food 2 essential blood formation", "P9.2.3 Food 3 essential blood formation", "P9.2.4 Food 4 essential blood formation", "P9.2.5 Food 5 essential blood formation", "P9.2.6 Food 6 essential blood formation", "P9.2.7 Food 7 essential blood formation", "P9.2.8 Food 8 essential blood formation", "P9.2.9 Food 9 essential blood formation", "P9.2.10 Food 10 essential blood formation", "P10. Potato in complementary feeding", "P11.1 Problem 1 potato in complementary feeding", "P11.2 Problem 2 potato in complementary feeding", "P11.3 Problem 3 potato in complementary feeding", "P11.4 Problem 4 potato in complementary feeding", "P11.5 Problem 5 potato in complementary feeding", "P12. Sweetpotato in complementary feeding", "P13.1 Problem 1 sweetpotato in complementary feeding", "P13.2 Problem 2 sweetpotato in complementary feeding", "P13.3 Problem 3 sweetpotato in complementary feeding", "P13.4 Problem 4 sweetpotato in complementary feeding", "P14.1  Heard OFSP", "P14.2.1 What heard OFSP 1", "P14.2.2 What heard OFSP 2", "P14.2.3 What heard OFSP 3", "P14.2.4 What heard OFSP 4", "P15. Nutritional services", "P16.1 Nutritional service  GO", "P16.2 Specify GO program", "P17.1 Nutritional service  NGO", "P17.2 Specify NGO program", "P18.1 Nutritional service other organization", "P18.2 Specify name other organization", "P18.3 Specify other program", "P19.1 Type of services 1", "P19.2 Type of services 2", "P19.3 Type of services 3", "P19.4 Type of services 4", "P19.5 Type of services 5", "P19.6 Type of services 6", "P19.7 Type of services 7", "P19.8 Type of services 8", "P20.1 When do you wash your hands 1", "P20.2 When do you wash your hands 2", "P20.3 When do you wash your hands 3", "P20.4 When do you wash your hands 4", "P20.1 Wash hands before eating", "P20.2 How wash hands before eating", "P21.1 Wash hands after toilet", "P21.2 How wash hands after toilet", "P22.1 Wash hands cleaning babies stool", "P22.2 How wash hands cleaning babies stool", "P23.1 Wash hands giving food to children", "P23.2 How wash hands giving food to children", "Q1. COVID-19 positive", "Q2. Covid-19 impact", "Q2a. OTHER Covid-19 impact", "Q3.1. COVID-19 regulation source (1)", "Q3.2. COVID-19 regulation source (2)", "Q3.3. COVID-19 regulation source (3)", "Q3.4. COVID-19 regulation source (4)", "Q3.5. COVID-19 regulation source (5)", "Q3.6. COVID-19 regulation source (6)", "Q3.7. COVID-19 regulation source (7)", "Q3a. OTHER COVID-19 regulation source", "Q4. Avoiding foods COVID-19", "Q5a. Raw meat?", "Q5b. Meat?", "Q5c. Raw milk?", "Q5d. Dairy products?", "Q5e. Uncooked vegetables?", "Q5f.  vegetables?", "Q5g. Are you avoiding fruits?", "Q6. Other foods", "Q7. Public transportation", "Q8. Market frequency", "Q9. Face mask", "Q10. Protective gloves", "Q11. Religion institution visit", "Q12. Hand wash", "Q13.1. Shopping behavior (1)", "Q13.2. Shopping behavior (2)", "Q13.3. Shopping behavior (3)", "Q13.4. Shopping behavior (4)", "Q14. Fruits and vegetables", "Q15. Hygiene products", "Q16.1. Precautionary measure 1", "Q16.2. Precautionary measure 2", "Q16.3. Precautionary measure 3", "Q16.4. Precautionary measure 4", "Q16.5. Precautionary measure 5", "Q16.6. Precautionary measure 6", "Q16.7. Precautionary measure 7", "Q16.8. Precautionary measure 8", "Q16.9. Precautionary measure 9", "Q16.10. Precautionary measure 10", "R1.  Boro labor shortage", "R2a.1 Respond to labor shortage (1)", "R2a.2  Respond to labor shortage (2)", "R2a.3 H Respond to labor shortage (3)", "R2a.4  Respond to labor shortage (4)", "R2a.5  Respond to labor shortage (5)", "R2a.6  Respond to labor shortage? (6)", "R2a.7  Respond to labor shortage (Other)", "R2b.1 Hired labour origin (1)", "R2b.2 Hired labour origin (2)", "R2b.3 Hired labour origin (3)", "R2b.4 Hired labour origin (4)", "R2b.5 Hired labour origin (5)", "R2b.6 Hired labour origin (6)", "R2b.7 Hired labour origin (Other)", "R2c. Returned Labour hire", "R2d. Trend of labour supply", "R2e. Male household labour", "R2f. Female household labour", "R2g. Children household labour", "R3. Mechanization", "R3.Labour saving tech", "R5a. Seed shortage", "R5b.1. Crops seed shortage (1)", "R5b.2.  Crops seed shortage (2)", "R5b.3.  Crops seed shortage (3)", "R5b.4.  Crops seed shortage (4)", "R5b.5.  Crops seed shortage (5)", "R5b.6.  Crops seed shortage (6)", "R5b.7.  Crops seed shortage (7)", "R5b.8.  Crops seed shortage (8)", "R5b.9.  Crops seed shortage (9)", "R5b.10.  Crops seed shortage (10)", "R5b.11.  Crops seed shortage (Other)", "R6. Save or market seed", "R7. Other input shortage", "R8.Input price increase", "R9. Difficulties in selling", "R10. Type of difficulties", "R11. Credit  changed", "R12.1. Input use change (1)", "R12.2. Input use change (2)", "R12.3. Input use change (3)", "R12.4. Input use change (4)", "R12.5. Input use change (5)", "R12.6. Input use change (Other)", "R13. Vendors change", "R14. Vender change due to Covid-19?", "R15. Price of agricultural commodity changed", "R16. Income change", "R17. Input change due Covid-19?", "R18.1 Household income aspect (1)", "R18.2 Household income aspect (2)", "R18.3 Household income aspect (3)", "R18.4 Household income aspect (4)", "R18.5 Household income aspect (5)", "R18.6 Household income aspect (Other)", "R19. Aid support", "R20.1 Type of aid (1)", "R20.2 Type of aid (2)", "R20.3 Type of aid (3)", "R20.4 Type of aid (4)", "R20.5 Type of aid (Other)", "S1. Cyclone Amphan", "S2.Monetary loss (Tk)", "S3.1. Items/resources loss (1)", "S3.2. Items/resources loss (2)", "S3.3. Items/resources loss (3)", "S3.4. Items/resources loss (4)", "S3.5. Items/resources loss (5)", "S3.6.Items/resources loss (6)", "S3a. HOUSE DAMAGE (Tk)", "S3b. LIVESTOCK (Tk)", "S3c. CROPS (Tk)", "S3d. FISH (Tk)", "S3e. JOBS (Tk)", "S3f. ELECTRICITY (Tk)", "S3g. OTHER EFFECTS (Tk)", "S4. Days flooding", "S5. Crop loss (%)", "S6. Electricity outage", "S7.  Cyclone information", "S8.1 Source cyclone (1)", "S8.2 Source cyclone (2)", "S8.3 Source cyclone (3)", "S8.4 Source cyclone (4)", "S8.5 Source cyclone (5)", "S8.6 Source cyclone (6)", "S8.7 Source cyclone (7)", "S8.8 Source cyclone (Other)", "S9. Saline water intrusion", "S10. Cyclones", "S11. Frequency of  extreme events", "S12. Move to cyclone shelter", "S13. Days in cyclone shelter", "S14. Reason to not use cyclone shelter", "S15: Days in house  under water", "S16. Amphan adaptation", "S17. BEFORE Cyclone Amphan", "S18. DURING Cyclone Amphan", "S19. AFTER Cyclone Amphan", "S20. Adapt to saline", "S21. Salinity adaptation method", "S22. Reduce labor", "S23. Neighbors for assistance", "S24. Reduce consumption", "S25. Reduce  variety of food", "S26. Reduce number of meals", "S27. Sell household items", "S28. Loans", "Remarks by Enumerator", "Interview End Time", "Interview Duration(HH:MM: SS)"


	d2 <- data.frame(
		year = r2[["B3.4 Age (years) of HH Member"]],
		adm2 = carobiner::fix_name(r2[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r2[["Union / commune"]], "title"),
		location = r2[["Village"]]
	)
##r2: "Household ID", "Baseline Household ID", "Interview date (DD-MM-YYYY)", "Gender", "B1. Total households members", "B2a. Adults (18yrs and Above)", "B2b. Adolescents (6-17yrs)", "B2c.  Children (3-5yrs)", "B2d.  Infants (0-2yrs)", "B3.1 HH Member ID", "B3.1. Still Member", "B3.2 Relation with the HH", "B3.2 Relation with the HH (OTHER)", "B3.3 Gender of HH Member", "B3.5 Marital status of HH member", "B3.6 Education of HH member", "B3.7 Main occupation of HH member", "B3.7.1 Main occupation of HH member (Other)", "B3.8 Secondary occupation of HH member", "B3.81 Secondary occupation of HH member (OTHER)", "Upazilla / Subdistrict", "Remarks by Enumerator"


	d3 <- data.frame(
		plot_id = r3[["C1.1 Plot ID"]],
		crop = tolower(r3[["C1.4I- Crop 1"]]),
		adm2 = carobiner::fix_name(r3[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r3[["Union / commune"]], "title"),
		location = r3[["Village"]]
	)
##r3: "Household ID", "Baseline Household ID", "Interview date (DD-MM-YYYY)", "Gender", "C1.2 Plot Description", "C1.3 Plot type", "C1.3 Plot type (OTHER)", "C1.4 Area of the plot", "C1.4I- Crop 2", "C1.4I- Crop 3", "C1.4IO- Crop (other)", "C1.4Ia- Amount produced (kg) Crop 1", "C1.4b. Total value Crop 1  (tk)", "C1.4Ia- Amount produced (kg) Crop 2", "C1.4b. Total value Crop 2  (tk)", "C1.4Ia- Amount produced (kg) Crop 3", "C1.4b. Total value Crop 3  (tk)", "C1.5 Distance of the plot from home", "C1.6 Ownership of the plot (last 12 months)", "C1.6 Ownership of the plot (last 12 months) (OTHER)", "C1.7 Who owns the plot?", "C1.7 Who owns the plot (OTHER)?", "C1.8 Amount (TK) of plot rented/ leased", "C1.9 If plot is rented/leased , provide amount (TK)", "Upazilla / Subdistrict", "Remarks by Enumerator"


	d4 <- data.frame(
		crop = tolower(r4[["D2.2 Crop season"]]),
		season = r4[["D2.2 Crop season"]],
		variety = r4[["D2.5 Main variety"]],
		rep = r4[["D2.7 Seed replacement"]],
		adm2 = carobiner::fix_name(r4[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r4[["Union / commune"]], "title"),
		location = r4[["Village"]]
	)
##r4: "Household ID", "Baseline Household ID", "Interview date (DD-MM-YYYY)", "Gender", "D2.3 Crop irrigated", "D2.3O. Crop (OTHER) irrigated", "D2.4 Cultivated area", "D2.5 Other variety", "D2.6 Seed source", "D2.6 Other seed source", "D2.8a Amount of seed (Kg.)", "D2.8b Cost of seed (per kg)", "D2.9a Amount of seedling (Kg.)", "D2.9b Cost of seedling (per kg)", "D2.10 Quantity harvested (Kg)", "D2.11 Quantity sold (kg)", "D2.12 Price (Kg) of the harvested crop", "D2.13a. Home consumption / shared  (Kg)", "D2.13B. Total value of the byproduct (tk)", "D2.14 Amount (TK) paid for land", "d2.3. Was this crop intercropped?", "d2.3.1. Intercrop 1", "d2.3.2. Intercrop 2", "d2.3.3. Intercrop 3", "d2.3.4. Intercrop 4", "d2.3.5. Intercrop 5", "D2.4 Source of water", "D2.4a Other source of water", "D2.5 Irrigation method", "D2.5 Other irrigation method", "D2.6 Energy used for Irrigation in this season", "D2.7 Number of iirrigation", "D2.8 Payment method for Irrigation in this season", "D2.8a Value of shared crop", "D2.9 Cash cost of Irrigation in this season", "D2.10 Cost of irrigation beyond cash expenditure", "D2.11.1 Cause of water scarcity (1)", "D2.11.2 Cause of water scarcity (2)", "D2.11.3 Cause of water scarcity (3)...42", "D2.11.3 Cause of water scarcity (3)...43", "D2.11.3 Cause of water scarcity (3)...44", "D2.11.3 Cause of water scarcity (3)...45", "D2.11.3 Cause of water scarcity (3)...46", "D2.11.3 Cause of water scarcity (3)...47", "D2.11.3 Cause of water scarcity (3)...48", "D2.11.3 Cause of water scarcity (3)...49", "D2.11.3 Cause of water scarcity (3)...50", "D2.11.3 Cause of water scarcity (3)...51", "D2.11.4 Cause of water scarcity (other)", "D2.12 Month of water scarcity", "Upazilla / Subdistrict", "Remarks by Enumerator"


	d5 <- data.frame(
		crop = tolower(r5[["D3.1 CROP no."]]),
		rep = r5[["D4.2.2 Animals for land preparation"]],
		adm2 = carobiner::fix_name(r5[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r5[["Union / commune"]], "title"),
		location = r5[["Village"]]
	)
##r5: "Household ID", "Baseline Household ID", "Interview date (DD-MM-YYYY)", "Gender", "D3.1 Crop OTHER", "D3.2 Inputs used", "D3.3 Inputs use frecuency", "D3.4 Amount of input use", "D3.5 Unit of inputs used", "D3.6 Costs of inputs use", "D4.2.1.Rent tools_machinery", "D4.2.3 Cost (TK) of the animals - for land preparation", "D4.2.4 Machinery been used for land preparation", "D4.2.6 Costs (TK) for rental/labor/fuels", "D4.2.7 Tools/machinery  used for planting", "D4.2.8 Plantind cost (TK) of Tools/machinery", "D4.2.9 Tools/machinery used for fertilizer application", "D4.2.10 Fertilizer application cost (TK) of Tools/machinery", "D4.2.11 Tools/machinery  used for pesticide application", "D4.2.12 Pesticide application cost (TK) of Tools/machinery", "D4.2.13 Tools/machinery  used for weeding", "D4.2.14 Weeding cost (TK) of Tools/machinery used", "D4.2.15 Tools/machinery  used for harvesting", "D4.2.16 Harvesting cost (TK) of Tools/machinery", "D5.4.2 Hired labour (MALE) - land preppartion", "D5.4.3 Hired labour (FEMALE) - land preppartion", "D5.4.4 Days of Family labour (MALE) - land preppartion", "D5.4.5 Days of Family labour (FEMALE) - land preppartion", "D5.4.6 Wage (TK/Day) Hired / Family labour (MALE) - land preparation", "D5.4.7 Wage (TK/Day) Hired / Family labour (FEMALE) - land preparation", "D5.4.8 Hired labour (MALE) - Planting", "D5.4.9 Hired labour (FEMALE) - Planting", "D5.4.10 Days of Family labour (MALE) - Planting", "D5.4.11 Days of Family labour (FEMALE) - Planting", "D5.4.12 Wage (TK/Day) Hired / Family labour (MALE) - Planting", "D5.4.13 Wage (TK/Day) Hired / Family labour (FEMALE) - Planting", "D5.4.14 Hired labour (MALE) - Fertilizer application", "D5.4.15 Hired labour (FEMALE) - Fertilizer application", "D5.4.16 Days of Family labour (MALE) - Fertilizer application", "D5.4.17 Days of Family labour (FEMALE) - Fertilizer application", "D5.4.18 Hired labour (MALE) - Pesticide application", "D5.4.19  Hired labour (FEMALE) - Pesticide applicat", "D5.4.20 Days of Family labour (MALE) - Pesticide application", "D5.4.21Days of Family labour (FEMALE) - Pesticide application", "D5.4.22 Hired labour (MALE) - Weeding", "D5.4.23  Hired labour (FEMALE) - Weeding", "D5.4.24 Days of Family labour (MALE) - Weeding", "D5.4.25 Days of Family labour (FEMALE) - Weeding", "D5.4.26 Wage (TK/Day) Hired / Family labour (MALE) - Weeding", "D5.4.27 Wage (TK/Day) Hired / Family labour (FEMALE) - Weeding", "D5.4.28 Hired labour (MALE) - Irrigation", "D5.4.29  Hired labour (FEMALE) - Irrigation", "D5.4.30 Days of Family labour (MALE) - Irrigation", "D5.4.31 Days of Family labour (FEMALE) - Irrigation", "D5.4.32 Hired labour (MALE) - Harvest", "D5.4.33  Hired labour (FEMALE) - Harvest", "D5.4.34 Days of Family labour (MALE) -  Harvest", "D5.4.35 Days of Family labour (FEMALE) - Harvest", "D5.4.36 Wage (TK/Day) Hired / Family labour (MALE) - Harvest", "D5.4.37 Wage (TK/Day) Hired / Family labour (FEMALE) - Harvest", "D5.4.38 Hired labour (MALE) -  Transportation", "D5.4.39  Hired labour (FEMALE) - Transportation", "D5.4.40 Days of Family labour (MALE) - Transportation", "D5.4.41 Days of Family labour (FEMALE) - Transportation", "D6.2.1. Harvest mid 2019 to mid 2020", "D6.2.2 Total days of Animal - threshing", "D6.2.3 Total Cost (Tk) of Animal used - threshing", "D6.2.4a Tools/machinery - Threshing", "D6.2.4b Which  Tools - Threshing", "D6.2.5 Cost of Tools/machinery used - threshing", "D6.2.6 Days of hired labour (MALE) - Threshing", "D6.2.7 Days of hired labour (FEMALE) - Threshing", "D6.2.8 Days of Family labour (MALE) - Threshing", "D6.2.9 Days of Family labour (FEMALE) - Threshing", "D6.2.10 Wage (TK/Day) of Hired / Family labour (MALE) - Threshing", "D6.2.11 Wage (TK/Day) of Hired / Family labour (FEMALE) - Threshing", "D6.2.12 Days of hired labour (MALE) - Drying", "D6.2.13 Days of hired labour (FEMALE) - Drying", "D6.2.14 Days of Family labour (MALE) - Drying", "D6.2.15 Days of Family labour (FEMALE) - Drying", "D6.2.16 Wage (TK/Day) of Hired / Family labour (MALE) - Drying", "D6.2.17 Wage (TK/Day) of Hired / Family labour (FEMALE) - Drying", "D6.2.18 Days of hired labour (MALE) - Sorting/Grading", "D6.2.19 Days of hired labour (FEMALE) - Sorting/Grading", "D6.2.20 Days of Family labour (MALE) - Sorting/Grading", "D6.2.21 Days of Family labour (FEMALE) - Sorting/Grading", "D6.2.22 Wage (TK/Day) of Hired / Family labour (MALE) - Sorting/Grading", "D6.2.23 Wage (TK/Day) of Hired / Family labour (FEMALE) - Sorting/Grading", "D6.2.24 Days of hired labour (MALE) - Packaging", "D6.2.25 Days of hired labour (FEMALE) - Packaging", "D6.2.26 Days of Family labour (MALE) - Packaging", "D6.2.27 Days of Family labour (FEMALE) - Packaging", "D6.2.28 Wage (TK/Day) of Hired / Family labour (MALE) - Packaging", "D6.2.29 Wage (TK/Day) of Hired / Family labour (FEMALE) - Packaging", "Upazilla / Subdistrict", "Remarks by Enumerator"


	d6 <- data.frame(
		adm2 = carobiner::fix_name(r6[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r6[["Union / commune"]], "title"),
		location = r6[["Village"]]
	)
##r6: "Household ID", "Baseline Household ID", "Interview date (DD-MM-YYYY)", "Gender", "E2. Assets", "E2.1 Assets (OTHER)", "E2.2 Asset number", "E2.3 Purchase price (Tk)", "Upazilla / Subdistrict", "Remarks by Enumerator"


	d7 <- data.frame(
		adm2 = carobiner::fix_name(r7[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r7[["Union / commune"]], "title"),
		location = r7[["Village"]]
	)
##r7: "Household ID", "Baseline Household ID", "Interview date (DD-MM-YYYY)", "Gender", "H1. Farm animal", "H1. Farm animal (other)", "H2.1 Total Value of all Livestock/Aquaculture", "H2.2 Total rearing costs (Tk) farm animals", "H2.3 Total revenue from selling (Tk) farm animals", "H2.4 Total value of household consumption (Tk)", "Upazilla / Subdistrict", "Remarks by Enumerator"


	d8 <- data.frame(
		year = r8[["I3.5 Income per year (Tk.) for man/husband"]],
		adm2 = carobiner::fix_name(r8[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r8[["Union / commune"]], "title"),
		location = r8[["Village"]]
	)
##r8: "Household ID", "Baseline Household ID", "Interview date (DD-MM-YYYY)", "Gender", "I3.1 Source of Income", "I3.3 Income per month (Tk.)", "I3.4 How many months husband receive income", "I3.6 Contribution to total income for man/husband", "I3.7 Income per month (Tk.) for woman/spouse", "I3.8 Months woman/spouse receive income", "I3.9 Income per year (Tk.) for woman/spouse", "I3.10 Contribution to total income for woman/spouse", "Upazilla / Subdistrict", "Remarks by Enumerator"


	d9 <- data.frame(
		adm2 = carobiner::fix_name(r9[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r9[["Union / commune"]], "title"),
		location = r9[["Village"]]
	)
##r9: "Household ID", "Baseline Household ID", "Interview date (DD-MM-YYYY)", "Gender", "J2.1. Item spent", "J2.1. Item spent  (OTHER)", "J2.2. Annual expenditure (Tk)", "Upazilla / Subdistrict", "Remarks by Enumerator"


	d10 <- data.frame(
		adm2 = carobiner::fix_name(r10[["District / province"]], "title"),
		adm3 = carobiner::fix_name(r10[["Union / commune"]], "title"),
		location = r10[["Village"]]
	)
##r10: "Household ID", "Baseline Household ID", "Interview date (DD-MM-YYYY)", "Gender", "M2.1 Food item - past 24 hour", "M2. How often consume these items", "M2.2 Who consumed the food item", "Upazilla / Subdistrict", "Remarks by Enumerator"


	d11a <- data.frame(
		adm2 = carobiner::fix_name(r11a[["Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh"]], "title")
	)


	d11b <- data.frame()
##r11b: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference"


	d11c <- data.frame()
##r11c: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Sections:"


	d11d <- data.frame()
##r11d: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Sections"


	d11e <- data.frame()
##r11e: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Sections"


	d11f <- data.frame()
##r11f: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Sections"


	d11g <- data.frame()
##r11g: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Sections"


	d11h <- data.frame()
##r11h: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Sections"


	d11i <- data.frame()
##r11i: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Sections"


	d11j <- data.frame()
##r11j: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Sections"


	d11k <- data.frame()
##r11k: "...1", "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "Sections"


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

