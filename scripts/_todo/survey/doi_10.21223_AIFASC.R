# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Adoption and Effects of Orange-Fleshed Sweetpotato in Malawi

International Potato Center (CIP) and partners collaborated to develop orange-fleshed sweetpotato varieties, such as Chipika, Ana-Kwanire, Kadyaubwerere, Mathuthu, Kaphulira and Zondeni . These varieties were formally released in Malawi between 2009 and 2011 and they were heavily promoted by different development organizations in the country. Its success was attributed to its health related attributes, high yield, high seed quality, and good taste. A representative household survey  to Malawian sweetpotato producers was conducted from May to July 2019. The sample size includes 2492 households located in 166 villages in 27 Districts.  Information was gathered on household and farm characteristics, adoption of sweetpotato varieties, potato production, and market participation, among others.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/AIFASC"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Sweetpotato producing households; universe: Sweetpotato households in Malawi; timeMethod: Cross-sectional; dataCollector: International Potato Center; collectorTraining: International Potato Center; samplingProcedure: We estimated that about 500,000 households have been reached through CIP and partner interventions distributing OFSP vines and we assume that the majority of these have, at some point, planted OFSP. Given a total population of about 15 million people in Malawi and assuming an average household size of 5 household members, totaling 3 million households, an adoption rate of about 17% was arrived at %. Adoption data at the varietal level is lacking. Therefore, a proxy varietal-level OFSP adoption rates by an assumed national-level 17% adoption rate for OFSP. In using this estimate, the minimum sample size for a simple random sample would be 602 households to estimate adoption of OFSP with a statistical power of 95% and 3% level of precision. To allow for clustering at the village level, further sampling adjustments are required. In doing so, prior studies suggest that an appropriate intra-cluster correlation for rural households in Malawi is close to 0.3. Our own calculations reveal expected costs of USD 2,015 for each additional sampled village, and USD 46 for each additional sampled household. Using all these parameters for the power calculation, the minimum sample size is 2,409 households. The available funds for this research allowed us to increase the total sample size to 2,520 households..; collectionMode: All 27 districts were sampled, except for Likoma island. This means that our sample is nationally representative of Malawi. At the next lower administrative level – the extension planning area (or EPA) – we randomly selected EPAs within each district. Two different EPA lists were established: one list was for intervention EPAs, i.e., with OFSP project/intervention, and another list for counterfactual villages, i.e., without any OFSP project/intervention. To validate intervention and non-intervention EPAs, a workshop was organized in April 2019. Workshop participants included all major CIP-partners and government extension agents who had knowledge of past and current OFSP dissemination activities. At the next administrative level – the section – we randomly drew from section lists. For non-intervention sections, validation of results was done during the workshop by contacting officials of the randomly selected sections. And in case where it was found that OFSP vines were disseminated in a section, that section was replaced with another one, until it was found that it was a section without intervention. The same sampling procedure was followed at the village-level: based on complete village lists, villages were randomly drawn and reconfirmed OFSP (non-)intervention by contacting section and village officials. For logistical purposes, 15 households per village were sampled. Within each intervention village, 11 households (participants) were randomly selected from the beneficiary list. Four additional households were selected from a different list of households which did not benefit from a project directly (non-participants). In villages without intervention/treatment, 15 counterfactual households were randomly selected from established household lists. Non-participants and counterfactual households were only selected if these had cultivated any type of sweetpotato in winter season of 2018 or rainfed season of 2019.; researchInstrument: Structured household survey questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "Adop01- A, C_Land, CII13-20, C_HHFood, D8-14, E, F21-25, F34-38_VitA, G and I - v1.xlsx"]
	f2 <- ff[basename(ff) == "Adop02- PartA_Household Composition, Roster - v1.xlsx"]
	f3 <- ff[basename(ff) == "Adop03- PartB_Variety Knowledge and Adoption, Roster - v1.xlsx"]
	f4 <- ff[basename(ff) == "Adop04- PartB_Characteristic Importance, Roster - v1.xlsx"]
	f5 <- ff[basename(ff) == "Adop05- PartB_Variety Xtic Performance, Roster - v1.xlsx"]
	f6 <- ff[basename(ff) == "Adop06- PartCII_Plot Level Production and Utilization, Roster - v1.xlsx"]
	f7 <- ff[basename(ff) == "Adop07- PartD_Training Exposure, Roster - v1.xlsx"]
	f8 <- ff[basename(ff) == "Adop08- PartF_Food Consumption 24HR Recall, Roster - v1.xlsx"]
	f9 <- ff[basename(ff) == "Adop09- PartFII_VitaminA Consumption 7Day Recall, Roster - v1.xlsx"]
	f10 <- ff[basename(ff) == "Adop10- PartG_Extension Services, Roster - v1.xlsx"]
	f11 <- ff[basename(ff) == "Adop11- PartGII_Group Membership, Roster - v1.xlsx"]
	f12 <- ff[basename(ff) == "Adop12- PartH_Other Income Sources, Roster - v1.xlsx"]
	f13 <- ff[basename(ff) == "Adop13- PartJI_Production Equipment, Roster - v1.xlsx"]
	f14 <- ff[basename(ff) == "Adop14- PartJII_Livestock, Roster - v1.xlsx"]
	f15 <- ff[basename(ff) == "Adop15- PartJIII_Household Belongings, Roster - v1.xlsx"]
	f16 <- ff[basename(ff) == "DataDictionary_Adoption OFSP Malawi.xlsx"]

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
	r11 <- carobiner::read.excel(f11)
	r12 <- carobiner::read.excel(f12)
	r13 <- carobiner::read.excel(f13)
	r14 <- carobiner::read.excel(f14)
	r15 <- carobiner::read.excel(f15)
	r16a <- carobiner::read.excel(f16, sheet="Adop01")
	r16b <- carobiner::read.excel(f16, sheet="Adop02")
	r16c <- carobiner::read.excel(f16, sheet="Adop03")
	r16d <- carobiner::read.excel(f16, sheet="Adop04")
	r16e <- carobiner::read.excel(f16, sheet="Adop05")
	r16f <- carobiner::read.excel(f16, sheet="Adop06")
	r16g <- carobiner::read.excel(f16, sheet="Adop07")
	r16h <- carobiner::read.excel(f16, sheet="Adop08")
	r16i <- carobiner::read.excel(f16, sheet="Adop09")
	r16j <- carobiner::read.excel(f16, sheet="Adop10")
	r16k <- carobiner::read.excel(f16, sheet="Adop11")
	r16l <- carobiner::read.excel(f16, sheet="Adop12")
	r16m <- carobiner::read.excel(f16, sheet="Adop13")
	r16n <- carobiner::read.excel(f16, sheet="Adop14")
	r16o <- carobiner::read.excel(f16, sheet="Adop15")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm2 = carobiner::fix_name(r1[["2. District"]], "title"),
		location = r1[["5. Village"]],
		year = r1[["Age of [RESPONDENT] (years)"]],
		adm3 = carobiner::fix_name(r1[["19. Distance to the nearest home of the community health surveillance assistants"]], "title"),
		variety = r1[["BI.14. Looking at all the area under variety Kenya in the most recent season of"]],
		season = r1[["BI.14. Looking at all the area under variety Kenya in the most recent season of"]],
		plot_id = r1[["BI.15. What was the area size of your first ever OFSP plot?"]],
		crop = tolower(r1[["BI.18. If area under OFSP variety(ies) has increased, which crop are you now gro...40"]])
	)
##r1: "Respondent ID", "1d. Interview date (DD-MM-YYYY)", "1e. Start time (HH:MM:SS)", "3. EPA", "4. Section", "4. [OTHER] Section", "10. Respondent is", "Sex of [RESPONDENT]", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "[OTHER] Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "15a. Which organizations did you receive vines from?...18", "15a. Which organizations did you receive vines from?...19", "15a. Which organizations did you receive vines from?...20", "15a. [OTHER] Which organization did you receive vines from?", "15b. Did your household grow sweetpotato in 2019?", "16. Distance to nearest market center (Walking minutes)", "17. Distance to nearest all-weather road (Walking minutes)", "18. Distance to nearest public health facility (Walking minutes)", "20. Distance to nearest OFSP vine multiplier  (Walking minutes)", "21. Distance to nearest government extension officer (Walking minutes)", "BI.13. Looking at all the area under sweetpotato during this year (2019) compare", "BI.15a. What was the unit of measure of your first ever OFSP plot?", "BI.15a. [OTHER] What was the unit of measure of your first ever OFSP plot?", "BI.16. Compared to year you first planted OFSP, has  the area under most recent", "BI.17. For which varieties of orange-fleshed sweetpotato have you expanded acrea...35", "BI.17. For which varieties of orange-fleshed sweetpotato have you expanded acrea...36", "BI.17. For which varieties of orange-fleshed sweetpotato have you expanded acrea...37", "BI.17. For which varieties of orange-fleshed sweetpotato have you expanded acrea...38", "BI.17. For which varieties of orange-fleshed sweetpotato have you expanded acrea...39", "BI.18. If area under OFSP variety(ies) has increased, which crop are you now gro...41", "BI.18. If area under OFSP variety(ies) has increased, which crop are you now gro...42", "BI.18. [OTHER] If area under OFSP variety(ies) has increased, which crop are you", "BI.19. What is the name of your most preferred sweetpotato variety?", "BI.19. [OTHER] What is the name of your most preferred sweetpotato variety?", "BI.20. What is the name of your most preferred OFSP variety?", "BI.20. [OTHER] Name of your most preferred OFSP variety?", "CI.1a. Total amount of land holdings owned", "CI.1b. Unit of total amount of land holdings owned", "CI.1b. [OTHER] Unit of total amount of land holdings owned", "CI.2a. Total amount of land under sweetpotato in most recent season of 2018 or 2", "CI.2b. Unit of Total amount of land under sweetpotato in most recent season of 2", "CI.2b. [OTHER] Unit of Total amount of land under sweetpotato in most recent sea", "CI.3a. Total area of land owned in most recent season of 2018 or 2019", "CI.3b. Unit of land owned in most recent season of 2018 or 2019", "CI.3b. [OTHER] Unit of land owned in most recent season of 2018 or 2019", "CI.4a. Total area of land rented-in in most recent season of 2018 or 2019", "CI.4b. Unit of land rented-in in most recent season of 2018 or 2019", "CI.4b. [OTHER] Unit of land rented-in in most recent season of 2018 or 2019", "CI.5a. Total area of land borrowed in most recent season of 2018 or 2019", "CI.5b. Unit of land borrowed in most recent season of 2018 or 2019", "CI.5b. [OTHER] Unit of land borrowed in most recent season of 2018 or 2019", "CI.6. Who makes decisions on how much land is used for sweetpotato production?", "CI.7. Do you have access to swamp land for farming", "CI.8. How far is the nearest swamp land from your homestead (In walking minutes)", "CI.9. Do you do sweetpotato vine conservation (i.e., conserving vines for next p", "CI.10. Have you ever used Triple S?", "CI.11. If Triple S has been used, who used it?", "CII.49. If OFSP and white/yellow fleshed sweetpotato were sold: For the same qua", "CII.50. If any sweetpotato was sold, who made decision on how much was sold?", "CII.51. If any sweetpotato was sold, who made decision on how revenue earned was", "CII.52. If any sweetpotato was sold, where was your major point of sale", "CII.52. [OTHER] If any sweetpotato was sold, where was your major point of sale", "CIII.1a. Does your household consume sweetpotato by Boiling it?", "CIII.1b. Does your household consume sweetpotato by Roasting it?", "CIII.1c. Does your household consume sweetpotato by Frying it?", "CIII.1d. Does your household consume sweetpotato by making it into Porridge?", "CIII.1e. Does your household consume sweetpotato in form of Futali (mashed with", "CIII.1f. Does your household consume sweetpotato when fresh/raw?", "CIII.1g. Does your household consume sweetpotato in any other way apart from (Bo", "CIII.1g. [OTHER] Ways your household consumes sweetpotato?", "CIII.2. Which variety of sweetpotato do you cook most frequently for your home c", "CIII.2. [OTHER] Which variety of sweetpotato do you cook most frequently for you", "CIII.3. What is 2nd most frequently consumed variety in your home", "CIII.3. [OTHER] What is 2nd most frequently consumed variety in your home", "CIII.4. What is the 3rd most frequently consumed variety in your household", "CIII.4. [OTHER] What is the 3rd most frequently consumed variety in your househo", "CIII.5. If your household consumes OFSP from sources other than own, where from", "CIII.5. [OTHER] If your household consumes OFSP from sources other than own, whe", "D.8. After 2017, how many agricultural trainings on sweetpotato /OFSP have you p", "D.9 After 2017, how many nutritional training sessions have you attended?", "D.10a. Have you listened to any program on orange-fleshed sweetpotato on the rad", "D.10b. How frequently did you hear it since January 2018?", "D.10c. Did you like the orange-fleshed sweetpotato program you heard over the ra", "D.11a. Have you received any flyers about the nutritious benefits of orange-fles", "D.11b. How many times have you received the fliers since 2016?", "D.11c. When was the last time you received a flyer? (YYYY)", "D.12 Have you seen/ participated in a drama/skit about orange-fleshed sweetpotat", "D.13 Have you listened to any OFSP songs?", "D.14a. Have you participated in any cooking demonstration?", "D.14b. Mention the year(s) you participated in the cooking demonstration", "D.14c. Have you received any OFSP recipes (i.e. how to make different foods from", "E.1a. In the past four weeks, did you worry that your household would not have e", "E.1b. How frequent in the past four weeks did you worry that your household woul", "E.2a. In the past four weeks, were you or any household member not able to eat t", "E.2b. How frequent in the past four weeks, were you or any household member not", "E.3a. In the past four weeks, did you or any household member have to eat a limi", "E.3b. How frequent in the past four weeks, did you or any household member have", "E.4a. In the past four weeks, did you or any household member have to eat some f", "E.4b. How frequent in the past four weeks, did you or any household member have", "E.5a. In the past four weeks, did you or any household member have to eat a smal", "E.5b. How frequent in the past four weeks, did you or any household member have", "E.6a. In the past four weeks, did you or any other household member have to eat", "E.6b. How frequent in the past four weeks, did you or any other household member", "E.7a. In the past four weeks, was there ever (a day when there was) no food to e", "E.7b. How frequent in the past four weeks, was there ever (a day when there was)", "E.8a. In the past four weeks, did you or any household member go to sleep at nig", "E.8b. How frequent in the past four weeks, did you or any household member go to", "E.9a. In the past four weeks, did you or any household member go a whole day and", "E.9b. How frequent in the past four weeks, did you or any household member go a", "E.10. What months of the year do you consume sweetpotato in your meals at least...121", "E.10. What months of the year do you consume sweetpotato in your meals at least...122", "E.10. What months of the year do you consume sweetpotato in your meals at least...123", "E.10. What months of the year do you consume sweetpotato in your meals at least...124", "E.10. What months of the year do you consume sweetpotato in your meals at least...125", "E.10. What months of the year do you consume sweetpotato in your meals at least...126", "E.10. What months of the year do you consume sweetpotato in your meals at least...127", "E.10. What months of the year do you consume sweetpotato in your meals at least...128", "E.10. What months of the year do you consume sweetpotato in your meals at least...129", "E.10. What months of the year do you consume sweetpotato in your meals at least...130", "E.10. What months of the year do you consume sweetpotato in your meals at least...131", "E.10. What months of the year do you consume sweetpotato in your meals at least...132", "E.11. In the last 12 months, which months did you have less than two meals a day...133", "E.11. In the last 12 months, which months did you have less than two meals a day...134", "E.11. In the last 12 months, which months did you have less than two meals a day...135", "E.11. In the last 12 months, which months did you have less than two meals a day...136", "E.11. In the last 12 months, which months did you have less than two meals a day...137", "E.11. In the last 12 months, which months did you have less than two meals a day...138", "E.11. In the last 12 months, which months did you have less than two meals a day...139", "E.11. In the last 12 months, which months did you have less than two meals a day...140", "E.11. In the last 12 months, which months did you have less than two meals a day...141", "E.11. In the last 12 months, which months did you have less than two meals a day...142", "E.11. In the last 12 months, which months did you have less than two meals a day...143", "E.11. In the last 12 months, which months did you have less than two meals a day...144", "E.12. In the last 12 months, which months did the household receive relief food...145", "E.12. In the last 12 months, which months did the household receive relief food...146", "E.12. In the last 12 months, which months did the household receive relief food...147", "E.12. In the last 12 months, which months did the household receive relief food...148", "E.12. In the last 12 months, which months did the household receive relief food...149", "E.12. In the last 12 months, which months did the household receive relief food...150", "E.12. In the last 12 months, which months did the household receive relief food...151", "E.12. In the last 12 months, which months did the household receive relief food...152", "E.12. In the last 12 months, which months did the household receive relief food...153", "E.12. In the last 12 months, which months did the household receive relief food...154", "E.12. In the last 12 months, which months did the household receive relief food...155", "E.12. In the last 12 months, which months did the household receive relief food...156", "FI.21. Yesterday, how many times did the adults and older children (>14 years ol", "FI.22. Yesterday, how many times did the children from 5 years to 14 years old e", "FI.23. Yesterday, how many times did the reference child [REFCHILD] in this hous", "FI.25a. Number of very small roots of OFSP eaten by reference child [REFCHILD] y", "FI.25b. Number of small roots of OFSP eaten by reference child [REFCHILD] yester", "FI.25c. Number of medium roots of OFSP eaten by reference child [REFCHILD] yeste", "FI.25d. Number of large roots of OFSP eaten by reference child [REFCHILD] yester", "FII.34. On a typical day in the last 7 days how many roots, if any, of orange-fl", "FII.35. What was the average size of these OFSP eaten by the reference child [RE", "FII.36. On a typical day in the last 7 days how many roots, if any, of orange-fl", "FII.37. What was the average size of these OFSP eaten by the reference woman [RE", "FII.38. If either the Ref child or Ref women consumed OFSP in the last 7 days, O", "FII.38. [OTHER] If either the Ref child or Ref women consumed OFSP in the last 7", "GI.3. Did you or any member of your household seek credit during the most recent", "GI.4. Did you/members of the household get the credit.", "GI.5. How much credit did you get (MK)", "GI.6. What was the source of credit...173", "GI.6. What was the source of credit...174", "GI.6. What was the source of credit...175", "GI.6. [OTHER] What was the source of credit", "GI.7. What was the credit for?...177", "GI.7. What was the credit for?...178", "GI.7. What was the credit for?...179", "GI.7. [OTHER] What was the credit for?", "I.1. Main walling material of main residential house", "I.1. [OTHER] Main walling material of main residential house", "I.2. Main roofing material of main residential house", "I.2. [OTHER] Main roofing material of main residential house", "I.3. Main residential house has glass windows", "I.4. Household main source of lighting?", "I.4. [OTHER] Household main source of lighting?", "I.5. Household main source of energy for cooking?", "I.5. [OTHER] Household main source of energy for cooking?", "I.6. Household main source of water for domestic use?", "I.6. [OTHER] Household main source of domestic use water?", "I.7. Distance to the main source of water for domestic use in walking minutes", "I.8. Do you have a pit latrine", "Interview End Time", "duration", "Last Edited", "Approval Status", "Remarks by Enumerator"


	d2 <- data.frame(
		year = r2[["A2.7. Number of years[HHM] took to obtain formal education"]],
		adm2 = carobiner::fix_name(r2[["2. District"]], "title")
	)
##r2: "Respondent ID", "A.1. How many people do live in this household", "A2.1. Member id", "A2.3. Sex of [HHM]", "A2.4. Relation of [HHM] to HH head", "A2.4. [OTHER] Relation of [HHM] to HH head", "A2.5. Age of [HHM]", "A2.6. Marital status of [HHM]", "A2.7. Formal education level of [HHM]", "A2.8. Can [HHM] read/write?", "A2.9. Main occupation of [HHM]", "A2.9. [OTHER] Main occupation of [HHM]", "A2.10. Secondary occupation of [HHM]", "A2.10. [OTHER] Secondary occupation of [HHM]", "A2.11. Is [HHM] pregnant or breastfeeding?", "A2.12. Is [HHM] disabled?", "A2.13. Degree of participation of [HHM] in sweetpotato production", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d3 <- data.frame(
		variety = r3[["BI.2. Flesh color of [VARIETY]"]],
		year = r3[["BI.3. Year [VARIETY] was known/ heard of?"]],
		season = r3[["BI.8a. If last planted in 2018, which season"]],
		hhid = r3[["BI.11. Did you share vines of [VARIETY] with other farmers?"]],
		adm2 = carobiner::fix_name(r3[["2. District"]], "title")
	)
##r3: "Respondent ID", "rank of (bi1_sp_var)    by respid", "BI.1. Sweetpotato varieties aware/heard of", "BI.1. [OTHER] Name of sweetpotato varieties aware/heard of", "BI.2. [OTHER] Flesh color of [VARIETY]", "BI.4. Do you consider [VARIETY]", "BI.5. Have you ever planted [VARIETY]", "BI.6. Year first planted (VARIETY)", "BI.7. Where did you obtain vines of [VARIETY] from?", "BI.7. [OTHER] Where did you obtain vines of [VARIETY] from?", "BI.8. Last year [VARIETY] was planted", "BI.9. If [VARIETY] was not planted in 2019, what was the main reason?", "BI.9. [OTHER] If [VARIETY] was not planted in 2019, what was the main reason?", "BI.10. What land have you used to grow [VARIETY]...17", "BI.10. What land have you used to grow [VARIETY]...18", "BI.10. What land have you used to grow [VARIETY]...19", "BI.10. [OTHER] What land have you used to grow [VARIETY]", "BI.12. The [VARIETY] vines were shared with...22", "BI.12. The [VARIETY] vines were shared with...23", "BI.12. The [VARIETY] vines were shared with...24", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "Age of [RESPONDENT] (years)", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d4 <- data.frame(
		season = r4[["BII.1a. Characteristics during the most recent season in 2018 or 2019"]],
		adm2 = carobiner::fix_name(r4[["2. District"]], "title"),
		year = r4[["Age of [RESPONDENT] (years)"]]
	)
##r4: "Respondent ID", "rank of (bii1a_var_xtic)    by respid", "BII.1b. [OTHER] Characteristic that is important in your choice of a sweetpotato", "BII.1c. How important is the [CHARACTERISTIC] in the choice of a sweetpotato var", "1a. Enumerator", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d5 <- data.frame(
		variety = r5[["BII.2a. Name of variety during the most recent season in 2018 or 2019"]],
		season = r5[["BII.2a. Name of variety during the most recent season in 2018 or 2019"]],
		adm2 = carobiner::fix_name(r5[["2. District"]], "title"),
		year = r5[["Age of [RESPONDENT] (years)"]]
	)
##r5: "Respondent ID", "rank of (bii2c_var_xtic)    by respid", "BII.2a.[OTHER] Name of variety", "BII.2b. Characteristic during the most recent season in 2018 or 2019", "BII.2b. [OTHER] Characteristic during the most recent season in 2018 or 2019", "BII.2c. Importance of Characteristic during the most recent season in 2018 or 20", "BII.2d. How well did the [VARIETY] grown during the most recent season in 2018 o", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d6 <- data.frame(
		plot_id = r6[["rank of (ci1_plot_no)    by respid"]],
		season = r6[["CII.2a. Most recent Season that Sweetpotato was cultivated on plot [PLOTNO] [PLO"]],
		location = r6[["CII.3. Location of the [PLOTNO] [PLOTNAME]"]],
		crop = tolower(r6[["CII.8. Was sweetpotato intercropped in plot [PLOTNO] [PLOTNAME]"]]),
		year = r6[["CII.29. Is any of the managers of the plot [PLOTNO] [PLOTNAME] less than 25 year"]],
		rep = r6[["CII.32. Which months did you do land preparation on plot [PLOTNO] [PLOTNAME]...40"]],
		adm2 = carobiner::fix_name(r6[["2. District"]], "title")
	)
##r6: "Respondent ID", "CII.1. Plot number", "CII.2. Plot name", "CII.3. [OTHER] Location of the [PLOTNO] [PLOTNAME]", "CII.4. Distance to plot [PLOTNO] [PLOTNAME] in walking minutes", "CII.5. Area of plot  [PLOTNO] [PLOTNAME]", "CII.6. Area units of [PLOTNO] [PLOTNAME]", "CII.6. [OTHER] Area units of plot [PLOTNO] [PLOTNAME]", "CII.7a. Was plot [PLOTNO] [PLOTNAME] irrigated to supplement rain", "CII.7b. Was fertilizer used in plot [PLOTNO] [PLOTNAME]", "CII.7c. Was Pesticides Applied on plot [PLOTNO] [PLOTNAME]", "CII.9. What was the main crop (intercrop) on plot  [PLOTNO] [PLOTNAME]", "CII.9. [OTHER] What was the main crop (intercrop) on plot  [PLOTNO] [PLOTNAME]", "CII.10. The proportion of Sweetpotato on plot  [PLOTNO] [PLOTNAME] was:", "CII.11. What propotion of this sweetpotato plot have been harvested?", "CII.12. Did the plot have Kaphulira?", "CII.13. What is/was the propotion of Kaphulira", "CII.14. Did the plot have Zondeni?", "CII.15. What is/was the propotion of Zondeni", "CII.16. Did the plot have Kadyau-Bwerere?", "CII.17. What is/was the propotion of Kadyau-Bwerere", "CII.18. Did the plot have Chipika?", "CII.19. What is/was the propotion of Chipika", "CII.20. Did the plot have Ana-Kwanire?", "CII.21. What is/was the propotion of Ana-Kwanire", "CII.22. Did the plot have Mathuthu?", "CII.23. What is/was the propotion of Mathuthu", "CII.24. Did the plot have Kenya?", "CII.25. What is/was the propotion of Kenya", "CII.26. Did the plot have other varieties?", "CII.27. What is/was the propotion of the Other Varieties", "CII.28. Who manages the plot [PLOTNO] [PLOTNAME]", "CII.30. Does your household own, borrow or rent plot [PLOTNO] [PLOTNAME]", "CII.31. What is the main Sweetpotato product on plot [PLOTNO] [PLOTNAME]", "CII.32. Which months did you do land preparation on plot [PLOTNO] [PLOTNAME]...41", "CII.32. Which months did you do land preparation on plot [PLOTNO] [PLOTNAME]...42", "CII.33. Which months did you plant on plot [PLOTNO] [PLOTNAME]...43", "CII.33. Which months did you plant on plot [PLOTNO] [PLOTNAME]...44", "CII.33. Which months did you plant on plot [PLOTNO] [PLOTNAME]...45", "CII.34. Which months did you do weeding/hilling up on plot [PLOTNO] [PLOTNAME]...46", "CII.34. Which months did you do weeding/hilling up on plot [PLOTNO] [PLOTNAME]...47", "CII.34. Which months did you do weeding/hilling up on plot [PLOTNO] [PLOTNAME]...48", "CII.35. Which months did you do peace-meal harvest only on plot [PLOTNO] [PLOTNA...49", "CII.35. Which months did you do peace-meal harvest only on plot [PLOTNO] [PLOTNA...50", "CII.35. Which months did you do peace-meal harvest only on plot [PLOTNO] [PLOTNA...51", "CII.35. Which months did you do peace-meal harvest only on plot [PLOTNO] [PLOTNA...52", "CII.35. Which months did you do peace-meal harvest only on plot [PLOTNO] [PLOTNA...53", "CII.36. Which months did you do main harvesting on plot [PLOTNO] [PLOTNAME]...54", "CII.36. Which months did you do main harvesting on plot [PLOTNO] [PLOTNAME]...55", "CII.36. Which months did you do main harvesting on plot [PLOTNO] [PLOTNAME]...56", "CII.36. Which months did you do main harvesting on plot [PLOTNO] [PLOTNAME]...57", "CII.36. Which months did you do main harvesting on plot [PLOTNO] [PLOTNAME]...58", "CII.37a. During the main harvest months how much did you harvest in total in lar", "CII.37b. During the main harvest months what units did you harvest from plot [PL", "CII.37b. [OTHER] During the main harvest months, what units did you harvest from", "CII.38a. During the main harvest months, how much of this large amount did you s", "CII.38b. During the main harvest months, what units did you sell or pay in-kind", "CII.38b. [OTHER] During the main harvest months, what units did you sell or pay", "CII.39. During the main harvest months, did you also harvest in small amounts fo", "CII.40a. During the main harvest months, think of how much you harvested during", "CII.40b. During the main harvest months what amount did you harvest from plot [P", "CII.40c. During the main harvest months, what unit measure did you harvest from", "CII.40c. [OTHER] During the main harvest months, what unit measure did you harve", "CII.41. During the main harvest months what proportion of this from plot [PLOTNO", "CII.42. During the main harvest months, what proportion of this from plot [PLOTN", "CII.43. During the main harvest months, what was the total revenues (MK) earned", "CII.44a. During the minor harvesting months, think of how much you harvested dur", "CII.44b. During the minor harvesting months, what amount in a week did you harve", "CII.44c. During the minor harvesting months, what units of harvest  in a week di", "CII.44c. [OTHER] During the minor harvesting months, what units of harvest  in a", "CII.45. During the minor harvesting months, did you sell any roots from plot [PL", "CII.46. During the minor harvesting months, what proportion was sold or used as", "CII.47. During the minor harvesting months, what proportion of this from plot [P", "CII.48. During the minor harvesting months, total revenues (MK) earned from plot", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "Age of [RESPONDENT] (years)", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d7 <- data.frame(
		year = r7[["D.3. Year training received (YYYY)"]],
		adm2 = carobiner::fix_name(r7[["2. District"]], "title")
	)
##r7: "Respondent ID", "rank of (d1_trainingtype)    by respid", "D.1. Training type", "D.2. Training provided?", "D.4. How many days did you attend the training?", "D.5a. On which agricultural components were you trained?...7", "D.5a. On which agricultural components were you trained?...8", "D.5a. On which agricultural components were you trained?...9", "D.5a. [OTHER] On which agricultural component were you trained?", "D.5b. On which Nutritional components were you trained?...11", "D.5b. On which Nutritional components were you trained?...12", "D.5b. On which Nutritional components were you trained?...13", "D.5b. [OTHER] On which Nutritional component were you trained?", "D.6a. Did your spouse also attend the training?", "D.6b. How many days of training did your spouse attend?", "D.7. How many other Household members attended the training", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "Age of [RESPONDENT] (years)", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d8 <- data.frame(
		adm2 = carobiner::fix_name(r8[["district"]], "title")
	)
##r8: "respid", "response_no", "fi2a_typ_food", "fi2b_household", "fi2b_refwoman", "fi2b_refchild", "fi1a_refwomanid", "fi1c_refchildid", "interviewdate", "epa", "section", "hh_category", "resp_sex", "resp_age", "resp_marital_st", "resp_relationship", "q12_grew_sp", "q13_grwn_ospf", "q14_rcvd_ofspvines", "enum_remarks"


	d9 <- data.frame(
		adm2 = carobiner::fix_name(r9[["2. District"]], "title"),
		year = r9[["Age of [RESPONDENT] (years)"]]
	)
##r9: "Respondent ID", "rank of (fii1a_food_items)    by respid", "FII.1a. Food items", "FII.1b. During the last seven days, on how many days did the ref child [REFCHILD", "FII.1c. During the last seven days, on how many days did the ref woman [REFWOMAN", "FI.1a. What is the name of the reference woman", "FI.1c. What is the name of the reference child", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d10 <- data.frame(
		hhid = r10[["GI.1. Did any agricultural extension officer or lead farmer get in contact with"]],
		adm2 = carobiner::fix_name(r10[["2. District"]], "title"),
		year = r10[["Age of [RESPONDENT] (years)"]]
	)
##r10: "Respondent ID", "rank of (gi2_ext_service)    by respid", "GI2.1. What sweetpotato extension services did you access during the most recent", "GI2.1. [OTHER] What sweetpotato extension service did you access during the most", "GI2.2. Total number of days of contact with extension officer/ lead farmer for [", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d11 <- data.frame(
		hhid = r11[["GII.1. Has any member of your household been involved in any farmer organization"]],
		year = r11[["GII.5. Year household member joined the group"]],
		adm2 = carobiner::fix_name(r11[["2. District"]], "title")
	)
##r11: "Respondent ID", "GII.1. Member ID", "GII.3. Type of group the household member is/was involved in", "GII.3. [OTHER] Type of group the household member is/was involved in", "GII.4. What are the two most important group functions...6", "GII.4. What are the two most important group functions...7", "GII.4. [OTHER] What are the two most important group functions", "GII.6. Still a member of the group", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "Age of [RESPONDENT] (years)", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d12 <- data.frame(
		adm2 = carobiner::fix_name(r12[["2. District"]], "title"),
		year = r12[["Age of [RESPONDENT] (years)"]]
	)
##r12: "Respondent ID", "rank of (h1_src_income)    by respid", "H.1. Source of income since January 2019", "H.1. [OTHER] Source of income since January 2019", "H.3. Quantity earned from [SOURCE] since January 2019", "H.4. Unit of measure", "H.4. [OTHER] Unit of measure", "H.5. Price/unit", "H.6. Total income (MK) earned since January 2019", "H.7. Who controls the income earned", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d13 <- data.frame(
		adm2 = carobiner::fix_name(r13[["2. District"]], "title"),
		year = r13[["Age of [RESPONDENT] (years)"]]
	)
##r13: "Respondent ID", "rank of (ji1_agric_equip_owned)    by respid", "JI.1. Agricultural equipment owned", "JI.1. [OTHER] Agricultural equipment owned", "JI.2. Number of [ASSET] owned", "JI.3. If you were to buy EACH of the ASSET your household owns from someone else", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d14 <- data.frame(
		adm2 = carobiner::fix_name(r14[["2. District"]], "title"),
		year = r14[["Age of [RESPONDENT] (years)"]]
	)
##r14: "Respondent ID", "rank of (jii1_lvstk)    by respid", "JII.1. Livestock owned", "JII.1. [OTHER]  Livestock owned", "JII.2. Number of [LIVESTOCK] owned", "JII.3. If you were to buy EACH of the LIVESTOCK your household owns from someone", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d15 <- data.frame(
		adm2 = carobiner::fix_name(r15[["2. District"]], "title"),
		year = r15[["Age of [RESPONDENT] (years)"]]
	)
##r15: "Respondent ID", "rank of (jiii1_durable_good)    by respid", "JIII.1.Durable goods owned", "JIII.1. [OTHER] Durable goods owned", "JIII.2. Number of [DURABLE_GOOD] owned", "JIII.3. If we were to buy ONE of your [DURABLE_GOOD] in its current condition, h", "1d. Interview date (DD-MM-YYYY)", "3. EPA", "4. Section", "10. Respondent is", "Sex of [RESPONDENT]", "[RESPONDENT] Marital status", "Relationship of [RESPONDENT] to Household Head", "12. Have you ever grown Sweetpotato?", "13. Have you ever grown orange-fleshed sweetpotato (OFSP)?", "14. Have you ever received orange-fleshed sweetpotato either from any organizati", "Remarks by Enumerator"


	d16a <- data.frame()
##r16a: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16b <- data.frame()
##r16b: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16c <- data.frame()
##r16c: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16d <- data.frame()
##r16d: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16e <- data.frame()
##r16e: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16f <- data.frame()
##r16f: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16g <- data.frame()
##r16g: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16h <- data.frame()
##r16h: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16i <- data.frame()
##r16i: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16j <- data.frame()
##r16j: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16k <- data.frame()
##r16k: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16l <- data.frame()
##r16l: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16m <- data.frame()
##r16m: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16n <- data.frame()
##r16n: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d16o <- data.frame()
##r16o: "OFSP_MALAWI ADOPTION DICTIONARY", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


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

