# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey of sweetpotato producers in Malawi, May to July 2019. The crop is sweetpotato. Country is Malawi.
# Adop02 is one row per household member. The table here is one row per Respondent ID. Sex is the household head. One household has no row marked Household head, so sex is missing there.
# District, EPA, and section are the place names on that roster. EPA is the extension planning area.

## ISSUES
# No latitude or longitude in the roster.
# missing variables: latitude, longitude
# unknown variables: ofsp_ever_

## NOT PROCESSED
# Adop01 and Adop03 to Adop15: land, production, food, training, livestock, and belongings. Production is not in kg/ha.
# DataDictionary_Adoption OFSP Malawi.xlsx.
# Member-level fields other than the head (relation, age, education, occupation).
# Adop01- A, C_Land, CII13-20, C_HHFood, D8-14, E, F21-25, F34-38_VitA, G and I - v1.xlsx: not read. sheet Sheet1: Respondent ID, 1d. Interview date (DD-MM-YYYY), 1e. Start time (HH:MM:SS), 2. District, 3. EPA, 4. Section, 4. [OTHER] Section, 5. Village, 10. Respondent is, Sex of [RESPONDENT], Age of [RESPONDENT] (years), [RESPONDENT] Marital status, .... Observation records are missing from d until this file is standardized.
# Adop03- PartB_Variety Knowledge and Adoption, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (bi1_sp_var)    by respid, BI.1. Sweetpotato varieties aware/heard of, BI.1. [OTHER] Name of sweetpotato varieties aware/heard of, BI.2. Flesh color of [VARIETY], BI.2. [OTHER] Flesh color of [VARIETY], BI.3. Year [VARIETY] was known/ heard of?, BI.4. Do you consider [VARIETY], BI.5. Have you ever planted [VARIETY], BI.6. Year first planted (VARIETY), BI.7. Where did you obtain vines of [VARIETY] from?, BI.7. [OTHER] Where did you obtain vines of [VARIETY] from?, .... Observation records are missing from d until this file is standardized.
# Adop04- PartB_Characteristic Importance, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (bii1a_var_xtic)    by respid, BII.1a. Characteristics during the most recent season in 2018 or 2019, BII.1b. [OTHER] Characteristic that is important in your choice of a sweetpotato, BII.1c. How important is the [CHARACTERISTIC] in the choice of a sweetpotato var, 1a. Enumerator, 1d. Interview date (DD-MM-YYYY), 2. District, 3. EPA, 4. Section, 10. Respondent is, Sex of [RESPONDENT], .... Observation records are missing from d until this file is standardized.
# Adop05- PartB_Variety Xtic Performance, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (bii2c_var_xtic)    by respid, BII.2a. Name of variety during the most recent season in 2018 or 2019, BII.2a.[OTHER] Name of variety, BII.2b. Characteristic during the most recent season in 2018 or 2019, BII.2b. [OTHER] Characteristic during the most recent season in 2018 or 2019, BII.2c. Importance of Characteristic during the most recent season in 2018 or 20, BII.2d. How well did the [VARIETY] grown during the most recent season in 2018 o, 1d. Interview date (DD-MM-YYYY), 2. District, 3. EPA, 4. Section, .... Observation records are missing from d until this file is standardized.
# Adop06- PartCII_Plot Level Production and Utilization, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (ci1_plot_no)    by respid, CII.1. Plot number, CII.2. Plot name, CII.2a. Most recent Season that Sweetpotato was cultivated on plot [PLOTNO] [PLO, CII.3. Location of the [PLOTNO] [PLOTNAME], CII.3. [OTHER] Location of the [PLOTNO] [PLOTNAME], CII.4. Distance to plot [PLOTNO] [PLOTNAME] in walking minutes, CII.5. Area of plot  [PLOTNO] [PLOTNAME], CII.6. Area units of [PLOTNO] [PLOTNAME], CII.6. [OTHER] Area units of plot [PLOTNO] [PLOTNAME], CII.7a. Was plot [PLOTNO] [PLOTNAME] irrigated to supplement rain, .... Observation records are missing from d until this file is standardized.
# Adop07- PartD_Training Exposure, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (d1_trainingtype)    by respid, D.1. Training type, D.2. Training provided?, D.3. Year training received (YYYY), D.4. How many days did you attend the training?, D.5a. On which agricultural components were you trained?...7, D.5a. On which agricultural components were you trained?...8, D.5a. On which agricultural components were you trained?...9, D.5a. [OTHER] On which agricultural component were you trained?, D.5b. On which Nutritional components were you trained?...11, D.5b. On which Nutritional components were you trained?...12, .... Observation records are missing from d until this file is standardized.
# Adop08- PartF_Food Consumption 24HR Recall, Roster - v1.xlsx: not read. sheet Sheet1: respid, response_no, fi2a_typ_food, fi2b_household, fi2b_refwoman, fi2b_refchild, fi1a_refwomanid, fi1c_refchildid, interviewdate, district, epa, section, .... Observation records are missing from d until this file is standardized.
# Adop09- PartFII_VitaminA Consumption 7Day Recall, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (fii1a_food_items)    by respid, FII.1a. Food items, FII.1b. During the last seven days, on how many days did the ref child [REFCHILD, FII.1c. During the last seven days, on how many days did the ref woman [REFWOMAN, FI.1a. What is the name of the reference woman, FI.1c. What is the name of the reference child, 1d. Interview date (DD-MM-YYYY), 2. District, 3. EPA, 4. Section, 10. Respondent is, .... Observation records are missing from d until this file is standardized.
# Adop10- PartG_Extension Services, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (gi2_ext_service)    by respid, GI.1. Did any agricultural extension officer or lead farmer get in contact with, GI2.1. What sweetpotato extension services did you access during the most recent, GI2.1. [OTHER] What sweetpotato extension service did you access during the most, GI2.2. Total number of days of contact with extension officer/ lead farmer for [, 1d. Interview date (DD-MM-YYYY), 2. District, 3. EPA, 4. Section, 10. Respondent is, Sex of [RESPONDENT], .... Observation records are missing from d until this file is standardized.
# Adop11- PartGII_Group Membership, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, GII.1. Has any member of your household been involved in any farmer organization, GII.1. Member ID, GII.3. Type of group the household member is/was involved in, GII.3. [OTHER] Type of group the household member is/was involved in, GII.4. What are the two most important group functions...6, GII.4. What are the two most important group functions...7, GII.4. [OTHER] What are the two most important group functions, GII.5. Year household member joined the group, GII.6. Still a member of the group, 1d. Interview date (DD-MM-YYYY), 2. District, .... Observation records are missing from d until this file is standardized.
# Adop12- PartH_Other Income Sources, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (h1_src_income)    by respid, H.1. Source of income since January 2019, H.1. [OTHER] Source of income since January 2019, H.3. Quantity earned from [SOURCE] since January 2019, H.4. Unit of measure, H.4. [OTHER] Unit of measure, H.5. Price/unit, H.6. Total income (MK) earned since January 2019, H.7. Who controls the income earned, 1d. Interview date (DD-MM-YYYY), 2. District, .... Observation records are missing from d until this file is standardized.
# Adop13- PartJI_Production Equipment, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (ji1_agric_equip_owned)    by respid, JI.1. Agricultural equipment owned, JI.1. [OTHER] Agricultural equipment owned, JI.2. Number of [ASSET] owned, JI.3. If you were to buy EACH of the ASSET your household owns from someone else, 1d. Interview date (DD-MM-YYYY), 2. District, 3. EPA, 4. Section, 10. Respondent is, Sex of [RESPONDENT], .... Observation records are missing from d until this file is standardized.
# Adop14- PartJII_Livestock, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (jii1_lvstk)    by respid, JII.1. Livestock owned, JII.1. [OTHER]  Livestock owned, JII.2. Number of [LIVESTOCK] owned, JII.3. If you were to buy EACH of the LIVESTOCK your household owns from someone, 1d. Interview date (DD-MM-YYYY), 2. District, 3. EPA, 4. Section, 10. Respondent is, Sex of [RESPONDENT], .... Observation records are missing from d until this file is standardized.
# Adop15- PartJIII_Household Belongings, Roster - v1.xlsx: not read. sheet Sheet1: Respondent ID, rank of (jiii1_durable_good)    by respid, JIII.1.Durable goods owned, JIII.1. [OTHER] Durable goods owned, JIII.2. Number of [DURABLE_GOOD] owned, JIII.3. If we were to buy ONE of your [DURABLE_GOOD] in its current condition, h, 1d. Interview date (DD-MM-YYYY), 2. District, 3. EPA, 4. Section, 10. Respondent is, Sex of [RESPONDENT], .... Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Dataset for: Adoption and Effects of Orange-Fleshed Sweetpotato in Malawi

International Potato Center (CIP) and partners collaborated to develop orange-fleshed sweetpotato varieties, such as Chipika, Ana-Kwanire, Kadyaubwerere, Mathuthu, Kaphulira and Zondeni . These varieties were formally released in Malawi between 2009 and 2011 and they were heavily promoted by different development organizations in the country. Its success was attributed to its health related attributes, high yield, high seed quality, and good taste. A representative household survey  to Malawian sweetpotato producers was conducted from May to July 2019. The sample size includes 2492 households located in 166 villages in 27 Districts.  Information was gathered on household and farm characteristics, adoption of sweetpotato varieties, potato production, and market participation, among others.
"

	uri <- "doi:10.21223/AIFASC"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Adop02- PartA_Household Composition, Roster - v1.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"), col_types = "text")
	id <- trimws(as.character(r[["Respondent ID"]]))
	rel <- trimws(as.character(r[["A2.4. Relation of [HHM] to HH head"]]))
	sex <- trimws(as.character(r[["A2.3. Sex of [HHM]"]]))
	sex <- c(Male = "male", Female = "female")[sex]
	size_raw <- trimws(as.character(r[["A.1. How many people do live in this household"]]))
	size <- rep(NA_real_, length(size_raw))
	size_ok <- !is.na(size_raw) & grepl("^[0-9]+$", size_raw)
	size[size_ok] <- as.numeric(size_raw[size_ok])
	rawdate <- trimws(as.character(r[["1d. Interview date (DD-MM-YYYY)"]]))
	okd <- grepl("^[0-9]{2}-[0-9]{2}-[0-9]{4}$", rawdate)
	date <- rep(NA_character_, length(rawdate))
	date[okd] <- format(as.Date(rawdate[okd], "%d-%m-%Y"), "%Y-%m-%d")
	ofsp <- trimws(as.character(r[["13. Have you ever grown orange-fleshed sweetpotato (OFSP)?"]]))
	ofsp <- c(Yes = "yes", No = "no")[ofsp]

	ids <- unique(id)
	pick <- integer(length(ids))
	for (i in seq_along(ids)) {
		ii <- which(id == ids[i])
		hh <- ii[rel[ii] == "Household head"]
		pick[i] <- if (length(hh)) hh[1] else ii[1]
	}

	d <- data.frame(
		trial_id = "OFSP_Malawi_2019",
		hhid = ids,
		country = "Malawi",
		adm2 = trimws(as.character(r[["2. District"]][pick])),
		adm3 = trimws(as.character(r[["3. EPA"]][pick])),
		location = trimws(as.character(r[["4. Section"]][pick])),
		date = date[pick],
		crop = "sweetpotato",
		hh_size = size[pick],
		sex = sex[pick],
		ofsp_ever_ = ofsp[pick],
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
