# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mama SASHA cohort in Western Kenya. Country Kenya is the Dataverse geographic coverage and the description.
# The location file names the facility, district, location, sublocation, and village. Personal names were not copied.
# INTVGPNAME is the intervention or comparison arm. This file does not contain the retinol-binding protein results.

## ISSUES
# No coordinates. Villages need georeferencing.
# No yield.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude. unknown variables: facility_, intervention_, sublocation_.

## NOT PROCESSED
# Cover-page and long and short report workbooks, questionnaires, the SAS file, and visit dictionaries. They were not used as the household location table.
# COHORT STUDY VISIT 1 DATA DICTIONARY.txt: documentation or a codebook, not an observation table.
# COHORT STUDY VISIT 2 DATA DICTIONARY.txt: documentation or a codebook, not an observation table.
# COHORT STUDY VISIT 3 DATA DICTIONARY.txt: documentation or a codebook, not an observation table.
# COHORT STUDY VISIT 4 DATA DICTIONARY.txt: documentation or a codebook, not an observation table.
# COVA_FC_COVERPAGE_CHILD_MR_RR_22FEB14_FINAl.xlsx: not read. sheet Data (of 2): DI_ROUND, FACILITY, HHID, MATERNAL_ID, MR2, CFNAME, CR02, CR03, CR04, VITA_MNTHS, LATEST1, LATEST2, .... Observation records are missing from d until this file is standardized.
# COVA_FC_COVERPAGE_MOTHER_MR_RR_22FEB14_FINAL.xlsx: not read. sheet Data (of 2): DI_ROUND, FAC_ID2, HHID, MATERNAL_ID, MFNAME, MR2, MR3, MR4, VITA_MNTHS, LATEST1, LATEST2, LATEST3, .... Observation records are missing from d until this file is standardized.
# COVA_FC_LONGREPORT_MR_RR_VERSION1_22FEB14_FINAL.xlsx: not read. sheet DATA (of 2): DROUND, FAC_ID2, HHID, PID, PNAME, SEX, AGEY, AGEM, PERIOD, PERIOD_DESCR, FOODTYPE, FOODTYPE_DESCR, .... Observation records are missing from d until this file is standardized.
# COVA_FC_LONGREPORT_MR_RR_VERSION2_22FEB14_FINAL.xlsx: not read. sheet Data (of 2): DROUND, SN, FAC_ID2, HHID, PID, FOODTYPE, PERIOD, RECNUM, RECIPE_CODE, RECIPE_DESCR, GCODE, GDESCR, .... Observation records are missing from d until this file is standardized.
# COVA_FC_SHORTREPORT_MR_RR_22FEB14_FINAL.xlsx: not read. sheet Data (of 2): DROUND, FAC_ID1, FAC_NAME, INTVGP, INTVGPNAME, FAC_ID2, HHID, PID, PNAME, SEX, AGEY, AGEM, .... Observation records are missing from d until this file is standardized.
# fc_merge_ea.sas7bdat: not read. .sas7bdat file. Observation records are missing from d until this file is standardized.

## NEW VARIABLES
# facility_, intervention_, sublocation_.


carob_script <- function(path) {

"
Replication Data for: Promotion of Orange-Fleshed Sweet Potato Increased Vitamin A Intakes and Reduced the Odds of Low Retinol-Binding Protein among Postpartum Kenyan Women

Background:Orange-fleshed sweet potato (OFSP) improves vitamin A (VA) status of young children; research with pregnant and lactating women is limited.Objective:We examined the effectiveness of the Mama SASHA (Sweetpotato Action for Security and Health in Africa) program to improve nutrition knowledge, diets, and nutritional status of pregnant and lactating women (PLW) in Western Kenya. Methods: Eight health facilities were allocated to the Mama SASHA intervention or comparison arms. PLW in intervention facilities received enhanced nutrition counseling at health clinics, were linked with community-based maternal support groups, and received vouchers for OFSP vine cuttings. Control PLW received clinic-based nutrition counseling only. A totalof 505 women in early and midpregnancy, attending their first antenatal care visit, and with no previous engagement in project activities were enrolled from the 8 facilities. Nutrition and health-seeking knowledge, food security, dietary patterns, and anthropometric measurements were collected at 4 time points at 9 mo postpartum. VA intakes were assessed with multipass 24-h recalls in a subsample of 206 mothers at 8–10 mo postpartum. VA status was assessed by using serum retinol-binding protein (RBP). Impacts were estimated with multilevel mixed models adjusted for clustering and differences at enrollment. Results: At enrollment, 22.9% of women had RBP <1.17mmol/L. By 9 mo postpartum, intervention women had significantly higher intakes of VA [adjusted difference = 297.0 retinol activity equivalent (RAE) units; 95% CI: 82, 513 RAE units;P= 0.01;n= 206], greater consumption of VA-rich fruit and vegetables in the previous 7 d (difference-in-difference estimate: 0.40 d; 95%CI: 0.23, 0.56 d;P< 0.01), and a 45% reduction in the odds of RBP <1.17mmol/L (OR: 0.55; 95% CI: 0.33, 0.92;P= 0.01). Conclusion: Promotion of OFSP to PLW through health services is a feasible strategy to improve women's nutrition knowledge, VA intakes, and maternal RBP. 

Promotion of Orange-Fleshed Sweet Potato Increased Vitamin A Intakes and Reduced the Odds of Low Retinol-Binding Protein among Postpartum Kenyan Women. Available from: https://www.researchgate.net/publication/316111399_Promotion_of_Orange-Fleshed_Sweet_Potato_Increased_Vitamin_A_Intakes_and_Reduced_the_Odds_of_Low_Retinol-Binding_Protein_among_Postpartum_Kenyan_Women [accessed Jul 23 2018].
"

	uri <- "doi:10.21223/P3/XAW0XY"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.3
	)

	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	f <- ff[basename(ff) == "COVA_FC_SURVEY LOCATION FILE_22FEB14_FINAL.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")
	d <- data.frame(
		hhid = chr(r$HHID),
		facility_ = chr(r$FAC_NAME),
		intervention_ = chr(r$INTVGPNAME),
		adm2 = tools::toTitleCase(tolower(chr(r$DISTNAME))),
		location = tools::toTitleCase(tolower(chr(r$VILLNAME))),
		sublocation_ = chr(r$SLOCNAME)
	)
	d <- d[!is.na(d$hhid) | !is.na(d$location), ]
	d$crop <- "sweetpotato"
	d$country <- "Kenya"
	d$trial_id <- paste0("MamaSASHA_", seq_len(nrow(d)))
	d$on_farm <- NA
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
