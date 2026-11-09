# R script for "carob"
# license: GPL (>=3)

## NOTES
# VISTA baseline, Nampula and Zambezia, Mozambique, May-June 2015. Country Mozambique is the title. Province, district, administrative post, and locality are on the household identification file. Labelled SPSS codes are converted with their value labels.
# Sweetpotato field counts are on the sweetpotato production file: upland and lowland fields planted last season. Those counts are not an area and not a yield. The one published bounding-box corner is not assigned to every household.
# Names of respondents and enumerators are not copied.

## ISSUES
# No coordinates and no yield. Fertilizer quantities are on another file and the unit is not applied here.
# write_files: missing carob_contributor, latitude, longitude. unknown variables: upland_fields_, lowland_fields_.

## NEW VARIABLES
# upland_fields_: number of sweetpotato fields on uplands (G02A).
# lowland_fields_: number of sweetpotato fields on lowlands (G03A).

## NOT PROCESSED
# Dietary diversity, seed-system modules, marketing, vitamin A knowledge, and the area and fertilizer file. Fertilizer unit codes are not converted to kg/ha.
# Household member and child modules. Names are not copied.
# BaseLineSurvey_VISTA Baseline Survey Dictionary Nampula_Mozambique_2015: documentation or a codebook, not an observation table.
# Lista of FILES contained in the folder_DATASET Mozambique_VISTA BASELINE SURVEY 2015.xlsx: not read. sheet Sheet1: 1. DATA. Observation records are missing from d until this file is standardized.
# PAG16_Baseline Survey VISTA_2015_Practices and Preferences.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAG2B_Baseline Survey VISTA_2015_Household members with age above 60 Months.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 11_Baseline Survey VISTA_2015_Seed System_General.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 11_Baseline Survey VISTA_2015_Seed System_Vine Dissemination.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 13_Baseline Survey VISTA_2015_Attitude, Knowledge, practices.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 17_Baseline Survey VISTA_2015_Women Knowledge on Vitamina A.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 19_Baseline Survey VISTA_2015_Dietary Diversity for Women and Children.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 2_Bseline Survey VISTA_2015_Demographic of Members of Household who are less or equal to 60 months of Age (5 years).sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 3_Baseline Survey VISTA_2015_Affiliation to ssociation or group.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 4_Baseline Survey VISTA_2015 Crop production.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 5_Baseline Survey VISTA_2015_Crop production_Area and use of fertilizer.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# PAGE 9_Baseline Survey VISTA_2015_Sweetpotato marketing and selling.sav: not read. SPSS .sav table. Observation records are missing from d until this file is standardized.
# Questionnaires for VISTA Modules of the  Baseline Survey in Portuguese.xls: design, layout, or protocol sheet, not plot or survey records.


carob_script <- function(path) {

"
Dataset for: The Viable Sweetpotato Technologies in Africa (VISTA)-Mozambique: The Baseline Survey  for Nampula and Zambezia Provinces

The baseline survey for Viable Sweetpotato Technologies in Africa (VISTA) project in Mozambique was conducted in May-June 2015 in four districts of Nampula province (Monapo, Meconta, Nampula-Rapale and Murrupula) and two of Zambezia province (Alto Molocue and Gurue) in Mozambique.  About 640 households were interviewed. Most of the visited households had at least one child less than 5 years old, and 54% of them were female. The average age of the household heads was 39 years, and most of them with fourth grade education level. Overall, about 10% of the household heads were women. Other relevant items included were related to farmer organizations, crop production, Seed system and related activities, farmers' practices, attitude and perception on sweetpotato, Vitamin A and other nutrition knowledge and household and individually dietary diversity.
"

	uri <- "doi:10.21223/P3/NN00OC"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=7,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "treatment",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.35
	)

	lab <- function(x) {
		z <- trimws(as.character(haven::as_factor(x)))
		z[!nzchar(z) | z %in% c("NA", "NaN")] <- NA_character_
		z
	}
	num <- function(x) {
		z <- trimws(as.character(x))
		ok <- is.na(z) | z == "NA" | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}
	f1 <- ff[basename(ff) == "PAGE 1_Baseline Survey VISTA_2015_Household identification and statement of agreement.sav"]
	r1 <- haven::read_sav(f1)
	f2 <- ff[basename(ff) == "PAGE 6_Baseline Survey VISTA_2015_Sweetpotato production.sav"]
	r2 <- haven::read_sav(f2)
	d <- data.frame(
		hhid = lab(r1$T_RG),
		treatment = lab(r1$IN_CT),
		adm1 = lab(r1$A01),
		adm2 = lab(r1$A02),
		adm3 = lab(r1$A03),
		location = lab(r1$A04)
	)
	q <- data.frame(
		hhid = lab(r2$T_RG),
		upland_fields_ = num(r2$G02A),
		lowland_fields_ = num(r2$G03A)
	)
	d <- merge(d, q, by="hhid", all.x=TRUE)
	d$country <- "Mozambique"
	d$crop <- "sweetpotato"
	d$geo_from_source <- FALSE
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- "Mozambique_VISTA_2015"
	d <- d[!is.na(d$hhid), ]
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
