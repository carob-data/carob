# R script for "carob"
# license: GPL (>=3)

## NOTES
# Potato seed-system household survey in Cotopaxi, Ecuador, September-October 2018. Stratified by canton. 260 rows.
# OBS is the observation number in Dictionary.xls. The canton is SECTION_META.sublocation (Latacunga, Saquisili, Salcedo, Pujili). Cotopaxi is the province named in the title and in that dictionary entry.
# Sex is F = female and M = male. The respondent-is-head item is Y/N. Education labels are in the dictionary. n/a is missing.

## ISSUES
# No latitude or longitude. Land area is not mapped because the unit column mixes hectare, solar (1764 m2), cuadra (7056 m2), and other.
# missing variables: latitude, longitude

## NOT PROCESSED
# Crop flags, potato variety and seed-source repeats, livestock, wild foods, food security, and poverty items in Survey_raw.xls. Those crop flags are not reshaped.
# Dictionary.xls is the codebook used for the columns above.


carob_script <- function(path) {

"
Dataset for: Characterizing local potato seed systems in the province of Cotopaxi-Ecuador

This survey was implemented using a stratified sampling design where each canton (Latacunga, Pujili, Salcedo and Saquisili) was considered as a different stratum. From September to October 2018, 118 farmers were surveyed in Latacunga, 67 in Pujili, 56 in Salcedo, and 19 in Saquisili.
"

	uri <- "doi:10.21223/P3/GONFFA"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "stratified sampling",
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 15,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Survey_raw.xls"]
	r <- carobiner::read.excel(f, na = c("", "NA"))
	sex <- trimws(as.character(r[["survey_grp.SECTION_HOUSEHOLD_INFO.RespondentDetails.respondentsex"]]))
	sex <- c("M" = "male", "F" = "female")[sex]
	hd <- trimws(as.character(r[["survey_grp.SECTION_HOUSEHOLD_INFO.RespondentDetails.respondent_is_head"]]))
	hd <- c("Y" = TRUE, "N" = FALSE)[hd]
	edu <- trimws(as.character(r[["survey_grp.SECTION_HOUSEHOLD_INFO.HouseholdHeadDetails.education_level"]]))
	edu <- c(
		"primary" = "primary",
		"No_school" = "no school",
		"secondary" = "secondary",
		"adult_education" = "adult education",
		"postsecondary" = "post-secondary",
		"n/a" = NA_character_
	)[edu]
	canton <- as.character(r[["SECTION_META.sublocation"]])
	canton[is.na(canton)] <- ""

	d <- data.frame(
		trial_id = "Cotopaxi_potato_seed_2018",
		hhid = trimws(as.character(r$OBS)),
		country = "Ecuador",
		adm1 = "Cotopaxi",
		adm2 = carobiner::fix_name(canton, "title"),
		location = carobiner::fix_name(canton, "title"),
		crop = "potato",
		sex = unname(sex),
		is_head = unname(hd),
		education = unname(edu),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
