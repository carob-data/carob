# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mama SASHA baseline household identification. Country is Kenya from geographicCoverage and from the abstract (Bungoma and Busia counties).
# District and village are the value labels on dist and vil. Interview dates are a19a, a19b, and a19c. Every year is 2011. Months in the file run from February through October.
# interv is 0 = control and 1 = intervention.

## ISSUES
# Latitude and longitude columns (a16a, a16b, a17a, a17b) and elevation (a18) are empty in every row. a15 records whether GPS was taken during the village census, but the coordinates are not in the file.
# Bungoma North, Bungoma East, Kimilili, and Bunyala are the 2011 district names on the file, not current county names.
# write_files: missing variables: latitude, longitude. unknown variables: intervention_.

## NOT PROCESSED
# Division, location, and sublocation codes. They are place names in the value labels, and they were not assigned to an admin level.
# Enumerator names, phone numbers, woman names, and interview clock times.
# Modules B through P (people, diet, knowledge, crops, assets, and anthropometry). Crop area in Module I is stored with a unit code.
# The intervention list, village maps, fieldwork report, dictionary, and questionnaire.
# Intervention List.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Mama SASHA Baseline Survey - Data Dictionary.lst: documentation or a codebook, not an observation table.
# Module B - Household Members 5 Years and Above.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module C - Household Members Less Than 5 Years.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module D - Dietary Diversity.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module E - Women's Knowledge about Nutrition and Vitamin A.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module F - Attitudes.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module G - Health and Nutrition Practice.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module H - Frequency of Consumption of Vitamin A Rich Foods past 7 Days.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module I - Crop Production.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module J - SP Production -Part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module J - SP Production -Part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module K - SP Knowledge & Practice.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module L - SP Preference & Practice.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module M1 - Household Assets.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module M2 - Radio Use.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module N - Household Asset Characteristics.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# Module P - Anthropemetry & Blood Sampling.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Replication Data for: MAMA SASHA Baseline Survey

The Mama SASHA baseline survey is a key part of a project evaluation strategy aimed at assessing the impact of orange-fleshed sweetpotato (OFSP) on the health and nutritional status of women and children aged between 6 and 23 months and pregnant women. Sub-Saharan Africa is one of the regions with the highest estimated prevalence rates of sub-clinical vitamin A deficiency (VAD) and the risk is greatest among children less than five years of age and pregnant  & lactating women. Introducing OFSP as an agriculture and local community action linked to ante-natal health services (ANC) for pregnant women aims at improving and sustaining health and productivity.  Past studies have shown potential of the OFSP to improve the vitamin A status of children less than 5 years of age. The baseline survey was conducted from March to May 2011 in Bungoma and Busia counties of Western Province of Kenya.  This was a proof-of-concept study in which there were 4 intervention health facility catchment areas and 4 control areas.  A list of qualifying facilities for consideration was constructed and facilities randomly selected and assigned to either the control or intervetnion category.  This baseline survey presents data on; household demographic and socio economic characteristics, women knowledge and attitudes on vitamin A, nutrition, health care and sweetpotatoes, household farming & crop production, vitamin A rich-food consumption among the women and children aged 6-23 months, ANC and maternity utilization, nutritional status of the women and the children aged 6-23 months.
"

	uri <- "doi:10.21223/P3/VSEUG6"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.35
	)

	from_labels <- function(x) {
		v <- attr(x, "labels")
		y <- trimws(as.character(x))
		if (is.null(v)) return(y)
		names(v)[match(y, as.character(unname(v)))]
	}

	f <- ff[basename(ff) == "Module A - Household Identification.dta"]
	r <- haven::read_dta(f)
	arm <- as.integer(r$interv)
	hid <- trimws(as.character(r$hhid))

	d <- data.frame(
		hhid = hid,
		country = "Kenya",
		adm2 = carobiner::fix_name(from_labels(r$dist), "title"),
		location = carobiner::fix_name(from_labels(r$vil), "title"),
		intervention_ = ifelse(arm == 1, TRUE, ifelse(arm == 0, FALSE, NA)),
		date = sprintf("%04d-%02d-%02d", as.integer(r$a19c), as.integer(r$a19b), as.integer(r$a19a)),
		trial_id = hid,
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
