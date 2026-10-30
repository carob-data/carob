# R script for "carob"
# license: GPL (>=3)

## NOTES
# Caregiver survey in Homa Bay County, Kenya. County and country are named in the description. The subcounty is scounty_name.
# study_grp is the randomized arm. quantity, units, and price are OFSP sales, not field yield. Names of children, caregivers, and enumerators were not copied.

## ISSUES
# No coordinates in the caregiver file. Villages need georeferencing.
# No kg/ha yield and no fertilizer rate.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude. unknown variables: center_, subcounty_, study_group_, grew_ofsp_, sale_quantity_, sale_units_, sale_price_.

## NOT PROCESSED
# Coordonne_es_GPS_Adamaoua csv: columns X and Y are decimal degrees, and the file has no country column. The dataset description does not name that survey, so the points were not given a country.
# The three Adamaoua SPSS files.
# The remaining caregiver-questionnaire items in the Homa Bay .dta.

## NEW VARIABLES
# study_group_, subcounty_, center_.
# grew_ofsp_, sale_quantity_, sale_units_, sale_price_.


carob_script <- function(path) {

"
Dataset for: Homa Bay preschooler caregiver Survey-data

Background: Persistent prevalence of high malnutrition in poor households in developing countries calls for enhancement of cost-effective nutrition interventions among the vulnerable groups. One responsive way is to promote regular consumption of home-grown biofortified foods, particularly in the micronutrient-deficient groups. Previous nutrition interventions have targeted adults with behavior change education, but have rarely explored the potential of nutrition education of preschoolers as change agents. Objectives: This study sought to assess the effect of nutrition education targeting preschool children and their caregivers on their consumption of vitamin A–biofortified orange-fleshed sweetpotato (OFSP) in rural farm households in Homa Bay County, Kenya. Methods: A total of 431 preschooler-caregiver pairs from 15 village-level clusters were recruited into a randomized controlled trial. The sample was randomized into 1 control (3 villages) and 3 treatment groups (4 villages each). Treatments involved channeling nutrition education to preschoolers through their learning materials (preschooler treatment); the caregivers through their mobile phones (caregiver treatment); and to both preschoolers and their caregivers simultaneously (integrated treatment). Baseline and follow-up household-level surveys were conducted with the caregivers, and consumption data were collected from the preschoolers using a child dietary diversity register. Class teachers sought 24-h consumption recalls of the preschoolers for 19 consecutive schooldays. Results: The results of a zero-inflated Poisson regression showed that the phone-mediated and multichanneled nutrition education approaches significantly increased the number of days of OFSP consumption. The integrated nutrition education approach significantly increased the preschoolers' likelihood to consume OFSP, number of OFSP consumption days, and likelihood to consume it more than once per week by 11%, 77%, and 20%, respectively. Conclusions: Nutrition education through OFSP-branded preschoolers' learning materials and phone-mediated messages provides effective nudges to the caregivers to feed their preschoolers regularly with OFSP. This could have implications for realizing sustainable nutrition programs in biofortified crop-growing areas.
"

	uri <- "doi:10.21223/KJY3XI"
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
		carob_completion = 20,
		carob_effort = 0.4
	)

	chr <- function(x) {
		x <- trimws(as.character(haven::zap_labels(x)))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	num <- function(x) {
		x <- trimws(as.character(haven::zap_labels(x)))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	f <- ff[grepl("^Homa_Bay", basename(ff))]
	r <- haven::read_dta(f)
	d <- data.frame(
		hhid = chr(r$study_no),
		center_ = chr(r$center_name),
		subcounty_ = chr(r$scounty_name),
		study_group_ = chr(r$study_grp),
		grew_ofsp_ = chr(r$grew_ofsp),
		sale_quantity_ = num(r$quantity),
		sale_units_ = chr(r$units),
		sale_price_ = num(r$price),
		stringsAsFactors = FALSE
	)
	miss <- is.na(d$hhid) | !nzchar(d$hhid)
	d$hhid[miss] <- paste0("row", seq_len(nrow(d))[miss])
	d$crop <- "sweetpotato"
	d$country <- "Kenya"
	d$adm1 <- "Homa Bay"
	d$trial_id <- paste0("HomaBay_", d$hhid, "_", seq_len(nrow(d)))
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
