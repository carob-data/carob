# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey of potato producers in Yunnan, China, July-September 2015. The file has 616 household codes. The abstract says 615.
# Country is China and adm1 is Yunnan from the title and geographic coverage. The village is a14_English. Two village names are blank.
# Occupation is the text in the file. Household size is family (number of household members). Farm size and elevation are the hectare and meter columns in the dictionary.

## ISSUES
# No latitude or longitude. Prefecture and County are numeric codes. The dictionary calls them names and does not list which code is which prefecture or county.
# The abstract says 615 households and the file has 616.
# missing variables: latitude, longitude

## NOT PROCESSED
# C88 adoption flags, trait scores, seed and harvest amounts, and the village-level potato area columns in Data_Adoption and Diffusion of C88.dta.
# Education is stored as codes 1, 2, and 3. The dictionary does not define those codes.
# Dictionary_Adoption and Diffusion of C88.xlsx.


carob_script <- function(path) {

"
Dataset for: Adoption and Diffusion of C88 Potato Variety in Yunnan, China

International Potato Center (CIP) and Yunnan Normal University collaborated to develop a late blight resistant potato variety, Cooperation-88 (C88). A household survey representative of Yunnan potato producers was conducted from July to September 2015. The sample size includes 615 households located in 41 villages. Information was gathered on household and farm characteristics, adoption of potato varieties, potato production, and market participation, among others.
"

	uri <- "doi:10.21223/P3/IYDCGU"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "clustered random sample",
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 15,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Data_Adoption and Diffusion of C88.dta"]
	r <- haven::read_dta(f)
	loc <- as.character(r$a14_English)
	loc[is.na(loc)] <- ""
	loc <- carobiner::fix_name(loc, "title")
	occ <- as.character(r$occupation)
	occ[is.na(occ)] <- ""
	occ <- trimws(occ)
	occ[!nzchar(occ)] <- NA_character_

	d <- data.frame(
		trial_id = "C88_Yunnan_2015",
		hhid = trimws(as.character(r$scode)),
		country = "China",
		adm1 = "Yunnan",
		location = loc,
		crop = "potato",
		age = as.numeric(r$age),
		hh_size = as.numeric(r$family),
		occupation = occ,
		farmland = as.numeric(r$totalarea_ha),
		elevation = as.numeric(r$elevation),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
