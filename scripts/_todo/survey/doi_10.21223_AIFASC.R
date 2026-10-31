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

	f <- ff[grepl("^Adop02", basename(ff))]
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
