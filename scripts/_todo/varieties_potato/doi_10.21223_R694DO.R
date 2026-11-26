# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are sheet Data in 12582_Data.xlsx.
# TTYA is the only tuber-yield column (t/ha adjusted -> kg/ha).
# AUDPC in percent-days (0-3030) is divided by 100 to match the terminag fraction-days standard (0-150).
# rAUDPC is the relative AUDPC (0-1). RLB is the late-blight resistance class.
# Kenya site coordinates (Njabini, Limuru, University of Nairobi) added from gazetteer (geo_from_source = FALSE).

## ISSUES
# No planting or harvest date, fertilizer rates, irrigation, or yield moisture reported.

## NOT PROCESSED
# Material_List Population and Accession_Code. Female and male parents are mapped when the institutional number matches.
# CLASS: not defined as a treatment in the file.
# 12582_DataDictionary_ElementDescription.xlsx: documentation or a codebook, not an observation table.
# 17582_DataDictionary_UniqueIdentifier.xls: documentation or a codebook, not an observation table.

carob_script <- function(path) {

"
Dataset for: Integrated management of late blight data

The integrated management of late blight was assessed in 3 sites: Njabini, Limuru and University of Nairobi in Kenya. The experiment consisted of 3 repetitions, 8 treatments and 1 control with 9 varieties, where the yield and late blight resistance were calculated
"

	uri <- "doi:10.21223/R694DO"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;variety",
		response_vars = "yield;AUDPC;rAUDPC",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "12582_Data.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")
	r1 <- carobiner::read.excel(f, sheet="Material_List")
	m <- match(trimws(as.character(r$INSTN)), trimws(as.character(r1$INSTN)))
	variety <- trimws(as.character(r$ACCENAME))
	blank <- is.na(variety) | !nzchar(variety)
	variety[blank] <- trimws(as.character(r$INSTN))[blank]

	d <- data.frame(
		crop = "potato",
		plot_id = as.character(r$PLOT),
		rep = as.integer(r$REP),
		treatment = trimws(as.character(r$TREATMENT)),
		variety_code = trimws(as.character(r$INSTN)),
		variety = variety,
		yield = as.numeric(r$TTYA) * 1000,
		yield_part = "tubers",
		yield_isfresh = TRUE,
		AUDPC = as.numeric(r$AUDPC) / 100,
		rAUDPC = as.numeric(r$rAUDPC),
		disease = "potato late blight",
		pathogen = "Phytophthora infestans",
		disease_severity = trimws(as.character(r$RLB)),
		stress = "disease",
		location = trimws(as.character(r$SITE)),
		country = "Kenya",
		trial_id = trimws(as.character(r$SITE)),
		on_farm = NA,
		is_survey = FALSE,
		geo_from_source = FALSE,
		irrigated = NA
	)
	d$latitude <- ifelse(grepl("Njabini", d$location, ignore.case=TRUE), -0.717,
		ifelse(grepl("Limuru", d$location, ignore.case=TRUE), -1.107,
		ifelse(grepl("Nairobi|UON|Kabete", d$location, ignore.case=TRUE), -1.253, NA_real_)))
	d$longitude <- ifelse(grepl("Njabini", d$location, ignore.case=TRUE), 36.667,
		ifelse(grepl("Limuru", d$location, ignore.case=TRUE), 36.643,
		ifelse(grepl("Nairobi|UON|Kabete", d$location, ignore.case=TRUE), 36.743, NA_real_)))
	d$female_parent <- NA_character_
	d$male_parent <- NA_character_
	ok <- !is.na(m)
	d$female_parent[ok] <- trimws(as.character(r1$FEMALE[m[ok]]))
	d$male_parent[ok] <- trimws(as.character(r1$MALE[m[ok]]))
	d$female_parent[!nzchar(d$female_parent)] <- NA_character_
	d$male_parent[!nzchar(d$male_parent)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
