# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are sheet Data in 12582_Data.xlsx.
# TTYA is the only tuber-yield column. The dictionary names it TbYldAj_Cp_tha, scale t/ha (adjusted). Stored as kg/ha.
# AUDPC in this dictionary is a numerical scale with upper limit 6000, not the fraction scale used by the vocabulary (0-150). Values are not rescaled.
# rAUDPC is the relative AUDPC (dictionary scale 0-1).
# RLB is the late-blight resistance class (text).
# Country Kenya is the dataset geographic coverage. The data sheet has no coordinates.

## ISSUES
# Njabini, Limuru, and the University of Nairobi need georeferencing.
# No planting or harvest date and no fertilizer.
# AUDPC follows the dictionary scale (upper limit 6000). Vocabulary maximum is 150.
# write_files: missing variables: carob_contributor; yield_moisture, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: out of bounds: AUDPC (0, 3030).

## NOT PROCESSED
# Material_List Population and Accession_Code. Female and male parents are mapped when the institutional number matches.
# CLASS: not defined as a treatment in the file.

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
		response_vars = "yield;rAUDPC",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "12582_Data.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")
	ml <- carobiner::read.excel(f, sheet="Material_List")
	m <- match(trimws(as.character(r$INSTN)), trimws(as.character(ml$INSTN)))
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
		AUDPC = as.numeric(r$AUDPC),
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
		irrigated = NA
	)
	d$female_parent <- NA_character_
	d$male_parent <- NA_character_
	ok <- !is.na(m)
	d$female_parent[ok] <- trimws(as.character(ml$FEMALE[m[ok]]))
	d$male_parent[ok] <- trimws(as.character(ml$MALE[m[ok]]))
	d$female_parent[!nzchar(d$female_parent)] <- NA_character_
	d$male_parent[!nzchar(d$male_parent)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
