# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are the Fieldbook of 01_Field_book_CRW_late_blight_resistant.xlsx.
# Variety names for the checks come from 03_Check-data. Other entries keep the CIP number.
# Parents are 02_Pedigree, matched on accession number.
# The field book stores missing AUDPC as the text NA.
# Crop is potato: the checks are named potato varieties and the trait is late blight.

## ISSUES
# No country, coordinates, planting date, harvest date, fertilizer, or yield.
# AUDPC in this field book exceeds the vocabulary maximum of 150. Values are not rescaled. Missing AUDPC is the text NA.
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, country, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: out of bounds: AUDPC (0, 3680).

## NOT PROCESSED
# rowgroup, colgroup, row, and col are field-layout positions. plot_id is built from them. They are not a stated replicate factor.
# Pedigree species abbreviations (mcd, phu, chc, and others).

carob_script <- function(path) {

"
Dataset for: New CWR LB- resistant sources

New CWR LB- resistant sources
"

	uri <- "doi:10.21223/CDSVCN"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "AUDPC",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.35
	)

	fb <- ff[basename(ff) == "01_Field_book_CRW_late_blight_resistant.xlsx"]
	ped <- ff[basename(ff) == "02_Pedigree_CRW_late_blight_resistant.xlsx"]
	ckf <- ff[basename(ff) == "03_Check-data_CRW_late_blight_resistant.xlsx"]
	r <- carobiner::read.excel(fb, sheet="Fieldbook")
	p <- carobiner::read.excel(ped, sheet="Pedigree")
	ck <- carobiner::read.excel(ckf, sheet="Check Info")

	code <- trimws(as.character(r$CIPN))
	mck <- match(code, trimws(as.character(ck[["CIP Number"]])))
	variety <- code
	ok <- !is.na(mck)
	variety[ok] <- trimws(as.character(ck[["Variety name"]][mck[ok]]))
	mp <- match(code, trimws(as.character(p$Accession_Number)))

	d <- data.frame(
		crop = "potato",
		variety_code = code,
		variety = variety,
		treatment = variety,
		variety_type = trimws(as.character(r$type)),
		plot_id = paste(r$rowgroup, r$row, r$col, sep="-"),
		AUDPC = {
			a <- trimws(as.character(r$AUDPC))
			a[a %in% c("NA", "")] <- NA_character_
			as.numeric(a)
		},
		disease = "potato late blight",
		pathogen = "Phytophthora infestans",
		stress = "disease",
		trial_id = "CWR_LB",
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA,
		stringsAsFactors = FALSE
	)
	d$female_parent <- NA_character_
	d$male_parent <- NA_character_
	okp <- !is.na(mp)
	d$female_parent[okp] <- trimws(as.character(p$Female_AcceNumb[mp[okp]]))
	d$male_parent[okp] <- trimws(as.character(p$Male_AcceNumb[mp[okp]]))

	carobiner::write_files(path, meta, d)
}
