# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are the Fieldbook of 01_Field_book_CRW_late_blight_resistant.xlsx.
# Variety names for the checks come from 03_Check-data. Other entries keep the CIP number.
# Parents are 02_Pedigree, matched on accession number.
# AUDPC in percent-days (0-3680) is divided by 100 to match the terminag fraction-days standard (0-150).
# Crop is potato: the checks are named potato varieties and the trait is late blight.

## ISSUES
# No country, coordinates, planting date, harvest date, fertilizer, or yield.

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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 50,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "01_Field_book_CRW_late_blight_resistant.xlsx"]
	f2 <- ff[basename(ff) == "02_Pedigree_CRW_late_blight_resistant.xlsx"]
	f3 <- ff[basename(ff) == "03_Check-data_CRW_late_blight_resistant.xlsx"]
	r <- carobiner::read.excel(f1, sheet="Fieldbook")
	r1 <- carobiner::read.excel(f2, sheet="Pedigree")
	r2 <- carobiner::read.excel(f3, sheet="Check Info")

	code <- trimws(as.character(r$CIPN))
	mck <- match(code, trimws(as.character(r2[["CIP Number"]])))
	variety <- code
	ok <- !is.na(mck)
	variety[ok] <- trimws(as.character(r2[["Variety name"]][mck[ok]]))
	mp <- match(code, trimws(as.character(r1$Accession_Number)))

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
			as.numeric(a) / 100
		},
		disease = "potato late blight",
		pathogen = "Phytophthora infestans",
		stress = "disease",
		trial_id = "CWR_LB",
		on_farm = NA,
		is_survey = FALSE,
		geo_from_source = FALSE,
		irrigated = NA
	)
	d$female_parent <- NA_character_
	d$male_parent <- NA_character_
	okp <- !is.na(mp)
	d$female_parent[okp] <- trimws(as.character(r1$Female_AcceNumb[mp[okp]]))
	d$male_parent[okp] <- trimws(as.character(r1$Male_AcceNumb[mp[okp]]))

	carobiner::write_files(path, meta, d)
}
