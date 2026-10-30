# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household rows in Household_Dataset_Huancavelica.xlsx. Two title rows sit above the column names (row 3).
# Country and region are in the sheet. Code is the household code.
# AgeHousHead is the household head. GenHousHead Male/Female is stored as male/female.
# Household size is the sum of adults and children in the sheet.
# A cell value of "No data" is missing.

## ISSUES
# No coordinates.
# A recorded value "No data" is stored as missing.
# write_files: missing variables: carob_contributor; geo_from_source, latitude, longitude.

## NOT PROCESSED
# RotationRatio_Huancavelica.xlsx and the shapefile.
# Livestock counts and labor columns on the household sheet.

carob_script <- function(path) {

"
Dataset for: Participatory mapping for long-term monitoring and red listing of cultivated potato populations in Huancavelica, Peru

Biodiversity sustains a wide variety of genetic traits that are very valuable for the potato's adaption to changing environments and successful cultivation in the future. It is assumed that several factors are threatening biodiversity in the Andes. Without robust baselines and indicators used for reliable time lines, it is impossible to assess diversity loss or increase. Long-term studies in situ are necessary to distinguish short term fluctuation from general tendencies caused by environmental and socio-cultural shifts and to understand ongoing evolutionary principles within the potato genepool. Powerful tools are needed that facilitate the management, evolution and exchange of mainly oral transmitted traditional knowledge. Participatory mapping is such a tool where cognitive spatial knowledge is transformed into maps at grassroots level by trained community members. Local knowledge can be documented in context with social, political, biotic and abiotic factors and if accompanied with comprehensive capacity building and the application of robust indicators will strengthen the cross-generation knowledge exchange and sensitization towards land use shifts, resource dynamics affected by climate and societal related factors and pressure.  Most local farmers practice traditional small scale agriculture with focus on potato production, partly organized in sectoral fellow systems. Because of the high biological diversity, coexistence of wild potato species and the extensive local knowledge owned by the potato farmers, communities of this study site contribute to generate knowledge on dynamics of potato diversity in situ and complement the diversity hotspot network 'Chirapaq Ñan' for long-term in situ monitoring of cultivated potato. This dataset contributes to i) elaborate robust baseline to assess spatial and temporal dynamics and distribution ranges of potato diversity within and among fields, families and altitudinal belts, ii) elaborate on the application of utilized diversity indexes for red listing of potato landraces and cultivar types. The dataset contains parcel and community maps, geographic distribution of cultivated potato populations, red listing of potato landraces and cultivar types and supplementary files (surveys, protocols) used for this study.
"

	uri <- "doi:10.21223/P3/CVRYSO"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "hh_size",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Household_Dataset_Huancavelica.xlsx"]
	r <- carobiner::read.excel(f, skip=2)
	num <- function(x) {
		y <- trimws(as.character(x))
		y[!nzchar(y) | y %in% c("NA", ".", "No data")] <- NA_character_
		as.numeric(y)
	}
	sex <- tolower(trimws(as.character(r$GenHousHead)))
	d <- data.frame(
		hhid = trimws(as.character(r$Code)),
		country = trimws(as.character(r$Country)),
		adm1 = trimws(as.character(r$Region)),
		location = trimws(as.character(r$Community)),
		age = num(r$AgeHousHead),
		sex = sex,
		hh_size = num(r[["# Adult_Hous"]]) + num(r[["# Chil_Hous"]]),
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA,
		trial_id = "Huancavelica_households"
	)
	carobiner::write_files(path, meta, d)
}
