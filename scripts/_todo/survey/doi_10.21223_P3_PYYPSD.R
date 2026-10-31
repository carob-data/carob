# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household table for the participatory mapping of cultivated potato in La Paz, Bolivia. The workbook has two title rows and then a header row. Country in the file is Bolivia. Region is La Paz. The crop is potato.
# AgeHousHead is years. GenHousHead is the sex of the household head. Household size is the sum of the adult and child counts on the family block.
# Community boundaries are shapefiles. They are not a household coordinate.

## ISSUES
# No household latitude or longitude in the workbook.
# Age and the adult and child counts are written as "no data" in a few cells. Those cells are missing.
# missing variables: latitude, longitude
# unknown variables: neighborhood_, n_plots_

## NOT PROCESSED
# Animal counts, farm labour, and off-farm income.
# Shapefiles for Coromata, Icrana, Pairumani, and Pongonhuyo.
# Family_Survey.pdf, MappingPlots_template.pdf, and Protocol_Spanish.pdf.


carob_script <- function(path) {

"
Dataset for: Participatory mapping for long-term monitoring and red listing of cultivated potato populations in La Paz, Bolivia.

Biodiversity sustains a wide variety of genetic traits that are very valuable for the potato's adaption to changing environments and successful cultivation in the future. It is assumed that several factors are threatening biodiversity in the Andes. Without robust baselines and indicators used for reliable time lines, it is impossible to assess diversity loss or increase. Long-term studies in situ are necessary to distinguish short term fluctuation from general tendencies caused by environmental and socio-cultural shifts and to understand ongoing evolutionary principles within the potato genepool. Powerful tools are needed that facilitate the management, evolution and exchange of mainly oral transmitted traditional knowledge. Participatory mapping is such a tool where cognitive spatial knowledge is transformed into maps at grassroots level by trained community members. Local knowledge can be documented in context with social, political, biotic and abiotic factors and if accompanied with comprehensive capacity building and the application of robust indicators will strengthen the cross-generation knowledge exchange and sensitization towards land use shifts, resource dynamics affected by climate and societal related factors and pressure.

Most local farmers practice traditional small scale agriculture with focus on potato production, partly organized in sectoral fellow systems. Because of the high biological diversity, coexistence of wild potato species and the extensive local knowledge owned by the potato farmers, communities of this study site contribute to generate knowledge on dynamics of potato diversity in situ and complement the diversity hotspot network 'Chirapaq Ñan' for long-term in situ monitoring of cultivated potato. This dataset contributes to i) elaborate robust baseline to assess spatial and temporal dynamics and distribution ranges of potato diversity within and among fields, families and altitudinal belts, ii) elaborate on the application of utilized diversity indexes for red listing of potato landraces and cultivar types. The dataset contains parcel and community maps, geographic distribution of cultivated potato populations, red listing of potato landraces and cultivar types and supplementary files (surveys, protocols) used for this study.
"

	uri <- "doi:10.21223/P3_PYYPSD"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Household_Dataset_LaPaz.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	country <- trimws(as.character(r[[1]]))
	keep <- !is.na(country) & country == "Bolivia"
	r <- r[keep, ]
	country <- country[keep]
	sex <- tolower(trimws(as.character(r[[7]])))
	sex[!sex %in% c("female", "male")] <- NA_character_
	num <- function(x) {
		x <- trimws(as.character(x))
		out <- rep(NA_real_, length(x))
		ok <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
		out[ok] <- as.numeric(x[ok])
		out
	}
	adults <- num(r[[9]])
	children <- num(r[[10]])
	code <- trimws(as.character(r[[3]]))

	d <- data.frame(
		trial_id = "potato_mapping_LaPaz",
		hhid = code,
		country = country,
		adm1 = trimws(as.character(r[[2]])),
		location = trimws(as.character(r[[4]])),
		neighborhood_ = trimws(as.character(r[[5]])),
		crop = "potato",
		age = num(r[[6]]),
		sex = sex,
		hh_size = adults + children,
		education = trimws(as.character(r[[8]])),
		n_plots_ = num(r[[13]]),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	ok <- !is.na(d$hhid) & nzchar(d$hhid)
	d <- d[ok, ]
	if (any(duplicated(d$hhid))) {
		d$hhid <- make.unique(d$hhid, sep = "_")
	}

	carobiner::write_files(path, meta, d)
}
