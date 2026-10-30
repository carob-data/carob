# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey for participatory mapping of cultivated potato in Junin, Peru. The sheet records Country and Region. Community is the location.
# The first two rows are grouped headers. Names come from the third row. Potato plot counts are Lar_Cam, Sma_Cam, and Total Plots. There is no yield.
# Age, gender, and education of the household head, and the child count, are not copied.

## ISSUES
# No coordinates. Community boundaries are shapefiles, not plot positions, so no centroid is used.
# The variety-by-area workbook is a community total (variety and area in m2) with no household id, so it is not joined.
# write_files: missing carob_contributor, latitude, longitude. unknown variable: plot_count_.

## NEW VARIABLES
# plot_count_: total potato plots (Total Plots).

## NOT PROCESSED
# Livestock counts, farm labor, and off-farm income.
# FrecuencyOfVarietiesByArea_Junin.xlsx and RotationRatio_Junin.xlsx. They are summaries without a household key.
# Community boundary shapefiles.


carob_script <- function(path) {

"
Dataset for: Participatory mapping for long-term monitoring and red listing of cultivated potato populations in Junin, Peru

Biodiversity sustains a wide variety of genetic traits that are very valuable for the potato's adaption to changing environments and successful cultivation in the future. It is assumed that several factors are threatening biodiversity in the Andes. Without robust baselines and indicators used for reliable time lines, it is impossible to assess diversity loss or increase. Long-term studies in situ are necessary to distinguish short term fluctuation from general tendencies caused by environmental and socio-cultural shifts and to understand ongoing evolutionary principles within the potato genepool. Powerful tools are needed that facilitate the management, evolution and exchange of mainly oral transmitted traditional knowledge. Participatory mapping is such a tool where cognitive spatial knowledge is transformed into maps at grassroots level by trained community members. Local knowledge can be documented in context with social, political, biotic and abiotic factors and if accompanied with comprehensive capacity building and the application of robust indicators will strengthen the cross-generation knowledge exchange and sensitization towards land use shifts, resource dynamics affected by climate and societal related factors and pressure.  Most local farmers practice traditional small scale agriculture with focus on potato production, partly organized in sectoral fellow systems. Because of the high biological diversity, coexistence of wild potato species and the extensive local knowledge owned by the potato farmers, communities of this study site contribute to generate knowledge on dynamics of potato diversity in situ and complement the diversity hotspot network 'Chirapaq Ñan' for long-term in situ monitoring of cultivated potato. This dataset contributes to i) elaborate robust baseline to assess spatial and temporal dynamics and distribution ranges of potato diversity within and among fields, families and altitudinal belts, ii) elaborate on the application of utilized diversity indexes for red listing of potato landraces and cultivar types. The dataset contains parcel and community maps, geographic distribution of cultivated potato populations, red listing of potato landraces and cultivar types and supplementary files (surveys, protocols) used for this study.
"

	uri <- "doi:10.21223/P3/DNQ4ED"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
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
		carob_completion = 30,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Household_Dataset_Junin.xlsx"]
	r <- carobiner::read.excel(f, sheet="data", skip=2)
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z)] <- NA_character_
		z
	}
	num <- function(x) {
		z <- txt(x)
		ok <- is.na(z) | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}
	d <- data.frame(
		hhid = paste(txt(r$Community), txt(r$Code), sep="_"),
		country = txt(r$Country),
		adm1 = txt(r$Region),
		location = txt(r$Community),
		crop = "potato",
		plot_count_ = num(r[["Total Plots"]]),
		geo_from_source = FALSE,
		is_survey = TRUE,
		on_farm = TRUE,
		trial_id = "Junin_potato_mapping"
	)
	d <- d[!is.na(d$country) | !is.na(d$location), ]
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
