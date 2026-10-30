# R script for "carob"
# license: GPL (>=3)

## NOTES
# Parcel records for cultivated potato in Pasco. Country, region, and community are columns in the rotation workbook.
# latitud and longitud are values in the millions, so they are a projected grid rather than decimal degrees. They were not copied into latitude and longitude.
# Cultivar_Final is a rotation code, not a variety name. Variety names and area in m2 are in the frequency workbook.
# Planting date and Harvest date are Excel day counts. They are stored as dates.

## ISSUES
# Sites need georeferencing from decimal degrees. The shapefiles are community boundaries, not plot points.
# No yield.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude. unknown variables: altitude_band_, area_recorded_, rotation_code_, area_m2_.

## NOT PROCESSED
# Household_Dataset_Pasco.xlsx: the first row is a grouped header.
# CommunityBoundaries shapefiles and the survey PDFs.
# Year columns 2001-2015 and the other rotation descriptors in RotationRatio_Pasco.xlsx.

## NEW VARIABLES
# area_recorded_: Area column, unit not stated.
# area_m2_: Area m2 in the frequency workbook.
# rotation_code_: Cultivar_Final.
# altitude_band_: R_Alt_100m.


carob_script <- function(path) {

"
Dataset for: Participatory mapping for long-term monitoring and red listing of cultivated potato populations in Pasco, Peru

Biodiversity sustains a wide variety of genetic traits that are very valuable for the potato's adaption to changing environments and successful cultivation in the future. It is assumed that several factors are threatening biodiversity in the Andes. Without robust baselines and indicators used for reliable time lines, it is impossible to assess diversity loss or increase. Long-term studies in situ are necessary to distinguish short term fluctuation from general tendencies caused by environmental and socio-cultural shifts and to understand ongoing evolutionary principles within the potato genepool. Powerful tools are needed that facilitate the management, evolution and exchange of mainly oral transmitted traditional knowledge. Participatory mapping is such a tool where cognitive spatial knowledge is transformed into maps at grassroots level by trained community members. Local knowledge can be documented in context with social, political, biotic and abiotic factors and if accompanied with comprehensive capacity building and the application of robust indicators will strengthen the cross-generation knowledge exchange and sensitization towards land use shifts, resource dynamics affected by climate and societal related factors and pressure.  Most local farmers practice traditional small scale agriculture with focus on potato production, partly organized in sectoral fellow systems. Because of the high biological diversity, coexistence of wild potato species and the extensive local knowledge owned by the potato farmers, communities of this study site contribute to generate knowledge on dynamics of potato diversity in situ and complement the diversity hotspot network 'Chirapaq Ñan' for long-term in situ monitoring of cultivated potato. This dataset contributes to i) elaborate robust baseline to assess spatial and temporal dynamics and distribution ranges of potato diversity within and among fields, families and altitudinal belts, ii) elaborate on the application of utilized diversity indexes for red listing of potato landraces and cultivar types. The dataset contains parcel and community maps, geographic distribution of cultivated potato populations, red listing of potato landraces and cultivar types and supplementary files (surveys, protocols) used for this study.
"

	uri <- "doi:10.21223/P3/VRAXFD"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
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
		carob_completion = 35,
		carob_effort = 0.4
	)

	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) {
			out <- rep(NA_character_, length(x))
			ok <- !is.na(x)
			# Excel serial day count from the Planting date and Harvest date columns.
			out[ok] <- as.character(as.Date(x[ok], origin="1899-12-30"))
			return(out)
		}
		x <- chr(x)
		out <- rep(NA_character_, length(x))
		iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
		dmy <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x)
		out[iso] <- as.character(as.Date(substr(x[iso], 1, 10)))
		if (any(dmy)) out[dmy] <- as.character(as.Date(x[dmy], "%d/%m/%Y"))
		out
	}

	fr <- ff[basename(ff) == "RotationRatio_Pasco.xlsx"]
	fv <- ff[basename(ff) == "FrecuencyOfVarietiesByArea_Pasco.xlsx"]
	r <- carobiner::read.excel(fr, sheet="data")
	d1 <- data.frame(
		country = chr(r$Country),
		adm1 = chr(r$Region),
		location = chr(r$Community),
		hhid = chr(r$Fam),
		plot_id = chr(r[[grep("^Code", names(r))[1]]]),
		altitude_band_ = chr(r$R_Alt_100m),
		area_recorded_ = num(r$Area),
		planting_date = as_date(r[["Planting date"]]),
		harvest_date = as_date(r[["Harvest date"]]),
		rotation_code_ = chr(r$Cultivar_Final),
		stringsAsFactors = FALSE
	)
	v <- carobiner::read.excel(fv, sheet="Lista_Area")
	d2 <- data.frame(
		country = "Peru",
		variety = chr(v$Variedad),
		area_m2_ = num(v[["Area m2"]]),
		stringsAsFactors = FALSE
	)
	d <- carobiner::bindr(d1, d2)
	miss <- is.na(d$hhid) | !nzchar(d$hhid)
	d$hhid[miss] <- paste0("row", seq_len(nrow(d))[miss])
	d$crop <- "potato"
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- paste0("Pasco_", seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
