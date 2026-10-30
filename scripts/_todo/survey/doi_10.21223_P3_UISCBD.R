# R script for "carob"
# license: GPL (>=3)

## NOTES
# Market survey, September to November 2003, in Huánuco, Junin, Cuzco, La Libertad, and Cajamarca. Department, province, district, and community are columns. The file does not contain a country column, and the title does not name the country, so country is omitted.
# Up to three crops are recorded. p1cantcul1(kg), p1cantcul2(kg), and p1cantcul3(kg) are kilograms. The area columns do not state a unit, so yield is not computed. Respondent names are not copied.
# Papa is mapped to potato. Other crop names stay in local_crop_ when they are not in the crop list used below.

## ISSUES
# No latitude or longitude. Altitude is elevation when the cell is numeric.
# Area unit is not stated.
# write_files: missing carob_contributor, country, latitude, longitude. unknown variables: local_crop_, harvest_kg_.

## NEW VARIABLES
# local_crop_: crop name as written when it is not mapped.
# harvest_kg_: harvested quantity in kilograms from the column name.

## NOT PROCESSED
# Respondent names, the rest of the attitude and market questions, and crop area, because the area unit is not in the column name.


carob_script <- function(path) {

"
Dataset for: Estudio de Mercado para variedades resistentes, baculovirus y feromonas en Cajamarca, Cusco, Huánuco, Junin y La Libertad.

El Estudio de Mercado para variedades de papa resistentes, baculovirus y feromonas, se realizó en los departamentos Huánuco, Junin, Cuzco, La Libertad y Cajamarca, en cada departamento se consideró a cuatro comunidades de 25 encuestados cada una, haciendo un total de 500 agricultores encuestados. Las encuestas fueron tomadas del mes de Setiembre-Noviembre del 2003.
"

	uri <- "doi:10.21223/P3/UISCBD"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
		carob_completion = 25,
		carob_effort = 0.35
	)

	f <- ff[grepl("^01_Encuesta", basename(ff))]
	r <- carobiner::read.excel(f)
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
	when <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(format(as.Date(x), "%Y-%m-%d"))
		txt(x)
	}
	crop_of <- function(x) {
		key <- tolower(txt(x))
		key <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa", "aeiou", key)
		c(papa="potato", maiz="maize", trigo="wheat", cebada="barley", habas="faba bean",
			arveja="pea", quinua="quinoa")[key]
	}
	base <- data.frame(
		hhid = txt(r$Codigo),
		date = when(r[["Fecha de entrevista"]]),
		adm1 = txt(r$Departamento),
		adm2 = txt(r$Provincia),
		adm3 = txt(r$Distrito),
		location = txt(r$Comunidad),
		elevation = num(r$Altitud),
		stringsAsFactors = FALSE
	)
	blocks <- list(
		c("P1cultivo1", "p1cantcul1(kg)"),
		c("p1cultivo2", "p1cantcul2(kg)"),
		c("p1cultivo3", "p1cantcul3(kg)")
	)
	parts <- lapply(blocks, function(cols) {
		data.frame(base, local_crop_ = txt(r[[cols[1]]]), harvest_kg_ = num(r[[cols[2]]]), stringsAsFactors = FALSE)
	})
	d <- do.call(rbind, parts)
	d <- d[!is.na(d$local_crop_) | !is.na(d$harvest_kg_), ]
	d$crop <- as.character(crop_of(d$local_crop_))
	d$local_crop_[!is.na(d$crop)] <- NA_character_
	d$geo_from_source <- FALSE
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- "Andean_market_2003"
	if (!any(!is.na(d$hhid))) d <- d
	d <- d[!is.na(d$hhid), ]
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
