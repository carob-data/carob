# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline household survey of CONPAPA in Bolivar and Tungurahua, Ecuador, 2009. Country, province, canton, parish, and community are columns in the database. The province column is spelled 2Provinvia in the file.
# 10Area01 and 10CantCosec01 have no unit in the column name, so they were not converted to hectares or kg/ha. Crop is set to potato only when the first crop name contains "papa".

## ISSUES
# No coordinates. Communities need georeferencing.
# No fertilizer rate.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude. unknown variables: household_size_, area_total_, crop_name_, crop_area_, crop_harvest_.

## NOT PROCESSED
# Household-member blocks 8Nombre_01 through 8Educacion_13, the later crop columns 10Cultivo02 to 10Cultivo06, variety sale blocks, and the association and housing modules in 01_Database SEP CONPAPA Ecuador.xlsx.
# The questionnaire document and PDF.

## NEW VARIABLES
# crop_name_, area_total_, crop_area_, crop_harvest_: first-crop fields, units not stated.
# household_size_.


carob_script <- function(path) {

"
Dataset for: Estudio de Base de Metodologías Participativas: “Seguimiento y Evaluación Participativa - SEP” Caso CONPAPA- Ecuador, 2009.

El presente estudio de base de la metodología Seguimiento y Evaluación Participativo (SEP) implementada por el Consorcio de Productores de Papa de la Región Central del Ecuador (CONPAPA), y da cuenta del valor inicial de los principales indicadores de alcances e impactos que se promueve con la aplicación de la metodología antes citada. CONPAPA implementa la metodología del SEP en dos proyectos que viene ejecutando en dos provincias del Ecuador, Bolívar y Tungurahua. El primero de ellos es el Proyecto 'Sistema de multiplicación de semillas de la Asociación San Francisco de Rumipampa' ejecutado en el cantón San Miguel de la provincia de Bolívar. El segundo es el Proyecto 'Producción y Comercialización de la Papa en la Asociación Señor de la Justicia de la localidad Shaushi', implementado en el cantón Quero de la provincia de Tungurahua. El número de agricultores (familias) encuestadas del Grupo Objetivo fueron de la comunidad de San Fco. de Rumipampa (30), Shaushi (26) y del Grupo Control de la comunidad de Cochabamba.
"

	uri <- "doi:10.21223/P3/BO6PJL"
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
		carob_completion = 20,
		carob_effort = 0.35
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
	f <- ff[basename(ff) == "01_Database SEP CONPAPA Ecuador.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")
	d <- data.frame(
		hhid = chr(r$NroEnc),
		country = chr(r[["1Pais"]]),
		adm1 = chr(r[["2Provinvia"]]),
		adm2 = chr(r[["3Canton"]]),
		adm3 = chr(r[["4Parroquia"]]),
		location = chr(r[["5Comunidad"]]),
		household_size_ = num(r[["7NroMiembros"]]),
		area_total_ = num(r[["9AreaTotal"]]),
		crop_name_ = chr(r[["10Cultivo01"]]),
		crop_area_ = num(r[["10Area01"]]),
		crop_harvest_ = num(r[["10CantCosec01"]])
	)
	d$crop <- NA_character_
	d$crop[grepl("papa", tolower(d$crop_name_))] <- "potato"
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- paste0("CONPAPA_", d$hhid)

	carobiner::write_files(path, meta, d)
}
