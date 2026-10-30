# R script for "carob"
# license: GPL (>=3)

## NOTES
# Participatory monitoring baseline for papa criolla growers, Colombia, 2008. Country, department, municipality, and vereda are columns (1Pais, 2Departam, 3Municipio, 4Vereda). Names stored in capitals are written in title case, so COLOMBIA is Colombia.
# Person names and household-member rosters are not copied. The study crop is potato.

## ISSUES
# No coordinates. Production quantities in the later questions are not mapped because the unit is not taken from a dictionary row here.
# write_files: missing carob_contributor, latitude, longitude.

## NOT PROCESSED
# Household member names, ages, and occupations.
# Input, price, and production modules. Their units are not applied in this table.


carob_script <- function(path) {

"
Dataset for: Metodología Participativa Seguimiento y Evaluación Participativa (SEP), Papa Criolla Colombia, 2008.

La metodología SEP, es una herramienta de gestión de proyectos que se aplica con el fin de contribuir a fortalecer a las organizaciones de base. Por sus características es altamente participativa, ya que permite que los beneficiarios en los distintos niveles –desde el nivel directivo hasta el de base- participen en el seguimiento y evaluación de un proyecto idea o emprendimiento colectivo. En el presente estudio el grupo objetivo del proyecto Agricultura Sostenible y Competitiva está conformado por las siguientes dos asociaciones: Asociación de Agricultores de Granada (ASOAGRA) y la Asociación Criolla Oro de Sibaté, quienes a su vez implementarán la Metodología Participativa SEP durante el proceso de implementación del proyecto, en el año 2008.
"

	uri <- "doi:10.21223/P3/RSP2EY"
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
		carob_effort = 0.25
	)

	f <- ff[grepl("^01_DataBase", basename(ff))]
	r <- carobiner::read.excel(f, sheet="All")
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z)] <- NA_character_
		z
	}
	when <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(format(as.Date(x), "%Y-%m-%d"))
		txt(x)
	}
	d <- data.frame(
		hhid = txt(r$N_Enc),
		date = when(r$FecApl),
		country = txt(r[["1Pais"]]),
		adm1 = txt(r[["2Departam"]]),
		adm2 = txt(r[["3Municipio"]]),
		location = txt(r[["4Vereda"]]),
		crop = "potato",
		geo_from_source = FALSE,
		is_survey = TRUE,
		on_farm = TRUE,
		trial_id = "Colombia_papa_criolla_2008",
		stringsAsFactors = FALSE
	)
	d <- d[!is.na(d$hhid), ]
	nice <- function(x) ifelse(!is.na(x) & toupper(x) == x, tools::toTitleCase(tolower(x)), x)
	d$country <- nice(d$country)
	d$adm1 <- nice(d$adm1)
	d$adm2 <- nice(d$adm2)
	d$location <- nice(d$location)
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
