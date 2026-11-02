# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mother-and-baby baseline for potato growers in Boyaca, Colombia, 2009. Country, department, municipality, and vereda are columns. The title places the work in Colombia and the country column is used as recorded.
# Person names are not copied. The crop is potato. Local unit conversions in 05_Conversion.docx are not applied because this table does not carry a production quantity.

## ISSUES
# No coordinates and no yield in this table.
# write_files: missing carob_contributor, latitude, longitude.

## NOT PROCESSED
# Names, household roster, and the production and conversion modules.


carob_script <- function(path) {

"
Dataset for: Estudio de base de la metodología Mama&Bebe Caso: Red Latinpapa - Colombia, 2009.

El presente es un informe del estudio de base de la metodología 'Mama y Bebé' (M&B) que viene implementando la Corporación PBA, en el marco del Proyecto Alianza Cambio Andino, en COOINPROPAS, del departamento de Boyacá, en el ano 2009. En este caso se ha utilizado un cuestionario semiestructurado de preguntas cerradas y estuvo dirigido a los jefes de familia de dos organizaciones de productoras de papa: Siachoque y Soracá, contándose en total 31 encuestas diligenciadas. La Selección Participativa de Variedades con la metodología Mama & Bebe ha sido aplicado en la región Andina, principalmente en Perú, con bastante éxito aportando a la liberación temprano y descentralizada de nuevas variedades rústicas de papa en zonas pobres. En el marco del Proyecto Alianza Cambio Andino se propone replicar la metodología sistemáticamente en el Perú y Colombia con un enfoque de cadenas involucrando a actores regionales claves de las cadenas de valor. El presente estudio de base de la metodología Mama y Bebe (M&B) es implementada por la Corporación PBA en el producto de papa y da cuenta del valor inicial de los principales indicadores de alcances e impactos que se promueve con la aplicación de la metodología antes citada.
"

	uri <- "doi:10.21223/P3/WZDOUX"
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
	r <- carobiner::read.excel(f)
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
		hhid = txt(r$NroEnc),
		date = when(r$FecApl),
		country = txt(r[["1Pais"]]),
		adm1 = txt(r[["2Departamento"]]),
		adm2 = txt(r[["3Municipio"]]),
		location = txt(r[["4Vereda"]]),
		crop = "potato",
		geo_from_source = FALSE,
		is_survey = TRUE,
		on_farm = TRUE,
		trial_id = "Colombia_mother_baby_2009"
	)
	d <- d[!is.na(d$hhid), ]
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
