# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline of the Mama y Bebe potato method in Peru, 2008-2009. Country is the 1Pais column. The crop is potato.
# Department, province, district, and community are columns. Grupo_Encuest is objetivo or control.
# 9NroMiembros is the number of household members.

## ISSUES
# No latitude or longitude. The abstract describes several sites above 3000 m, not one coordinate.
# Sex is coded 1 and 2. This dictionary does not print which code is male or female, so sex is not mapped.
# missing variables: carob_contributor
# missing variables: latitude, longitude
# unknown variables: survey_group_

## NOT PROCESSED
# Names and the rest of the household roster.
# Production and plot questions. Units are not taken from the closed questionnaire.
# Dictionary, the docx questionnaire, and the pdf.


carob_script <- function(path) {

"
Dataset for: Estudio de base de la metodología Madre&Bebe Caso: Red Latinpapa - Peru, 2008.

El presente es un informe del estudio de base de la metodología 'Mamá y Bebé' (M&B), desde 2008, la Red Latinpapa formó un equipo de trabajo para seleccionar un grupo de clones avanzados del programa de mejoramiento del CIP, integrando a varias instituciones como los programas nacionales de investigación (INIA, universidades e institutos), las ONGs, grupos de agricultores y empresas privadas (tiendas, procesadoras y restaurantes). Cada equipo, por región o lugar, se llama 'Consorcio de SPV'. Con estos consorcios, la Red Latinpapa halló como aliado al Proyecto Alianza Cambio Andino, para difundir el método Mamá y Bebé (MyB) en Perú. En 2008, el método se implementó en cuatro regiones productoras de papa en los Andes centrales, a una altitud mayor de 3 000 msnm, donde prevalecen los pequeños agricultores: Cusco, Huancavelica, Huánuco y Junín. En el primer año, solo tres consorcios establecieron los ensayos en las parcelas Mamá y Bebé según los lineamientos del método: el consorcio Junín (liderado por el Instituto Nacional de Innovación Agraria/Santa Ana), el consorcio Huancavelica-Junín (liderado por la ONG Grupo Yanapai) y el consorcio Huancavelica (liderado por la Universidad para el Desarrollo Andino - UDEA). Los consorcios cuentan con un estudio de línea de base y un estudio de monitoreo e impacto, elaborados por Cambio Andino. La capacitación se realizó en junio de 2008, en la estación experimental del CIP en Huancayo, Junín. Participaron 14 técnicos y 3 agricultores. Luego, los técnicos ejecutaron el método. Establecieron los ensayos a finales de 2008, en la época de lluvias. En 2009, se hicieron las evaluaciones en floración (en febrero) y las evaluaciones en cosecha (en mayo). En este ciclo, los consorcios de Cusco, Huancavelica, Junín y Huancavelica-Junín establecieron los ensayos en siete lugares.
"

	uri <- "doi:10.21223/P3_CYDNZ7"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.3
	)

	f <- ff[grepl("^01_DataBase", basename(ff))]
	r <- carobiner::read.excel(f, na=c("", "NA"))

	d <- data.frame(
		trial_id = "Mama_y_Bebe_Peru_2008",
		hhid = trimws(as.character(r$NroEnc)),
		country = trimws(as.character(r[["1Pais"]])),
		adm1 = trimws(as.character(r[["2Departamento"]])),
		adm2 = trimws(as.character(r[["3Provinvia"]])),
		adm3 = trimws(as.character(r[["4Distrito"]])),
		location = trimws(as.character(r[["5Comunidad"]])),
		crop = "potato",
		hh_size = as.numeric(r[["9NroMiembros"]]),
		survey_group_ = trimws(as.character(r$Grupo_Encuest)),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
