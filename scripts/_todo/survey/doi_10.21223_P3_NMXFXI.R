# R script for "carob"
# license: GPL (>=3)

## NOTES
# Survey of native-potato producers, July 2007. The abstract says the 170 interviews in this file are the Peru case. The crop is potato.
# Department, province, district, and community are columns. 1_3Fecha is the interview date.

## ISSUES
# 1_9Latitud, 1_10Longitud, and 1_11Altitud are empty.
# missing variables: carob_contributor
# missing variables: latitude, longitude

## NOT PROCESSED
# Plot area, scacos, and kilos. The area unit is not in the column name, so kilos are not a yield in kg/ha.
# Household roster, including 6_1Sexo, pests, and the rest of the questionnaire.
# Data dictionary and the pdf questionnaires.


carob_script <- function(path) {

"
Dataset for: Innovaciones Tecnológicas y Mercados Diferenciados para Productores de Papa Nativa.

El proyecto tuvo como objetivo general 'Mejorar la calidad de vida de pequeños productores de papas nativas de la región andina, a través de la incorporación de innovaciones tecnológicas a sus sistemas productivos y el acceso a mercados diferenciados, para aprovechar la biodiversidad de papas nativas'. El proyecto se constituyó en una plataforma para validar y enriquecer las experiencias exitosas alrededor del cultivo de papas nativas, centrándose en los temas: caracterización de la biodiversidad, generación de innovaciones tecnológicas de producción y post cosecha, incorporación de las papas nativas en cadenas de valor, fortalecimiento de los productores en producción y gestión empresarial, difusión de la biodiversidad y gastronomía andina a través de campañas publicitarias dirigidas a revalorizar las papas nativas. Las encuestas fueron 170 y se realizaron en julio del 2007 en el caso de Perú. Las comunidades beneficiarias fueron: San Cristóbal de Ñahuín, Vista Alegre, Mullaca, San José de Aymara y Pazos en el Distrito de Pazos, Provincia de Tayacaja en la Región Huancavelica; y en las Comunidades de Marcavalle, Patalá, 2 de Mayo y Libertadores en el Distrito de Pucará, Provincia de Huancayo en la Región Junín.
"

	uri <- "doi:10.21223/P3_NMXFXI"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.3
	)

	f <- ff[grepl("^01_Encuesta", basename(ff))]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	fecha <- r[["1_3Fecha"]]
	if (inherits(fecha, "Date") || inherits(fecha, "POSIXt")) {
		fecha <- format(fecha, "%Y-%m-%d")
	} else {
		fecha <- trimws(as.character(fecha))
	}

	d <- data.frame(
		trial_id = "papa_nativa_Peru_2007",
		hhid = trimws(as.character(r$Codigo)),
		country = "Peru",
		adm1 = trimws(as.character(r[["1_5Departamento"]])),
		adm2 = trimws(as.character(r[["1_6Provincia"]])),
		adm3 = trimws(as.character(r[["1_7Distrito"]])),
		location = trimws(as.character(r[["1_8Comunidad"]])),
		date = fecha,
		crop = "potato",
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
