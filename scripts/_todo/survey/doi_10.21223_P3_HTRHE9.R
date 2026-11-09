# R script for "carob"
# license: GPL (>=3)

## NOTES
# Consumer interviews about native colored potatoes outside supermarkets in San Isidro, La Molina, and Miraflores, Lima, June 2015. 114 respondents.
# The title names those Lima districts and does not contain a country column. Country is Peru from that place. Sex is F = femenino and M = masculino in the dictionary. The respondent id is encuestado (encuesta restarts within a district).

## ISSUES
# No latitude or longitude.
# missing variables: latitude, longitude

## NOT PROCESSED
# Enumerator name and the perception items v1 to v11 (knowledge, frequency, importance, and agreement with Wong).
# The dictionary workbook and the questionnaire pdf.
# 02_Data dictionary- La percepcion de los consumidores sobre las papas nativas de color.xlsx: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Cuestionario sobre la percepción de los consumidores sobre las papas nativas de color en supermercados distritos San Isidro, La Molina y Miraflores en Lima.

Conocer la percepción de los consumidores sobre las papas nativas de color en supermercados distritos San Isidro, La Molina y Miraflores en Lima. Se aplicaron encuestas a los consumidores – amas de casa que acuden a los supermercados en la ciudad de Lima en el mes de Junio del 2015 a 114 consumidores.
"

	uri <- "doi:10.21223/P3/HTRHE9"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "01_DataBase Percepcion de los consumidores sobre las papas nativas de color.xlsx"]
	r <- carobiner::read.excel(f, na = c("", "NA"))
	sex <- trimws(as.character(r$Sexo))
	sex <- c("F" = "female", "M" = "male")[sex]
	when <- as.Date(r$fecha)

	d <- data.frame(
		trial_id = "Native_potato_consumers_Lima_2015",
		hhid = trimws(as.character(r$encuestado)),
		country = "Peru",
		location = carobiner::fix_name(as.character(r$distrito), "title"),
		crop = "potato",
		sex = unname(sex),
		age = as.numeric(r$edad),
		date = as.character(when),
		on_farm = FALSE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
