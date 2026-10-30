# R script for "carob"
# license: GPL (>=3)

## NOTES
# Consumer survey of Miskipapa native potatoes in Lima, 2021. Country is Peru and the city is Lima. These are consumers, not farm plots.
# Age and the amount purchased are the brackets printed on the form, not a year of age and not a single number of kilograms.

## ISSUES
# No farm coordinates. District of residence is the place name.
# missing variables: carob_contributor
# missing variables: latitude, longitude
# unknown variable: age_band_ (age?)
# unknown variables: kilos_purchased_

## NOT PROCESSED
# Purchase frequency, motives, price the consumer would pay, and the open comments.
# Pais de Nacimiento is the country of birth, not the survey country.


carob_script <- function(path) {

"
Dataset for: Potato native Miskipapa market survey in 2021

The native potato varieties have been successfully marketed to consumers in Lima in 2021 who wish to support the small farmer and the conservation of biodiversity.   The survey was carried out on 93 consumers of native potatoes of the Miskipapa brand from different districts of Lima. This dataset included information about the demand of Miskipapa Consumers by gender, Product demand analysis, Percentage of delivery and pickup method, Percentage of distribution by districts,  Major districts of distributions in Lima, Number of kilos of native potatoes that consumers buy, and Frequency of purchase of native potatoes.
"

	uri <- "doi:10.21223/PFUUMA"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.25
	)

	f <- ff[basename(ff) == "01_Encuesta_sobre_demanda_Miskipapa_2021.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	sex <- trimws(as.character(r[["Género"]]))
	sex <- c(F = "female", M = "male")[sex]
	kilos <- "¿Cuántos kilos de papa nativa surtida suele comprar en cada oportunidad?"

	d <- data.frame(
		trial_id = "Miskipapa_Lima_2021",
		hhid = trimws(as.character(r$Numeracion)),
		country = "Peru",
		adm1 = "Lima",
		location = trimws(as.character(r[["DISTRITO de residencia"]])),
		crop = "potato",
		sex = sex,
		age_band_ = trimws(as.character(r$Edad)),
		kilos_purchased_ = trimws(as.character(r[[kilos]])),
		on_farm = FALSE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
