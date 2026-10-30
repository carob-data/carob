# R script for "carob"
# license: GPL (>=3)

## NOTES
# Consumer survey of Miskipapa native potatoes in Lima, 2020. Country Peru and city Lima are the Dataverse geographic coverage.
# Kilos in question 3.6.a are the amount bought per occasion, not a field yield.
# Question 2.3 is place of birth, not the district of residence.

## ISSUES
# No coordinates. Districts need georeferencing.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude. unknown variables: survey_year_, birth_place_, respondent_age_, household_size_, children_under_18_, occupation_, purchase_kg_.

## NOT PROCESSED
# 02_Encuesta_demanda-I_2020_Formularios de Google.pdf: the blank form.
# Remaining answer columns in 01_Encuesta_sobre_demanda_Miskipapa_2020.xlsx (pickup point, suggestions, purchase frequency, motivation, price opinion, and free-text comments): kept out of the table because they are multi-choice text without a coded dictionary in this version.

## NEW VARIABLES
# respondent_age_, household_size_, children_under_18_, occupation_, birth_place_, purchase_kg_.


carob_script <- function(path) {

"
Dataset for: Potato native Miskipapa market survey in 2020

The native potato varieties have been successfully marketed to consumers in Lima in 2020 who wish to support the small farmer and the conservation of biodiversity. The survey was carried out on 130 consumers of native potatoes of the Miskipapa brand from different districts of Lima. In the exploratory surveys in 2020 women buy more native potatoes (76%) compared to men (24%). This difference may be due to the fact that they make decisions at home for food preparation. The largest districts of native potato consumption of MiskiPapa 2020 are found in Miraflores, followed by Santiago de Surco with 23%, La Molina with 20% and San Borja with 9%.
"

	uri <- "doi:10.21223/WCWUNE"
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
		carob_completion = 45,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "01_Encuesta_sobre_demanda_Miskipapa_2020.xlsx"]
	r <- carobiner::read.excel(f, sheet="Encuesta_2020")
	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	d <- data.frame(
		date = as.character(as.Date(r$Fecha)),
		survey_year_ = chr(r[["Año_encuesta"]]),
		location = chr(r[["1.1. DISTRITO:"]]),
		respondent_age_ = num(r[["2.1. Edad (años)"]]),
		farmer_gender = chr(r[["2.2. Género"]]),
		birth_place_ = chr(r[["2.3. Lugar de Nacimiento (Ciudad/ País):"]]),
		household_size_ = num(r[["2.4.a. Nro. de integrantes del hogar:"]]),
		children_under_18_ = num(r[["2.4.b. ¿Cuántos menores de 18 años?"]]),
		occupation_ = chr(r[["2.5. Ocupación:"]]),
		purchase_kg_ = num(r[["3.6.a. ¿Cuántos kilos de papa nativa surtida suele comprar en cada oportunidad?"]]),
		stringsAsFactors = FALSE
	)
	d$crop <- "potato"
	d$country <- "Peru"
	d$adm1 <- "Lima"
	d$trial_id <- paste0("Miskipapa_2020_", seq_len(nrow(d)))
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$hhid <- as.character(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
