# R script for "carob"
# license: GPL (>=3)

## NOTES
# Consumer survey of Miskipapa native potatoes in Lima, 2022. Country Peru and city Lima are the Dataverse geographic coverage.
# P.03 is the country and city of birth, not the place of the interview.
# P.20 is kilos bought per occasion, not a field yield. P.01 is an age range, not years.

## ISSUES
# No coordinates. Districts need georeferencing.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude. unknown variables are the questionnaire columns listed under NEW VARIABLES.

## NOT PROCESSED
# Comparison 2020 - 2021 consumer surveys (key indicators) (4).docx: a summary note, not the microdata.

## NEW VARIABLES
# birth_country_, birth_city_, age_range_, occupation_, occupation_detail_, household_size_, heard_from_, customer_since_, bought_before_, uses_delivery_, pickup_place_, nearby_pickup_, pickup_district_2023_, pickup_reference_2023_, purchase_frequency_, purchase_for_, buys_elsewhere_, elsewhere_place_, purchase_kg_, motivation_, mixture_conserves_, mixture_preference_, mixture_why_, wants_variety_names_, wants_origin_, wants_recipes_, news_channel_, offer_frequency_, willingness_to_pay_, would_buy_sprouted_, sprouted_why_, suggestion_, would_recommend_.


carob_script <- function(path) {

"
Dataset for: Potato native Miskipapa market survey in 2022

The native potato varieties have been successfully marketed to consumers in Lima - 2022 who wish to support the small farmer and the conservation of biodiversity.  The survey was carried out on 100 consumers of native potatoes of the Miskipapa brand from different districts of Lima.  This dataset included information about the demand of Miskipapa Consumers by gender, Product demand analysis, Percentage of delivery and pickup method, Percentage of distribution by districts, Major districts of distributions in Lima, Number of kilos of native potatoes that consumers buy, and Frequency of purchase of native potatoes.
"

	uri <- "doi:10.21223/CACKFD"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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
		carob_completion = 90,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "01_Encuesta_sobre_demanda_Miskipapa_2022.xlsx"]
	r <- carobiner::read.excel(f, sheet="Encuesta_2022")
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
		hhid = chr(r$UsuarioID),
		date = as.character(as.Date(r$Fecha)),
		form_ = chr(r$Form),
		survey_year_ = chr(r[["Año_encuestada"]]),
		age_range_ = chr(r$P.01),
		farmer_gender = chr(r$P.02),
		birth_country_ = chr(r$P.03_pais),
		birth_city_ = chr(r$P.03_ciudad),
		occupation_ = chr(r$P.04),
		occupation_detail_ = chr(r$P.04_profesional),
		household_size_ = num(r$P.05),
		location = chr(r$P.06),
		heard_from_ = chr(r$P.08),
		customer_since_ = chr(r$P.09),
		bought_before_ = chr(r$P.10),
		uses_delivery_ = chr(r$P.11),
		pickup_place_ = chr(r$P.12),
		nearby_pickup_ = chr(r$P.13),
		pickup_district_2023_ = chr(r$P.14),
		pickup_reference_2023_ = chr(r$P.15),
		purchase_frequency_ = chr(r$P.16),
		purchase_for_ = chr(r$P.17),
		buys_elsewhere_ = chr(r$P.18),
		elsewhere_place_ = chr(r$P.19),
		purchase_kg_ = num(r$P.20),
		motivation_ = chr(r$P.21),
		mixture_conserves_ = chr(r$P.22),
		mixture_preference_ = chr(r$P.23),
		mixture_why_ = chr(r$P.24),
		wants_variety_names_ = chr(r$P.25),
		wants_origin_ = chr(r$P.26),
		wants_recipes_ = chr(r$P.27),
		news_channel_ = chr(r$P.28),
		offer_frequency_ = chr(r$P.29),
		willingness_to_pay_ = chr(r$P.30),
		would_buy_sprouted_ = chr(r$P.31),
		sprouted_why_ = chr(r$P.32),
		suggestion_ = chr(r$P.33),
		would_recommend_ = chr(r$P.34)
	)
	d$crop <- "potato"
	d$country <- "Peru"
	d$adm1 <- "Lima"
	d$trial_id <- paste0("Miskipapa_2022_", seq_len(nrow(d)))
	d$on_farm <- FALSE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
