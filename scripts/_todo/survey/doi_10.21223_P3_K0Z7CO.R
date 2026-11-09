# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline of the native-potato EPCP study in Chimborazo, Ecuador, 2009. Country, province, canton, parish, and community are columns. The crop is potato.
# One province value is stored as Chimboraazo. That is Chimborazo.
# Interview dates are day-month-year (FecApl).

## ISSUES
# One canton value is the word Canton.
# No latitude or longitude. The dictionary names sex but does not give the codes, so sex is not mapped.
# missing variables: latitude, longitude

## NOT PROCESSED
# Names, the member roster, and the sex, occupation, and education codes.
# Plot tenure, irrigation codes, crop names, production, and area. Area is recorded in cuadras and solares as well as hectares, in a wide block of up to eight plots.
# Livestock counts, costs, and the remaining opinion questions.
# Dictionary, the Word questionnaire, and the Word report.
# 02_Data dictionary_Caso Papa Nativa - EPCP - ECUADOR.xlsx: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Metodología del Enfoque Participativo de Cadenas Productivas (EPCP) en Ecuador: Caso Papas Nativas, 2009.

El presente estudio de base de la metodología del Enfoque Participativo de Cadenas Productivas (EPCP) es implementada por la Fundación Marco en el cultivo de papa nativa. El ámbito de la intervención fue ejecutado en Ecuador, en las comunidades de El Belén, Labranza y San Isidro en la parroquia Sicalpa, cantón Colta, provincia de Chimborazo. La población objetivo fueron los productores que conforman las tres comunidades El Belén (17), Labranza (36) y San Isidro (36) y como grupo control otra comunidad Culluctus (50) que no forma parte del proyecto del EPCP. El objetivo del presente estudio es describir e ilustrar la situación inicial de la población objetivo involucrado en la implementación de la metodología “Enfoque Productivo de Cadenas Productivas” que se viene implementando en diferentes Comunidades productores de papa nativa en el Ecuador.
"

	uri <- "doi:10.21223/P3/K0Z7CO"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "01_DataBase Caso Papa Nativa - EPCP Ecuador.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	prov <- trimws(as.character(r[["2Provincia"]]))
	prov[prov == "Chimboraazo"] <- "Chimborazo"
	when <- as.Date(trimws(as.character(r$FecApl)), "%d%m%Y")

	d <- data.frame(
		trial_id = "EPCP_potato_Ecuador_2009",
		hhid = trimws(as.character(r$NroEnc)),
		country = trimws(as.character(r[["1Pais"]])),
		adm1 = carobiner::fix_name(prov, "title"),
		adm2 = carobiner::fix_name(trimws(as.character(r[["3Canton"]])), "title"),
		adm3 = carobiner::fix_name(trimws(as.character(r[["4Parroquia"]])), "title"),
		location = carobiner::fix_name(trimws(as.character(r[["5Comunidad"]])), "title"),
		crop = "potato",
		hh_size = as.numeric(r[["7NroMiembros"]]),
		date = as.character(when),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
