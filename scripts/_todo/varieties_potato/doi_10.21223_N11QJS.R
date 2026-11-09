# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are Tabla 12 (laboratory minerals and quality). Header row includes INSTN, BREEDERCODE, Mean_FEDW, and Mean_ZNDW.
# Those means are stored as recorded. The header does not print mg/kg, so they are not renamed to a standard iron term.
# Country Peru is in the dataset description. The tables cover several departments and are not one coordinate.
# Publication doi:10.4160/9789290605935 is the report DOI in the metadata.
# The abstract states fertilizer rates. It does not say whether the figures are elemental or oxide, so N, P, and K are not filled.

## ISSUES
# No yield in kg/ha in this laboratory table.
# Sites need georeferencing.
# write_files: ; yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: fe_dw_mean_, zn_dw_mean_.

## NOT PROCESSED
# Soil, date, vote, organoleptic, and correlation workbooks other than Tabla 12.
# Standard deviations and sample sizes next to the mineral means.
# 00 Lista Tablas y Figuras.xlsx: not read. sheet Lista: Tabla 1. Lista de las muestras de suelos enviadas para laborario de suelos. Observation records are missing from d until this file is standardized.
# Tabla 01. Lista de las muestras de suelos.xlsx: not read. sheet Tabla 1: ORD, Adm1, Adm2, Adm3, Tipo de muestra, Muestras Totales, Descripción : Análisis de suelo, Costo unitario (Referencial), Costo Total. Observation records are missing from d until this file is standardized.
# Tabla 01a.- Análisis de suelos de 9 localidades del Perú.xlsx: not read. sheet Tabla1a: Ciudad, Locality, Altitude, Sand_%_mean, Sand_%_sd, Silt_%_mean, Silt_%_sd, Clay_%_mean, Clay_%_sd, Soil texture, Percentage of water saturation_%_mean, Percentage of water saturation_%_sd, .... Observation records are missing from d until this file is standardized.
# Tabla 02. Organigrama de trabajo para el monitoreo.xlsx: not read. sheet Tabla 2 : Tabla 2. Organigrama de trabajo para el monitoreo, manejo, evaluación y logística de los experimetos de campo instalados en 10 localidades de la Sierra del Perú, ...2, ...3, ...4, ...5, ...6, ...7, ...8. Observation records are missing from d until this file is standardized.
# Tabla 03  Fechas de siembra, aporque  y evaluaciones realizadas en los experimetos de campo instalados en 10 localidades de la Sierra del Perú.xlsx: not read. sheet Tabla 3: Tabla 3.  Fechas de siembra, aporque  y evaluaciones realizadas en los experimetos de campo instalados en 10 localidades de la Sierra del Perú, ...2, ...3, ...4, ...5, ...6, ...7, ...8, ...9, ...10, ...11, Prueba de Dormancia y Brotamiento, .... Observation records are missing from d until this file is standardized.
# Tabla 04  Kit-materiales-PVS.xlsx: not read. sheet Tabla 4 : Tabla 4.  KIT-MATERIALES-PVS, ...2. Observation records are missing from d until this file is standardized.
# Tabla 05, 5.1, 5.2 y 5.3 Resultados de las votaciones por los criterios de selecci n en la etapa de la floracion.xlsx: not read. sheet Tabla5_Resultados_globales (of 6): Orden, Grupo de Variables, Criterios de seleccion, Puntaje Hombres (N° granos Maíz), Puntaje Mujeres (N° granos Habas), Puntaje Global, Orden de importancia por criterios, Puntaje Global por grupo de criterios. Observation records are missing from d until this file is standardized.
# Tabla 06 Selección de los clones preferidos por parcela en la etapa de la floración.xlsx: not read. sheet Tabla 6: Tabla 6: Selección de los clones preferidos por parcela en la etapa de la floración.. Observation records are missing from d until this file is standardized.
# Tabla 07, 7.1,  7.2 y 7.3 Resultados de las votaciones por los criterios de selecci n en la etapa de Cosecha.xlsx: not read. sheet Tabla7_ResultadosGlobales (of 6): Orden, Grupo de Variables, Criterios de seleccion, Puntaje Hombres (N° granos Maíz), Puntaje Mujeres (N° granos Habas), Puntaje Global, Orden de importancia por criterios, Puntaje Global por grupo de criterios. Observation records are missing from d until this file is standardized.
# Tabla 08 Selección de los clones preferidos por parcela en la etapa de cosecha..xlsx: not read. sheet Tabla 8: Tabla 8: Selección de los clones preferidos por parcela en la etapa de cosecha.. Observation records are missing from d until this file is standardized.
# Tabla 09 Cartilla de evaluación de pruebas organolépticas en campo.xlsx: not read. sheet Tabla 9: Tabla 9: Cartilla de evaluación de pruebas organolépticas en campo. Observation records are missing from d until this file is standardized.
# Tabla 10 Cartilla de evaluación de pruebas organolépticas en la estación CIP- La Molina.xlsx: not read. sheet Tabla 10: Tabla 10: Cartilla de evaluación de pruebas organolépticas en la estación CIP- La Molina. Observation records are missing from d until this file is standardized.
# Tabla 11 Cronograma de envío de muestras liofilizadas para análisis de minerales en la Universidad de Nottingham.xlsx: not read. sheet Tabla 11: Tabla 11: Cronograma de envío de muestras liofilizadas para análisis de minerales en la Universidad de Nottingham, ...2, ...3, ...4, ...5, ...6, ...7. Observation records are missing from d until this file is standardized.
# Tabla 13 Arreglo Evaluación organoléptica en el laboratorio de procesamiento del CIP.xlsx: not read. sheet Tabla 13: Tabla 13 .- Arreglo Evaluación organoléptica en el laboratorio de procesamiento del CIP, ...2, ...3, ...4, ...5, ...6, ...7. Observation records are missing from d until this file is standardized.
# Tabla 14 y 15 Resultados Correlaciones significativas,  intervalos de confianza, analisis de varianza.xlsx: not read. sheet Tabla 14 (of 2): Tabla 14.-  Correlaciones significativas  e  intervalos de confianza. Observation records are missing from d until this file is standardized.
# Tabla 16  Selección de 12 clones biofortificados tetraploides para ser evaluados en los  ensayos .xlsx: not read. sheet Tabla 16 : ORD, INSTN, BREEDER CODE, Area de Adaptación, Area2, Female, Male, n_FEDW, Mean_FEDW, Sd_FEDW, Min_FEDW, Max_FEDW, .... Observation records are missing from d until this file is standardized.

## NEW VARIABLES
# fe_dw_mean_: Mean_FEDW as recorded
# zn_dw_mean_: Mean_ZNDW as recorded

carob_script <- function(path) {

"
Dataset for: Potential new varieties of biofortified potato with high iron and zinc content, to contribute to food and nutritional security in high Andean family farming.

In total 30 tetraploid BIOT clones and 4 commercial varieties were evaluated in multi-location trials in nine field experiments, in five departments of Peru using participatory varietal selection. Trials were planted according to local practices using 40 plants per plot with a randomized complete block design with 3 replications at each location. The plot size was 9.72m2, with 4 rows of 2.7m long and 0.9m between rows. Nitrogen, Phosphorus, Potassium (NPK) (180-180-160) and organic amendment ('guano de corral') was applied before planting. Soil samples of 1 kg each were collected before planting and sent to the agricultural chemistry laboratory Valle Grande – Cañete for soil, salinity and micronutrient content analysis. Participatory varietal selection (PVS) evaluations were conducted following the protocol 'Participatory Varietal Selection of Potato Using the Mother & Baby Trial Design: A Gender-responsive Trainer's Guide'. The data was collected using Highly Interactive Data Analysis Platform, i.e. HIDAP, software. Evaluations at the flowering, harvest and post-harvest (storage) phase were performed. Farmers identified and ranked characteristics of potato that they considered important, and afterwards they selected the genotype they preferred. Each farmer's varietal selection and the criteria for selection were recorded.
"

	uri <- "doi:10.21223/N11QJS"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = "doi:10.4160/9789290605935",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "fe_dw_mean_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Tabla 12 Los resultados del porcentaje de materia seca, color de hojuelas, contenido de azúcares, análisis organolépticos en laboratorio.xlsx"]
	r <- as.data.frame(carobiner::read.excel(f, sheet="Tabla 12_data", col_names=FALSE))
	hdr <- trimws(as.character(unlist(r[1, ])))
	y <- r[-1, ]
	names(y) <- make.names(hdr, unique=TRUE)
	num <- function(v) {
		z <- trimws(as.character(v))
		z[z %in% c("", "NA")] <- NA_character_
		as.numeric(z)
	}
	variety <- trimws(as.character(y$BREEDERCODE))
	blank <- is.na(variety) | !nzchar(variety)
	variety[blank] <- trimws(as.character(y$INSTN))[blank]
	d <- data.frame(
		crop = "potato",
		variety_code = trimws(as.character(y$INSTN)),
		variety = variety,
		fe_dw_mean_ = num(y$Mean_FEDW),
		zn_dw_mean_ = num(y$Mean_ZNDW),
		country = "Peru",
		trial_id = "PVS_sierra",
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA
	)
	d$treatment <- d$variety
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]
	d$plot_id <- as.character(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
