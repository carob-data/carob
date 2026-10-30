# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2018 resurvey of potato producers in Carchi, Ecuador, on pre-harvest losses and a late-blight decision-support tool. Country Ecuador and province Carchi are columns. Canton, parish, and recinto are columns.
# quant_ha_M03_01_3_1 is hectares (the sheet converts metres with factor 0.0001). quant_quint_M3a_01A7_1 is quintals (conversion factor 1, unit string Quintales). Quintals are not converted to kilograms.
# gps-Altitude is metres. There is no latitude or longitude. tratamiento is 0 or 1 as stored.
# M02_03_1a_1 is the code 1, not a variety name.

## ISSUES
# No latitude or longitude. Elevation is left as recorded.
# A quintal is not given in kilograms, so yield is omitted.
# write_files: missing carob_contributor, latitude, longitude. unknown variables: area_ha_, production_quintals_. out of bounds: elevation (2047.727, 5533.388).

## NEW VARIABLES
# area_ha_: potato area in hectares, column quant_ha_M03_01_3_1.
# production_quintals_: harvested quantity in quintals, column quant_quint_M3a_01A7_1.

## NOT PROCESSED
# Input costs, fertilizer product lists, pesticide lists, loss-perception modules, buyer names, and the decision-tool qualitative scores.
# quant_cm_M02_03_6_1 and quant_cm_M02_03_7_1 are centimetres, but the sheet does not say which is the plant spacing and which is the row spacing.
# Variety slots other than the first production column.


carob_script <- function(path) {

"
Dataset for: Value food looses at production level in potato value chain of Carchi-Ecuador

La medición de las pérdidas de alimentos (cantidad y valor) y sus causas a lo largo de la cadena de valor puede proporcionar información para mejorar su eficiencia mediante el diseño y la implementación de cambios tecnológicos / institucionales. Esto podría apoyar el aumento de la disponibilidad de alimentos al reducir las pérdidas y desperdicio en los puntos críticos donde ocurren, y así reducir el uso de insumos agrícolas y la presión sobre la tierra. En la cadena de valor de la papa de Ecuador, con origen en la zona de producción de la provincia de Carchi, y cuya producción de papa fresca sirve a los consumidores finales de la ciudad de Quito y a las empresas de transformación, el CIP realizó un estudio en el año 2016. Este estudio fue liderado por el programa PIM (Política, Institución y Mercados), cuyo propósito era desarrollar un marco metodológico para medir las pérdidas de valor y sus causas a lo largo de cadenas alimentarias en los países en desarrollo mediante la prueba de diferentes modelos. Un hallazgo importante del caso de estudio particular de Ecuador mostró que las pérdidas en esta cadena alimentaria son absorbidas principalmente por los productores en etapas previas a la cosecha, siendo una de las principales causas de riesgo de pérdida el ataque del tizón tardío de la papa. Estos resultados, finalmente motivaron la implementación de un estudio piloto (Randomized Control Trial) en el año 2018 con la misma muestra de productores que fueron estudiados el 2016, para investigar el efecto de la implementación de la 'Herramienta de apoyo a la decisión (HAD) para el manejo integrado del tizón tardío de la papa'. Esta base de datos muestra los datos recopilados utilizando la misma encuesta que se aplicó inicialmente en el año 2016, aplicada en 2018 a la muestra de productores que participaron en el estudio piloto de uso de la HDA. La encuesta incluye cinco secciones que recogieron los siguientes datos: i) Caracterización de la muestra de productores en términos de variables socioeconómicas y geográficas básicas ii) Opciones agronómicas en etapas previas a la cosecha en términos de tipos de insumos y su valor, iii) La cantidad total de papa producida y su calidad en términos de categorías de mercado y daños a la calidad; Registros de producción de papa en la misma parcela y la misma variedad: peor y mejor campaña; el destino final del producto (consumo doméstico, mercado, alimento para animales, semilla) y las pérdidas en la negociación del mercado debido a brechas en la calidad, iv) Pérdidas reportadas en términos de desperdicio (pérdida total) o pérdida de valor de mercado (pérdida de calidad) en los procesos agrícolas precosecha-cosecha-postcosecha. v) Una evaluación cualitativa del paquete tecnológico utilizado por el grupo de tratamiento de productores.
"

	uri <- "doi:10.21223/AWMXVT"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "treatment",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.45
	)

	f <- ff[grepl("^01_DataSet", basename(ff))]
	who <- carobiner::read.excel(f, sheet="1. DATOS PRODUCTOR")
	prod <- carobiner::read.excel(f, sheet="3. DATOS PRODUCCION Y CALIDAD")
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z)] <- NA_character_
		z
	}

	d <- data.frame(
		hhid = txt(who$unique_id),
		treatment = txt(who$tratamiento),
		country = "Ecuador",
		adm1 = txt(who$provincia),
		adm2 = txt(who$canton),
		adm3 = txt(who$parroquia),
		location = txt(who$recinto),
		elevation = as.numeric(who[["gps-Altitude"]]),
		crop = "potato",
		stringsAsFactors = FALSE
	)
	q <- data.frame(
		hhid = txt(prod$unique_id),
		area_ha_ = as.numeric(prod$quant_ha_M03_01_3_1),
		production_quintals_ = as.numeric(prod$quant_quint_M3a_01A7_1),
		stringsAsFactors = FALSE
	)
	d <- merge(d, q, by="hhid", all.x=TRUE)
	d$geo_from_source <- FALSE
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- "Carchi_potato_loss_2018"
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
