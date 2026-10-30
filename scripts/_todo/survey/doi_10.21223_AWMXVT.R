# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Value food looses at production level in potato value chain of Carchi-Ecuador

La medición de las pérdidas de alimentos (cantidad y valor) y sus causas a lo largo de la cadena de valor puede proporcionar información para mejorar su eficiencia mediante el diseño y la implementación de cambios tecnológicos / institucionales. Esto podría apoyar el aumento de la disponibilidad de alimentos al reducir las pérdidas y desperdicio en los puntos críticos donde ocurren, y así reducir el uso de insumos agrícolas y la presión sobre la tierra. En la cadena de valor de la papa de Ecuador, con origen en la zona de producción de la provincia de Carchi, y cuya producción de papa fresca sirve a los consumidores finales de la ciudad de Quito y a las empresas de transformación, el CIP realizó un estudio en el año 2016. Este estudio fue liderado por el programa PIM (Política, Institución y Mercados), cuyo propósito era desarrollar un marco metodológico para medir las pérdidas de valor y sus causas a lo largo de cadenas alimentarias en los países en desarrollo mediante la prueba de diferentes modelos. Un hallazgo importante del caso de estudio particular de Ecuador mostró que las pérdidas en esta cadena alimentaria son absorbidas principalmente por los productores en etapas previas a la cosecha, siendo una de las principales causas de riesgo de pérdida el ataque del tizón tardío de la papa. Estos resultados, finalmente motivaron la implementación de un estudio piloto (Randomized Control Trial) en el año 2018 con la misma muestra de productores que fueron estudiados el 2016, para investigar el efecto de la implementación de la 'Herramienta de apoyo a la decisión (HAD) para el manejo integrado del tizón tardío de la papa'. Esta base de datos muestra los datos recopilados utilizando la misma encuesta que se aplicó inicialmente en el año 2016, aplicada en 2018 a la muestra de productores que participaron en el estudio piloto de uso de la HDA. La encuesta incluye cinco secciones que recogieron los siguientes datos: i) Caracterización de la muestra de productores en términos de variables socioeconómicas y geográficas básicas ii) Opciones agronómicas en etapas previas a la cosecha en términos de tipos de insumos y su valor, iii) La cantidad total de papa producida y su calidad en términos de categorías de mercado y daños a la calidad; Registros de producción de papa en la misma parcela y la misma variedad: peor y mejor campaña; el destino final del producto (consumo doméstico, mercado, alimento para animales, semilla) y las pérdidas en la negociación del mercado debido a brechas en la calidad, iv) Pérdidas reportadas en términos de desperdicio (pérdida total) o pérdida de valor de mercado (pérdida de calidad) en los procesos agrícolas precosecha-cosecha-postcosecha. v) Una evaluación cualitativa del paquete tecnológico utilizado por el grupo de tratamiento de productores.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/AWMXVT"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Oficina Para Estudio del Agro CIA. Ltda.",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Potato producing households; universe: Potato aproducing households from the randomized control trial that tested DST for late blight management between 2017 and 2018; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: We randomly selected seven cantones, Tulcan, Huaca, Espejo, Montufar, San Pedro de Huaca, Bolivar, and Mira, in the Carchi province. Across the 6 cantones, we randomly selected parroquias, and sampled 265 households from the 300 households that participated in the randomized control trial to test DST technology to manage late blight in potato crop.; collectionMode: "Enumerators visited selected random households in each canton and collected the information required in the five-section questionnaires regarding agronomic management variables to characterize farmers´ production systems.  The set included variables such as pest and diseases pressure, technical assistance, amount of fertilizers and pesticides applied, seed quality, irrigation methods, all of which could have influenced the effect of the DST on pre-harvest losses. The survey was applied through tablets using the software “ Survey CTO” for a programed data collection and automatic tabulation, seeking the reduction of typing errors and keeping internal consistency of answers. 19.	Enumerators used Graphic materials to help producers to interpret questions correctly and avoid confusion and misleading answers."; researchInstrument: Structured household survey questionnaire",
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "01_DataSet - Perdidas precosecha - Uso DST tizón.xlsx"]
	f2 <- ff[basename(ff) == "02_Datadictionary_Perdidas_Precosecha_Uso DST Tizon.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="DATA DICTIONARY-antes de elimna")
	r1b <- carobiner::read.excel(f1, sheet="DICTIONARIO DE SECCIONES")
	r1c <- carobiner::read.excel(f1, sheet="1. DATOS PRODUCTOR")
	r1d <- carobiner::read.excel(f1, sheet="2. DATOS AGRONOMICOS PARCELA")
	r1e <- carobiner::read.excel(f1, sheet="3. DATOS PRODUCCION Y CALIDAD")
	r1f <- carobiner::read.excel(f1, sheet="4. DATOS PERCEPCION PERDIDA")
	r1g <- carobiner::read.excel(f1, sheet="5. DATOS HERRAMIENTA TIZON")
	r2a <- carobiner::read.excel(f2, sheet="1. DATOS PRODUCTOR")
	r2b <- carobiner::read.excel(f2, sheet="2. DATOS AGRONOMICOS PARCELA")
	r2c <- carobiner::read.excel(f2, sheet="3. DATOS PRODUCCION Y CALIDAD")
	r2d <- carobiner::read.excel(f2, sheet="4. DATOS PERCEPCION PERDIDAS ")
	r2e <- carobiner::read.excel(f2, sheet="5. DATOS HERRAMIENTA TIZON")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame()
##r1a: "Ecuador_Prod18", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18"


	d1b <- data.frame()
##r1b: "...1", "...2", "...3", "numero de variables principales", "opción multiple"


	d1c <- data.frame(
		adm1 = carobiner::fix_name(r1c[["encuestador"]], "title"),
		adm2 = carobiner::fix_name(r1c[["provincia"]], "title"),
		elevation = r1c[["gps-Altitude"]]
	)
##r1c: "prov_id", "canton", "parroquia", "recinto", "unique_id", "tratamiento", "M01_03", "M01_04", "M01_05", "nivel_estudios", "ano_estudios", "M01_11_1", "M01_11_2", "M01_11_3", "M01_11_4", "M01_11_5", "M01_11_6", "M01_11_7", "M01_11_8", "M01_11_9", "M01_11_10", "M01_11_11", "M01_11_12", "M01_11_o", "M01_11B1q", "M01_11B2q", "M01_11B3q", "M01_11B4q", "M01_11B5q", "M01_11B6q", "M01_11B7q", "M01_11B8q", "M01_11B9q", "M01_11B10q", "M01_11B11q", "M01_11B1", "M01_11B2", "M01_11B3", "M01_11B4", "M01_11B5", "M01_11B6", "M01_11B7", "M01_11B8", "M01_11B9", "M01_11B10", "M01_11B11", "M02_01a", "estado_animo", "calidad_informacion", "comentarios"


	d1d <- data.frame(
		variety = r1d[["variedades_fertilizantes_count_1_1"]]
	)
##r1d: "unique_id", "tratamiento", "M02_02B_unidad1", "M02_02B_unidad1_string", "M02_02B_conversion", "M02_02B", "quant_ha_M02_02B", "M02_03_1a_1", "calc_var6_1", "M02_03_4_unidad_1", "M02_03_4_unidad_string_1", "M02_03_4_conversion_1", "M02_03_4_1", "quant_quint_M02_03_4_1", "M02_03_4p_1", "price_per_quintal_M02_03_4p_1", "M02_03_5_1", "M02_03_6_unidad5_1", "M02_03_6_unidad5_string_1", "M02_03_6_conversion_1", "M02_03_6_1", "quant_cm_M02_03_6_1", "M02_03_7_unidad5_1", "M02_03_7_unidad5_string_1", "M02_03_7_conversion_1", "M02_03_7_1", "quant_cm_M02_03_7_1", "M02_04_1", "M02_05a_1_1", "M02_05a_2_1", "M02_05a_3_1", "M02_05a_4_1", "M02_05a_5_1", "M02_05a_6_1", "M02_05a_7_1", "M02_05a_8_1", "M02_05a_9_1", "M02_05a_10_1", "M02_05a_11_1", "M02_05a_12_1", "M02_05a_o_1", "M2_06a_1", "M2_06b_1", "M02_07a_1", "M02_07b1_1", "M02_07b2_1", "M2_07_1", "M2_08_1_1", "M2_08_2_1", "M2_08_3_1", "M2_08aa_1", "M2_08a_1", "M2_08b_1", "M2_09_1", "fertilizaciones_1_1", "fertilizaciones_2_1", "fertilizaciones_3_1", "calc_var22_1", "calc_var52_1_1", "calc_var62_1_1", "M2_10_a_1_1_1", "M2_10_a_2_1_1", "M2_10_a_3_1_1", "M2_10_a_4_1_1", "M2_10_a_5_1_1", "M2_10_a_6_1_1", "M2_10_a_7_1_1", "M2_10_a_8_1_1", "M2_10_a_9_1_1", "M2_10_a_10_1_1", "M2_10_a_11_1_1", "M2_10_a_12_1_1", "M2_10_a_13_1_1", "M2_10_a_14_1_1", "M2_10_a_15_1_1", "M2_10_a_16_1_1", "M2_10_a_17_1_1", "M2_10_a_18_1_1", "M2_10_a_19_1_1", "M2_10_a_o_1_1", "calc_var21_1_1", "calc_var61_1_1_1", "M2_10_d_1_1_1", "M02_10_c_unidad3_1_1_1", "M02_10_c_unidad3_string_1_1_1", "M02_10_c_conversion_1_1_1", "M02_10_c_1_1_1", "quant_kg_M02_10_c_1_1_1", "M02_10_p_1_1_1", "price_per_kilo_M02_10_p_1_1_1", "calc_var51_1_1_2", "calc_var61_1_1_2", "M2_10_d_1_1_2", "M02_10_c_unidad3_1_1_2", "M02_10_c_unidad3_string_1_1_2", "M02_10_c_conversion_1_1_2", "M02_10_c_1_1_2", "quant_kg_M02_10_c_1_1_2", "M02_10_p_1_1_2", "price_per_kilo_M02_10_p_1_1_2", "calc_var51_1_1_3", "calc_var61_1_1_3", "M2_10_d_1_1_3", "M02_10_c_unidad3_1_1_3", "M02_10_c_unidad3_string_1_1_3", "M02_10_c_conversion_1_1_3", "M02_10_c_1_1_3", "quant_kg_M02_10_c_1_1_3", "M02_10_p_1_1_3", "price_per_kilo_M02_10_p_1_1_3", "calc_var51_1_1_4", "calc_var61_1_1_4", "M2_10_d_1_1_4", "M02_10_c_unidad3_1_1_4", "M02_10_c_unidad3_string_1_1_4", "M02_10_c_conversion_1_1_4", "M02_10_c_1_1_4", "quant_kg_M02_10_c_1_1_4", "M02_10_p_1_1_4", "price_per_kilo_M02_10_p_1_1_4", "calc_var51_1_1_5", "calc_var61_1_1_5", "M2_10_d_1_1_5", "M02_10_c_unidad3_1_1_5", "M02_10_c_unidad3_string_1_1_5", "M02_10_c_conversion_1_1_5", "M02_10_c_1_1_5", "quant_kg_M02_10_c_1_1_5", "M02_10_p_1_1_5", "price_per_kilo_M02_10_p_1_1_5", "calc_var52_1_2", "calc_var62_1_2", "M2_10_a_1_1_2", "M2_10_a_2_1_2", "M2_10_a_3_1_2", "M2_10_a_4_1_2", "M2_10_a_5_1_2", "M2_10_a_6_1_2", "M2_10_a_7_1_2", "M2_10_a_8_1_2", "M2_10_a_9_1_2", "M2_10_a_10_1_2", "M2_10_a_11_1_2", "M2_10_a_12_1_2", "M2_10_a_13_1_2", "M2_10_a_14_1_2", "M2_10_a_15_1_2", "M2_10_a_16_1_2", "M2_10_a_17_1_2", "M2_10_a_18_1_2", "M2_10_a_19_1_2", "M2_10_a_o_1_2", "calc_var21_1_2", "calc_var51_1_2_1", "calc_var61_1_2_1", "M2_10_d_1_2_1", "M02_10_c_unidad3_1_2_1", "M02_10_c_unidad3_string_1_2_1", "M02_10_c_conversion_1_2_1", "M02_10_c_1_2_1", "quant_kg_M02_10_c_1_2_1", "M02_10_p_1_2_1", "price_per_kilo_M02_10_p_1_2_1", "calc_var51_1_2_2", "calc_var61_1_2_2", "M2_10_d_1_2_2", "M02_10_c_unidad3_1_2_2", "M02_10_c_unidad3_string_1_2_2", "M02_10_c_conversion_1_2_2", "M02_10_c_1_2_2", "quant_kg_M02_10_c_1_2_2", "M02_10_p_1_2_2", "price_per_kilo_M02_10_p_1_2_2", "calc_var51_1_2_3", "calc_var61_1_2_3", "M2_10_d_1_2_3", "M02_10_c_unidad3_1_2_3", "M02_10_c_unidad3_string_1_2_3", "M02_10_c_conversion_1_2_3", "M02_10_c_1_2_3", "quant_kg_M02_10_c_1_2_3", "M02_10_p_1_2_3", "price_per_kilo_M02_10_p_1_2_3", "calc_var52_1_3", "calc_var62_1_3", "M2_10_a_1_1_3", "M2_10_a_2_1_3", "M2_10_a_3_1_3", "M2_10_a_4_1_3", "M2_10_a_5_1_3", "M2_10_a_6_1_3", "M2_10_a_7_1_3", "M2_10_a_8_1_3", "M2_10_a_9_1_3", "M2_10_a_10_1_3", "M2_10_a_11_1_3", "M2_10_a_12_1_3", "M2_10_a_13_1_3", "M2_10_a_14_1_3", "M2_10_a_15_1_3", "M2_10_a_16_1_3", "M2_10_a_17_1_3", "M2_10_a_18_1_3", "M2_10_a_19_1_3", "M2_10_a_o_1_3", "calc_var21_1_3", "calc_var51_1_3_1", "calc_var61_1_3_1", "M2_10_d_1_3_1", "M02_10_c_unidad3_1_3_1", "M02_10_c_unidad3_string_1_3_1", "M02_10_c_conversion_1_3_1", "M02_10_c_1_3_1", "quant_kg_M02_10_c_1_3_1", "M02_10_p_1_3_1", "price_per_kilo_M02_10_p_1_3_1", "problemas_1_1", "problemas_2_1", "problemas_3_1", "insectos_1_1", "insectos_2_1", "insectos_3_1", "insectos_4_1", "insectos_5_1", "insectos_6_1", "insectos_7_1", "insectos_8_1", "insectos_9_1", "insectos_10_1", "insectos_11_1", "insectos_o_1", "M2_11_h1a_1", "M2_11_h1b_1", "M2_11_h1c_1", "M2_11_h1d_1", "M2_11_h1e_1", "M2_11_h1k_1", "M2_11_h1l_1", "M2_11_h1m_1", "insecticida_1_1", "insecticida_2_1", "insecticida_3_1", "insecticida_4_1", "insecticida_5_1", "insecticida_6_1", "insecticida_7_1", "insecticida_8_1", "insecticida_9_1", "insecticida_10_1", "insecticida_11_1", "insecticida_12_1", "insecticida_13_1", "insecticida_14_1", "insecticida_15_1", "insecticida_16_1", "insecticida_17_1", "insecticida_18_1", "insecticida_19_1", "insecticida_20_1", "insecticida_o_1", "calc_var24_1", "variedades_insecticida_count_1", "calc_var54_1_1", "calc_var64_1_1", "M2_11_c1_1_1", "dias_insecticida_1_1", "M2_11_f1_1_1", "M2_11_g1_1_1", "M2_11_g1p_1_1", "calc_var54_1_2", "calc_var64_1_2", "M2_11_c1_1_2", "dias_insecticida_1_2", "M2_11_f1_1_2", "M2_11_g1_1_2", "M2_11_g1p_1_2", "calc_var54_1_3", "calc_var64_1_3", "M2_11_c1_1_3", "dias_insecticida_1_3", "M2_11_f1_1_3", "M2_11_g1_1_3", "M2_11_g1p_1_3", "calc_var54_1_4", "calc_var64_1_4", "M2_11_c1_1_4", "dias_insecticida_1_4", "M2_11_f1_1_4", "M2_11_g1_1_4", "M2_11_g1p_1_4", "enfermedades_1_1", "enfermedades_2_1", "enfermedades_3_1", "enfermedades_4_1", "enfermedades_5_1", "enfermedades_6_1", "enfermedades_7_1", "enfermedades_8_1", "enfermedades_9_1", "enfermedades_10_1", "enfermedades_11_1", "enfermedades_12_1", "enfermedades_13_1", "enfermedades_14_1", "enfermedades_15_1", "enfermedades_16_1", "enfermedades_o_1", "M2_11_h2a_1", "M2_11_h2b_1", "M2_11_h2c_1", "M2_11_h2d_1", "M2_11_h2e_1", "M2_11_h2f_1", "M2_11_h2g_1", "M2_11_h2h_1", "M2_11_h2i_1", "M2_11_h2j_1", "M2_11_h2k_1", "M2_11_h2l_1", "M2_11_h2m_1", "M2_11_h2n_1", "M2_11_h2o_1", "M2_11_h2p_1", "fungicida_1_1", "fungicida_2_1", "fungicida_3_1", "fungicida_4_1", "fungicida_5_1", "fungicida_6_1", "fungicida_7_1", "fungicida_8_1", "fungicida_9_1", "fungicida_10_1", "fungicida_11_1", "fungicida_12_1", "fungicida_13_1", "fungicida_14_1", "fungicida_15_1", "fungicida_16_1", "fungicida_17_1", "fungicida_18_1", "fungicida_19_1", "fungicida_20_1", "fungicida_21_1", "fungicida_22_1", "fungicida_23_1", "fungicida_24_1", "fungicida_25_1", "fungicida_26_1", "fungicida_27_1", "fungicida_28_1", "fungicida_29_1", "fungicida_30_1", "fungicida_31_1", "fungicida_32_1", "fungicida_33_1", "fungicida_34_1", "fungicida_35_1", "fungicida_o_1", "calc_var26_1", "calc_var56_1_1", "calc_var66_1_1", "M2_11_c2_1_1", "dias_fungicida_1_1", "M2_11_f2_1_1", "M2_11_g2_1_1", "M2_11_g2p_1_1", "calc_var56_1_2", "calc_var66_1_2", "M2_11_c2_1_2", "dias_fungicida_1_2", "M2_11_f2_1_2", "M2_11_g2_1_2", "M2_11_g2p_1_2", "calc_var56_1_3", "calc_var66_1_3", "M2_11_c2_1_3", "dias_fungicida_1_3", "M2_11_f2_1_3", "M2_11_g2_1_3", "M2_11_g2p_1_3", "calc_var56_1_4", "calc_var66_1_4", "M2_11_c2_1_4", "dias_fungicida_1_4", "M2_11_f2_1_4", "M2_11_g2_1_4", "M2_11_g2p_1_4", "calc_var56_1_5", "calc_var66_1_5", "M2_11_c2_1_5", "dias_fungicida_1_5", "M2_11_f2_1_5", "M2_11_g2_1_5", "M2_11_g2p_1_5", "calc_var56_1_6", "calc_var66_1_6", "M2_11_c2_1_6", "dias_fungicida_1_6", "M2_11_f2_1_6", "M2_11_g2_1_6", "M2_11_g2p_1_6", "herbicida_1", "herbicida_1_1", "herbicida_2_1", "herbicida_3_1", "herbicida_4_1", "herbicida_5_1", "herbicida_6_1", "herbicida_7_1", "herbicida_o_1", "variedades_herbicida_count_1", "calc_var58_1_1", "calc_var68_1_1", "M2_11_c3_1_1", "dias_herbicida_1_1", "M2_11_f3_1_1", "M2_11_g3_1_1", "M2_11_g3p_1_1", "M2_11_h3_1_1", "M2_11_c3_1_2", "dias_herbicida_1_2", "M2_11_f3_1_2", "M2_11_g3_1_2", "M2_11_g3p_1_2", "M2_11_h3_1_2", "aporque_1", "M2_13_1", "M2_14_1_1", "M2_14_2_1", "M2_14_3_1", "M2_14_4_1", "M2_14_5_1", "M2_14_6_1", "M2_14_7_1", "M2_14_8_1", "M2_14_9_1", "M2_14_10_1", "M2_14_11_1", "M2_14_12_1", "M2_14_13_1", "M2_14_o_1", "M2_15_1"


	d1e <- data.frame()
##r1e: "unique_id", "tratamiento", "M3a_01A1_1", "M3a_01A2_1", "M03_01_3_unidad1_1", "M03_01_3_unidad1_string_1", "M03_01_3_conversion_1", "M03_01_3_1", "quant_ha_M03_01_3_1", "M3a_01A7_unidad_1", "M3a_01A7_unidad_string_1", "M3a_01A7_conversion_1", "M3a_01A7_1", "quant_quint_M3a_01A7_1", "M3a_01A9_unidad_1", "M3a_01A9_unidad_string_1", "M3a_01A9_conversion_1", "M3a_01A9_1", "quant_quint_M3a_01A9_1", "M3a_01A11_unidad_1", "M3a_01A11_unidad_string_1", "M3a_01A11_conversion_1", "M3a_01A11_1", "quant_quint_M3a_01A11_1", "Produccion_total_papa_1", "M3a_01A13_1", "M3a_01A14_1", "M3a_01A15_1", "M3a_01A16_1", "M3a_01A17_1", "M3a_01A18_1", "M3a_03A_intro_1", "M3a_03A1_1", "M3a_03A2_1", "M3a_03A3_1", "M3a_03A4_1", "M3a_03A5_1", "M3a_03A6_1", "M3a_03A7_1", "M3a_03A8_1", "M3a_03A9_1", "M3a_03A10_1", "M3a_03A10_O_1", "M3a_04_1_1", "M3a_04_2_1", "M3a_04_3_1", "M3a_04_4_1", "M3a_04_5_1", "M3a_04_6_1", "M3a_04_7_1", "M3a_04_8_1", "M3a_04_O_1", "intro_M4a_05_2_1", "intro_M4a_05_3_1", "intro_M4a_05_4_1", "intro_M4a_05_5_1", "intro_M4a_05_6_1", "intro_M4a_05_7_1", "intro_M4a_05_8_1", "M4a_05B1_unidad_1", "M4a_05B1_unidad_string_1", "M4a_05B1_conversion_1", "M4a_05B1_1", "quant_quint_M4a_05B1_1", "M4a_05B2_1", "price_per_quintal_M4a_05B2_1", "M4a_05C1_unidad_1", "M4a_05C1_unidad_string_1", "M4a_05C1_conversion_1", "M4a_05C1_1", "quant_quint_M4a_05C1_1", "M4a_05C2_1", "price_per_quintal_M4a_05C2_1", "M4a_05E1_unidad_1", "M4a_05E1_unidad_string_1", "M4a_05E1_conversion_1", "M4a_05E1_1", "quant_quint_M4a_05E1_1", "M4a_05E2_1", "price_per_quintal_M4a_05E2_1", "M4a_05G1_unidad_1", "M4a_05G1_unidad_string_1", "M4a_05G1_conversion_1", "M4a_05G1_1", "quant_quint_M4a_05G1_1", "M4a_05G2_1", "price_per_quintal_M4a_05G2_1", "M4a_05H1_unidad_1", "M4a_05H1_unidad_string_1", "M4a_05H1_conversion_1", "M4a_05H1_1", "quant_quint_M4a_05H1_1", "M4a_05H2_1", "price_per_quintal_M4a_05H2_1", "M4a_06A1_1", "M4a_06A2_1", "M4a_06A3_1", "M4a_06A4_1", "M4a_06A5_1", "M4a_06A6_1", "M4a_07A_intro_1", "M4a_07A1_1", "M4a_07A2_1", "M4a_07A3_1", "M4a_07A4_1", "M4a_07A5_1", "M4a_07A6_1", "M4a_07A7_1", "M4a_07A8_1", "M4a_07A9_1", "M4a_07A10_1", "M4a_07A11_1", "M4a_07A11_O_1", "M4a_08A1_1", "M4a_08A2_1", "M4a_08A3_1", "M4a_08A4_1", "M4a_08A5_1", "M4a_08A6_1", "M4a_08A7_1", "M4a_08A8_1", "M4a_08A9_1", "M4a_08A10_1", "M4a_08A11_1", "M4a_08A11_o_1", "M4a_08B1_1", "M4a_08B2_1", "M4a_08B3_1", "M4a_08B4_1", "M4a_08B5_1", "M4a_08B6_1", "M4a_08B7_1", "M4a_08B8_1", "M4a_08B9_1", "M4a_08B10_1", "M4a_08B11_1", "M4a_09A_1", "M4a_09B_1", "intro_SellerLoop1_papa_1", "buyer_number_papa_1_1", "M5a_02_name_1_1", "M5a_02_lastname_1_1", "M5a_02A1_1_1", "m5a_02A1_o_1_1", "M5a_02B1_1_1", "M5a_02C1_1_1", "M5a_02D1b_1_1", "M5a_02F1_unidad_1_1", "M5a_02F1_unidad_string_1_1", "M5a_02F1_conversion_1_1", "M5a_02F1_1_1", "quant_quint_M5a_02F1_1_1", "M5a_02G1a1_1_1", "M5a_02G1b1_1_1", "M5a_02G1a2_1_1", "M5a_02G1b2_1_1", "precio3_M5a_02G1a3_1_1", "precio3_M5a_02G1b3_1_1", "M5a_02H1_1_1", "M5a_02I1_1_1", "M5a_02I1_O_1_1", "M5a_02J1a_1_1", "M5a_02J1b_1_1", "M5a_02J1c_1_1", "M5a_02J1d_1_1", "M5a_02J1e_1_1", "M5a_02J1f_1_1", "M5a_02J1ap_1_1", "M5a_02J1bp_1_1", "M5a_02J1cp_1_1", "M5a_02J1dp_1_1", "M5a_02J1ep_1_1", "M5a_02J1fp_1_1", "M5a_02K1_intro_1_1", "M5a_02K1a_1_1", "M5a_02K1b_1_1", "M5a_02K1c_1_1", "M5a_02K1d_1_1", "M5a_02K1e_1_1", "M5a_02K1f_1_1", "M5a_02K1g_1_1", "M5a_02K1h_1_1", "M5a_02K1i_1_1", "M5a_02K1j_1_1", "M5a_02K1k_1_1", "M5a_02K1k_O_1_1", "buyer_number_papa_1_2", "M5a_02_name_1_2", "M5a_02_lastname_1_2", "M5a_02A1_1_2", "m5a_02A1_o_1_2", "M5a_02B1_1_2", "M5a_02C1_1_2", "M5a_02D1b_1_2", "M5a_02F1_unidad_1_2", "M5a_02F1_unidad_string_1_2", "M5a_02F1_conversion_1_2", "M5a_02F1_1_2", "quant_quint_M5a_02F1_1_2", "M5a_02G1a1_1_2", "M5a_02G1b1_1_2", "M5a_02G1a2_1_2", "M5a_02G1b2_1_2", "M5a_02G1a3_1_2", "M5a_02G1b3_1_2", "M5a_02H1_1_2", "M5a_02I1_1_2", "M5a_02J1a_1_2", "M5a_02J1b_1_2", "M5a_02J1c_1_2", "M5a_02J1d_1_2", "M5a_02J1e_1_2", "M5a_02J1f_1_2", "M5a_02J1ap_1_2", "M5a_02J1bp_1_2", "M5a_02J1cp_1_2", "M5a_02J1dp_1_2", "M5a_02J1ep_1_2", "M5a_02J1fp_1_2", "M5a_02K1_intro_1_2"


	d1f <- data.frame()
##r1f: "unique_id", "tratamiento", "M3a_05_1", "M3a_06A1_1", "M3a_06A1_1_1", "M3a_06A1_2_1", "M3a_06A1_3_1", "M3a_06A1_4_1", "M3a_06A1_5_1", "M3a_06A1_6_1", "M3a_06A1_7_1", "M3a_06A1_8_1", "M3a_06A1_O_1", "M3a_06A2_unidad_1", "M3a_06A2_unidad_string_1", "M3a_06A2_conversion_1", "M3a_06A2_1", "quant_quint_M3a_06A2_1", "M3a_06A5_1", "price_per_quintal_M3a_06A5_1", "M3a_06A2_unidadp_1", "M3a_06A2_unidadp_string_1", "M3a_06A2p_conversion_1", "M3a_06A2p_1", "quant_quint_M3a_06A2p_1", "M3a_06A5p_1", "price_per_quintal_M3a_06A5p_1", "M3a_07_1", "M3a_08A4_unidad_1", "M3a_08A4_unidad_string_1", "M3a_08A5_conversion_1", "M3a_08A3_1", "quant_quint_M3a_08A5_1", "M3a_08A6_1", "price_per_quintal_M3a_08A6_1", "M3_03a_1", "M3_03a_1_1", "M3_03a_2_1", "M3_03a_3_1", "M3_03a_4_1", "M3_03a_5_1", "M3_03a_6_1", "M3_03a_7_1", "M3_03a_o_1", "M4a_01A2_m_1", "M4a_01A4_m_1", "ph_papa_etapas_1_1", "ph_papa_etapas_2_1", "ph_papa_etapas_3_1", "act_papa_cave_1_1", "act_papa_cave_2_1", "act_papa_cave_3_1", "act_papa_cave_4_1", "act_papa_cave_5_1", "act_papa_cave_6_1", "act_papa_cave_o_1", "M4a_04A1_1", "M4a_04A9a_1_1", "M4a_04A9a_2_1", "M4a_04A9a_3_1", "M4a_04A9a_4_1", "M4a_04A9a_5_1", "M4a_04A9a_6_1", "M4a_04A9a_o_1", "M4a_04A3_unidad_1", "M4a_04A3_unidad_string_1", "M4a_04A4_conversion_1", "M4a_04A2_1", "quant_quint_M4a_04A4_1", "M4a_04A5_1", "price_per_quintal_M4a_04A5_1", "M4a_04A7_unidad_1", "M4a_04A7_unidad_string_1", "M4a_04A8_conversion_1", "M4a_04A6_1", "quant_quint_M4a_04A8_1", "M4a_04A9_1", "price_per_quintal_M4a_04A9_1", "temp_papa_cave_1", "act_papa_selec_1_1", "act_papa_selec_2_1", "act_papa_selec_3_1", "act_papa_selec_4_1", "act_papa_selec_5_1", "act_papa_selec_6_1", "act_papa_selec_7_1", "act_papa_selec_o_1", "M4a_04B1_1", "M4a_04B9a_1_1", "M4a_04B9a_2_1", "M4a_04B9a_3_1", "M4a_04B9a_4_1", "M4a_04B9a_5_1", "M4a_04B9a_6_1", "M4a_04B9a_7_1", "M4a_04B9a_8_1", "M4a_04B9a_9_1", "M4a_04B9a_o_1", "M4a_04B3_unidad_1", "M4a_04B3_unidad_string_1", "M4a_04B3_conversion_1", "M4a_04B2_1", "quant_quint_M4a_04B4_1", "M4a_04B5_1", "price_per_quintal_M4a_04B5_1", "M4a_04B7_unidad_1", "M4a_04B7_unidad_string_1", "M4a_04B7_conversion_1", "M4a_04B6_1", "quant_quint_M4a_04B8_1", "M4a_04B9_1", "price_per_quintal_M4a_04B9_1", "temp_papa_clasf_papa_1", "M4a_03A_1", "M4a_03B_unidad_1", "M4a_03B_unidad_string_1", "M4a_03B_conversion_1", "M4a_03B_1", "quant_quint_M4a_03B_1", "M4a_03C_1", "M4a_03C_O_1", "M4a_03D_1", "M4a_03D_O_1", "act_papa_alm_1_1", "act_papa_alm_2_1", "act_papa_alm_3_1", "act_papa_alm_4_1", "act_papa_alm_o_1", "M4a_04C1_1", "M4a_04C3_unidad_1", "M4a_04C7_unidad_string_1...131", "M4a_04C7_conversion_1", "M4a_04C2_1", "quant_quint_M4a_04C4_1", "M4a_04C5_1", "price_per_quintal_M4a_04C5_1", "M4a_04C7_unidad_1", "M4a_04C7_unidad_string_1...138", "M4a_04C8_conversion_1", "M4a_04C6_1", "quant_quint_M4a_04C8_1", "M4a_04C9_1", "price_per_quintal_M4a_04C9_1", "M4a_04C9a_1_1", "M4a_04C9a_2_1", "M4a_04C9a_3_1", "M4a_04C9a_4_1", "M4a_04C9a_5_1", "M4a_04C9a_o_1", "temp_papa_almacenamiento_1", "perdida_total_papa_1", "M4a_05A1_unidad_1", "M4a_05A1_unidad_string_1", "M4a_05A1_conversion_1", "M4a_05A1_1", "quant_quint_M4a_05A1_1", "M4a_05A2_1", "price_per_quintal_M4a_05A2_1", "M4a_05B1_perd_1", "M4a_05B1a_unidad_1", "M4a_05B1a_unidad_string_1", "M4a_05B1a_conversion_1", "M4a_05B1a_1", "quant_quint_perd_M4a_05B1a_1", "perd_M4a_05B1a_q_1", "price_per_quintal_perd_M4a_05B1a_q_1", "M4a_05B1p_unidad_1", "M4a_05B1p_unidad_string_1", "M4a_05B1p_conversion_1", "M4a_05B1p_1", "quant_quint_perd_M4a_05B1p_1", "perd_M4a_05B1p_q_1", "M4a_05C1_perd_1", "M4a_05C1a_unidad_1", "M4a_05C1a_unidad_string_1", "M4a_05C1a_conversion_1", "M4a_05C1a_1", "quant_quint_perd_M4a_05C1a_1", "perd_M4a_05C1a_q_1", "price_per_quintal_perd_M4a_05C1a_q_1", "M4a_05C1p_unidad_1", "M4a_05C1p_unidad_string_1", "M4a_05C1p_conversion_1", "M4a_05C1p_1", "quant_quint_perd_M4a_05C1p_1", "perd_M4a_05C1p_q_1", "price_per_quintal_perd_M4a_05C1p_q_1", "M4a_05E1_perd_1", "M4a_05G1_perd_1", "M4a_05G1a_unidad_1", "M4a_05G1a_unidad_string_1", "M4a_05G1a_conversion_1", "M4a_05G1a_1", "quant_quint_perd_M4a_05G1a_1", "perd_M4a_05G1a_q_1", "price_per_quintal_perd_M4a_05G1a_q_1", "M4a_05G1p_unidad_1", "M4a_05G1p_unidad_string_1", "M4a_05G1p_conversion_1", "M4a_05G1p_1", "quant_quint_perd_M4a_05G1p_1", "perd_M4a_05G1p_q_1", "price_per_quintal_perd_M4a_05G1p_q_1", "M4a_05H1_perd_1", "M4a_05H1a_unidad_1", "M4a_05H1a_unidad_string_1", "M4a_05H1a_conversion_1", "M4a_05H1a_1", "quant_quint_perd_M4a_05H1a_1", "perd_M4a_05H1a_q_1", "price_per_quintal_perd_M4a_05H1a_q_1", "M4a_05H1p_unidad_1", "M4a_05H1p_unidad_string_1", "M4a_05H1p_conversion_1", "M4a_05H1p_1", "quant_quint_perd_M4a_05H1p_1", "perd_M4a_05H1p_q_1", "price_per_quintal_perd_M4a_05H1p_q_1"


	d1g <- data.frame()
##r1g: "unique_id", "tratamiento", "riesgo_rueda", "prod_rueda", "uso_rueda", "costo_rueda", "salud_rueda", "fung_rueda", "apl_rueda", "ventajas_rueda", "venta_rueda", "facil_uso", "dif_rueda", "alm_rueda", "conv_rueda", "cont_rueda", "asist_lancha", "asist_lancha_1", "asist_lancha_2", "asist_lancha_3", "asist_lancha_4", "asist_lancha_5", "ord_asist1", "ord_asist2", "ord_asist3", "tranq_rueda", "cap_lancha"


	d2a <- data.frame()
##r2a: "Value food looses at production level in potato value chain of Carchi-Ecuador 2019-2020", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2b <- data.frame()
##r2b: "Value food looses at production level in potato value chain of Carchi-Ecuador 2019-2020", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2c <- data.frame()
##r2c: "Value food looses at production level in potato value chain of Carchi-Ecuador 2019-2020", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2d <- data.frame()
##r2d: "Value food looses at production level in potato value chain of Carchi-Ecuador 2019-2020", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d2e <- data.frame()
##r2e: "Value food looses at production level in potato value chain of Carchi-Ecuador 2019-2020", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

