# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Potential new varieties of biofortified potato with high iron and zinc content, to contribute to food and nutritional security in high Andean family farming.

In total 30 tetraploid BIOT clones and 4 commercial varieties were evaluated in multi-location trials in nine field experiments, in five departments of Peru using participatory varietal selection. Trials were planted according to local practices using 40 plants per plot with a randomized complete block design with 3 replications at each location. The plot size was 9.72m2, with 4 rows of 2.7m long and 0.9m between rows. Nitrogen, Phosphorus, Potassium (NPK) (180-180-160) and organic amendment ('guano de corral') was applied before planting. Soil samples of 1 kg each were collected before planting and sent to the agricultural chemistry laboratory Valle Grande – Cañete for soil, salinity and micronutrient content analysis. Participatory varietal selection (PVS) evaluations were conducted following the protocol 'Participatory Varietal Selection of Potato Using the Mother & Baby Trial Design: A Gender-responsive Trainer's Guide'. The data was collected using Highly Interactive Data Analysis Platform, i.e. HIDAP, software. Evaluations at the flowering, harvest and post-harvest (storage) phase were performed. Farmers identified and ranked characteristics of potato that they considered important, and afterwards they selected the genotype they preferred. Each farmer's varietal selection and the criteria for selection were recorded.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/N11QJS"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = NA,
		
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
	

	f1 <- ff[basename(ff) == "00 Lista Tablas y Figuras.xlsx"]
	f2 <- ff[basename(ff) == "Tabla 01. Lista de las muestras de suelos.xlsx"]
	f3 <- ff[basename(ff) == "Tabla 01a.- Análisis de suelos de 9 localidades del Perú.xlsx"]
	f4 <- ff[basename(ff) == "Tabla 02. Organigrama de trabajo para el monitoreo.xlsx"]
	f5 <- ff[basename(ff) == "Tabla 03  Fechas de siembra, aporque  y evaluaciones realizadas en los experimetos de campo instalados en 10 localidades de la Sierra del Perú.xlsx"]
	f6 <- ff[basename(ff) == "Tabla 04  Kit-materiales-PVS.xlsx"]
	f7 <- ff[basename(ff) == "Tabla 05, 5.1, 5.2 y 5.3 Resultados de las votaciones por los criterios de selecci n en la etapa de la floracion.xlsx"]
	f8 <- ff[basename(ff) == "Tabla 06 Selección de los clones preferidos por parcela en la etapa de la floración.xlsx"]
	f9 <- ff[basename(ff) == "Tabla 07, 7.1,  7.2 y 7.3 Resultados de las votaciones por los criterios de selecci n en la etapa de Cosecha.xlsx"]
	f10 <- ff[basename(ff) == "Tabla 08 Selección de los clones preferidos por parcela en la etapa de cosecha..xlsx"]
	f11 <- ff[basename(ff) == "Tabla 09 Cartilla de evaluación de pruebas organolépticas en campo.xlsx"]
	f12 <- ff[basename(ff) == "Tabla 10 Cartilla de evaluación de pruebas organolépticas en la estación CIP- La Molina.xlsx"]
	f13 <- ff[basename(ff) == "Tabla 11 Cronograma de envío de muestras liofilizadas para análisis de minerales en la Universidad de Nottingham.xlsx"]
	f14 <- ff[basename(ff) == "Tabla 12 Los resultados del porcentaje de materia seca, color de hojuelas, contenido de azúcares, análisis organolépticos en laboratorio.xlsx"]
	f15 <- ff[basename(ff) == "Tabla 13 Arreglo Evaluación organoléptica en el laboratorio de procesamiento del CIP.xlsx"]
	f16 <- ff[basename(ff) == "Tabla 14 y 15 Resultados Correlaciones significativas,  intervalos de confianza, analisis de varianza.xlsx"]
	f17 <- ff[basename(ff) == "Tabla 16  Selección de 12 clones biofortificados tetraploides para ser evaluados en los  ensayos .xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7a <- carobiner::read.excel(f7, sheet="Tabla5_Resultados_globales")
	r7b <- carobiner::read.excel(f7, sheet="Tabla 5.1_Sierra_Norte")
	r7c <- carobiner::read.excel(f7, sheet="Tabla 5.2_Sierra_Central")
	r7d <- carobiner::read.excel(f7, sheet="Tabla 5.3_Sierra_Sur")
	r7e <- carobiner::read.excel(f7, sheet="Todas-localidades")
	r7f <- carobiner::read.excel(f7, sheet="resumen_region")
	r8 <- carobiner::read.excel(f8)
	r9a <- carobiner::read.excel(f9, sheet="Tabla7_ResultadosGlobales")
	r9b <- carobiner::read.excel(f9, sheet="Tabla 7.1 SierraNorte")
	r9c <- carobiner::read.excel(f9, sheet="Tabla 7.2 SierraCentral")
	r9d <- carobiner::read.excel(f9, sheet="Tabla 7.3 SierraSur")
	r9e <- carobiner::read.excel(f9, sheet="Resumen_region")
	r9f <- carobiner::read.excel(f9, sheet="Todas_localidades")
	r10 <- carobiner::read.excel(f10)
	r11 <- carobiner::read.excel(f11)
	r12 <- carobiner::read.excel(f12)
	r13 <- carobiner::read.excel(f13)
	r14a <- carobiner::read.excel(f14, sheet="Tabla 12_data")
	r14b <- carobiner::read.excel(f14, sheet="Dictionary")
	r15 <- carobiner::read.excel(f15)
	r16a <- carobiner::read.excel(f16, sheet="Tabla 14")
	r16b <- carobiner::read.excel(f16, sheet="Tabla 15")
	r17 <- carobiner::read.excel(f17)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Tabla 1. Lista de las muestras de suelos enviadas para laborario de suelos"


	d2 <- data.frame(
		adm1 = carobiner::fix_name(r2[["Adm1"]], "title"),
		adm2 = carobiner::fix_name(r2[["Adm2"]], "title"),
		adm3 = carobiner::fix_name(r2[["Adm3"]], "title")
	)
##r2: "ORD", "Tipo de muestra", "Muestras Totales", "Descripción : Análisis de suelo", "Costo unitario (Referencial)", "Costo Total"


	d3 <- data.frame(
		elevation = r3[["Altitude"]],
		soil_texture = tolower(r3[["Soil texture"]]),
		soil_P = r3[["Phosphorus_ppm_mean"]],
		soil_N = r3[["Nitrogen total_%_mean"]],
		soil_K = r3[["Potassium_ppm_mean"]],
		Mg_fertilizer = r3[["Exchangeable Magnesium_mEq / 100 g_mean"]],
		K_fertilizer = r3[["Exchangeable Potassium_mEq / 100 g_mean"]],
		S_fertilizer = r3[["Sulfate_mEq / L_mean"]],
		B_fertilizer = r3[["Soluble Boron_mg/L_mean"]],
		soil_B = r3[["Boron_ppm_mean"]],
		soil_Zn = r3[["Zinc_ppm_mean"]]
	)
##r3: "Ciudad", "Locality", "Sand_%_mean", "Sand_%_sd", "Silt_%_mean", "Silt_%_sd", "Clay_%_mean", "Clay_%_sd", "Percentage of water saturation_%_mean", "Percentage of water saturation_%_sd", "CaCO3_%_mean", "Temp (E.C.)_°C_mean", "Soil Electrical conductivity (E.S)_dS/m_mean", "Soil Electrical conductivity (E.S)_dS/m_sd", "Temp (pH)_°C_mean", "Temp (pH)_°C_sd", "Soil pH__mean", "Soil pH__sd", "Phosphorus_ppm_sd", "Organic matter_%_mean", "Organic matter_%_sd", "Nitrogen total_%_sd", "Potassium_ppm_sd", "Exchangeable Calcium_mEq / 100 g_mean", "Exchangeable Calcium_mEq / 100 g_sd", "Exchangeable Magnesium_mEq / 100 g_sd", "Exchangeable Sodium_mEq / 100 g_mean", "Exchangeable Sodium_mEq / 100 g_sd", "Exchangeable Potassium_mEq / 100 g_sd", "P.A.I. (Porcentaje de acidez intercambiable)_%_mean", "P.A.I. (Porcentaje de acidez intercambiable)_%_sd", "Exchangeable Sodium Percentage_%_mean", "Exchangeable Sodium Percentage_%_sd", "Efective Cation Exchange Capacity_mEq / 100 g_mean", "Efective Cation Exchange Capacity_mEq / 100 g_sd", "Soluble Chloride_mg/L_mean", "Soluble Chloride_mg/L_sd", "Sulfate_mEq / L_sd", "Nitrate_mEq / L_mean", "Nitrate_mEq / L_sd", "Carbonate_mEq / L_mean", "Bicarbonate_mEq / L_mean", "Bicarbonate_mEq / L_sd", "Soluble Calcium_mEq / L_mean", "Soluble Calcium_mEq / L_sd", "Soluble Magnesium_mEq / L_mean", "Soluble Magnesium_mEq / L_sd", "Soluble Sodium_mEq / L_mean", "Soluble Sodium_mEq / L_sd", "Soluble Potasium_mEq / L_mean", "Soluble Potasium_mEq / L_sd", "Soluble Boron_mg/L_sd", "Boron_ppm_sd", "Copper_ppm_mean", "Copper_ppm_sd", "Zinc_ppm_sd", "Manganese_ppm_mean", "Manganese_ppm_sd", "Iron_ppm_mean", "Iron_ppm_sd", "Aluminium + hidrogenum_mEq / 100 g_mean", "Aluminium + hidrogenum_mEq / 100 g_sd"


	d4 <- data.frame()
##r4: "Tabla 2. Organigrama de trabajo para el monitoreo, manejo, evaluación y logística de los experimetos de campo instalados en 10 localidades de la Sierra del Perú", "...2", "...3", "...4", "...5", "...6", "...7", "...8"


	d5 <- data.frame()
##r5: "Tabla 3.  Fechas de siembra, aporque  y evaluaciones realizadas en los experimetos de campo instalados en 10 localidades de la Sierra del Perú", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "Prueba de Dormancia y Brotamiento", "...13", "...14", "...15", "...16"


	d6 <- data.frame()
##r6: "Tabla 4.  KIT-MATERIALES-PVS", "...2"


	d7a <- data.frame()
##r7a: "Orden", "Grupo de Variables", "Criterios de seleccion", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Orden de importancia por criterios", "Puntaje Global por grupo de criterios"


	d7b <- data.frame(
		adm1 = carobiner::fix_name(r7b[["Region"]], "title")
	)
##r7b: "Orden", "Grupo de Variables", "Criterios de seleccion", "Ciudad", "Localidad", "Altitud", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Coeficiente de correlacion entre los criterios de selección de hombres y mujeres"


	d7c <- data.frame(
		adm1 = carobiner::fix_name(r7c[["Region"]], "title")
	)
##r7c: "Orden", "Grupo de Variables", "Criterios de seleccion", "Ciudad", "Localidad", "Altitud", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Coeficiente de correlacion entre los criterios de selección de hombres y mujeres"


	d7d <- data.frame(
		adm1 = carobiner::fix_name(r7d[["Region"]], "title")
	)
##r7d: "Orden", "Grupo de Variables", "Criterios de seleccion", "Ciudad", "Localidad", "Altitud", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Coeficiente de correlacion entre los criterios de selección de hombres y mujeres"


	d7e <- data.frame(
		adm1 = carobiner::fix_name(r7e[["Region"]], "title")
	)
##r7e: "Orden", "Grupo de Variables", "Criterios de seleccion", "Ciudad", "Localidad", "Altitud", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global"


	d7f <- data.frame(
		adm1 = carobiner::fix_name(r7f[["Region"]], "title")
	)
##r7f: "Orden", "Grupo de Variables", "number", "Criterios de seleccion", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Puntaje Global por grupo de criterios", "Orden de importancia por criterios", "Coeficiente de correlacion entre los criterios de selección de hombres y mujeres"


	d8 <- data.frame(
		variety = r8[["Tabla 6: Selección de los clones preferidos por parcela en la etapa de la floración."]]
	)
##r8: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21"


	d9a <- data.frame()
##r9a: "Orden", "Grupo de Variables", "Criterios de seleccion", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Orden de importancia por criterios", "Puntaje Global por grupo de criterios"


	d9b <- data.frame(
		adm1 = carobiner::fix_name(r9b[["Region"]], "title")
	)
##r9b: "Orden", "Grupo de Variables", "Criterios de seleccion", "Ciudad", "Localidad", "Altitud", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Coeficiente de correlación entre los criterios de selección de Hombre y Mujeres"


	d9c <- data.frame(
		adm1 = carobiner::fix_name(r9c[["Region"]], "title")
	)
##r9c: "Orden", "Grupo de Variables", "Criterios de seleccion", "Ciudad", "Localidad", "Altitud", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Coeficiente de correlación entre los criterios de selección de Hombre y Mujeres"


	d9d <- data.frame(
		adm1 = carobiner::fix_name(r9d[["Region"]], "title")
	)
##r9d: "Orden", "Grupo de Variables", "Criterios de seleccion", "Ciudad", "Localidad", "Altitud", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Coeficiente de correlación entre los criterios de selección de Hombre y Mujeres"


	d9e <- data.frame(
		adm1 = carobiner::fix_name(r9e[["Region"]], "title")
	)
##r9e: "Orden", "Grupo de Variables", "Criterios de seleccion", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global", "Puntaje Global por grupo de criterios", "Orden de importancia por criterios", "Coeficiente de correlación entre los criterios de selección de Hombre y Mujeres"


	d9f <- data.frame(
		adm1 = carobiner::fix_name(r9f[["Region"]], "title")
	)
##r9f: "#", "Grupo", "Criterios", "Ciudad", "Localidad", "Altitud", "Puntaje Hombres (N° granos Maíz)", "Puntaje Mujeres (N° granos Habas)", "Puntaje Global"


	d10 <- data.frame(
		variety = r10[["Tabla 8: Selección de los clones preferidos por parcela en la etapa de cosecha."]]
	)
##r10: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21"


	d11 <- data.frame()
##r11: "Tabla 9: Cartilla de evaluación de pruebas organolépticas en campo", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11"


	d12 <- data.frame()
##r12: "Tabla 10: Cartilla de evaluación de pruebas organolépticas en la estación CIP- La Molina", "...2", "...3"


	d13 <- data.frame()
##r13: "Tabla 11: Cronograma de envío de muestras liofilizadas para análisis de minerales en la Universidad de Nottingham", "...2", "...3", "...4", "...5", "...6", "...7"


	d14a <- data.frame(
		yield = r14a[["Yield"]],
		N_fertilizer = r14a[["n_FEDW"]]
	)
##r14a: "ORD", "INSTN", "BREEDERCODE", "Flowering", "Female", "Male", "Harvest", "Organoleptic", "Mean_FEDW", "Sd_FEDW", "Min_FEDW", "Max_FEDW", "n_ZNDW", "Mean_ZNDW", "Sd_ZNDW", "Min_ZNDW", "Max_ZNDW", "n_ASC_FW", "Mean_ASC_FW", "Sd_ASC_FW", "Min_ASC_FW", "Max_ASC_FW", "n_TTYNA", "Mean_TTYNA", "Sd_TTYNA", "Min_TTYNA", "Max_TTYNA", "n_DM", "Mean_DM", "Sd_DM", "Min_DM", "Max_DM", "n_Tub_Size", "Mean_Tub_Size", "Sd_Tub_Size", "Min_Tub_Size", "Max_Tub_Size", "n_Tuber_Apper", "Mean_Tuber_Apper", "Sd_Tuber_Apper", "Min_Tuber_Apper", "Max_Tuber_Apper", "n_PGLU", "Mean_PGLU", "Sd_PGLU", "Min_PGLU", "Max_PGLU", "n_AGLU", "Prom_AGLU", "Sd_AGLU", "Min_AGLU", "Max_AGLU", "n_CHIPPING", "Mean_CHIPPING", "Sd_CHIPPING", "Min_CHIPPING", "Max_CHIPPING", "n_FLABOIL", "Mean_FLABOIL", "Sd_FLABOIL", "Min_FLABOIL", "Max_FLABOIL", "n", "Mean_TEXTABL", "Sd_TEXTABL", "Min_TEXTABL", "Max_TEXTABL", "n_DAR", "Mean_DAR", "Sd_DAR", "Min_DAR", "Max_DAR", "n_STFLA", "Mean_STFLA", "Sd_STFLA", "Min_STFLA", "Max_STFLA", "n_Tgly_FW", "Mean_Tgly_FW", "Sd_Tgly_FW", "Min_Tgly_FW", "Max_Tgly_FW"


	d14b <- data.frame(
		crop = tolower(r14b[["Crop"]])
	)
##r14b: "Name_spanish", "Other", "Variable synonyms", "Variable ID", "Variable name", "Variable label", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref", "Category 1", "Category 2", "Category 3", "Category 4", "Category 5", "Category 6", "Category 7", "Category 8", "Category 9", "Category 10"


	d15 <- data.frame()
##r15: "Tabla 13 .- Arreglo Evaluación organoléptica en el laboratorio de procesamiento del CIP", "...2", "...3", "...4", "...5", "...6", "...7"


	d16a <- data.frame()
##r16a: "Tabla 14.-  Correlaciones significativas  e  intervalos de confianza", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10"


	d16b <- data.frame(
		Zn_fertilizer = r16b[["Tabla 15.- Analisis de varianza y coeficientes de variación estimada para rendimiento total de tubérculo (tn.ha-1),  contenido de hierro (mg Kg-1) BS y contenido de zinc  (mg Kg-1) BS"]]
	)
##r16b: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13"


	d17 <- data.frame(
		N_fertilizer = r17[["n_FEDW"]]
	)
##r17: "ORD", "INSTN", "BREEDER CODE", "Area de Adaptación", "Area2", "Female", "Male", "Mean_FEDW", "Sd_FEDW", "Min_FEDW", "Max_FEDW", "n_ZNDW", "Mean_ZNDW", "Sd_ZNDW", "Min_ZNDW", "Max_ZNDW", "n_ASC_FW", "Mean_ASC_FW", "Sd_ASC_FW", "Min_ASC_FW", "Max_ASC_FW", "n_TTYNA", "Mean_TTYNA", "Sd_TTYNA", "Min_TTYNA", "Max_TTYNA", "n_DM", "Mean_DM", "Sd_DM", "Min_DM", "Max_DM", "n_Tub_Size", "Mean_Tub_Size", "Sd_Tub_Size", "Min_Tub_Size", "Max_Tub_Size", "n_Tuber_Apper", "Mean_Tuber_Apper", "Sd_Tuber_Apper", "Min_Tuber_Apper", "Max_Tuber_Apper", "n_AGLU", "Prom_AGLU", "Sd_AGLU", "Min_AGLU", "Max_AGLU", "n_CHIPPING", "Mean_CHIPPING", "Sd_CHIPPING", "Min_CHIPPING", "Max_CHIPPING", "n_FLABOIL", "Mean_FLABOIL", "Sd_FLABOIL", "Min_FLABOIL", "Max_FLABOIL", "n_TEXTABL", "Mean_TEXTABL", "Sd_TEXTABL", "Min_TEXTABL", "Max_TEXTABL", "n_DAR", "Mean_DAR", "Sd_DAR", "Min_DAR", "Max_DAR", "n_STFLA", "Mean_STFLA", "Sd_STFLA", "Min_STFLA", "Max_STFLA", "n_Tgly_FW", "Mean_Tgly_FW", "Sd_Tgly_FW", "Min_Tgly_FW", "Max_Tgly_FW"


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

