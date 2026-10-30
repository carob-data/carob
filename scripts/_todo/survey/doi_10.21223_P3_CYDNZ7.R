# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Estudio de base de la metodología Madre&Bebe Caso: Red Latinpapa - Peru, 2008.

El presente es un informe del estudio de base de la metodología 'Mamá y Bebé' (M&B), desde 2008, la Red Latinpapa formó un equipo de trabajo para seleccionar un grupo de clones avanzados del programa de mejoramiento del CIP, integrando a varias instituciones como los programas nacionales de investigación (INIA, universidades e institutos), las ONGs, grupos de agricultores y empresas privadas (tiendas, procesadoras y restaurantes). Cada equipo, por región o lugar, se llama 'Consorcio de SPV'. Con estos consorcios, la Red Latinpapa halló como aliado al Proyecto Alianza Cambio Andino, para difundir el método Mamá y Bebé (MyB) en Perú. En 2008, el método se implementó en cuatro regiones productoras de papa en los Andes centrales, a una altitud mayor de 3 000 msnm, donde prevalecen los pequeños agricultores: Cusco, Huancavelica, Huánuco y Junín. En el primer año, solo tres consorcios establecieron los ensayos en las parcelas Mamá y Bebé según los lineamientos del método: el consorcio Junín (liderado por el Instituto Nacional de Innovación Agraria/Santa Ana), el consorcio Huancavelica-Junín (liderado por la ONG Grupo Yanapai) y el consorcio Huancavelica (liderado por la Universidad para el Desarrollo Andino - UDEA). Los consorcios cuentan con un estudio de línea de base y un estudio de monitoreo e impacto, elaborados por Cambio Andino. La capacitación se realizó en junio de 2008, en la estación experimental del CIP en Huancayo, Junín. Participaron 14 técnicos y 3 agricultores. Luego, los técnicos ejecutaron el método. Establecieron los ensayos a finales de 2008, en la época de lluvias. En 2009, se hicieron las evaluaciones en floración (en febrero) y las evaluaciones en cosecha (en mayo). En este ciclo, los consorcios de Cusco, Huancavelica, Junín y Huancavelica-Junín establecieron los ensayos en siete lugares.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/CYDNZ7"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: La unidad de análisis es principalmente la familia de los agricultores que participan en la aplicación de M&B y el informante ha sido el representante de la familia que pertenece a la organización de base que forma parte de la aplicación M&B.; universe: Los pequeños agricultores productores de papas de los departamentos del: Cusco, Huancavelica, Huánuco y Junín.; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: Se han aplicado un total de 115 cuestionarios, Cusco (16), Huancavelica (53), Huánuco (12) y Junín (34), siendo la duración entre 30 a 60 minutos. El número de agricultores (familias) por entrevistar en cada uno de los estratos conformados se ha determinado considerando una asignación aproximadamente proporcional al tamaño del estrato, de este modo se lograrán estimaciones cuasi-ponderadas.; collectionMode: Se ha utilizado la técnica de ENCUESTAS para medir y cuantificar las principales características de los agricultores y su organización donde se implementará la metodología M&B.; researchInstrument: El instrumento utilizado ha sido un cuestionario generalmente de preguntas cerradas, dirigido a productores (agricultores), de aproximadamente 36 preguntas.",
		
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
	

	f1 <- ff[basename(ff) == "01_DataBase Madre&Bebe - Peru.xlsx"]
	f2 <- ff[basename(ff) == "02_Data dictionary - Metodologia Mama&Bebe - Peru.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm2 = carobiner::fix_name(r1[["3Provinvia"]], "title"),
		variety = r1[["12Variedad_01"]]
	)
##r1: "NroEnc", "NomEnc", "FecApl", "1Pais", "2Departamento", "4Distrito", "5Comunidad", "6Centro Poblado", "Grupo_Encuest", "7Apellidos", "8Nombre", "9NroMiembros", "10Nombre_01", "10Relacion_01", "10Sexo_01", "10Edad_01", "10Ocupacion_01", "10Educacion_01", "10Enc_01", "10Nombre_02", "10Relacion_02", "10Sexo_02", "10Edad_02", "10Ocupacion_02", "10Educacion_02", "10Enc_02", "10Nombre_03", "10Relacion_03", "10Sexo_03", "10Edad_03", "10Ocupacion_03", "10Educacion_03", "10Enc_03", "10Nombre_04", "10Relacion_04", "10Sexo_04", "10Edad_04", "10Ocupacion_04", "10Educacion_04", "10Enc_04", "10Nombre_05", "10Relacion_05", "10Sexo_05", "10Edad_05", "10Ocupacion_05", "10Educacion_05", "10Enc_05", "10Nombre_06", "10Relacion_06", "10Sexo_06", "10Edad_06", "10Ocupacion_06", "10Educacion_06", "10Enc_06", "10Nombre_07", "10Relacion_07", "10Sexo_07", "10Edad_07", "10Ocupacion_07", "10Educacion_07", "10Enc_07", "10Nombre_08", "10Relacion_08", "10Sexo_08", "10Edad_08", "10Ocupacion_08", "10Educacion_08", "10Enc_08", "10Nombre_09", "10Relacion_09", "10Sexo_09", "10Edad_09", "10Ocupacion_09", "10Educacion_09", "10Enc_09", "11CulPapaM", "11PapaMParc", "11PapaMSem", "11CulPapaNatA", "11PapaNAParc", "11PapaNASem", "11CulPapaNH", "11PapaNHParc", "11PapaNHSem", "11CulOtr1", "11Otr1Parc", "11Otr1Sem", "11CulOtr2", "11Otr2Parc", "11Otr2Sem", "11CulOtr3", "11Otr3Parc", "11Otr3Sem", "11CulOtr4", "11Otr4Parc", "11Otr4Sem", "11CulOtr5", "11Otr5Parc", "11Otr5Sem", "11Observaciones", "12Siembra_01", "12CantSem_01", "12Origen_01", "12AnosSiemb_01", "12Cosecha_01", "12Venta_01", "12Semilla_01", "12AutoCons_01", "12Procesa_01", "12OtrosUsos_01", "12Siembra_02", "12Variedad_02", "12CantSem_02", "12Origen_02", "12AnosSiemb_02", "12Cosecha_02", "12Venta_02", "12Semilla_02", "12AutoCons_02", "12Procesa_02", "12OtrosUsos_02", "12Siembra_03", "12Variedad_03", "12CantSem_03", "12Origen_03", "12AnosSiemb_03", "12Cosecha_03", "12Venta_03", "12Semilla_03", "12AutoCons_03", "12Procesa_03", "12OtrosUsos_03", "12Siembra_04", "12Variedad_04", "12CantSem_04", "12Origen_04", "12AnosSiemb_04", "12Cosecha_04", "12Venta_04", "12Semilla_04", "12AutoCons_04", "12Procesa_04", "12OtrosUsos_04", "12Siembra_05", "12Variedad_05", "12CantSem_05", "12Origen_05", "12AnosSiemb_05", "12Cosecha_05", "12Venta_05", "12Semilla_05", "12AutoCons_05", "12Procesa_05", "12OtrosUsos_05", "13Variedades_01", "13Tipo_01", "13Cantidad_01", "13Precio_01", "13DondeVend_01", "13AQuienVend_01", "13Variedades_02", "13Tipo_02", "13Cantidad_02", "13Precio_02", "13DondeVend_02", "13AQuienVend_02", "13Variedades_03", "13Tipo_03", "13Cantidad_03", "13Precio_03", "13DondeVend_03", "13AQuienVend_03", "13Variedades_04", "13Tipo_04", "13Cantidad_04", "13Precio_04", "13DondeVend_04", "13AQuienVend_04", "13Variedades_05", "13Tipo_05", "13Cantidad_05", "13Precio_05", "13DondeVend_05", "13AQuienVend_05", "13Variedades_06", "13Tipo_06", "13Cantidad_06", "13Precio_06", "13DondeVend_06", "13AQuienVend_06", "14Var1", "14Var1Carac1", "14Var1Carac2", "14Var1Carac3", "14Var2", "14Var2Carac1", "14Var2Carac2", "14Var2Carac3", "14Var3", "14Var3Carac1", "14Var3Carac2", "14Var3Carac3", "15ProbDes1", "15ProbDes2", "15ProbDes3", "15ProbCos1", "15ProbCos2", "15ProbCos3", "15ProbPos1", "15ProbPos2", "15ProbPos3", "16Alcanza", "16Porque", "17PerAsocP", "18NomAsoc1", "18TipoAsoc1", "18Espec1", "18NomAsoc2", "18TipoAsoc2", "18Espec2", "19Afirm1", "19Cuant1", "19Tipo1", "19Inst1", "19Afirm2", "19Cuant2", "19Tipo2", "19Inst2", "19Afirm3", "19Cuant3", "19Tipo3", "19Inst3", "19Afirm4", "19Cuant4", "19Tipo4", "19Inst4", "20FreqAct1", "20FreqAct2", "20FreqAct3", "20FreqAct4", "20FreqAct5", "20FreqAct6", "20OthEsp", "20FreqOth", "21Afirm1", "21Porque1", "21Afirm2", "21Porque2", "21Afirm3", "21Porque3", "21Afirm4", "21Porque4", "21Afirm5", "21Porque5", "22NCLider", "22NCSocio", "22NCVecino", "22NCInterm", "22NCMayor", "22NCEmpre", "22NCAutor", "22NCTecAgro", "22NCONG", "22NCOInv", "22Otro", "22NCOtro", "23a", "23b", "23c", "23Otro", "24PorcVent", "25GFAlim", "25GFSalud", "25GFEduc", "25GFTrans", "25GFVest", "25GFSLuz", "25GFSAgua", "25GFSTelf", "25GFSGas", "25GFCable", "25GFEspar", "25GFMueb", "25GFOtros", "26Vivienda", "26Otro", "27ViviPared", "27Otro", "28ViviTecho", "28Otro", "29Vivipiso", "29Otro", "30agua", "30Otro", "31alumb", "31Otro", "32Higuiene", "32Otro", "33Cocinar", "33Otro", "34a", "34aCuant", "34b", "34bCuant", "34c", "34cCuant", "34d", "34dCuant", "34e", "34eCuant", "34f", "34fCuant", "34g", "34gCuant", "34h", "34hCuant", "34i", "34iCuant", "34j", "34jCuant", "34Otro", "34k", "34kCuant", "35a", "35aCuant", "35b", "35bCuant", "35c", "35cCuant", "35d", "35dCuant", "35e", "35eCuant", "35f", "35fCuant", "35gEyH", "35g", "35gCuan", "35hEyH", "35h", "35hCuant", "36a", "36aCuant", "36b", "36bCuant", "36c", "36cCuant", "36d", "36dCuant", "36e", "36eCuant", "36f", "36fCuant", "36gOtro", "36g", "36gCuant"


	d2 <- data.frame()
##r2: "Variable name", "Description", "Base_datos", "Descripcion"


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

