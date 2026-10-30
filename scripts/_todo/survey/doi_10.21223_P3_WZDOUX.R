# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Estudio de base de la metodología Mama&Bebe Caso: Red Latinpapa - Colombia, 2009.

El presente es un informe del estudio de base de la metodología 'Mama y Bebé' (M&B) que viene implementando la Corporación PBA, en el marco del Proyecto Alianza Cambio Andino, en COOINPROPAS, del departamento de Boyacá, en el ano 2009. En este caso se ha utilizado un cuestionario semiestructurado de preguntas cerradas y estuvo dirigido a los jefes de familia de dos organizaciones de productoras de papa: Siachoque y Soracá, contándose en total 31 encuestas diligenciadas. La Selección Participativa de Variedades con la metodología Mama & Bebe ha sido aplicado en la región Andina, principalmente en Perú, con bastante éxito aportando a la liberación temprano y descentralizada de nuevas variedades rústicas de papa en zonas pobres. En el marco del Proyecto Alianza Cambio Andino se propone replicar la metodología sistemáticamente en el Perú y Colombia con un enfoque de cadenas involucrando a actores regionales claves de las cadenas de valor. El presente estudio de base de la metodología Mama y Bebe (M&B) es implementada por la Corporación PBA en el producto de papa y da cuenta del valor inicial de los principales indicadores de alcances e impactos que se promueve con la aplicación de la metodología antes citada.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/WZDOUX"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: La unidad de análisis es principalmente la familia de los agricultores que participan en la aplicación de M&B y el informante ha sido el representante de la familia que pertenece a la organización de base que forma parte de la aplicación M&B. Los registros de datos de las encuestas todos los miembros de la organización COOINPROPAS, quienes participan en la implementación de la metodología M&B.; universe: Los productores de papas de los Municipios de Siachoque y Soracá de la Provincia de Soracá en Colombia.; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: Se han aplicado un total de 31 cuestionarios (17 en la zona de Siachoque y 14 en Sorocá), siendo la duración entre 30 a 60 minutos. El número de agricultores (familias) por entrevistar en cada uno de los estratos conformados se ha determinado considerando una asignación aproximadamente proporcional al tamaño del estrato, de este modo se lograrán estimaciones cuasi-ponderadas.; collectionMode: Se ha utilizado la técnica de ENCUESTAS para medir y cuantificar las principales características de los agricultores y su organización donde se implementará la metodología M&B.; researchInstrument: El instrumento utilizado ha sido un cuestionario generalmente de preguntas cerradas, dirigido a productores (agricultores), de aproximadamente 35 preguntas.",
		
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
	

	f1 <- ff[basename(ff) == "01_DataBase Mother&Baby Colombia.xlsx"]
	f2 <- ff[basename(ff) == "02_Diccionario de Variables MyB - Colombia.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm2 = carobiner::fix_name(r1[["3Municipio"]], "title"),
		variety = r1[["11Variedad_01"]]
	)
##r1: "NroEnc", "NomEnc", "FecApl", "1Pais", "2Departamento", "4Vereda", "5Finca", "Grupo_Encuest", "6Apellidos", "7Nombre", "8NroMiembros", "9Nombre_01", "9Relacion_01", "9Sexo_01", "9Edad_01", "9Ocupacion_01", "9Educacion_01", "9Enc_01", "9Nombre_02", "9Relacion_02", "9Sexo_02", "9Edad_02", "9Ocupacion_02", "9Educacion_02", "9Enc_02", "9Nombre_03", "9Relacion_03", "9Sexo_03", "9Edad_03", "9Ocupacion_03", "9Educacion_03", "9Enc_03", "9Nombre_04", "9Relacion_04", "9Sexo_04", "9Edad_04", "9Ocupacion_04", "9Educacion_04", "9Enc_04", "9Nombre_05", "9Relacion_05", "9Sexo_05", "9Edad_05", "9Ocupacion_05", "9Educacion_05", "9Enc_05", "9Nombre_06", "9Relacion_06", "9Sexo_06", "9Edad_06", "9Ocupacion_06", "9Educacion_06", "9Enc_06", "9Nombre_07", "9Relacion_07", "9Sexo_07", "9Edad_07", "9Ocupacion_07", "9Educacion_07", "9Enc_07", "9Nombre_08", "9Relacion_08", "9Sexo_08", "9Edad_08", "9Ocupacion_08", "9Educacion_08", "9Enc_08", "9Nombre_09", "9Relacion_09", "9Sexo_09", "9Edad_09", "9Ocupacion_09", "9Educacion_09", "9Enc_09", "10Cul01_01", "10Parc01_01", "10Sem01Local_01", "10Sem01_01", "10Area01Local_01", "10Area01_01", "10Cul01_02", "10Parc01_02", "10Sem01Local_02", "10Sem01_02", "10Area01Local_02", "10Area01_02", "10Cul01_03", "10Parc01_03", "10Sem01Local_03", "10Sem01_03", "10Area01Local_03", "10Area01_03", "10Cul01_04", "10Parc01_04", "10Sem01Local_04", "10Sem01_04", "10Area01Local_04", "10Area01_04", "10Cul01_05", "10Parc01_05", "10Sem01Local_05", "10Sem01_05", "10Area01Local_05", "10Area01_05", "10Cul01_06", "10Parc01_06", "10Sem01Local_06", "10Sem01_06", "10Area01Local_06", "10Area01_06", "10Observaciones", "11Siembra_01", "11CantSemBul_01", "11CantSemKg_01", "11Origen_01", "11AnosSiemb_01", "11CosechaBul_01", "11CosechaKg_01", "11VentaBul_01", "11VentaKg_01", "11SemillaBul_01", "11SemillaKg_01", "11AutoConsBul_01", "11AutoConsKg_01", "11ProcesaBul_01", "11ProcesaKg_01", "11OtrosUsosBul_01", "11OtrosUsosKg_01", "11Siembra_02", "11Variedad_02", "11CantSemBul_02", "11CantSemKg_02", "11Origen_02", "11AnosSiemb_02", "11CosechaBul_02", "11CosechaKg_02", "11VentaBul_02", "11VentaKg_02", "11SemillaBul_02", "11SemillaKg_02", "11AutoConsBul_02", "11AutoConsKg_02", "11ProcesaBul_02", "11ProcesaKg_02", "11OtrosUsosBul_02", "11OtrosUsosKg_02", "11Siembra_03", "11Variedad_03", "11CantSemBul_03", "11CantSemKg_03", "11Origen_03", "11AnosSiemb_03", "11CosechaBul_03", "11CosechaKg_03", "11VentaBul_03", "11VentaKg_03", "11SemillaBul_03", "11SemillaKg_03", "11AutoConsBul_03", "11AutoConsKg_03", "11ProcesaBul_03", "11ProcesaKg_03", "11OtrosUsosBul_03", "11OtrosUsosKg_03", "11Siembra_04", "11Variedad_04", "11CantSemBul_04", "11CantSemKg_04", "11Origen_04", "11AnosSiemb_04", "11CosechaBul_04", "11CosechaKg_04", "11VentaBul_04", "11VentaKg_04", "11SemillaBul_04", "11SemillaKg_04", "11AutoConsBul_04", "11AutoConsKg_04", "11ProcesaBul_04", "11ProcesaKg_04", "11OtrosUsosBul_04", "11OtrosUsosKg_04", "12Variedades_01", "12Cantidad0Bul_01", "12Cantidad0Kg_01", "12Precio0Bul_01", "12Precio0Kg_01", "12DondeVend0_01", "12AQuienVend0_01", "12Cantidad1Bul_01", "12Cantidad1Kg_01", "12Precio1Bul_01", "12Precio1Kg_01", "12DondeVend1_01", "12AQuienVend1_01", "12CantidadPBul_01", "12CantidadPKg_01", "12PrecioPBul_01", "12PrecioPKg_01", "12DondeVendP_01", "12AQuienVendP_01", "12CantidadRBul_01", "12CantidadRKg_01", "12PrecioRBul_01", "12PrecioRKg_01", "12DondeVendR_01", "12AQuienVendR_01", "12Variedades_02", "12Cantidad0Bul_02", "12Cantidad0Kg_02", "12Precio0Bul_02", "12Precio0Kg_02", "12DondeVend0_02", "12AQuienVend0_02", "12Cantidad1Bul_02", "12Cantidad1Kg_02", "12Precio1Bul_02", "12Precio1Kg_02", "12DondeVend1_02", "12AQuienVend1_02", "12CantidadPBul_02", "12CantidadPKg_02", "12PrecioPBul_02", "12PrecioPKg_02", "12DondeVendP_02", "12AQuienVendP_02", "12CantidadRBul_02", "12CantidadRKg_02", "12PrecioRBul_02", "12PrecioRKg_02", "12DondeVendR_02", "12AQuienVendR_02", "12Variedades_03", "12Cantidad0Bul_03", "12Cantidad0Kg_03", "12Precio0Bul_03", "12Precio0Kg_03", "12DondeVend0_03", "12AQuienVend0_03", "12Cantidad1Bul_03", "12Cantidad1Kg_03", "12Precio1Bul_03", "12Precio1Kg_03", "12DondeVend1_03", "12AQuienVend1_03", "12CantidadPBul_03", "12CantidadPKg_03", "12PrecioPBul_03", "12PrecioPKg_03", "12DondeVendP_03", "12AQuienVendP_03", "12CantidadRBul_03", "12CantidadRKg_03", "12PrecioRBul_03", "12PrecioRKg_03", "12DondeVendR_03", "12AQuienVendR_03", "12Variedades_04", "12Cantidad0Bul_04", "12Cantidad0Kg_04", "12Precio0Bul_04", "12Precio0Kg_04", "12DondeVend0_04", "12AQuienVend0_04", "12Cantidad1Bul_04", "12Cantidad1Kg_04", "12Precio1Bul_04", "12Precio1Kg_04", "12DondeVend1_04", "12AQuienVend1_04", "12CantidadPBul_04", "12CantidadPKg_04", "12PrecioPBul_04", "12PrecioPKg_04", "12DondeVendP_04", "12AQuienVendP_04", "12CantidadRBul_04", "12CantidadRKg_04", "12PrecioRBul_04", "12PrecioRKg_04", "12DondeVendR_04", "12AQuienVendR_04", "13Var1", "13Var1Carac1", "13Var1Carac2", "13Var1Carac3", "13Var2", "13Var2Carac1", "13Var2Carac2", "13Var2Carac3", "13Var3", "13Var3Carac1", "13Var3Carac2", "13Var3Carac3", "14ProbDes1", "14ProbDes2", "14ProbDes3", "14ProbCos1", "14ProbCos2", "14ProbCos3", "14ProbPos1", "14ProbPos2", "14ProbPos3", "15Alcanza", "15Porque", "16PerAsocP", "17NomAsoc1", "17TipoAsoc1", "17Espec1", "17NomAsoc2", "17TipoAsoc2", "17Espec2", "18Afirm1", "18Cuant1", "18Tipo1", "18Inst1", "18Afirm2", "18Cuant2", "18Tipo2", "18Inst2", "18Afirm3", "18Cuant3", "18Tipo3", "18Inst3", "18Afirm4", "18Cuant4", "18Tipo4", "18Inst4", "19FreqAct1", "19FreqAct2", "19FreqAct3", "19FreqAct4", "19FreqAct5", "19FreqAct6", "19OthEsp", "19FreqOth", "20Afirm1", "20Porque1", "20Afirm2", "20Porque2", "20Afirm3", "20Porque3", "20Afirm4", "20Porque4", "20Afirm5", "20Porque5", "21NCLider", "21NCSocio", "21NCVecino", "21NCInterm", "21NCMayor", "21NCEmpre", "21NCAutor", "21NCTecAgro", "21NCONG", "21NCOInv", "21Otro", "21NCOtro", "22a", "22b", "22c", "22Otro", "23PorcVent", "24GFAlim", "24GFSalud", "24GFEduc", "24GFTrans", "24GFVest", "24GFSLuz", "24GFSAgua", "24GFSTelf", "24GFSGas", "24GFCable", "24GFEspar", "24GFMueb", "24GFOtros", "25Vivienda", "25Otro", "26ViviPared", "26Otro", "27ViviTecho", "27Otro", "28Vivipiso", "28Otro", "29agua", "29Otro", "30alumb", "30Otro", "31Higuiene", "31Otro", "32Cocinar", "32Otro", "33a", "33aCuant", "33b", "33bCuant", "33c", "33cCuant", "33d", "33dCuant", "33e", "33eCuant", "33f", "33fCuant", "33g", "33gCuant", "33h", "33hCuant", "33i", "33iCuant", "33j", "33jCuant", "33KOtro", "33k", "33kCuant", "34a", "34aCuant", "34b", "34bCuant", "34c", "34cCuant", "34d", "34dCuant", "34e", "34eCuant", "34fEyH", "34f", "34fCuant", "34gEyH", "34g", "34gCuan", "34hEyH", "34h", "34hCuant", "35a", "35aCuant", "35b", "35bCuant", "35c", "35cCuant", "35d", "35dCuant", "35e", "35eCuant", "35fOtro", "35f", "35fCuant"


	d2 <- data.frame()
##r2: "Variable name", "Description", "Base_de_Datos", "Comentarios"


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

