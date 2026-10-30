# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Metodología del Enfoque Participativo de Cadenas Productivas (EPCP) en Colombia: Caso Ñame, 2008.

El presente informe tiene como objetivo principal describir la situación inicial de la población objetivo y control previo a la implementación de la metodología participativa 'Enfoque Participativo de Cadenas Productivas (EPCP)' ejecutado por la Corporación PBA y en el marco del proyecto de Cambio Andino, 2007-2010. El ámbito de la intervención fue la zona atlántica de Colombia, en los Municipios de Ovejas, Sampues, San Jacinto, Toluviejo, Los Palmitos, El Carmen, en los departamentos de Sucre y Bolívar. La población objetivo fueron los productores que conforman cinco asociaciones: APROS, ASOPROMAPES, ASOMUDEPAS, ASOPROYUS Y NAPOLES VENECIA y como grupo control otras asociaciones que no forman parte del proyecto del EPC. En el primer caso se aplicó un cuestionario de preguntas cerradas y fue aplicado a los jefes de familia pertenecientes a las asociaciones del grupo objetivo y a los jefes de familias de otras asociaciones del grupo control. En total se realizaron 71 encuestas al grupo objetivo y 35 al grupo control, en el año 2008.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/AZZZRF"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Los registros dados por los productores de Ñame de los departamentos de Sucre y Bolívar.; universe: Los productores de Ñame, que conforman cinco asociaciones: APROS, ASOPROMAPES, ASOMUDEPAS, ASOPROYUS Y NAPOLES VENECIA y como grupo control otras asociaciones que no forman parte del proyecto del EPC, de los departamentos de Sucre y Bolívar.; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: El proyecto es ejecutado por la Corporación PBA, en la zona atlántica de Colombia. El objetivo principal es identificar nuevos productos y mercados para la comercialización del ñame, de manera que los productores organizados puedan articularse con otros actores diferentes a los tradicionales (intermediarios).; collectionMode: Se seleccionaron 106 productores de cinco asociaciones: APROS, ASOPROMAPES, ASOMUDEPAS, ASOPROYUS Y NAPOLES VENECIA y como grupo control otras asociaciones que no forman parte del proyecto del EPC. La encuesta consta de un total de 56 preguntas.; researchInstrument: Se aplicó una encuesta con preguntas estructuradas.",
		
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
	

	f1 <- ff[basename(ff) == "01_DataBase Name Colombia.xlsx"]
	f2 <- ff[basename(ff) == "02_Data dictionary_Caso Name - EPCP - COLOMBIA.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm2 = carobiner::fix_name(r1[["3Municipo"]], "title"),
		adm1 = carobiner::fix_name(r1[["Cod_Encuestado"]], "title"),
		variety = r1[["13Variedad1"]]
	)
##r1: "NroEnc", "NomEnc", "NomSup", "FecApl", "1Pais", "2Departamento", "4Correg_Vereda", "5NomFinca", "6Apellidos", "6Nombre", "7NroMiembros", "8Nombre_01", "8Relacion_01", "8Sexo_01", "8Edad_01", "8Ocupacion_01", "8Educacion_01", "8Nombre_02", "8Relacion_02", "8Sexo_02", "8Edad_02", "8Ocupacion_02", "8Educacion_02", "8Nombre_03", "8Relacion_03", "8Sexo_03", "8Edad_03", "8Ocupacion_03", "8Educacion_03", "8Nombre_04", "8Relacion_04", "8Sexo_04", "8Edad_04", "8Ocupacion_04", "8Educacion_04", "8Nombre_05", "8Relacion_05", "8Sexo_05", "8Edad_05", "8Ocupacion_05", "8Educacion", "8Nombre_06", "8Relacion_06", "8Sexo_06", "8Edad_06", "8Ocupacion_06", "8Educacion_06", "8Nombre_07", "8Relacion_07", "8Sexo_07", "8Edad_07", "8Ocupacion_07", "8Educacion_07", "8Nombre_08", "8Relacion_08", "8Sexo_08", "8Edad_08", "8Ocupacion_08", "8Educacion_08", "8Nombre_09", "8Relacion_09", "8Sexo_09", "8Edad_09", "8Ocupacion_09", "8Educacion_09", "8Nombre_10", "8Relacion_10", "8Sexo_10", "8Edad_10", "8Ocupacion_10", "8Educacion_10", "9NESiembra", "9NEArea", "9NEAreaHa", "9NEDistSurco", "9NEDistSurcMt", "9NEDistPlanta", "9NEDistPlanMt", "9NETamSem", "9NETamSemGr", "9NECantSem", "9NECantSemKg", "9NCSiembra", "9NCArea", "9NCAreaHa", "9NCDistSurco", "9NCDistSurcMt", "9NCDistPlanta", "9NCDistPlanMt", "9NCTamSem", "9NCTamSemGr", "9NCCantSem", "9NCCantSemKg", "9NDSiembra", "9NDArea", "9NDAreaHa", "9NDDistSurco", "9NDDistSurcMt", "9NDDistPlanta", "9NDDistPlanMt", "9NDTamSem", "9NDTamSemGr", "9NDCantSem", "9NDCantSemKg", "9N1Cultivo", "9N1Siembra", "9N1Area", "9N1AreaHa", "9N1DistSurco", "9N1DistSurcMt", "9N2Cultivo", "9N2Siembra", "9N2Area", "9N2AreaHa", "9N2DistSurco", "9N2DistSurcMt", "10Tenencia", "10Riego", "10Pendiente", "11ConserSem", "11Porque1", "11Porque2", "12Realiza1", "12Realiza2", "13Cosecha1", "13Consumo1", "13Venta1", "13Semilla1", "13Proces1", "13Otro1", "13Variedad2", "13Cosecha2", "13Consumo2", "13Venta2", "13Semilla2", "13Proces2", "13Otro2", "13Variedad3", "13Cosecha3", "13Consumo3", "13Venta3", "13Semilla3", "13Proces3", "13Otro3", "14Relacion", "14Porque", "15PPro1", "15PPro2", "15PPro3", "15Cos1", "15Cos2", "15Cos3", "15Pos1", "15Pos2", "15Pos3", "16_1aCant", "16_1aCostU", "16_1aCostT", "16_1aCostHa", "16_1bCant", "16_1bCostU", "16_1bCostT", "16_1bCostHa", "16_1cCant", "16_1cCostU", "16_1cCostT", "16_1cCostHa", "16_1dCant", "16_1dCostU", "16_1dCostT", "16_1dCostHa", "16_1eCant", "16_1eCostU", "16_1eCostT", "16_1eCostHa", "16_1fCant", "16_1fCostU", "16_1fCostT", "16_1fCostHa", "16_2aClase", "16_2aCant", "16_2aCostU", "16_2aCostT", "16_2aCostHa", "16_2bClase", "16_2bCant", "16_2bCostU", "16_2bCostT", "16_2bCostHa", "16_2b2Clase", "16_2b2Cant", "16_2b2CostU", "16_2b2CostT", "16_2b2CostHa", "16_2cClase", "16_2cCant", "16_2cCostU", "16_2cCostT", "16_2cCostHa", "16_2dClase", "16_2dCant", "16_2dCostU", "16_2dCostT", "16_2dCostHa", "16_2eCant", "16_2eCostU", "16_2eCostT", "16_2eCostHa", "16_3aCant", "16_3aCostU", "16_3aCostT", "16_3aCostHa", "16_3bCant", "16_3bCostU", "16_3bCostT", "16_3bCostHa", "16_3cCant", "16_3cCostU", "16_3cCostT", "16_3cCostHa", "16_3dCant", "16_3dCostU", "16_3dCostT", "16_3dCostHa", "16_3eCant", "16_3eCostU", "16_3eCostT", "16_3eCostHa", "16_3fCant", "16_3fCostU", "16_3fCostT", "16_3fCostHa", "16_3g1Cant", "16_3g1CostU", "16_3g1CostT", "16_3g1CostHa", "16_3g2Cant", "16_3g2CostU", "16_3g2CostT", "16_3g2CostHa", "16_3g3Cant", "16_3g3CostU", "16_3g3CostT", "16_3g3CostHa", "16_3hCant", "16_3hCostU", "16_3hCostT", "16_3hCostHa", "16_3iCant", "16_3iCostU", "16_3iCostT", "16_3iCostHa", "16_3j1Cant", "16_3j1CostU", "16_3j1CostT", "16_3j1CostHa", "16_3j2Cant", "16_3j2CostU", "16_3j2CostT", "16_3j2CostHa", "16_3j3Cant", "16_3j3CostU", "16_3j3CostT", "16_3j3CostHa", "16_3kCant", "16_3kCostU", "16_3kCostT", "16_3kCostHa", "16_3lCant", "16_3lCostU", "16_3lCostT", "16_3lCostHa", "17ProPro", "17Porque", "17Prob", "18Proces", "19SubProd", "19IdentMerc", "19MercCom", "20Factores", "21PersVend_01", "21GradConf_01", "21DonVend_01", "21Variedad_01", "21CantVen_01", "21TipCalid_01", "21PrecVent_01", "21AcuOral_01", "21AcuEsc_01", "21NoAcu_01", "21SeCumple_01", "21PersVend_02", "21GradConf_02", "21DonVend_02", "21Variedad_02", "21CantVen_02", "21TipCalid_02", "21PrecVent_02", "21AcuOral_02", "21AcuEsc_02", "21NoAcu_02", "21SeCumple_02", "21PersVend_03", "21GradConf_03", "21DonVend_03", "21Variedad_03", "21CantVen_03", "21TipCalid_03", "21PrecVent_03", "21AcuOral_03", "21AcuEsc_03", "21NoAcu_03", "21SeCumple_03", "22PotMerc1", "22Obs1", "22PotMerc2", "22Obs2", "22PotMerc3", "22Obs3", "23Fuente1", "23Obs1", "23Fuente2", "23Obs2", "24ProdCap", "24ProTema", "24ProTipo", "24ProdQuien", "24ProTiempo", "24PostCap", "24PostTema", "24PostTipo", "24PostQuien", "24PostTiempo", "24PartCap", "24PartTema", "24PartTipo", "24PartQuien", "24PartTiempo", "24Otro", "24OtroCap", "24OtroTema", "24OtroTipo", "24OtroQuien", "24OtroTiempo", "25BrindeAT", "26InstPer1", "26TecInf1", "26Vinc1", "26TipVinc1", "26InstPer2", "26TecInf2", "26Vinc2", "26TipVinc2", "27ProdCom", "27ProdObs", "27Conf", "27ConfObs", "27Int", "27IntObs", "28PerOrg", "Grupo", "29NomOrg1", "29TipAsoc1", "29NomOrg2", "29TipAsoc2", "29NomOrg3", "29TipAsoc3", "29NomOrg4", "29TipAsoc4", "30LegAsoc", "31Cargo", "31Cual", "32EleccionD", "32OtroEsp", "33ApoyFinan", "33ApoyInst", "33ApoyOtro", "34RelacInts", "35Empr1", "35Tip?Rel1", "35NivConf1", "35ConvAli1", "35Empr2", "35Tip?Rel2", "35NivConf2", "35ConvAli2", "35Empr3", "35Tip?Rel3", "35NivConf3", "35ConvAli3", "36a", "36b", "36c", "36d", "36e", "36f", "36Otro", "37Alt01Des", "37AltCod1", "37Alt02Des", "37AltCod2", "37AltNing", "37AltAA", "37AltCap", "37AltFac", "37AltApoyo", "37AltAcceso", "37AltSomos", "37AltTrabaj", "38Prob01Des", "38ProbCod1", "38Prob02Des", "38ProbCod2", "38Prob03Des", "38ProbCod3", "38ProbAgri", "38ProbDCond", "38Probgest", "38ProbFac", "38ProbInter", "38ProbConfl", "38ProbFinan", "38Otros", "38ProbOtros", "39a", "39b", "39c", "39d", "39e", "39f", "39otros", "39g", "40a", "40aporque", "40b", "40bporque", "40c", "40cporque", "40d", "40dporque", "41a", "41b", "41c", "41d", "41e", "41f", "41g", "41h", "42a", "42b", "42c", "42Otro", "43NIngreso", "44OtrasIng1", "44OtrasIng2", "45a", "45b", "45c", "45d", "45e", "45f1", "45f2", "45f3", "45f4", "45f5", "45g", "45h", "45Otros", "45i", "46Vivienda", "46Otro", "47ViviPared", "47Otro", "48ViviTecho", "48Otro", "49Vivipiso", "49Otro", "50agua", "50Otro", "51alumb", "51Otro", "52Higuiene", "53Cocinar", "53Otro", "54a", "54aCuant", "54b", "54bCuant", "54c", "54cCuant", "54d", "54dCuant", "54e", "54eCuant", "54f", "54fCuant", "54g", "54gCuant", "54h", "54hCuant", "54i", "54iCuant", "54j", "54jCuant", "54Otro", "54k", "54kCuant", "55a", "55aCuant", "55b", "55bCuant", "55c", "55cCuant", "55d", "55dCuant", "55e", "55eCuant", "55fOtro", "55f", "55fCuant", "56a", "56aCuant", "56b", "56bCuant", "56c", "56cCuant", "56d", "56dCuant", "56e", "56eCuant", "56fOtro", "56f", "56fCuant"


	d2 <- data.frame()
##r2: "Variable name", "Descripcion_de_la_Variable", "Valor_de_Variable", "Observaciones", "Comentarios"


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

