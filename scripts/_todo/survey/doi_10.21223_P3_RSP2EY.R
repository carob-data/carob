# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Metodología Participativa Seguimiento y Evaluación Participativa (SEP), Papa Criolla Colombia, 2008.

La metodología SEP, es una herramienta de gestión de proyectos que se aplica con el fin de contribuir a fortalecer a las organizaciones de base. Por sus características es altamente participativa, ya que permite que los beneficiarios en los distintos niveles –desde el nivel directivo hasta el de base- participen en el seguimiento y evaluación de un proyecto idea o emprendimiento colectivo. En el presente estudio el grupo objetivo del proyecto Agricultura Sostenible y Competitiva está conformado por las siguientes dos asociaciones: Asociación de Agricultores de Granada (ASOAGRA) y la Asociación Criolla Oro de Sibaté, quienes a su vez implementarán la Metodología Participativa SEP durante el proceso de implementación del proyecto, en el año 2008.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/RSP2EY"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: La unidad de análisis es principalmente la familia de los agricultores que participan en la aplicación del SEP, y el informante ha sido el representante de la familia que pertenece a las organizaciones de base que forma parte de la aplicación del SEP (ASOAGRA y CRIOLLA ORO).; universe: Pequeños agricultores de papa criolla de los Municipios de Granada y Sibaté, del departamento de Cundinamarca en Colombia.; timeMethod: Cross-sectional; dataCollector: International Potato Center; Corporación PBA, en el marco del Proyecto Alianza Cambio Andino; samplingProcedure: El ámbito de alcance del presente estudio ha comprendido la zona de intervención de la Corporación PBA en la aplicación de la “Metodología de Seguimiento y Evaluación Participativa”, siendo dicha zona de intervención a los miembros de las organizaciones de ASOAGRA (Municipio de Granada, 20 productores) y CRIOLLA ORO (Municipio de Sibaté, 15 productores).; collectionMode: Se han aplicado un total de 35 cuestionarios, la duración de la aplicación de cada cuestionario fue entre 30 a 60 minutos.; researchInstrument: Se aplico una encuesta con preguntas estructuradas, generalmente de preguntas cerradas, dirigido a productores (agricultores), de aproximadamente 46 preguntas divididas en 8 ítems.",
		
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
	

	f1 <- ff[basename(ff) == "01_DataBase  SEP Colombia - Papa Criolla.xlsx"]
	f2 <- ff[basename(ff) == "02_Data dictionary_SEP_Colombia.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		N_fertilizer = r1[["N_Enc"]],
		adm2 = carobiner::fix_name(r1[["3Municipio"]], "title")
	)
##r1: "NomEnc", "FecApl", "1Pais", "2Departam", "4Vereda", "5NombFinca", "6Apellid/Nombres", "7NroMiembros", "8Nombre_01", "8Parentesco_01", "8Sexo_01", "8Edad_01", "8Ocupacion_01", "8Educacion_01", "8Nombre_02", "8Parentesco_02", "8Sexo_02", "8Edad_02", "8Ocupacion_02", "8Educacion_02", "8Nombre_03", "8Parentesco_03", "8Sexo_03", "8Edad_03", "8Ocupacion_03", "8Educacion_03", "8Nombre_04", "8Parentesco_04", "8Sexo_04", "8Edad_04", "8Ocupacion_04", "8Educacion_04", "8Nombre_05", "8Parentesco_05", "8Sexo_05", "8Edad_05", "8Ocupacion_05", "8Educacion_05", "8Nombre_06", "8Parentesco_06", "8Sexo_06", "8Edad_06", "8Ocupacion_06", "8Educacion_06", "8Nombre_07", "8Parentesco_07", "8Sexo_07", "8Edad_07", "8Ocupacion_07", "8Educacion_07", "9AreaTotal", "9Cultivo1", "9Cultivo2", "9Cultivo3", "9Cultivo4", "10Producto1", "10AreaCult1", "10CantCos1", "10Consumo1", "10Venta1", "10Semilla1", "10Procesam1", "10Otro1", "10Producto2", "10AreaCult2", "10CanCos2", "10Consumo2", "10Venta2", "10Semilla2", "10Procesam2", "10Otros2", "11Producto1", "11CantKg", "11Precio(Peso/kg)", "11Mercado", "11Producto2", "Field37", "Field38", "Field39", "12FormVenta", "13ProduccProm", "13Por que", "14CompCalidad", "14Por que", "15AsocPert", "15NombAsociac", "16OtraAsocPerten1", "16OtraAsocPerten2", "16OtraAsocPerten3", "16Tipo", "17AsocConfor", "18Incorporacion", "19CargOcupa", "20Reuniones", "20Porque", "21CuanTiempo", "21Otro", "22EleccionDirec", "23RenovDirectiva", "23Otros", "24CapacyPasan", "24CompraInsumos", "24ApoyoComercializ", "24RelacinInstituc", "24AccesoFinanProy", "24SomosReconoc", "24TrabajMancomunad", "24Otroh", "24Otroi", "25TomaDesiciones", "26ApoyoMutuo", "26ObservA", "26ApoyoProblemas", "26ObservB", "26IntercExperi", "26ObservC", "26ReunionSociales", "26ObservD", "26ResoluConflictos", "26ObservE", "26Otros", "26ObservOtros", "27DesinteresMiemb", "27ConsductAutoritaria", "27FaltaInterrelacion", "27ConflicSocios", "27FaltaRecursos", "27NorindenCuenta", "27FaltaUnion", "27FaltaDireccJD", "27FaltaCooperacGrupo", "28ReunionVision", "28IdentifProblemas", "28ProgramEstrateg", "28ConformComResp", "28ElaboracFormat", "28SeguimActivProg", "28ComunicResultad", "28TomaDesicion", "29ConsiderImportante", "29ConsideTieneHabil", "29ConsidAsocAtienPet", "30SientConfianzProp", "30Observac1", "30OpinionTomenCuent", "30Observac2", "31InfluenensuAsociac", "31InfluensuComunidad", "32Organizac/Instituc1", "32Relacion1", "32Confianza1", "32Organizac/Instituc2", "32Relacion2", "32Confianza2", "32Organizac/Instituc3", "32relacion3", "32Confianza3", "32Organiz/Instituc4", "32Relacion4", "32Confianza4", "32Organiz/Instituc5", "32Relacion5", "32Confianza5", "33ApoyoMutuo", "33ReunionActivid", "33SeguimActiv", "33OpinTomenCuent", "33MiemInforAvanc", "33ProdPartMejorar", "34ParticenPlanific", "34IdentifdeProblem", "34AsambRendCuent", "34FiestPatronales", "34CampeoDeportiv", "34CampaSolidarid", "34Otros", "34Especificar", "35GastAlimen", "35GastSalud", "35GastEducacion", "35GastTransp", "35GastVestido", "35GastLuz", "35GastAgua", "35GastTelef", "35GastGas", "35GastCabInter", "35GastEsparc", "35GastMuebles", "35Otros", "36VivOcupada", "37MatePared", "37Otro", "38MatPared", "38Otro", "39MatPiso", "39Otro", "40AbastAgua", "40Otro", "41TipoAlumb", "41Otro", "42TipoServHig", "42Otro", "43UtilCocinar", "43Otro", "44Radio", "44RadCuantos", "44Tv", "44TvCuantos", "44EquiSonid", "44ESCuantos", "44Nevera", "44NevCuantos", "44EstaGas", "44EstCuant", "44TelefCel", "44TelCelCuant", "44Biciclet", "44BiciCuanto", "44Auto/Camion", "44AuCaCuant", "44Motocic", "44MotoCuant", "44DVD", "44DvdCuanto", "44MaqCoser", "44MaqCosCuant", "44Computadora", "44CompuCuanto", "45Herramient", "45HerramCuant", "45Tractor", "45TracCuanto", "45BomAgua", "45BACuant", "45BomEspalda", "45BECuanto", "45Aspersor", "45AsperCuant", "45MotorFumi", "45MFCuanto", "45Trilladora", "45TriCuanto", "45Picapasto", "45PicPasCuant", "46AvesCorral", "46ACCuanto", "46Ovejas", "46OveCuanto", "46Cerdos", "46CerCuanto", "46CabMulBur", "46CMBCuanto", "46Vacas", "46VaCuanto"


	d2 <- data.frame()
##r2: "Variable name", "Description", "Grupo", "Temas"


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

