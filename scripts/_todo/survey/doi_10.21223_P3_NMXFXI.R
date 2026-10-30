# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Innovaciones Tecnológicas y Mercados Diferenciados para Productores de Papa Nativa.

El proyecto tuvo como objetivo general 'Mejorar la calidad de vida de pequeños productores de papas nativas de la región andina, a través de la incorporación de innovaciones tecnológicas a sus sistemas productivos y el acceso a mercados diferenciados, para aprovechar la biodiversidad de papas nativas'. El proyecto se constituyó en una plataforma para validar y enriquecer las experiencias exitosas alrededor del cultivo de papas nativas, centrándose en los temas: caracterización de la biodiversidad, generación de innovaciones tecnológicas de producción y post cosecha, incorporación de las papas nativas en cadenas de valor, fortalecimiento de los productores en producción y gestión empresarial, difusión de la biodiversidad y gastronomía andina a través de campañas publicitarias dirigidas a revalorizar las papas nativas. Las encuestas fueron 170 y se realizaron en julio del 2007 en el caso de Perú. Las comunidades beneficiarias fueron: San Cristóbal de Ñahuín, Vista Alegre, Mullaca, San José de Aymara y Pazos en el Distrito de Pazos, Provincia de Tayacaja en la Región Huancavelica; y en las Comunidades de Marcavalle, Patalá, 2 de Mayo y Libertadores en el Distrito de Pucará, Provincia de Huancayo en la Región Junín.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/NMXFXI"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Hogares de pequeños productores de papa. Registro individual de producción por agricultor. Cada registro corresponde a un agricultor participante en una temporada agrícola determinada e incluye variables de producción, variedades y productos para controlar las plagas entre otros.; universe: Hogares de productores de papa en los departamentos de Huancavelica y Junin. Los 170 agricultores participantes de los distritos seleccionados de los departamentos del Perú, en el 2003.; timeMethod: De corte transversal; dataCollector: Instituto Nacional de Investigación y Extensión Agraria (INIA-Perú); samplingProcedure: El propósito fue seleccionar 5 comunidades, San Cristóbal de Ñahuín, Vista Alegre, Mullaca, San José de Aymara y Pazos en el Distrito de Pazos, Provincia de Tayacaja en la Región Huancavelica y 4 Comunidades: Marcavalle, Patalá, 2 de Mayo y Libertadores en el Distrito de Pucará, Provincia de Huancayo en la Región Junín, haciendo un total de 170 agricultores encuestados en el año 2007.; collectionMode: Encuestadores seleccionaron 90 familias del distrito Pucara y 80 agricultores del distrito de Pazos, lo que hacen un total de 170 hogares. Cada encuesta contiene un total de 11 preguntas.; researchInstrument: Encuestas de preguntas estructuradas a cada hogar del agricultor.",
		
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
	

	f1 <- ff[basename(ff) == "01_Encuesta Datos -Proyecto Innovaciones Tecnologicas y Mercados Diferenciados para Productores de Papa Nativas.xlsx"]
	f2 <- ff[basename(ff) == "02_Data Dictionary.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm1 = carobiner::fix_name(r1[["1_2Encuestador"]], "title"),
		adm2 = carobiner::fix_name(r1[["1_6Provincia"]], "title"),
		variety = r1[["2_7Variedad1"]],
		K_fertilizer = r1[["3_5PotasioCantidad1"]],
		rep = r1[["11_1PrepTerrVar"]]
	)
##r1: "Codigo", "1_3Fecha", "1_4Nombre", "1_5Departamento", "1_7Distrito", "1_8Comunidad", "1_9Latitud", "1_10Longitud", "1_11Altitud", "2_1Areatotal", "2_2Nroparcelas", "2_3Areapasto", "2_4AreaDescanzo", "2_5C1nombre", "2_5C1Area", "2_5C1Scacos", "2_5C1Kilos", "2_5C1Tenencia", "2_5C1Riego", "2_5C2nombre", "2_5C2Area", "2_5C2Scacos", "2_5C2Kilos", "2_5C2Tenencia", "2_5C2Riego", "2_5C3nombre", "2_5C3Area", "2_5C3Scacos", "2_5C3Kilos", "2_5C3Tenencia", "2_5C3Riego", "2_5C4nombre", "2_5C4Area", "2_5C4Scacos", "2_5C4Kilos", "2_5C4Tenencia", "2_5C4Riego", "2_5C5nombre", "2_5C5Area", "2_5C5Scacos", "2_5C5Kilos", "2_5C5Tenencia", "2_5C5Riego", "2_6V1Nombre", "2_6V1Area", "2_6V1Sacos", "2_6V1CosechaSacos", "2_6V1ConsumoSacos", "2_6V1SemillaSacos", "2_6V1SemillaDonde", "2_6V1SemillaFreqComp", "2_6V1ProcesaSacos", "2_6V1ProcesaProduc", "2_6V1VentaSacos", "2_6V1VentaDonde", "2_6V1Precio", "2_6V2Nombre", "2_6V2Area", "2_6V2Sacos", "2_6V2CosechaSacos", "2_6V2ConsumoSacos", "2_6V2SemillaSacos", "2_6V2SemillaDonde", "2_6V2SemillaFreqComp", "2_6V2ProcesaSacos", "2_6V2ProcesaProduc", "2_6V2VentaSacos", "2_6V2VentaDonde", "2_6V2Precio", "2_6V3Nombre", "2_6V3Area", "2_6V3Sacos", "2_6V3CosechaSacos", "2_6V3ConsumoSacos", "2_6V3SemillaSacos", "2_6V3SemillaDonde", "2_6V3SemillaFreqComp", "2_6V3ProcesaSacos", "2_6V3ProcesaProduc", "2_6V3VentaSacos", "2_6V3VentaDonde", "2_6V3Precio", "2_6V4Nombre", "2_6V4Area", "2_6V4Sacos", "2_6V4CosechaSacos", "2_6V4ConsumoSacos", "2_6V4SemillaSacos", "2_6V4SemillaDonde", "2_6V4SemillaFreqComp", "2_6V4ProcesaSacos", "2_6V4ProcesaProduc", "2_6V4VentaSacos", "2_6V4VentaDonde", "2_6V4Precio", "2_6V5Nombre", "2_6V5Area", "2_6V5Sacos", "2_6V5CosechaSacos", "2_6V5ConsumoSacos", "2_6V5SemillaSacos", "2_6V5SemillaDonde", "2_6V5SemillaFreqComp", "2_6V5ProcesaSacos", "2_6V5ProcesaProduc", "2_6V5VentaSacos", "2_6V5VentaDonde", "2_6V5Precio", "2_6V6Nombre", "2_6V6Area", "2_6V6Sacos", "2_6V6CosechaSacos", "2_6V6ConsumoSacos", "2_6V6SemillaSacos", "2_6V6SemillaDonde", "2_6V6SemillaFreqComp", "2_6V6ProcesaSacos", "2_6V6ProcesaProduc", "2_6V6VentaSacos", "2_6V6VentaDonde", "2_6V6Precio", "2_6V7Nombre", "2_6V7Area", "2_6V7Sacos", "2_6V7CosechaSacos", "2_6V7ConsumoSacos", "2_6V7SemillaSacos", "2_6V7SemillaDonde", "2_6V7SemillaFreqComp", "2_6V7ProcesaSacos", "2_6V7ProcesaProduc", "2_6V7VentaSacos", "2_6V7VentaDonde", "2_6V7Precio", "2_6V8Nombre", "2_6V8Area", "2_6V8Sacos", "2_6V8CosechaSacos", "2_6V8ConsumoSacos", "2_6V8SemillaSacos", "2_6V8SemillaDonde", "2_6V8SemillaFreqComp", "2_6V8ProcesaSacos", "2_6V8ProcesaProduc", "2_6V8VentaSacos", "2_6V8VentaDonde", "2_6V8Precio", "2_6V9Nombre", "2_6V9Area", "2_6V9Sacos", "2_6V9CosechaSacos", "2_6V9ConsumoSacos", "2_6V9SemillaSacos", "2_6V9SemillaDonde", "2_6V9SemillaFreqComp", "2_6V9ProcesaSacos", "2_6V9ProcesaProduc", "2_6V9VentaSacos", "2_6V9VentaDonde", "2_6V9Precio", "2_6V10Nombre", "2_6V10Area", "2_6V10Sacos", "2_6V10CosechaSacos", "2_6V10ConsumoSacos", "2_6V10SemillaSacos", "2_6V10SemillaDonde", "2_6V10SemillaFreqComp", "2_6V10ProcesaSacos", "2_6V10ProcesaProduc", "2_6V10VentaSacos", "2_6V10VentaDonde", "2_6V10Precio", "2_7Variedad2", "2_7Variedad3", "2_7Variedad4", "2_7Variedad5", "2_7Variedad6", "2_7Variedad7", "2_7Variedad8", "2_8Variedad1", "2_8Motivo1", "2_8Variedad2", "2_8Motivo2", "2_8Variedad3", "2_8Motivo3", "2_8Variedad4", "2_8Motivo4", "2_9Comercial", "2_9ComercOtros", "2_9Mezcla", "2_9MezclaOtros", "2_10TrojaC", "2_10TrojaS", "2_10SacosC", "2_10SacosS", "2_10TierraC", "2_10TierraS", "2_10AlmacenC", "2_10AlmacenS", "2_10Otro", "2_10OtroC", "2_10OtroS", "3_1GuanoUtiliza", "3_2GuanoTipo1", "3_2GuanoCantidad1", "3_2GuanoKilosSaco1", "3_2GuanoPrecios1", "3_2GuanoCuantos1", "3_2GuanoTipo2", "3_2GuanoCantidad2", "3_2GuanoKilosSaco2", "3_2GuanoPrecios2", "3_2GuanoCuantos2", "3_3UtilizaAbono", "4_4FoliarTipo1", "4_4FoliarCantidad1", "4_4FoliarUnidad1", "4_4FoliarPrecio1", "4_4FoliarTipo2", "4_4FoliarCantidad2", "4_4FoliarUnidad2", "4_4FoliarPrecio2", "3_5QuimicoUtiliza", "3_5UreaCantidad1", "3_5UreaPrecio1", "3_5PotasioPrecio1", "3_5FosforoCantidad1", "3_5FosforoPrecio1", "3_5NitratoCantidad1", "3_5NitratoPrecio1", "3_5Comp2020Cant1", "3_5Comp2020Prec1", "3_5Otros1", "3_5OtrosCant1", "3_5OtrosPrecio1", "3_5Otros2", "3_5OtrosCant2", "3_5OtrosPrecio2", "4_1Plaga1a", "4_1Producto1a", "4_1NroAplic1a", "4_1Plaga1b", "4_1Producto1b", "4_1NroAplic1b", "4_1Plaga1c", "4_1Producto1c", "4_1NroAplic1c", "4_1Plaga2a", "4_1Producto2a", "4_1NroAplic2a", "4_1Plaga2b", "4_1Producto2b", "4_1NroAplic2b", "4_1Plaga2c", "4_1Producto2c", "4_1NroAplic2c", "4_1Plaga3a", "4_1Producto3a", "4_1NroAplic3a", "4_1Plaga3b", "4_1Producto3b", "4_1NroAplic3b", "4_1Plaga3c", "4_1Producto3c", "4_1NroAplic3c", "4_1Plaga4a", "4_1Producto4a", "4_1NroAplic4a", "4_1Plaga4b", "4_1Producto4b", "4_1NroAplic4b", "4_1Plaga4c", "4_1Producto4c", "4_1NroAplic4c", "4_1Enferm1a", "4_1EnfProduc1a", "4_1EnfAplic1a", "4_1Enferm1b", "4_1EnfProduc1b", "4_1EnfAplic1b", "4_1Enferm1c", "4_1EnfProduc1c", "4_1EnfAplic1c", "4_1Enferm2a", "4_1EnfProduc2a", "4_1EnfAplic2a", "4_1Enferm2b", "4_1EnfProduc2b", "4_1EnfAplic2b", "4_1Enferm2c", "4_1EnfProduc2c", "4_1EnfAplic2c", "4_1Enferm3a", "4_1EnfProduc3a", "4_1EnfAplic3a", "4_1Enferm3b", "4_1EnfProduc3b", "4_1EnfAplic3b", "4_1Enferm3c", "4_1EnfProduc3c", "4_1EnfAplic3c", "4_1Enferm4a", "4_1EnfProduc4a", "4_1EnfAplic4a", "4_1Enferm4b", "4_1EnfProduc4b", "4_1EnfAplic4b", "4_1Enferm4c", "4_1EnfProduc4c", "4_1EnfAplic4c", "5_1Ventonsumo", "5_1VentMercado", "5_1VentMas Rendimiento", "5_1VentMenos Plagas", "5_1VentMayor Precio", "5_1VentOtrosV1", "5_1VentOtros1", "5_1VentOtrosV2", "5_1VentOtros2", "5_1VentOtrosV3", "5_1VentOtros3", "5_1DesvNoVende", "5_1DesvNoSemilla", "5_1DesvPlagas", "5_1DesvEnferm", "5_1DesvOtrosV1", "5_1DesvOtros1", "5_1DesvOtrosV2", "5_1DesvOtros2", "5_1DesvOtrosV3", "5_1DesvOtros3", "6_1Nombre1", "6_1GradoParent1", "6_1Sexo1", "6_1Edad1", "6_1GradoInst1", "6_1EstadoCivil1", "6_1InvolucraAgri1", "6_1Nombre2", "6_1GradoParent2", "6_1Sexo2", "6_1Edad2", "6_1GradoInst2", "6_1EstadoCivil2", "6_1InvolucraAgri2", "6_1Nombre3", "6_1GradoParent3", "6_1Sexo3", "6_1Edad3", "6_1GradoInst3", "6_1EstadoCivil3", "6_1InvolucraAgri3", "6_1Nombre4", "6_1GradoParent4", "6_1Sexo4", "6_1Edad4", "6_1GradoInst4", "6_1EstadoCivil4", "6_1InvolucraAgri4", "6_1Nombre5", "6_1GradoParent5", "6_1Sexo5", "6_1Edad5", "6_1GradoInst5", "6_1EstadoCivil5", "6_1InvolucraAgri5", "6_1Nombre6", "6_1GradoParent6", "6_1Sexo6", "6_1Edad6", "6_1GradoInst6", "6_1EstadoCivil6", "6_1InvolucraAgri6", "6_1Nombre7", "6_1GradoParent7", "6_1Sexo7", "6_1Edad7", "6_1GradoInst7", "6_1EstadoCivil7", "6_1InvolucraAgri7", "6_1Nombre8", "6_1GradoParent8", "6_1Sexo8", "6_1Edad8", "6_1GradoInst8", "6_1EstadoCivil8", "6_1InvolucraAgri8", "6_1Nombre9", "6_1GradoParent9", "6_1Sexo9", "6_1Edad9", "6_1GradoInst9", "6_1EstadoCivil9", "6_1InvolucraAgri9", "7_1AsocProd", "7_1AsocProdQuien", "7_1AsoProdForma", "7_1AsocProdCargo", "7_1Club", "7_1ClubQuien", "7_1ClubForma", "7_1ClubCargo", "7_1AsocPadres", "7_1AsocPadresQuien", "7_1AsocPadreForma", "7_1AsoPadrescCargo", "7_1ComRegantes", "7_1ComRegQuien", "7_1ComRegForma", "7_1ComRegCargo", "7_1Otros", "7_1OtrosResp", "7_1OtroQuien", "7_1OtroForma", "7_1OtroCargo", "7_2AsocRelac", "7_2AsocGrado", "7_2AgencRelac", "7_2AgencGrado", "7_2INIARel", "7_2INIAGrado", "7_2JuntaRelc", "7_2JuntaGrado", "7_2ProdRelac", "7_2ProdGrado", "7_2IntermRel", "7_2IntermGrado", "7_2VecinosRel", "7_2VecinosGrado", "7_2ProducRel", "7_2ProducGrado", "7_2TiendaRel", "7_2TiendasGrado", "7_2ONGRel", "7_2ONGGrado", "7_2FeriasRel", "7_2FeriasGrad", "7_2Otros", "7_2OtrosRel", "7_2OtrosGrado", "8_1Vivienda", "8_1Otra", "8_2Agua", "8_2Otra", "8_3Alumbrado", "8_3Otra", "8_4Servicios", "8_4Otros", "8_5Bicicleta", "8_5BicicletaCuant", "8_5Moto", "8_5MotoCuant", "8_5Carro", "8_5CarroCuant", "8_5TV", "8_5TvCuant", "8_5Telefono", "8_5TelefonoCuant", "8_5Tractor", "8_5TractorCuant", "8_5Yunta", "8_5YuntaCuant", "8_5Mochi", "8_5MochiCuant", "8_6Cria", "8_6Vacas", "8_6Toros", "8_6Caballos", "8_6Burros", "8_6Ovinos", "8_6Pollos", "8_6Patos", "8_6Alpacas", "8_6Cerdos", "8_6Llamas", "8_6Cabras", "8_6Cuyes", "8_6Conejos", "8_7Otras", "8_7OtrasDesc", "8_8Actividad", "8_8ActividadOtros", "9_1Finaciamiento", "9_1Quien1", "9_1DeQuien1", "9_1EnqueUtil1", "9_1Quien2", "9_1DeQuien2", "9_1EnqueUtil2", "9_1Quien3", "9_1DeQuien3", "9_1EnqueUtil3", "10_1Fuente", "10_2Capacitacion", "10_2Quien1", "10_2Dequien1", "10_2Tema1", "10_2Quien2", "10_2Dequien2", "10_2Tema2", "10_3TemaCapacit", "11_1ComInVar", "11_1PrepSemVar", "11_1SiembraVar", "11_1CultivoVar", "11_1ControlVar", "11_1CosechalVar", "11_1ProcesarVar", "11_1AlmacVar", "11_1VentaVar", "11_1LleVar", "11_1AniMenoVar", "11_1AniMayVar", "11_ComercVar", "11_1CocinarVar", "11_1LimpiarVar", "11_1LaVar", "11_1AlimVar", "11_1ManTieVar", "11_1ManDinVar", "11_1ComInMuj", "11_1PrepTerrMuj", "11_1PrepSemMuj", "11_1SiembraMuj", "11_1CultivoMuj", "11_1ControlMuj", "11_1CosechalMuj", "11_1ProcesarMuj", "11_1AlmacMuj", "11_1VentaMuj", "11_1LleMuj", "11_1AniMenoMuj", "11_1AniMayMuj", "11_ComercMuj", "11_1CocinarMuj", "11_1LimpiarMuj", "11_1LaMuj", "11_1AlimMuj", "11_1ManTieMuj", "11_1ManDinMuj", "11_1ComInAmb", "11_1PrepTerrAmb", "11_1PrepSemAmb", "11_1SiembraAmb", "11_1CultivoAmb", "11_1ControlAmb", "11_1CosechalAmb", "11_1ProcesarAmb", "11_1AlmacAmb", "11_1VentaAmb", "11_1LleAmb", "11_1AniMenoAmb", "11_1AniMayAmb", "11_ComercAmb", "11_1CocinarAmb", "11_1LimpiarAmb", "11_1LaAmb", "11_1AlimAmb", "11_1ManTieAmb", "11_1ManDinAmb", "11_1ComInVaM", "11_1PrepTerrVaM", "11_1PrepSemVaM", "11_1SiembraVaM", "11_1CultivoVaM", "11_1ControlVaM", "11_1CosechalVaM", "11_1ProcesarVaM", "11_1AlmacVaM", "11_1VentaVaM", "11_1LleVaM", "11_1AniMenoVaM", "11_1AniMayVaM", "11_ComercVaM", "11_1CocinarVaM", "11_1LimpiarVaM", "11_1LaVaM", "11_1AlimVaM", "11_1ManTieVaM", "11_1ManDinVaM", "11_1ComInMuM", "11_1PrepTerrMuM", "11_1PrepSemMuM", "11_1SiembraMuM", "11_1CultivoMuM", "11_1ControlMuM", "11_1CosechalMuM", "11_1ProcesarMuM", "11_1AlmacMuM", "11_1VentaMuM", "11_1LleMuM", "11_1AniMenoMuM", "11_1AniMayMuM", "11_ComercMuM", "11_1CocinarMuM", "11_1LimpiarMuM", "11_1LaMuM", "11_1AlimMuM", "11_1ManTieMuM", "11_1ManDinMuM"


	d2 <- data.frame()
##r2: "Variable name", "Descripcion_de_Variables", "Valor_de_la_Variable", "Observacion"


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

