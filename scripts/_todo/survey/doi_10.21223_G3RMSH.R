# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Linea de base de Conocimientos, Actitudes y Prácticas (CAP) sobre Deficiencia de Hierro (Anemia) en familias productoras de papa en la Sierra de La Libertad

Through a gender-responsive pilot intervention, the project Anemia Zero is contributing to improve food security, nutrition, gender equality, and empowerment in potato-based production systems in high Andean regions of Peru (Curgos and Julcan Districts, Province Sanchez Carrion, Department of La Libertad) and to scale agricultural and nutrition innovations in coordination with the social protection programs (SPP) Juntos and Cuna Mas (both overseen by the Ministry of Development and Social Inclusion (MIDIS), as well as with other stakeholders. This survey was conducted between October 10th and December 21st, 2020 to measure current knowledge, attitude and practices corresponding to Anemia among 242 potato producing households (142 households from the intervention group and 100 households from the control group. The survey presented 75 questions divided in the following sections: (1) Informed consent (2 questions), (A) Socioeconomic information from both parents and children (21 questions), (B) knowledge, attitude and practices corresponding to Anemia (31 questions), (C) crop production aspects (20 questions).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/G3RMSH"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Instituto de Investigación Nutricional - IIN; PATAZ",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Smallholder potato producing households; universe: Potato producing households in Julcán, and Sánchez Carrión; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: We purposively selected two provinces in Julcán, and Sanchez Carrión, in northern Peru. In each district, we also purposively selected 242 smallholder potato producing households, 142 households in the intervention area of Sánchez Carrión and 100 households in the control group in Julcan. This is the baseline survey of the Knowledge, attitudes and practices about Anemia.; collectionMode: Enumerators visited selected random households in each village and collected the information required in the 11-page questionnaire with 75 questions in 4 sections including a informed consent using Microsoft Form survey template.; researchInstrument: Structured household survey questionnaire",
		
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
	

	f1 <- ff[basename(ff) == "01_DataSet_CAP Anemia - La Libertad 2021 - Linea de Base v3.xlsx"]
	f2 <- ff[basename(ff) == "02_DataDictionary_CAP Anemia - La Libertad 2021 - Linea de Base v3.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm2 = carobiner::fix_name(r1[["Provincia"]], "title"),
		variety = r1[["¿Cuáles son las variedades de papa que son ricas en hierro?"]],
		rep = r1[["¿Qué tan bueno cree que sea preparar comidas con alimentos ricos en hierro, por ejemplo res, hígado, sangrecita?"]]
	)
##r1: "Grupo", "ID", "¿Está de acuerdo con que le haga estas preguntas?", "¿Tiene usted algún comentario para hacernos antes de comenzar?", "Código:", "Fecha de la encuesta: (Dia - Mes - Año)", "Etapa de la encuesta:", "Departamento", "Distrito", "Comunidad", "Persona Entrevistada: Genero", "Persona Entrevistada: Relación Con el niño/niña", "Datos generales del niño/niña: Fecha de nacimiento  (Dia - Mes - Año)", "Datos generales del niño/niña: Edad en meses (coloque solo el número de meses)", "Datos generales del niño/niña. Genero:", "Datos generales de la madre: Edad en años (coloque solo el número de años)", "Madre: Grado de instruccion", "Madre: A que se dedica", "Madre: Cuantos hijos vivos tiene:", "Madre: Cuantos niños menores de 3 años viven en la casa", "Datos generales del padre: Edad del padre en años (del niño índice de la encuesta)", "Padre:  Grado de instrucción:", "Padre: A que se dedica principalmente", "¿Ha escuchado hablar de la anemia o deficiencia en hierro?", "¿Cómo se puede observar en alguien que tiene anemia?", "P24a", "P24b", "P24c", "P24d", "P24CP", "¿Cuáles pudieran ser las consecuencias por la carencia de hierro en la dieta de los niños?", "P25a", "P25b", "P25c", "P25d", "P25CP", "¿Cuáles pudieran ser las consecuencias por la carencia de hierro en la dieta de las mujeres embarazadas?", "P26a", "P26b", "P26c", "P26d", "P26CP", "¿Cuáles son las causas de la anemia?", "P27a", "P27b", "P27c", "P27d", "P27CP", "¿Cómo se puede prevenir la anemia?", "P28a", "P28b", "P28c", "P28d", "P28CP", "¿Me puede decir que alimentos son ricos en hierro?", "P29a", "P29b", "P29c", "P29d", "P29e", "P29f", "P29CP", "Y ¿sabe que alimentos ayudan a que el hierro se aproveche mejor?", "P30a", "P30b", "P30c", "P30CP", "Por el contrario ¿Qué alimentos hacen que el hierro NO se aproveche bien en el cuerpo?", "P31a", "P31b", "P31CP", "¿Considera que la papa es buena para combatir la anemia en los niños?", "Si la respuesta fue "Sí",  preguntar: ¿Cómo se enteró sobre esta propiedad de la papa?", "P33a", "P34a", "P34b", "P34c", "P34d", "P35_1", "P35_2", "P35_3", "P35_4", "P35_5", "P35_6", "P35_7", "P35_8", "P35_9", "P35_10", "P35_11", "P35_12", "P35_13", "P35_14", "P35_15", "P35_16", "P35_17", "P35_18", "P35_19", "P35_20", "Si en la pregunta anterior marcó "Otros" ¿Cuales son esos otros alimentos que consumió?", "P36_a", "Usualmente su niño (nombre del niño) consume cítricos frescos o jugos de cítricos; por ejemplo mandarinas, naranja, lima,  aguaymanto, piña , kiwi, etc.", "Si Pregunta anterior dijo "Sí" ¿Esto es todos los días?", "¿Cuándo suele consumir las frutas cítricas? (Leer las siguientes opciones)", "Usualmente, ¿le dá a su niño de beber té o café?", "Si la pregunta anterior dijo "Si" ¿Esto es todos los días?", "Usualmente, cuando le da a su niño de beber té o café, en qué momento se lo ofrece  (Lea las siguientes opciones al entrevistado)", "¿Cree que es probable (posible) que su hijo tenga anemia?", "Si en la pregunta anterior dijo "Poco Probable" ¿Me podría decir por qué razones es poco probable?", "P44a", "P44b", "¿Qué tan grave cree que sea la anemia en los niños/niñas?", "Si en la pregunta anterior dijo "Grave" ¿Me podría decir por qué razones es Grave?", "P46a", "P46b", "P46c", "Si en la pregunta anterior, dijo "No es bueno" ¿Me podría decir por qué razones no es bueno?", "Y ¿Es difícil para usted preparar comidas con alimentos ricos en hierro por ejemplo: res, hígado, sangrecita?", "Si en la pregunta anterior, dijo "Difícil" ¿Me podría decir por qué razones es difícil?", "¿Siente la confianza de preparar comidas con alimentos ricos en hierro como por ejemplo res, hígado, sangrecita?", "Si en la pregunta anterior respondio "No" ¿Me podría decir por qué razones?", "¿Usted cree que la alimentación que le da a su niño es baja, normal (lo suficiente), o alta en hierro?", "¿Que tanto le gusta el sabor de  higado, sangrecita, res  a su niño?", "¿Cuál es el área total que suele sembrar de cultivos, incluyendo los pastos? ( Cantidad - Unidad de Medida)", "P55a", "¿Cuáles son los cultivos que sembró la campaña pasada? (Cultivo1 – Cultivo 2 - ..........)", "P56a", "P56b", "P56c", "P56d", "P56e", "P56f", "P56g", "P56h", "P56i", "¿Qué variedades de papa sembró, en la última campaña de siembra? liste el nombre de las Tres principales, en orden de importancia: Sembró:  VARIEDAD 1", "Para la VARIEDAD 1:  ¿Cuál es la procedencia de la semilla que utiliza?", "Para la VARIEDAD 1: ¿Qué cantidad de semilla sembró la campaña pasada?", "P61.Has", "Para la VARIEDAD 1: ¿Qué cantidad de papa cosechó, en la última temporada de siembra? (Cantidad - Unidad de Medida)", "P62a Toneladas", "Para la VARIEDAD 1: ¿Qué cantidad ha destinado para venta? (Cantidad - Unidad de Medida)", "P63a Toneladas", "Para la VARIEDAD 1: ¿Qué cantidad ha destinado para consumo? (Cantidad - Unidad de Medida)", "P64a Tonelada", "Para la VARIEDAD 1: ¿Qué cantidad ha destinado para semilla? (Cantidad - Unidad de Medida)", "P65a Toneladas", "Sembró: VARIEDAD 2", "Para la VARIEDAD 2:  ¿Cuál es la procedencia de la semilla que utiliza?", "Para la VARIEDAD 2: ¿Qué cantidad de semilla sembró la campaña pasada?", "P67.Has", "Para la VARIEDAD 2: ¿Qué cantidad de papa cosechó, en la última temporada de siembra? (Cantidad - Unidad de Medida)", "P68a Toneladas", "Para la VARIEDAD 2: ¿Qué cantidad ha destinado para venta? (Cantidad - Unidad de Medida)", "P69a Toneladas", "Para la VARIEDAD 2: ¿Qué cantidad ha destinado para consumo? (Cantidad - Unidad de Medida)", "P70a Toneladas", "Para la VARIEDAD 2: ¿Qué cantidad ha destinado para semilla? (Cantidad - Unidad de Medida)", "P71a Toneladas", "Sembró: VARIEDAD 3", "Para la VARIEDAD 3:  ¿Cuál es la procedencia de la semilla que utiliza?", "Para la VARIEDAD 3: ¿Qué cantidad de semilla sembró la campaña pasada?", "P73a Has", "Para la VARIEDAD 3: ¿Qué cantidad de papa cosechó, en la última temporada de siembra? (Cantidad - Unidad de Medida)", "P74a Toneladas", "Para la VARIEDAD 3: ¿Qué cantidad ha destinado para venta? (Cantidad - Unidad de Medida)", "P75a Toneladas", "Para la VARIEDAD 3: ¿Qué cantidad ha destinado para consumo? (Cantidad - Unidad de Medida)", "Toneladas", "Para la VARIEDAD 3: ¿Qué cantidad ha destinado para semilla? (Cantidad - Unidad de Medida)", "P77a Toneladas", "Además de las tres variedades que sembró en la campaña anterior, va a sembrar otras variedades más, ¿cuáles son estas variedades? Variedad 4 – Variedad 5 – Variedad 6 - .........................."


	d2 <- data.frame()
##r2: "Nro", "Variable name", "Abbreviation", "Description", "Method", "Formula", "Unit", "Reference", "...9"


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

