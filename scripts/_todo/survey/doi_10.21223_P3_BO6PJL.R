# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Estudio de Base de Metodologías Participativas: “Seguimiento y Evaluación Participativa - SEP” Caso CONPAPA- Ecuador, 2009.

El presente estudio de base de la metodología Seguimiento y Evaluación Participativo (SEP) implementada por el Consorcio de Productores de Papa de la Región Central del Ecuador (CONPAPA), y da cuenta del valor inicial de los principales indicadores de alcances e impactos que se promueve con la aplicación de la metodología antes citada. CONPAPA implementa la metodología del SEP en dos proyectos que viene ejecutando en dos provincias del Ecuador, Bolívar y Tungurahua. El primero de ellos es el Proyecto 'Sistema de multiplicación de semillas de la Asociación San Francisco de Rumipampa' ejecutado en el cantón San Miguel de la provincia de Bolívar. El segundo es el Proyecto 'Producción y Comercialización de la Papa en la Asociación Señor de la Justicia de la localidad Shaushi', implementado en el cantón Quero de la provincia de Tungurahua. El número de agricultores (familias) encuestadas del Grupo Objetivo fueron de la comunidad de San Fco. de Rumipampa (30), Shaushi (26) y del Grupo Control de la comunidad de Cochabamba.
"

	uri <- "doi:10.21223/P3/BO6PJL"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: La unidad de análisis es principalmente la familia de los agricultores que participan en la aplicación del SEP, y el informante ha sido el representante de la familia que pertenece a las organizaciones de base que forma parte de la aplicación del SEP (Trabajadores Autónomos San Francisco de Rumipampa y Señor de Justicia). Los registros de datos dado los productores de papas nativas.; universe: Los 82 productores de papas nativas en Ecuador, de las comunidades el grupo objetivo lo formaron las comunidades San Fco. de Rumipampa (30) y Shaushi (26), el grupo control por las comunidades San Luis (11) y Cochabamba (15).; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: La técnica que se utilizó en el grupo objetivo fue el censo, es decir se entrevistó a todos los socios de las dos asociaciones.; collectionMode: La encuesta se aplicó a los productores del grupo objetivo como de Control, la cual consta de 46 preguntas.; researchInstrument: Se aplico una encuesta con preguntas estructuradas.",
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Your Name",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "01_Database SEP CONPAPA Ecuador.xlsx"]
	f2 <- ff[basename(ff) == "02_Data_dictionary SEP CONPAPA Ecuador.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
