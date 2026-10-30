# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Potato native Miskipapa market survey in 2021

The native potato varieties have been successfully marketed to consumers in Lima in 2021 who wish to support the small farmer and the conservation of biodiversity.   The survey was carried out on 93 consumers of native potatoes of the Miskipapa brand from different districts of Lima. This dataset included information about the demand of Miskipapa Consumers by gender, Product demand analysis, Percentage of delivery and pickup method, Percentage of distribution by districts,  Major districts of distributions in Lima, Number of kilos of native potatoes that consumers buy, and Frequency of purchase of native potatoes.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/PFUUMA"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Yanapai Group",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: One Consumer of native potatoes of the Miskipapa brand; universe: 93 consumers of native potatoes of the Miskipapa brand from different districts of Lima; dataCollector: Yanapai and International Potato Center",
		
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
	

	f1 <- ff[basename(ff) == "01_Encuesta_sobre_demanda_Miskipapa_2021.xlsx"]

	r1 <- carobiner::read.excel(f1)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm2 = carobiner::fix_name(r1[["DISTRITO de residencia"]], "title"),
		variety = r1[["¿Prefiere la papa nativa surtida (mezcla de variedades) o separada por variedad?"]]
	)
##r1: "Numeracion", "Marca temporal", "Año", "¿Cómo supo de MISKI PAPA de AGUAPAN?", "Usuario", "¿Ha comprado MISKI PAPA de AGUAPAN?", "¿Desde cuándo?", "¿Dónde/ cómo suele recibir @miski.papa?", "¿Recogería las papas nativas en un lugar cercano?", "DISTRITO", "Urbanización/ Zona/ Feria", "Edad", "Género", "Lugar de Nacimiento (Ciudad)", "Pais de Nacimiento", "Ocupación", "Nro. de integrantes del hogar", "¿Cuántos menores de 18 años?", "¿Con qué frecuencia suele comprar papas nativas?", "¿Realiza compras de papa nativa en otro(s) sitio(s)?", "* Si es que marco "Sí", ¿Dónde?", "¿Qué le motiva a comprar MISKI PAPA? (Puede marcar más de una opción).", "* ¿Por qué?", "¿Cuántos kilos de papa nativa surtida suele comprar en cada oportunidad?", "Fomentamos el comercio directo y las compras grupales para beneficio de los productores y de los consumidores. ¿Suele comprar solamente para su hogar o aglutina pedidos de un grupo más amplio (comunidad/ edificio/ vecindario)?", "¿Le interesaría saber más acerca de la procedencia de las papas que Ud. adquiere y de los agricultores que producen MISKI PAPA?", "* Si la respuesta es sí, ¿De quién/es suele comprar?", "¿Está Ud. de acuerdo en que vender papas nativas en mezcla es algo novedoso para preservar la diversidad?", "¿Le interesaría saber el nombre de las variedades papas nativas que consume?", "El objetivo de MISKI PAPA es que el dinero regrese al campo en un esquema de comercio justo. ¿Cuánto está dispuesto a pagar por un kilo de papas nativas en mezcla?", "¿Tendría alguna sugerencia de cómo mejorar el acceso y la presentación de MISKI PAPA?*", "¿Tiene algún comentario o mensaje adicional?"


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

