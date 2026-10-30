# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Potato native Miskipapa market survey in 2020

The native potato varieties have been successfully marketed to consumers in Lima in 2020 who wish to support the small farmer and the conservation of biodiversity. The survey was carried out on 130 consumers of native potatoes of the Miskipapa brand from different districts of Lima. In the exploratory surveys in 2020 women buy more native potatoes (76%) compared to men (24%). This difference may be due to the fact that they make decisions at home for food preparation. The largest districts of native potato consumption of MiskiPapa 2020 are found in Miraflores, followed by Santiago de Surco with 23%, La Molina with 20% and San Borja with 9%.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/WCWUNE"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP; Yanapai Group",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: One Consumer of native potatoes of the Miskipapa brand; universe: Consumers of native potatoes of the Miskipapa brand from different districts of Lima; dataCollector: Yanapai and International Potato Center",
		
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
	

	f1 <- ff[basename(ff) == "01_Encuesta_sobre_demanda_Miskipapa_2020.xlsx"]

	r1 <- carobiner::read.excel(f1)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm2 = carobiner::fix_name(r1[["1.1. DISTRITO:"]], "title"),
		variety = r1[["3.5.a ¿Prefiere la papa nativa surtida (mezcla de variedades) o separada por variedad?"]]
	)
##r1: "Fecha", "Año_encuesta", "Usuario", "1.3.a.Punto de recojo", "1.3.b. Si hubiera un punto de recojo a menos de 1 km de su casa, ¿lo recogería ahí o prefiere seguir recibiendo su pedido en su domicilio?", "1. 4. ¿Qué sugerencia tendría para el futuro de cómo tener acceso a estas papas nativas?", "2.1. Edad (años)", "2.2. Género", "2.3. Lugar de Nacimiento (Ciudad/ País):", "2.4.a. Nro. de integrantes del hogar:", "2.4.b. ¿Cuántos menores de 18 años?", "2.5. Ocupación:", "3.1. ¿Con qué frecuencia compra papas nativas?", "3.2. ¿Cuándo empezó a comprar papa nativa con nosotros (AGUAPAN)?", "3.3. ¿Realiza compras de papa nativa en otro(s) sitio(s)?", "3.3.* Si es que marco "Sí", ¿Dónde?", "3.4. ¿Qué le motivó a comprar papa nativa?", "3.5.b ¿Por qué?", "3.5.c ¿Está Ud. de acuerdo que vender en mezcla es algo novedoso para preservar la diversidad?", "3.6.a. ¿Cuántos kilos de papa nativa surtida suele comprar en cada oportunidad?", "3.6.b. ¿Suele comprar solamente para su hogar o aglutina pedidos de un grupo más amplio (comunidad/ edificio/ vecindario)?", "3.7.a. ¿Sabe de dónde provienen o quién cultiva las papas que Ud. compra ?", "3.7.b. Si la respuesta es sí, ¿De quién/es suele comprar?", "3.8.a. ¿Considera que el precio que paga por kilo de papas nativas de AGUAPAN (S/5.00) es:", "3.9.a ¿Le interesaría saber el nombre de algunas de las variedades?", "3.9.b. ¿Qué le gustaría que mejore en los lotes de papas nativas que Ud. compra de AGUAPAN?", "3.9.c. ¿Qué le gustaría que mejore del servicio de venta y entrega de papas nativas que Ud. compra de AGUAPAN?", "Si marcó "Otro", por favor especifique:", "3.8.b.  Actualmente hacemos en Lima envíos a domicilio con un recargo de S/10.00.", "...32"


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

