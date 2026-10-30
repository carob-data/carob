# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Exploratory field study of economic profitability among native potato and mashua producers: plot-level data.

This dataset comes from an exploratory field study with 21 farmers in the districts of Pazos (Huancavelica) and Quilcas (Junín), Peru. Semi-structured interviews were conducted with 11 farmers in Pazos and 10 farmers in Quilcas to capture and compare information on production costs and revenues for two groups of target plots with specific cropping combinations: (a) plots with improved potato varieties and (b) plots with native potato varieties grown with mashua. The data are provided at plot level.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/N4NC3R"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
NA
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
	

	f1 <- ff[basename(ff) == "01_Data_ economic profitability.xlsx"]
	f2 <- ff[basename(ff) == "02_Data_dictionary_costs.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		N_fertilizer = r1[["N_trab_total"]],
		P_fertilizer = r1[["P_papa_vendida_UA"]],
		variety = r1[["Variedad_PMEJ"]],
		rep = r1[["Cantidad_MODM_JORNAL_PreparacionTerreno"]]
	)
##r1: "ID_Agricultor", "Zona_de_estudio", "Edad", "Genero", "Pareja_presente", "N_trab_rem", "N_trab_fam", "P_mashua_vendida_UA", "Nro_parcelas_papa", "Nro_parcelas_papa_y_mashua", "Cantidad_semilla_KG_PAPA", "Cantidad_semilla_KG_MASHUA", "Usos_no_comerciales_PAPA", "Usos_no_comerciales_MASHUA", "Razones_para_sembrar_mashua_sola", "Razones_para_no_sembrar_mashua_sola", "ID_Parcela", "Sector", "Pendiente", "Acceso_vial", "Tiempo_al_hogar_a_pie_min", "Tipo_de_propiedad", "Al_partir_Cuanto", "Al_partir_Por_Cuanto", "Al_partir_Principal_beneficiario", "Proposito_de_siembra_VENDER", "Tipo_de_mezcla", "Razones_de_siembra_MASHUA", "Nombre_producto", "Origen_semilla_producto", "Es_semilla_certificada_producto", "Variedad_PNAT", "Variedad_MASH", "Tamanio_semilla_PMEJ", "Tamanio_semilla_PNAT", "Tamanio_semilla_MASH", "Tamanio_semilla_OTRO", "Cultivos_con_igual_densidad_de_siembra", "Distancia_entre_surcos_cm_PMEJ", "Distancia_entre_surcos_cm_PNAT", "Distancia_entre_surcos_cm_PMASH", "Distancia_entre_surcos_cm_OTRO", "Distancia_entre_semillas_cm_PMEJ", "Distancia_entre_semillas_cm_PNAT", "Distancia_entre_semillas_cm_MASH", "Distancia_entre_semillas_cm_OTRO", "Costo_Tierra_observado", "Precio_Tierra_observado", "Unidad_de_alquiler", "Observacion_Input_Tierra", "Cantidad_MODM_JORNAL_Ciclo", "Cantidad_MODM_JORNAL_SiembraFertilizacion", "Cantidad_MODM_JORNAL_ManejoCultivo", "Cantidad_MODM_JORNAL_CosechaSeleccion", "Precio_MODM_JORNAL", "Cantidad_MODF_JORNAL_Ciclo", "Cantidad_MODF_JORNAL_PreparacionTerreno", "Cantidad_MODF_JORNAL_SiembraFertilizacion", "Cantidad_MODF_JORNAL_ManejoCultivo", "Cantidad_MODF_JORNAL_CosechaSeleccion", "Precio_MODF_JORNAL", "Cantidad_semilla_KG_PMEJ", "Precio_semilla_KG_PMEJ", "Cantidad_semilla_KG_PNAT", "Precio_semilla_KG_PNAT", "Cantidad_semilla_KG_MASH", "Precio_semila_KG_MASH", "Cantidad_semilla_KG_OTRO", "Precio_semilla_KG_OTRO", "Cantidad_guano_de_corral_KG", "Precio_guano_de_corral_KG", "Cantidad_guano_de_isla_KG", "Precio_guano_de_isla_KG", "Costo_fertilizante_organico_otros", "Nombres_fertilizante_organico_otros", "Cantidad_fertilizante_balanceado_KG", "Precio_fertilizante_balanceado_KG", "Costo_fertilizante_quimico_otros", "Nombres_fertilizante_quimico_otros", "Nombre_producto_fumigado", "Funcion_producto_fumigado", "Composicion_producto_fumigado", "Formulacion_producto_fumigado", "Nro_de_aplicaciones", "Mochilas_por_aplicacion", "Dosis_en_mochila_por_aplicacion_producto_fumigado", "Unidad_de_dosis_producto_fumigado", "Precio_producto_fumigado", "Unidad_de_venta_producto_fumigado", "Tiempo_fumigacion_x_mochila_horas", "Cantidad_tractor_HORA", "Precio_tractor_HORA", "Cantidad_yunta", "Unidad_de_uso_yunta", "Precio_yunta", "Unidad_de_alquiler_yunta", "Observacion_input_yunta", "Cantidad_cosecha_medida_TOTAL_KG_PMEJ", "Cantidad_cosecha_esperada_TOTAL_KG_PMEJ", "Cantidad_cosecha_esperada_KG1RA_PMEJ", "Cantidad_cosecha_esperada_KG2DA_PMEJ", "Cantidad_cosecha_esperada_KG3RA_PMEJ", "Cantidad_cosecha_historica_TOTAL_KG_PMEJ", "Cantidad_cosecha_historica_KG1RA_PMEJ", "Cantidad_cosecha_historica_KG2DA_PMEJ", "Cantidad_cosecha_historica_KG3RA_PMEJ", "Precio_venta_KG1RA_PMEJ", "Precio_venta_KG2DA_PMEJ", "Precio_venta_KG3RA_PMEJ", "Cantidad_cosecha_medida_TOTAL_KG_PNAT", "Cantidad_cosecha_esperada_TOTAL_KG_PNAT", "Cantidad_cosecha_esperada_KG1RA_PNAT", "Cantidad_cosecha_esperada_KG2DA_PNAT", "Cantidad_cosecha_esperada_KG3RA_PNAT", "Cantidad_cosecha_historica_TOTAL_KG_PNAT", "Cantidad_cosecha_historica_KG1RA_PNAT", "Cantidad_cosecha_historica_KG2DA_PNAT", "Cantidad_cosecha_historica_KG3RA_PNAT", "Precio_venta_KG1RA_PNAT", "Precio_venta_KG2DA_PNAT", "Precio_venta_KG3RA_PNAT", "Cantidad_cosecha_medida_TOTAL_KG_MASH", "Cantidad_cosecha_esperada_TOTAL_KG_MASH", "Cantidad_cosecha_esperada_KG1RA_MASH", "Cantidad_cosecha_esperada_KG2DA_MASH", "Cantidad_cosecha_esperada_KG3RA_MASH", "Cantidad_cosecha_historica_TOTAL_KG_MASH", "Cantidad_cosecha_historica_KG1RA_MASH", "Cantidad_cosecha_historica_KG2DA_MASH", "Cantidad_cosecha_historica_KG3RA_MASH", "Precio_venta_KG1RA_MASH", "Precio_venta_KG2DA_MASH", "Precio_venta_KG3RA_MASH", "Cosecha_afectada", "Tipo_de_afeccion", "Tipos_de_cultivos_afectados", "Actividades_PreparacionTerreno", "Herramientas_manuales_PreparacionTerreno", "Actividades_SiembraFertilizacion", "Herramientas_manuales_SiembraFertilizacion", "Actividades_ManejoCultivo", "Herramientas_manuales_ManejoCultivo", "Actividades_CosechaSeleccion", "Herramientas_manuales_CosechaSeleccion", "Meses_PreparacionTerreno", "Meses_SiembraFertilizacion", "Meses_ManejoCultivo", "Meses_CosechaSeleccion", "Mes_PrecioVentaKG_PMEJ", "Mes_PrecioVentaKG_PNAT", "Mes_PrecioVentaKG_MASH", "Mercado_venta_PMEJ", "Mercado_venta_PNAT", "Mercado_venta_MASH", "Fecha_entrevista_M1", "Fecha_entrevista_M2", "Fecha_entrevista_M3", "Fecha_entrevista_M4", "Fecha_entrevista_M5"


	d2 <- data.frame()
##r2: "Nombre de variable", "Descripcion", "Tipo de dato", "Tipo de pregunta"


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

