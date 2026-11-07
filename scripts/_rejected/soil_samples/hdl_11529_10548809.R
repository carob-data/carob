# R script for "carob"
# license: GPL (>=3)

#REJECTED because these records are also included (with 2 decimal coordinates) in the more recent dataset doi_10.71682_10549439.R

## ISSUES
#1. Added other variables not in terminag vocabulary

carob_script <- function(path) {
  
"
Fertility Maps of Chamic, Chiapas
  
Soil sampling with 1x1 km grid in the agricultural area of the Chamic Municipality at at 0 to 30 cm depth. (2022-10-14)
"
 
  uri <- "hdl:11529/10548809"
  group <- "soil_samples"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication=NA,
		project=NA,
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Blessing Dzuda",
		carob_effort = NA,
		carob_date="2025-09-05",
		carob_completion = 100,	
		design=NA,
		notes = "1. Deleted the coordinates from the dataset because they were from Guatemala instead of Mexico. 2. Coordinates were filled in by looking up the places on google maps. NA locations were looked up using GADM. 3.some locations appear to be far away from the rest of the other records when visualized on a map, but since the location name was found in the correct adm1/2 provided by the dataset, i proceeded with those locations points"
  )
  
  f <- ff[basename(ff) == "BD_Suelo_Chamic_Chiapas.xlsx"]
  r <- carobiner::read.excel(f, sheet ="Suelos", na=c("NA","."))
  r <- r[-1, ] #removing the second row with units
  
  d <- data.frame(
    country = "Mexico",
    adm1 = r$Estado,
    adm2 = r$Municipio,
    location= r$Rancho,
    #longitude = as.numeric(r$Longitud),
    #latitude = as.numeric(r$Latitud), discarding the lat and lon data since all coordinates are in Guatemala instead of Mexico
    soil_sand =as.numeric(r$Arena),
    soil_clay =as.numeric(r$Arcilla),
    soil_silt =as.numeric(r$Limo),
    soil_texture = r$`Clase Textural`,
    soil_WHC_sat =as.numeric(r$`Punto de Saturación`),
    soil_bd =as.numeric(r$DAP),
    soil_pH = as.numeric(r$`pH(1:2 Agua)`),
    soil_SOM =as.numeric(r$`Materia Orgánica`),
    soil_N =as.numeric(r$`Nitrógeno (N-NO3)`),
    soil_K =as.numeric(r$`Potasio (K)`),
    soil_Ca =as.numeric(r$`Calcio (Ca)`),
    soil_Mg =as.numeric(r$`Magnesio (Mg)`),
    soil_Na =as.numeric(r$`Sodio (Na)`),
    soil_Fe =as.numeric(r$`Hierro (Fe)`),
    soil_Zn =as.numeric(r$`Zinc (Zn)`),
    soil_Mn =as.numeric(r$`Manganeso (Mn)`),
    soil_Cu =as.numeric(r$`Cobre (Cu)`),
    soil_B =as.numeric(r$`Boro (B)`),
    soil_Al =as.numeric(r$`Alumino (Al)`),
    soil_S =as.numeric(r$`Azufre (S)`),
    soil_CEC=as.numeric(r$`(CIC)`),
    soil_EC =as.numeric(r$`CE (Extracto)`),
    geo_from_source = FALSE
  )
  
   #cleaning and combining phosphorus from 2 diff tests
   r$`Fosforo Bray 1` <- as.numeric(r$`Fosforo Bray 1`)
   r$`Fosforo Olsen`  <- as.numeric(r$`Fosforo Olsen`)
   d$soil_P <- ifelse(!is.na(r$`Fosforo Bray 1`), r$`Fosforo Bray 1`, r$`Fosforo Olsen`)
   d$soil_P_method <- ifelse(!is.na(r$`Fosforo Bray 1`), "Bray1",
                             ifelse(!is.na(r$`Fosforo Olsen`), "Olsen", NA))
   
   
   #splitting depth column
   depth <- do.call(rbind, strsplit(r$Profundidad, "-"))
   d$depth_top <- as.numeric(depth[,1])
   d$depth_bottom <- as.numeric(depth[,2])

   
   tex <- d$soil_texture
   d$soil_texture <- gsub("Franco Arcillo Arenoso","sandy clay loam",d$soil_texture)
   d$soil_texture <- gsub("Franco Arcilloso","clay loam", d$soil_texture)
   d$soil_texture <- gsub("Arcilla", "clay", d$soil_texture)
   d$soil_texture <- gsub("Franco", "loam", d$soil_texture)
   
   soilmeta <- data.frame(
     variable = c("soil_Al", "soil_B", "soil_Ca", "soil_Fe", "soil_K", "soil_Mg", "soil_Mn", "soil_Na", "soil_S", "soil_P", "soil_Zn", "soil_Cu","soil_N"),
     method = c("Potassium Chloride","DTPA+Sorbitol pH 7","Ammonium Acetate","DTPA+Sorbitol pH 7","Ammonium Acetate","Ammonium Acetate","DTPA+Sorbitol pH 7","Ammonium Acetate","Turbidimetric","Bray1;Olsen","DTPA+Sorbitol pH 7","DTPA+Sorbitol pH 7","Salicyclic Acid Nitration")
   )

   d$location <- carobiner::fix_name(d$location, "title")
   
   geo <- data.frame(
     location = c(
       "20 De Abril", "2da Ampliacion Rodulfo Figueroa", "Agua Zarca", "Coloncito", "El Paraiso", "Flor De Mayo", "Joaquin Miguel Gutierrez", "Joaquin Miguel Gutierrez (quespala)", "Juan Sabines", "Linda Vista", "Llano Grande", "Los Laureles", "Niagara", "Nueva Agua Zarca", "Nueva Linda", # 15 values
       "Nuevo Villaflores", "Pequena Propiedad", "Perlas Del Grijalva", "Pueblo Nuevo", "Rancheria Los Cerritos", "Reforma", "Reforma Agraria", "Revolucion", "Rio Grande", "Rodulfo Figueroa", "San Antonio", "San Caralampio", "San Caralampio-Maquina Vieja", "San Caralimpio", "San Francisco", # next 15 values
       "San Francisco Playa Grande", "San Gregorio", "San Jose Las Palmas", "Sinaloa", "Tamaulipas", "Tierra Blanca"
     ),
     latitude = c(
       15.852,15.854,15.846,15.781,15.85,15.856,15.81,15.81,15.855,16.261,15.827,15.892,15.663,15.846,17.098, # 15 values
       15.857,15.667,15.893,18.147,15.432,15.856,15.85,15.84,15.848,15.918,15.833,15.834,15.827,15.827,15.071, # next 15 values
       15.839,15.839,19.367,16.6,25.169,18.431
     ),
     longitude = c(
       -91.971,-92.06,-92.007,-92.072,-91.973,-92.007,-91.945,-91.945,-91.982,-92.124,-91.922,-92.112,-92.143,-92.007,-92.435, # 15 values
       -91.981,-92.148,-92.061,-96.079,-92.891,-92.007,-92.004,-91.977,-92.004,-91.898,-92,-92.033,-92.038,-92.038,-92.311, # next 15 values
       -92.012,-91.982,-98.93,-93.624,-98.187,-96.352
     )
   )   
   d <- merge(d,geo,by="location", all.x=TRUE)
   
   d$longitude[is.na(d$location)] <- -92.088
   d$latitude[is.na(d$location)] <- 15.7628
   d$geo_uncertainty <- ifelse(is.na(d$location),20954,NA)
   d$geo_source <- ifelse(is.na(d$location),"GADM 4.1, adm2","Google Maps")
   

  carobiner::write_files(path, meta, d)
 }
