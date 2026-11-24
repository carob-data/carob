# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {
  
"
Fertility Maps of Tlalquiltenango, Morelos. 2020

Soil sampling with 1x1 km grid in the agricultural area of the Tlalquiltenango Municipality at at 0 to 30 cm depth.
"
  
  uri <- "hdl:11529/10548654"
  group <- "soil_samples"
  ff <- carobiner::get_data(uri, path, group)
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication=NA,
		project=NA,
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Mitchelle Njukuya",
		carob_effort = NA,
		carob_date="2025-09-18",
		carob_completion = 100,	
		design=NA,
		notes = NA
  )
  
  f <- ff[basename(ff) == "BD_Suelo_Tlaquiltenango.xlsx"]
  r <- carobiner::read.excel(f, sheet ="Tlaquiltenango", na=".")
  r <- r[-1, ] #removing the second row with units
  
  d <- data.frame(
    country = "Mexico",
    adm1 = r$Estado,
    adm2= r$Municipio,
    location =tolower(r$Rancho),
    latitude = r$Latitud,
    longitude = r$Longitud,
    soil_sand =as.numeric(r$Arena),
    soil_clay =as.numeric(r$Arcilla),
    soil_silt =as.numeric(r$Limo),
    soil_texture = r$`Clase Textural`,
    soil_WHC_sat =as.numeric(r$`Punto de Saturación`),
    soil_bd =as.numeric(r$`Densidad Aparente`),
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
    geo_from_source = TRUE
  )
  
  d$soil_P <- as.numeric(r$`Fosforo Bray 1`)
  d$soil_P_method <- "Bray1"
  
  
  depth <- do.call(rbind, strsplit(r$Profundidad, "-"))
  d$depth_top <- as.numeric(depth[,1])
  d$depth_bottom <- as.numeric(depth[,2])
  d$soil_texture <- gsub("Franco Arcillo Arenoso","sandy clay loam",d$soil_texture)
  d$soil_texture <- gsub("Franco Arcilloso","clay loam",d$soil_texture)
  d$soil_texture <- gsub("Franco","loam",d$soil_texture)
  d$soil_texture <- gsub("Arcilla","clay",d$soil_texture)
  
  soilmeta <- data.frame(
    variable = c("soil_Al", "soil_B", "soil_Ca", "soil_Fe", "soil_K", "soil_Mg", "soil_Mn", "soil_Na", "soil_S", "soil_Zn", "soil_Cu","soil_N"),
    method = c("Mehlich3"))
  soilmeta$soil_P <- "Bray1"
  
  #fix geolocation names and add coordinates
  x <- tolower(trimws(gsub("\\s+", " ", d$location)))
  x[x %in% c("na", "")] <- NA
  geo <- as.data.frame(rbind(
    ## raw name,                       location,                         lat,      lon,       uncertainty (m)
    c("santiopan",                     "Santiopa",                       18.42965, -98.95135, NA),     # https://www.geonames.org/3514575
    c("xochipala",                     "Xochipala",                      18.42419, -99.0514,  NA),     # https://www.geonames.org/3514443
    c("rancho viejo",                  "Rancho Viejo",                   18.42444, -99.01361, NA),     # https://www.geonames.org/8878427
    c("chimalacatlan",                 "Chimalacatlán",                  18.46327, -99.09242, NA),     # https://www.geonames.org/3530859
    c("huatla",                        "Huautla",                        18.44,    -99.02472, NA),     # https://www.geonames.org/3526990
    c("cuaxitlan",                     "Coaxitlán",                      18.44944, -99.1825,  NA),     # https://www.geonames.org/3761666
    c("coaxitlan",                     "Coaxitlán",                      18.44944, -99.1825,  NA),     # https://www.geonames.org/3761666
    c("loaxitlan",                     "Coaxitlán",                      18.44944, -99.1825,  NA),     # https://www.geonames.org/3761666
    c("ajuchitlan",                    "Ajuchitlán",                     18.46357, -98.97257, NA),     # https://www.geonames.org/3533122
    c("juchitlan",                     "Ajuchitlán",                     18.46357, -98.97257, NA),     # https://www.geonames.org/3533122
    c("ajuchitlan, campo cardona",     "Ajuchitlán, Campo Cardona",      18.46357, -98.97257, 2000),   # https://www.geonames.org/3533122
    c("ajuchitlan, campo tierra larga","Ajuchitlán, Campo Tierra Larga", 18.46357, -98.97257, 2000),   # https://www.geonames.org/3533122
    c("nexpa",                         "Nexpa",                          18.52032, -99.14553, NA),     # https://www.geonames.org/3522755
    c("la mezquitera",                 "La Mezquitera",                  18.5621,  -99.10031, NA),     # https://www.geonames.org/3532666
    c("la mezquitera la piaña",        "La Mezquitera, La Piaña",        18.5621,  -99.10031, 2000),   # https://www.geonames.org/3532666
    c("valle de vazquez",              "Valle de Vázquez",               18.52858, -99.06943, NA),     # https://www.geonames.org/3524045
    c("lorenzo de vazquez",            "Lorenzo Vázquez",                18.57361, -99.07278, NA),     # https://www.geonames.org/8860777
    c("tlaquiltenango",                "Tlaquiltenango",                 18.63429, -99.1632,  NA),     # https://www.geonames.org/3515373
    c("xicatlacotla",                  "Xicatlacotla",                   18.51667, -99.19306, NA),     # https://www.geonames.org/8867048
    c("la maroma",                     "La Maroma",                      18.62167, -99.14639, NA),     # https://www.geonames.org/8898440
    c("lo elotes",                     "Los Elotes",                     18.51958, -99.11951, NA),     # https://www.geonames.org/3524080
    c("los elotes",                    "Los Elotes",                     18.51958, -99.11951, NA),     # https://www.geonames.org/3524080
    c("la era",                        "La Era",                         18.54821, -99.10783, NA),     # https://www.geonames.org/13436333
    c("las bovedas",                   "Las Bóvedas",                    18.55917, -99.1125,  NA),     # https://www.geonames.org/8870366
    c("san jose de pala",              "San José de Pala",               18.55667, -99.01472, NA),     # https://www.geonames.org/8864790
    c("quilamula",                     "Quilamula",                      18.51049, -99.01895, NA),     # https://www.geonames.org/3520897
    c("quilamula - campo huayacares",  "Quilamula, Campo Huayacares",    18.51049, -99.01895, 2000),   # https://www.geonames.org/3520897
    c("quilamula (las presas)",        "Quilamula, Las Presas",          18.51049, -99.01895, 2000),   # https://www.geonames.org/3520897
    c("las presas",                    "Quilamula, Las Presas",          18.51049, -99.01895, 2000),   # https://www.geonames.org/3520897
    c("quilamula (la linea)",          "Quilamula, La Línea",            18.51049, -99.01895, 2000),   # https://www.geonames.org/3520897
    c("quilamula / san jose jepala",   "Quilamula/San José de Pala",     18.51049, -99.01895, 5000),   # https://www.geonames.org/3520897
    ## not in GeoNames: centre of Tlaquiltenango municipality
    c("san jose",                      "San José",                       18.55041, -99.12255, 20000),  # https://www.geonames.org/8582393
    c("loma del muerto",               "Loma del Muerto",                18.55041, -99.12255, 20000),  # https://www.geonames.org/8582393
    c("el guayabo",                    "El Guayabo",                     18.55041, -99.12255, 20000),  # https://www.geonames.org/8582393
    c("zacualpa",                      "Zacualpa",                       18.55041, -99.12255, 20000),  # https://www.geonames.org/8582393
    c("la gra",                        "La Gra",                         18.55041, -99.12255, 20000),  # https://www.geonames.org/8582393
    c("pamarunera",                    "Pamarunera",                     18.55041, -99.12255, 20000),  # https://www.geonames.org/8582393
    c("las ventillas",                 "Las Ventillas",                  18.55041, -99.12255, 20000)   # https://www.geonames.org/8582393
  ))
  
  names(geo) <- c("raw", "location", "latitude", "longitude", "geo_uncertainty")
  i <- match(x, geo$raw)
  d$location <- geo$location[i]
  d$latitude <- as.numeric(geo$latitude[i])
  d$longitude <- as.numeric(geo$longitude[i])
  d$geo_uncertainty <- as.numeric(geo$geo_uncertainty[i])
  d$geo_from_source <- FALSE
  
  carobiner::write_files(path, meta, d)
}
