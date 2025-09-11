# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {
  
"
Fertility Maps of Cadereyta, Querétaro.
  
Soil sampling with 1x1 km grid in the agricultural area of the Cadereyta Municipality at at 0 to 30 cm depth. (2022-07-22)
"
 
  uri <- "hdl:11529/10548745"
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
		carob_date="2025-09-09",
		completion = 100,	
		design=NA,
		notes = NA
  )
  
  f <- ff[basename(ff) == "BD_Suelo_Cadereyta.xlsx"]
  r <- carobiner::read.excel(f, sheet ="Cadereyta", na=".")
  #units <- r[1,]
  r <- r[-1, ] #removing the second row with units
  
  d <- data.frame(
    country = "Mexico",
    adm1 = r$Estado,
    adm2= r$Municipio,
    location=tolower(r$Localidad),
    longitude = r$Longitud,
    latitude = r$Latitud,
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
    soil_EC =as.numeric(r$`CE (Extracto)`),
    geo_from_source = TRUE
  )
  
   #cleaning and combining phosphorus from 2 diff tests
   r$`Fosforo Bray 1` <- as.numeric(r$`Fosforo Bray 1`)
   r$`Fosforo Olsen`  <- as.numeric(r$`Fosforo Olsen`)
   d$soil_P <- ifelse(!is.na(r$`Fosforo Bray 1`), r$`Fosforo Bray 1`, r$`Fosforo Olsen`)
   d$soil_P_method <- ifelse(!is.na(r$`Fosforo Bray 1`), "Bray 1", "Olsen")

   #splitting depth column
   depth <- do.call(rbind, strsplit(r$Profundidad, "-"))
   d$depth_top <- as.numeric(depth[,1])
   d$depth_bottom <- as.numeric(depth[,2])
   d$soil_texture <- gsub("Franco Arcillo Arenoso","sandy clay loam",d$soil_texture)
   d$soil_texture <- gsub("Franco Arcilloso","clay loam",d$soil_texture)
   d$soil_texture <- gsub("Arcilla","clay",d$soil_texture)
   d$soil_texture <- gsub("Franco","loam",d$soil_texture)

   soilmeta <- data.frame(
     variable = c("soil_Al", "soil_B", "soil_Ca", "soil_Fe", "soil_K", "soil_Mg", "soil_Mn", "soil_Na", "soil_S", "soil_Zn", "soil_Cu","soil_N"),
     method = c("Mehlich3")
   )
   
  carobiner::write_files(path, meta, d)
}
