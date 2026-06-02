# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
  "Nutrient omission experiments aim to identify nutritional deficiencies in maize production systems in different regions of Mexico. (2022-07-06)"
  
  uri <- "hdl:11529/10548723"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
   data_organization = "CIMMYT",
   publication= NA,
   project = NA,
   data_type = "experiment",
   treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
   response_vars = "yield", 
   completion = 100,
   carob_contributor = "Blessing Dzuda",
   carob_date = "2025-05-29",
   notes = NA,
   design = "Randmized Complete Block Design"
   )
  
  f <- ff[basename(ff) == "BD Ensayos Omision VF.xlsx"]
  
  clean_num <- function(x) {
    x <- as.character(x)
    x <- gsub(",", ".", x)            
    x <- gsub("^ND$", NA, x)          
    x <- gsub("^\\.$", NA, x)       
    x <- gsub("[^0-9\\.\\-]", "", x)  
    x <- gsub("^\\s*$", NA, x)         
    as.numeric(x)
  }
  
  read_omision_sheet <- function(f, sheet) {
    r <- carobiner::read.excel.hdr(f, sheet = sheet, skip = 3, hdr = 3, fix_names = TRUE, lower = F)
    
    
    r <- r[!(r$ID.Exp == "ID Exp" | is.na(r$ID.Exp)), ]
    
    r <- r[!grepl("[A-Za-z/()]", r$Rendimiento.14pct.hum._.kg.ha) & 
             !is.na(r$Rendimiento.14pct.hum._.kg.ha), ]
    
    P_col <- if ("P2O5.kg.ha.aplicado.como.superfosfato.triple" %in% names(r)) {
      r$P2O5.kg.ha.aplicado.como.superfosfato.triple
    } else {
      r$P2O5.kg.ha.aplicado.como.DAP
    }
    
    d <- data.frame(
      country= "Mexico",
      adm1 = r$Estado,
      adm2= r$Municipio,
      adm3= r$Localidad,
      land_prep_method = r$Labranza,
      planting_date= r$Fecha.de.siembra,
      crop= "maize",
      variety = r$Híbrido,
      plant_density= clean_num(r$Densidad.de.siembra_plantas.ha),
      rep= as.integer(clean_num(r$Reps_)),
      treatment= r$Descripción.de.tratamiento,
      yield= clean_num(r$Rendimiento.14pct.hum._.kg.ha),
      N_fertilizer= clean_num(r$Nutrientes.evaluados_N.kg.ha.aplicado.como.urea),
      P_fertilizer = clean_num(P_col) / 2.29,
      K_fertilizer = clean_num(r$K2O.kg.ha.aplicado.como.cloruro.de.potasio) / 1.2051
    )
    
    return(d)
  }
  
  d2011 <- read_omision_sheet(f, sheet = "2011")
  d2012 <- read_omision_sheet(f, sheet = "2012")
  d2013 <- read_omision_sheet(f, sheet = "2013")
  
  d <- rbind(d2011, d2012, d2013)
  d <- unique(d)
  d[d == "."] <- NA
  
  d$planting_date[d$planting_date == "5/8/13"] <- as.character(
    as.Date("5/8/13", format = "%m/%d/%y"))
  
  d$planting_date <- sapply(d$planting_date, function(x) {
    if (is.na(x)) {NA} else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
      x} else {as.character(as.Date(as.numeric(x), origin = "1899-12-30"))}}) 
  
  d$adm2 <- trimws(d$adm2)  
  
  #lat_lon
  d$adm2[d$adm2 == "Atotonilco El Grande"] <- "Atotonilco el Grande"
  d$adm2[d$adm2 == "Iguala de la independencia"] <- "Iguala de la Independencia"
  d$adm2[d$adm2 == "Huamantla, Tlaxcala"] <- "Huamantla"
  d$adm2[d$adm2 == "San Andres Sinaxtla"] <- "San Andrés Sinaxtla"
  d$adm2[d$adm2 == "Tepatitlan"] <- "Tepatitlán de Morelos"
  
  loc <- data.frame(
    adm2= c("Tepatitlán de Morelos", "La Barca", "Celaya", 
            "Indaparapeo", "Atotonilco el Grande", "Ayapango", 
            "Tlaltenango", "Pedro Escobedo", "Etchojoa", "Benito Juárez", 
            "Huatabampo", "La Trinitaria", "Iguala de la Independencia", 
            "Cocula", "San Pablo Huixtepec", "San Andrés Sinaxtla", 
            "Tlacolula de Matamoros", "Tulancingo", "Texcoco", 
            "Huamantla", "Cocotitlán", "Muna", "Peto", "Ébano", 
            "Tamuín", "La Concordia", "Chiapa de Corzo", "Juchitepec", 
            "Temamatla", "Metepec", "Álvaro Obregón", "Zaachila", 
            "Ayoquezco de Aldama", "Santa Catarina Quiane", "Othón P. Blanco", 
            "Delicias"),
    
    longitude=c(-102.7629, -102.5459, -100.8183, 
                -100.9682, -98.6682, -98.8346, -98.3456, -100.1443, 
                -109.6273, -113.5383, -109.642, -91.8967, -99.5377, 
                -99.6596, -96.7821, -97.281, -96.4758, -98.3675, 
                -98.9892, -97.923, -98.8619, -89.7122, -88.9205, 
                -98.3786, -98.7795, -92.8316, -93.0115, -98.8788, 
                -98.8698, -98.3214, -101.0411, -96.7683, -96.8431, 
                -96.7408, -88.3117, -105.4696),
    
    latitude=c(20.8014, 
               20.2861, 20.5239, 19.7889, 20.2867, 19.1216, 19.1714, 
               20.5029, 26.9106, 31.3186, 26.8248, 16.1382, 18.3466, 
               18.2388, 16.8197, 17.4668, 16.9558, 20.0981, 19.4779, 
               19.3134, 19.2308, 20.4916, 20.1224, 22.2175, 22.0059, 
               16.0965, 16.7075, 19.1008, 19.2036, 20.2375, 19.823, 
               16.9323, 16.685, 16.8803, 18.5017, 28.1912))
  
  d <- merge(d,loc,by="adm2", all.x = TRUE)
  
  #manual fixes of lat_lon
  manual_adm2 <- data.frame(
    adm2 = c("Acaxochitlán", "Cajeme", "Los Salates", "Ocozocoautla de Espinoza", "Santiago Mixac"),
    latitude  = c(20.158133, 28.625013, 24.2619, 16.757440, 19.220699),
    longitude = c(-98.202394, -111.539185, -107.0228, -93.373955, -98.338848)
  )
  
  # adm1 level fixes where adm2 and 3 was NA
  manual_adm1 <- data.frame(
    adm1 = c("Sinaloa", "Tlaxcala","Oaxaca","Chiapas"),
    latitude  = c(25.020233, 19.447779,17.08154,16.2558349),
    longitude = c(-107.411426, -98.183454,-96.7749,-93.574682)
  )
  
  # fix adm2 level
  for (i in 1:nrow(manual_adm2)) {
    idx <- d$adm2 == manual_adm2$adm2[i] & is.na(d$latitude)
    d$latitude[idx]  <- manual_adm2$latitude[i]
    d$longitude[idx] <- manual_adm2$longitude[i]
  }
  
  # fix adm1 level 
  for (i in 1:nrow(manual_adm1)) {
    idx <- d$adm1 == manual_adm1$adm1[i] & is.na(d$latitude)
    d$latitude[idx]  <- manual_adm1$latitude[i]
    d$longitude[idx] <- manual_adm1$longitude[i]
  }
  
  d$N_fertilizer[is.na(d$N_fertilizer)] <- 0
  d$P_fertilizer[is.na(d$P_fertilizer)] <- 0
  d$K_fertilizer[is.na(d$K_fertilizer)] <- 0
  d$adm3 <- tools::toTitleCase(tolower(d$adm3))
  d$land_prep_method <- ifelse(d$land_prep_method=="Convencional","conventional","none")
  d$trial_id <- paste(d$adm3,d$planting_date, sep = "_")
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$geo_from_source <- FALSE
  d$yield_part <- "grain"
  d$yield_moisture <- 14
  d$yield_isfresh <- FALSE
  d<- unique(d)
 
  carobiner::write_files(path, meta, d)
}