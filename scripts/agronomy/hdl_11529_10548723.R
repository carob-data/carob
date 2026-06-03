# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. location(probably research centers) in raw dataset are all uppercase. 
#2. missing values in the treatment's variables is because there were "ND" (no data) from the trial dataset
#3. yield raw data contains empty values
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
    x <- gsub("^-$", NA, x)
    x <- gsub("[^0-9\\.\\-]", "", x)  
    x <- gsub("^\\s*$", NA, x)         
    as.numeric(x)
  }
  
  
  read_omision_sheet <- function(f, sheet) {
<<<<<<< HEAD
    r <- carobiner::read.excel.hdr(f, sheet = "2012", skip = 3, hdr = 3, fix_names = TRUE, lower = F)
    
    r <- r[!(r$ID.Exp == "ID Exp" | is.na(r$ID.Exp)), ]
    
    
    
    #    r <- r[!grepl("[A-Za-z/()]", r$Rendimiento.14pct.hum._.kg.ha) & 
    #             !is.na(r$Rendimiento.14pct.hum._.kg.ha), ]
    
    if ("P2O5.kg.ha.aplicado.como.superfosfato.triple" %in% names(r)) {
      P_col <- r$P2O5.kg.ha.aplicado.como.superfosfato.triple
      P_type <- "TSP"
    } else {
      P_col <- r$P2O5.kg.ha.aplicado.como.DAP
      P_type <- "DAP"
=======
    r <- carobiner::read.excel.hdr(f, sheet = sheet, skip = 3, hdr = 3, fix_names = TRUE, lower = F)
    
    r <- r[!(r$ID.Exp == "ID Exp" | is.na(r$ID.Exp)), ]
    
#    r <- r[!grepl("[A-Za-z/()]", r$Rendimiento.14pct.hum._.kg.ha) & 
#             !is.na(r$Rendimiento.14pct.hum._.kg.ha), ]
    
    if ("P2O5.kg.ha.aplicado.como.superfosfato.triple" %in% names(r)) {
		P_col <- r$P2O5.kg.ha.aplicado.como.superfosfato.triple
		P_type <- "TSP"
    } else {
		P_col <- r$P2O5.kg.ha.aplicado.como.DAP
		P_type <- "DAP"
>>>>>>> c986f4fe0fe816fb79a22700518aa0c367e6e1ca
    }
    
    d <- data.frame(
      country= "Mexico",
      adm1 = r$Estado,
      adm2= r$Municipio,
      location = r$Localidad,
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
      K_fertilizer = clean_num(r$K2O.kg.ha.aplicado.como.cloruro.de.potasio) / 1.2051,
<<<<<<< HEAD
      Zn_fertilizer=clean_num(r$Zn.kg.ha.aplicado.como.sulfato.de.zinc),
      B_fertilizer=clean_num(r$B.kg.ha.aplicado.como.bórax),
      Fe_fertilizer=clean_num(r$Fe),
      S_fertilizer=clean_num(r$S),
      fertilizer_type=P_type
    )
    
    # season constraints
    
    frost_inc <- clean_num(r$Heladas_Incidencia.pct._pct)
    frost_sev <- clean_num(r$Severidad.pct._pct)
    
    drought_inc <- clean_num(r$Sequía_Incidencia.pct._pct)
    drought_sev <- clean_num(r$Severidad.pct._pct.1)
    
    frost <- (!is.na(frost_inc) & frost_inc != 0) |
      (!is.na(frost_sev) & frost_sev != 0)
    
    drought <- (!is.na(drought_inc) & drought_inc != 0) |
      (!is.na(drought_sev) & drought_sev != 0)
    
    d$season_constraint <- ifelse(
      frost & drought, "frost;drought",
      ifelse(
        frost, "frost",
        ifelse(drought, "drought", NA_character_)
      )
=======
	  fertilizer_type=P_type
>>>>>>> c986f4fe0fe816fb79a22700518aa0c367e6e1ca
    )
    
    return(d)
  }
  
  d2011 <- read_omision_sheet(f, sheet = "2011")
  d2012 <- read_omision_sheet(f, sheet = "2012")
  d2013 <- read_omision_sheet(f, sheet = "2013")
  
  d <- rbind(d2011, d2012, d2013)
  #d <- unique(d)
  d[d == "."] <- NA
  
  d$planting_date[d$planting_date == "5/8/13"] <- "2013-05-08"
  
  d$planting_date <- sapply(d$planting_date, function(x) {
<<<<<<< HEAD
    if (is.na(x)) {
      NA
    } else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
      x
    } else {
      as.character(as.Date(as.numeric(x), origin = "1899-12-30"))
    }
  }) 
=======
		if (is.na(x)) {
			NA
		} else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
			x
		} else {
			as.character(as.Date(as.numeric(x), origin = "1899-12-30"))
		}
	}) 
>>>>>>> c986f4fe0fe816fb79a22700518aa0c367e6e1ca
  
  d$adm2 <- trimws(d$adm2)  
  
  #lat_lon
  d$adm2[d$adm2 == "Atotonilco El Grande"] <- "Atotonilco el Grande"
  d$adm2[d$adm2 == "Iguala de la independencia"] <- "Iguala de la Independencia"
  d$adm2[d$adm2 == "Huamantla, Tlaxcala"] <- "Huamantla"
  d$adm2[d$adm2 == "San Andres Sinaxtla"] <- "San Andrés Sinaxtla"
  d$adm2[d$adm2 == "Tepatitlan"] <- "Tepatitlán de Morelos"
  
  loc <- data.frame(
<<<<<<< HEAD
    location = c("Indaparapeo", "Atotonilco", "Charo", "Huatabampo", "Jecopaco", "Bacame", "Iguala",
                 "San Pablo Huixtepec", "San Andrés Sinaxtla", "Tlacolula de Matamoros",
                 "Espinal de Morelos", "Muna", "Xoy", "Valle Hermoso", "Metepec", "Zaachila",
                 "Ayoquezco de Aldama", "Chocani", "Xul-Ha", "Ejido Juan Sarabia", "Guadalupe Septien",
                 "Sitio Experimental Ébano", "Campo Experimental Centro Altos de Jalisco", "San Andres Sinaxtla"),
=======
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

>>>>>>> c986f4fe0fe816fb79a22700518aa0c367e6e1ca
    
    longitude = c(-100.9722, -100.7955, -101.0442, -109.6419, -109.7671, -109.5925, -99.5397, -96.7842,
                  -97.2830, -96.4723, -93.4278, -89.7131, -88.9711, -97.8159, -99.5986, -96.7518, -96.8431,
                  -97.5024, -88.4639, -88.4863, -100.1119, -98.4660, -102.7127, -97.2830),
    
<<<<<<< HEAD
    latitude = c(19.7921, 21.0056, 19.7467, 26.8271, 27.1979, 27.1585, 18.3448, 16.8211, 17.4670, 16.9552,
                 16.7083, 20.4849, 20.1230, 25.6684, 19.2529, 16.9487, 16.6842, 17.6658, 18.5517, 18.5172,
                 20.5284, 22.1725, 20.8733, 17.4670)
=======
    latitude=c(20.8014, 
               20.2861, 20.5239, 19.7889, 20.2867, 19.1216, 19.1714, 
               20.5029, 26.9106, 31.3186, 26.8248, 16.1382, 18.3466, 
               18.2388, 16.8197, 17.4668, 16.9558, 20.0981, 19.4779, 
               19.3134, 19.2308, 20.4916, 20.1224, 22.2175, 22.0059, 
               16.0965, 16.7075, 19.1008, 19.2036, 20.2375, 19.823, 
               16.9323, 16.685, 16.8803, 18.5017, 28.1912))
  
  d <- merge(d, loc, by="adm2", all.x = TRUE)
  
  #manual fixes of lat_lon
  manual_adm2 <- data.frame(
    adm2 = c("Acaxochitlán", "Cajeme", "Los Salates", "Ocozocoautla de Espinoza", "Santiago Mixac"),
    latitude  = c(20.158133, 28.625013, 24.2619, 16.757440, 19.220699),
    longitude = c(-98.202394, -111.539185, -107.0228, -93.373955, -98.338848)
>>>>>>> c986f4fe0fe816fb79a22700518aa0c367e6e1ca
  )
  
  d <- merge(d, loc, by="location", all.x = TRUE)
  
  loc2 <- data.frame(
    adm2 = c("Las Margaritas", "La Concordia", "Cajeme", "Celaya", "Cocula", "Delicias", "Santiago Mixac",
             "La Trinitaria", "Tamuín", "La Barca", "Indaparapeo", "Huamantla", "Tulancingo", "Álvaro Obregón",
             "Atotonilco el Grande", "Tepatitlán de Morelos", "Cocotitlán", "Juchitepec", "Temamatla", "Chiapa de Corzo",
             "Ocozocoautla de Espinoza", "Pedro Escobedo", "Acaxochitlán", "Tlaltenango", "Texcoco", "Ayapango",
             "Santa Catarina Quiane"),
    
    longitude = c(-91.9880, -92.6860, -109.9401, -100.8113, -103.8228, -105.4595, -98.3384, -92.0518,
                  -98.7839, -102.5458, -100.9722, -97.9234, -98.3691, -101.0457, -98.6689, -102.7624,
                  -98.8666, -98.8816, -98.8721, -93.0081, -93.3741, -100.1395, -98.2022, -103.3001,
                  -98.8832, -98.8028, -96.7412),
    
    latitude = c(16.3155, 16.1148, 27.4934, 20.5280, 20.3654, 28.1871, 19.2193, 16.1189, 22.0056,
                 20.2907, 19.7921, 19.3185, 20.0905, 19.8169, 20.2865, 20.8167, 19.2315, 19.0911,
                 19.2018, 16.7022, 16.7604, 20.5010, 20.1545, 21.7793, 19.5060, 19.1293, 16.8817))
  
  lon_values <- setNames(loc2$longitude, loc2$adm2)
  lat_values  <- setNames(loc2$latitude,  loc2$adm2)
  
<<<<<<< HEAD
  d$longitude[is.na(d$longitude)] <- lon_values[d$adm2[is.na(d$longitude)]]
  d$latitude[is.na(d$latitude)]   <- lat_values[d$adm2[is.na(d$latitude)]]
  
  #using adm1 since adm2 & location are empty
  d$longitude[d$adm1 == "Sinaloa" & is.na(d$longitude)] <- -107.3898
  d$latitude[d$adm1 == "Sinaloa"  & is.na(d$latitude)]  <-   24.8091
  
  d$longitude[d$adm1 == "Tlaxcala" & is.na(d$longitude)] <- -98.2375
  d$latitude[d$adm1 == "Tlaxcala"  & is.na(d$latitude)]  <-  19.3182
  

     
   #d$N_fertilizer[is.na(d$N_fertilizer)] <- 0
    #d$P_fertilizer[is.na(d$P_fertilizer)] <- 0
    #d$K_fertilizer[is.na(d$K_fertilizer)] <- 0
  
    # likely research stations and institutional sites
    research_stations <- c(
      "Campo Experimental Centro Altos de Jalisco","CEBAJ","CEVAF","CSAEGRO","Sitio Experimental Ébano",
      "Campo 16","DU1OH", "DU2OT", "FMOH", "FVOH", "GGBOT", "TOOT")
    
  d$on_farm <- !d$location %in% research_stations
    
  d$land_prep_method <- ifelse(d$land_prep_method=="Convencional","conventional","none")
  d$trial_id <- paste(d$location, d$planting_date, sep = "_")
=======
# Why set NA to zero? If that is correct, you need to comment in that.  
#  d$N_fertilizer[is.na(d$N_fertilizer)] <- 0
#  d$P_fertilizer[is.na(d$P_fertilizer)] <- 0
#  d$K_fertilizer[is.na(d$K_fertilizer)] <- 0

##wrong. Some of these are abbrevations
##  d$location <- tools::toTitleCase(tolower(d$location))
  d$land_prep_method <- ifelse(d$land_prep_method=="Convencional","conventional","none")
  d$trial_id <- paste(d$location, d$planting_date, sep = "_")
  d$on_farm <- FALSE
>>>>>>> c986f4fe0fe816fb79a22700518aa0c367e6e1ca
  d$is_survey <- FALSE
  d$irrigated <- TRUE
  d$geo_from_source <- FALSE
  d$yield_part <- "grain"
<<<<<<< HEAD
  d$yield_moisture <- 14# from original variable name r$Rendimiento.14pct.hum._.kg.ha
  d$yield_isfresh <- TRUE
  d <- unique(d)
  
=======
  # how do you know? If there is a source, mention that
  # if not, remove it (do not guess)
  d$yield_moisture <- 14
  # yield_mosture > 0, so isfresh must be TRUE
  d$yield_isfresh <- TRUE
  d <- unique(d)
 
>>>>>>> c986f4fe0fe816fb79a22700518aa0c367e6e1ca
  carobiner::write_files(path, meta, d)
}