carob_script <- function(path) {
  
  "
  Dataset for: Efecto de una barra nutricional 'NUTRIBARRA ANDINA' en el control de la anemia en niños menores de 5 años en Chugay y Curgos, La Libertad, Peru.

  Estudio sobre el efecto de una barra nutricional (NUTRIBARRA ANDINA) en el control de la anemia
  en niños menores de 5 años en Chugay y Curgos, La Libertad, Peru.
  "
  
  uri <- "doi:10.21223/OMOIO4"
  group <- "survey"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "survey",
    treatment_vars = "treatment",
    response_vars = "hb_",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-10",
    carob_completion = 85,
    carob_effort = 1.5
  )
  
  ## Source files
  f1 <- ff[basename(ff) == "01_Datos_NUTRI_BARRAS.xlsx"]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1)
  
  ## Convert dates safely
  r1$date_initial <- as.character(
    ifelse(grepl("-", r1$`Fecha-tamizaje-inicial`),
           as.Date(r1$`Fecha-tamizaje-inicial`, format = "%d-%m-%y"),
           as.Date(suppressWarnings(as.numeric(r1$`Fecha-tamizaje-inicial`)), origin = "1899-12-30"))
  )
  
  ## Coordinates
  geo <- data.frame(
    location = c("CHUGAY", "CURGOS"),
    latitude = c(-7.78167, -7.75000),
    longitude = c(-77.8683, -77.8500),
    geo_from_source = FALSE
  )
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = "OMOIO4",
    hhid = r1$Codigo,
    treatment = tolower(r1$Grupo),
    farmer_gender = ifelse(r1$Sexo == "M", "male", "female"),
    location = (r1$Distrito),
    adm2 = (r1$Provincia),
    country = "Peru",
    is_survey = TRUE,
    on_farm = FALSE,
    date_initial_ = r1$date_initial,
    hb_initial_ = as.numeric(r1$`HB-Tamizado1`),
    hb_month1_ = as.numeric(r1$`HB-Tamizado2`),
    hb_month2_ = as.numeric(r1$`HB-Tamizado-3`),
    hb_month3_ = as.numeric(r1$`HB-Tamizado-4`),
    anemia_initial_ = r1$Nivel_Anemia,
    anemia_month1_ = r1$Nivel_Anemia_Tamizado2,
    anemia_month2_ = r1$Nivel_Anemia_Tamizado3,
    anemia_month3_ = r1$Nivel_Anemia_Tamizado4
  )
  
  ## Merge coordinates
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  ## Remove rows with missing key variables
  d <- d[!is.na(d$hhid) | !is.na(d$treatment), ]
  
  carobiner::write_files(path, meta, d)
}