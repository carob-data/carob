# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
"The production of potato (Solanum tuberosum) in farmers' fields is heavily affected by the late blight [Phythophthora infestans (Mont) de Bary] disease. The International Potato Center (CIP) has a breeding population with resistance to late blight, heat tolerance and high yields of tubers called LBHT (Gastelo et al 2015).  In 2019-2020, 2721 clones were screened for resistance to late blight and tuber yield in natural field conditions in Oxapampa at 10º35'S, 1850 masl, under high disease pressure, along with the parents and three (resistant, moderately resistant and highly susceptible) check varieties, in a 65x43 row-column design (John 1989), with two replications. \r\nThe data analysis was performed using the mixed model analysis, that makes spatial corrections, the BLUPs (best linear unbiased predictions) of late blight resistance measured through the area under the disease progress curve (AUDPC), marketable and total tuber Adjusted yield (MTYA, TTYA) and unadjusted yield per hectare (MTYNA, TTYNA) were obtained. In the clones the AUDPC values were in the range of 89 to 2310,   and in the controls it was from 210 to 759, the TTYA in the clones was from 1.91 to 54.22 t / ha, superior to the controls, which had from 5.68 to 20.98 t / ha. (Link dataverse) At harvest 500 clones with high level of late blight resistance and tuber yield greater than susceptible controls and good agronomic characteristics (skin color and tuber shape) were selected, with AUDPC values from 89 to 683, with yields under the pressure of the disease, from 9.08 to 47.64 t / ha."
  
  uri <- "doi:10.21223/GCJRH6"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
      data_organization = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable;AUDPC", 
      carob_contributor = "Henry Juarez",
      carob_effort = NA,
      carob_date = "2024-04-12",
      notes = NA
  )
  
  f <- ff[grep("Oxapampa", basename(ff))]
  
  d <- carobiner::read.excel(f, sheet="OXA19-01")               
  minimal <- carobiner::read.excel(f, sheet="Minimal")
  
  m <- as.list(minimal$Value)
  names(m) <- minimal$Factor
  
  colnames(d) <- d[1,]
  d <- d[2:nrow(d),]
  d <- d[,1:12]
  
  d$yield <- as.numeric(d$TTYNA) *1000
  d$yield_marketable <- as.numeric(d$MTYNA) * 1000
  
  d$rep <- as.integer(NA)
  d$variety <- d$`Accession number`
  d$AUDPC <- as.numeric(d$AUDPC) / 100
  d$country <-m$Country
  d$adm1 <- m$Admin1
  d$adm2 <- m$Admin2
  d$adm3 <- m$Admin3
  d$location <- m$Locality
  d$longitude <- as.numeric(m$Longitude)
  d$latitude = as.numeric(m$Latitude)
  d$elevation = as.numeric(m$Elevation)
  d$planting_date = m$`Begin date`
  d$harvest_date = m$`End date`
  d$trial_id = gsub(".xls", "", basename(f))
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$crop <- "potato"
  d$pathogen <- "Phytophthora infestans"
  d$yield_part <- "tubers"
  d$geo_from_source <- TRUE
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  d <- d[,-c(1:6,8:12)]
  
  carobiner::write_files(path = path, metadata = meta,wide=d)
  
  
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
