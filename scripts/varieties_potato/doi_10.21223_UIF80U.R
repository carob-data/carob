# R script for "carob"

carob_script  = function(path) {
 
"The genetic improvement Program for resistance to late blight at CIP has developed advanced potato clones with high levels of resistance to late blight under natural conditions of infection in the field, in the town of Oxapampa where the virulence genes of P. infestans R5 and R9 are not present, or if they are, their frequency is very low, not allowing to evaluate the reaction of clones resistant to these genes, so it was necessary to evaluate the reaction of these clones under controlled conditions where environmental conditions are optimal. to have a high pressure from the pathogen and from these 2 R genes. 22 advanced clones of the B3C3 population with high levels of resistance to late blight and two susceptible varieties (Yungay and Canchan) as controls were evaluated, these clones were inoculated with 4 isolates of Phytophthora infestans: (POX67, PLL69, PPI112, PPA61) , with a sporangia concentration of 3x103, each plant was inoculated with 30-60 ml of the sporangia suspension, two experiments were carried out, one under greenhouse conditions and the other in a humid chamber in La Molina, Lima, The experiment was from September 2019 to January 2020, the statistical design of complete random blocks was used with 4 repetitions, 4 plants were planted per clone in each experiment, 1 plant was a repetition."
  
  uri  = "doi:10.21223/UIF80U"
  group  = "varieties_potato"
  ff   = carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
      data_organization = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "AUDPC", 
      carob_contributor = "Henry Juarez",
      carob_effort = NA,
      carob_date = "2024-05-12",
      notes = NA
  )
  
  f  = ff[grep("LB_resistance", basename(ff))]
  
  r  = carobiner::read.excel(f=f, sheet="DATA")
  
  d = data.frame(
   country  = "Peru",
   adm3  = "Oxapampa",
   latitude = -10.5852,
   longitude = -75.4080,
   record_id  = as.integer(1:nrow(r)),
   planting_date = "2019-09-01",
   harvest_date = "2020-01-01",
   on_farm = TRUE,
   is_survey = FALSE,
   crop = "potato",
   pathogen  = "Phytophthora infestans",
   yield_part = "none",
   geo_from_source = FALSE,
   trial_id = gsub(".xlsx", "", basename(f[1])),
   yield  = as.numeric(NA),
   AUDPC  =  r$AUDPC / 100,
   irrigated=TRUE
   )
  
  d$rep  = as.integer(r$Rep)
  d$variety = r$`Accession number`
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
  
}

