# R script for "carob"

carob_script <- function(path) {
  
"The B3C3 population came from crossing the elite clones of B3C2 population, in 2011 under quarantine greenhouses at La Molina. For this, 30,000 genotypes (60 families with 500 seeds each) were planted. At harvest 21685 genotypes, were selected (72%). These selected clones were planted in La Victoria - Huancayo, to increase the number of tubers and make selection for agronomic attributes, where 3123 clones (10.41% of the original population) were selected. This dataset is part of the following selection where selected clones were evaluated for three seasons (2012, 2013 and 2014), in Oxapampa where the environmental conditions (rain, relative humidity, temperature) are great to have a high pressure of late blight allowing us to select clones with resistance to this disease. In 2014-2015 at Oxapampa, the third evaluation of resistance to late blight was conducted. 370 clones were planted in plots of 10 plants with two replications. Furthermore, variety Kory, semiresistant Amarilis and Yungay, a susceptible variety, were used as controls to the resistant clones. The selected clones showed lower values in the scale of suceptibility to late blight, than the susceptible witness Yungay. 94 clones with marketable and total tuber yield above controls and with scale values of susceptibility to late blight lower than Control yungay were selected, but all clones were tested again for resistance to late blight, because that year the presence of late blight was very low."
  
  uri <- "doi:10.21223/P3/FFORMJ"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
      data_organization = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "AUDPC;rAUDPC",
      carob_contributor = "Henry Juarez",
      carob_date = "2024-09-13",
      notes = NA
  )
  
  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[grep("PTLate", basename(ff))]
  f <- f[grep("^0._|_Data_dictionary", basename(f), invert=TRUE)]
  d <- lapply(f, process, addvars=c("AUDPC","rAUDPC","TTWP","MTWP"))
  d <- do.call(rbind, d)
  
  d$planting_date <- as.Date(as.numeric(d$planting_date), origin="1899-12-30") |> as.character()
  d$harvest_date <- as.Date(as.numeric(d$harvest_date), origin="1899-12-30") |> as.character()
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$crop <- "potato"
  d$pathogen <- "Phytophthora infestans"
  d$yield_part <- "tubers"
  d$geo_from_source <- TRUE
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
}

