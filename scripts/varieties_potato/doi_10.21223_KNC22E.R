# R script for "carob"


carob_script <- function(path) {
  
"In 2019-2020 season, three clones with high levels of resistance to late blight were evaluated in adaptation and efficiency trials for tuber yield, dry matter content, frying and baking quality throughout Peru in six locations, compared to two varieties planted by farmers. and very well accepted by final consumers, Canchan and Unica, these are currently also used for frying in sticks, but without stability in all crops due to the genotype x environment interaction. The randomized complete block design was used with three replications of 150 plants, the fertilization dose was 200-220-180 Kg of NPK, using potassium sulfate as a source of potassium to improve frying quality. At harvest, samples were taken to determine dry matter, reducing sugar content, traditional and blanched frying color, and baking quality. The clone was equal to or superior to the controls for the yield of tubers, it presented good quality of frying color in all localities compared to the control varieties that did not present good quality of frying color in all localities, It is expected to complete all the documents requested by the Peruvian Seed Authority (SENASA) to be registered as a new potato variety with resistance to late blight and quality for frying and / or baking."
  
  uri <- "doi:10.21223/KNC22E"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
      data_organization = "CIP;PATAZ",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable", 
      carob_contributor = "Henry Juarez",
      carob_date = "2024-09-13",
      notes = NA
  )
  
  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[grep("PTYield1", basename(ff))]
  d <- lapply(f, process)
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
}

