# R script for "carob"

# NOTES 
# rAUDPC needs to be fixed
# https://github.com/reagro/carob/pull/609

carob_script <- function(path) {
  
"The LBHT x LTVR population came from crossing two populations developed at CIP: LBHT for late blight resistance and LTVR for virus resistance in order to exploit heterosis for tuber yield, in 2013 under quarantine greenhouses at La Molina. 7200  genotypes (45 families with 160 seeds each) were planted. At harvest 258 clones were selected. Since 2015 until 2019, these clones were tested for late blight,PVX and PVY virus resistance, heat and drought tolerance,  marketable tuber yield, dry matter and quality for industrial processing. The experiments were planted in sites where environmental conditions are favorable to have high pressure of biotic and abiotic stress, that allows us to select clones with resistance and / or tolerance to these factors. Thirty-nine clones were selected for late blight resistance, heat tolerance, some clones have drought tolerance, resistance to virus PVX and or PVY and good quality for industrial processing."
  
  uri <- "doi:10.21223/7WC7FM"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=5, minor=0,
      data_organization = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable", 
      carob_contributor = "Henry Juarez",
      carob_date = "2024-09-24",
      notes = "rAUDPC needs to be fixed"
  )
  
  
  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[grep("_PT", basename(ff))]
  f <- f[grep("0._", basename(f), invert=TRUE)]
  
  d <- lapply(f, process, addvars=c("AUDPC","rAUDPC"))
  d <- do.call(rbind, d)

  carobiner::write_files(path = path, metadata = meta, wide=d)
}

