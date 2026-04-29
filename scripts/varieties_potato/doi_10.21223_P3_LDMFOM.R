# R script for "carob"

carob_script <- function(path) {
  
    "The Mother and Baby (M&B) trial methodology was adapted by CIP for Participatory Variety Selection (PVS) through decentralized evaluation networks and multi-year evaluations in potato growing areas in the Andean region. The M&B trial design encourages active participation of farmers through the application of treatments through systematic evaluations and selections of treatments in their own plots called \"Baby trials\" (i.e. farmer managed trials) and in fields with an experimental design called \"Mother trials\" (i.e. researcher managed trials).Objective: Analyze characteristics, attributes and preferences that men and women have when selecting a new potato variety at the phase of flowering and harvesting.A M&B trial was performed to evaluate 21 clones of the population B1C5 with late blight resistance at the locality of Quilcas, in Quilcas province, in Junín department, Peru. The trial design was a Randomized Complete Block Design (RCBD) with 3 replicates for Mother trials and 3 replicates for Baby trials during 2009-2010. In this experiment characteristics of plant (size, type of foliage), yield, quality, and pest and disease resistance during flowering and harvesting were evaluated. The number of participants was 9 at flowering phase (men=6 and women=3) and 21 at harvesting phase (men=9 and women=12). Additionally, an organoleptic evaluation was assessed at harvesting to evaluate appearance, taste and texture. The number of panelists was (men=4, women=5) for the organoleptic evaluation."
  
  uri <- "doi:10.21223/P3/LDMFOM"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
      data_organization = "CIP",
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
  
  f <- ff[grep("PTPV", basename(ff))]
  f <- f[grep("^0._|_Data_dictionary|_processed", basename(f), invert=TRUE)]
  d <- lapply(f, process, addvars=c("AUDPC","rAUDPC","TTWP"))
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)

}

