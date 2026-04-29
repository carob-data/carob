# R script for "carob"

carob_script <- function(path) {
  
    "The Mother and Baby (M&B) trial methodology was adapted by CIP for Participatory Variety Selection (PVS) through decentralized evaluation networks and multi-year evaluations in potato growing areas in the Andean region. The M&B trial design encourages active participation of farmers through the application of treatments through systematic evaluations and selections of treatments in their own plots called \"Baby trials\" (i.e. farmer managed trials) and in fields with an experimental design called \"Mother trials\" (i.e. researcher managed trials).Objective: Analyze characteristics, attributes and preferences that men and women have when selecting a new potato variety at the phase of flowering and harvesting.A M&B trial was performed to evaluate 11 clones of the population B1C5 with late blight resistance at the localities of Alacoto, Chuquitambo and Macullida, in the provinces of Pataz and Sanchez Carrion, in La Libertad department, Peru. The trial design was a Randomized Complete Block Design (RCBD) with 3 replicates for Mother trials and 3 replicates for Baby trials during 2012-2013. In this experiment characteristics of plant (size, type of foliage), yield, quality, and pest and disease resistance during flowering and harvesting were evaluated.The number of participants at flowering phase was 20 (men=14 and women=6), 12 (men=5, women = 5),  9 (men=4, women = 5); and the number of participants at harvesting phase was 17 (men=12 and women=5), 12 (men=5, women = 5) and 9 (men=4, women = 5) in Alacoto, Chuquitambo and Macullida respectively. Additionally, an organoleptic evaluation was assessed at harvesting to evaluate appearance, taste and texture. The number of panelists was 10 (men=5, women=5) in Alacoto, Chuquitambo respectively and 10 (men=4, women=6) in Macullida for the organoleptic evaluation."
  
  uri <- "doi:10.21223/P3/KATPZ8"
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
  f <- f[grep("^0._|_Data_dictionary", basename(f), invert=TRUE)]
  d <- lapply(f, process, addvars=c("AUDPC","rAUDPC","TTWP"))
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)

}
