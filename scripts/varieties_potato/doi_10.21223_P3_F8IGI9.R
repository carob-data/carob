# R script for "carob"


carob_script <- function(path) {
  
"The Mother and Baby (M&B) trial methodology was adapted by CIP for Participatory Variety Selection (PVS) through decentralized evaluation networks and multi-year evaluations in potato growing areas in the Andean region. The M&B trial design encourages active participation of farmers through the application of treatments through systematic evaluations and selections of treatments in their own plots called \"Baby trials\" (i.e. farmer managed trials) and in fields with an experimental design called \"Mother trials\" (i.e. researcher managed trials). Objective: Analyze characteristics, attributes and preferences that men and women have when selecting a new potato variety at the phase of flowering and harvesting. A M&B trial was performed to evaluate 20 clones of the population B3C2 with late blight resistance at the locality of Chaquicocha, Pataz province in La Libertad department, Peru. The trial design was a Randomized Complete Block Design (RCBD) with 3 replicates during 2009-2010. In this experiment, characteristics of plant (size, type of foliage) and resistance to diseases during flowering and harvesting were evaluated."
  
  uri <- "doi:10.21223/P3/F8IGI9"
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
      carob_date = "2024-09-13",
	  design = "RCBD",
      notes = NA
  )


  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[basename(ff) == "PTPV200907_CHAQUI_Exp2.xls"]
  f <- f[grep("^0._|_Data_dictionary", basename(f), invert=TRUE)]
  d <- process(f)

  carobiner::write_files(path = path, metadata = meta, wide=d)
  
}

