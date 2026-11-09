# R script for "carob"

## NOTES
# 1. In the late blight fieldbook, the "rAUDPC" column holds the absolute AUDPC
#    (%.days) computed over all six LB evaluations, and the "AUDPC" column the same
#    computation over the first five evaluations only. Both are exact trapezoid sums
#    with 7 days between evaluations (e.g. LB = 0,5,10,75,90,100 gives
#    "AUDPC" = 135*7 = 945 and "rAUDPC" = 230*7 = 1610), so the evaluation interval
#    is known even though evaluation dates are not reported.
#    AUDPC and rAUDPC are recomputed from the six-evaluation values accordingly.
#    See https://github.com/reagro/carob/pull/609
# 2. Trials were conducted at CIP experimental stations (San Ramon CIPSRM-1;
#    Oxapampa OXAPMP), so on_farm is FALSE.

carob_script <- function(path) {
  
"The LBHT x LTVR population came from crossing two populations developed at CIP: LBHT for late blight resistance and LTVR for virus resistance in order to exploit heterosis for tuber yield, in 2013 under quarantine greenhouses at La Molina. 7200  genotypes (45 families with 160 seeds each) were planted. At harvest 258 clones were selected. Since 2015 until 2019, these clones were tested for late blight,PVX and PVY virus resistance, heat and drought tolerance,  marketable tuber yield, dry matter and quality for industrial processing. The experiments were planted in sites where environmental conditions are favorable to have high pressure of biotic and abiotic stress, that allows us to select clones with resistance and / or tolerance to these factors. Thirty-nine clones were selected for late blight resistance, heat tolerance, some clones have drought tolerance, resistance to virus PVX and or PVY and good quality for industrial processing."
  
  uri <- "doi:10.21223/7WC7FM"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=5, minor=1,
      data_organization = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable;AUDPC;rAUDPC", 
      carob_contributor = "Henry Juarez",
      carob_LLM = "Claude Fable 5",
      carob_effort = NA,
      carob_date = "2024-09-24",
      notes = NA
  )
    
  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[grep("_PT", basename(ff))]
  f <- f[grep("0._", basename(f), invert=TRUE)]
  
  d <- lapply(f, process, addvars=c("AUDPC","rAUDPC"))
  d <- do.call(carobiner::bindr, d)

  ## recompute AUDPC and rAUDPC (NOTES 1). After process_cip_lbvars, d$rAUDPC
  ## holds the raw fieldbook "rAUDPC" column: the absolute AUDPC in %.days over
  ## all six evaluations (5 intervals of 7 days = 35 days).
  audpc <- d$rAUDPC
  d$AUDPC <- audpc / 100           # fraction.days
  d$rAUDPC <- audpc / (100 * 35)   # fraction of the maximum possible AUDPC

  ## CIP experimental stations (NOTES 2)
  d$on_farm <- FALSE

  carobiner::write_files(path = path, metadata = meta, wide=d)
}

