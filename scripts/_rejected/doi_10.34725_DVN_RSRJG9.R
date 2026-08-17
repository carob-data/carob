# R script for "carob"
# license: GPL (>=3)

#### NOTES
## REJECT or Pending subject to full data availability
# 80 destructively-sampled plots/subplots, 16 blocks (A1-A8, B1-B8) x up to 5 subsamples each.

###ISSUES

# Plot area is missing so yield (kg/ha) is not possible to calculate
# Exact location is also unknown besides that it is Kenya
# Source file and dataset metadata give no plot/sample area
#    so raw weights and counts CANNOT be converted to the per-ha 
#    units terminag's yield/fwy_*/dmy_*/cob_density/stem_density fields require
# No column on year/season


carob_script <- function(path) {
  
  "Maize yield for 2012_2013 and 2013_2014 short rains
Maize productivity before tree influence was recorded to determine other
factors contributing to yield variability (rainfall and soil variability).
Long-Term Agroforestry Trial, Kenya (World Agroforestry Centre / ICRAF)."
  
  uri <- "doi:10.34725/DVN/RSRJG9"
  group <- "agronomy"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                  data_organization = "ICRAF",
                                  publication = "",
                                  project = "Trees for Food Security",
                                  carob_date = "2026-08-11",
                                  design = "on-farm agroforestry trial, tree species x maize intercrop, destructive sampling by subplot",
                                  data_type = "trial",
                                  treatment_vars = "treatment",
                                  response_vars =NA,
                                  carob_contributor = "Stella Muthoni",
                                  notes = NA,
                                  carob_completion = 40,
                                  carob_effort = 1
  )
  
  f1 <- ff[basename(ff) == "Combined maize yield 2012_13 and 2013_14 short rains.tsv"]
  r1 <- read.delim(f1, header = TRUE)
  
  # split "JK A1 S2" into plot ("A1") and sample ("S2")
  parts <- strsplit(r1$Plot.sampleno., " ")
  plot_id  <- sapply(parts, function(x) x[2])
  sample_id <- sapply(parts, function(x) x[3])
  sample_id[sample_id == plot_id] <- "S1"   # A1's first sample was labeled "A1" not "S1" - normalized
  
  d1 <- data.frame(
    plot_id   = plot_id,
    rep = sample_id,
    treatment = trimws(r1$Treatment),
    soil_type = r1$Soiltype,
    
    # raw per-sample weights/counts 
    raw_total_freshweight_kg = r1$TotalSampleFwt.kg.,
    raw_cob_freshweight_kg   = r1$Fwtofcob.kg.,
    raw_cob_dryweight_g      = r1$Dwtofcob.g.,
    raw_grain_weight_g       = r1$Wtofgrain.g.,
    n_cobs   = r1$No.ofcobs,
    n_stalks = r1$No.ofstalks,
    raw_husk_weight_g = r1$Wtofpaper.g.
  )
  
  carobiner::write_files(path, meta, d1)
}
