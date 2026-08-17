# R script for "carob"
# license: GPL (>=3)
#### NOTES
## REJECT or Pending subject to full data availability
# 35 paired near/away samples across 3 plots (JK A2, A4, A5), 5 tree species
###ISSUES
# Plot/sample area is missing so yield (kg/ha) is not possible to calculate
# No column on year/season despite dataset title referencing 2015/2016
# Exact location is also unknown besides that it is Kenya
carob_script <- function(path) {
  
  "Maize yield 2015_2016 short rains
Maize productivity was measured adjacent and away from tree rows to
determine influence of trees on crop productivity.
Long-Term Agroforestry Trial, Kenya (World Agroforestry Centre / ICRAF)."
  
  uri <- "doi:10.34725/DVN/KLG19F"
  group <- "agronomy"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
                                  data_organization = "ICRAF",
                                  publication = "",
                                  project = "Trees for Food Security",
                                  carob_date = "2026-08-11",
                                  design = "paired near-tree / away-from-tree destructive sampling, 5 tree species",
                                  data_type = "trial",
                                  treatment_vars = "treatment",
                                  response_vars = NA,
                                  carob_contributor = "Stella Muthoni",
                                  notes = NA,
                                  carob_completion = 40,
                                  carob_effort = 1
  )
  
  f1 <- ff[basename(ff) == "Maize yield 2015_2016 short rains_1.tsv"]
  r1 <- read.delim(f1, header = TRUE)
  
  species  <- sub(" S[0-9]+$", "", r1$Annotation)
  rep_num  <- sub("^.* S", "S", r1$Annotation)
  
  d1_near <- data.frame(
    plot_id    = r1$Plotcode,
    plant_code = r1$Plantcode,
    rep        = rep_num,
    treatment  = species,
    sample_location = "near",
    raw_total_freshweight_g  = r1$TotalFwt.g.Near,
    raw_cob_freshweight_g    = r1$CobFwt.g.Near,
    raw_cob_dryweight_g      = r1$CobDwt.g.Near,
    raw_grain_dryweight_g    = r1$GrainDwt.g.Near,
    raw_stover_freshweight_g = r1$StoversampleFwt.g.Near,
    raw_stover_dryweight_g   = r1$StoverDwt.g.Near
  )
  
  d1_away <- data.frame(
    plot_id    = r1$Plotcode,
    plant_code = r1$Plantcode,
    rep        = rep_num,
    treatment  = species,
    sample_location = "away",
    raw_total_freshweight_g  = r1$TotalFwt.g.Away,
    raw_cob_freshweight_g    = r1$CobFwt.g.Away,
    raw_cob_dryweight_g      = r1$CobDwt.g.Away,
    raw_grain_dryweight_g    = r1$GrainDwt.g.Away,
    raw_stover_freshweight_g = r1$StoversampleFwt.g.Away,
    raw_stover_dryweight_g   = r1$StoverDwt.g.Away
  )
  
  d <- rbind(d1_near, d1_away)
  
  carobiner::write_files(path, meta, d)
}
