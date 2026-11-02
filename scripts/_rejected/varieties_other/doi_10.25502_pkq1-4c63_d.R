# R script for "carob"
# license: GPL (>=3)

### REJECT

## NOTES
# Per data description, there seem to be more data available out there.
# The actual exported data covers only water absorption and cooking time
# Other than the country being Nigeria, there is no location distinguishing the 3 mentioned trials

# Suggestion to reject

carob_script <- function(path) {
  
  "
Biochemical, cooking, sensory and textural properties of the boiled food product of white yam (D. rotundata) genotypes grown at different locations

Specific biochemical properties and textural attributes determine the final quality and acceptability of yam food products. 
This study assessed the flour and cooking qualities (boiled yam) of sixteen elite 
white yam genotypes (D. rotundata) grown in three locations. Fresh yam samples were cut into
regular-shaped pieces and boiled using the standard procedure. Sub-samples were oven-dried at 65 degC for 72 h 
and milled to flour. The biochemical profiling for the yam flour showed, on average, 61.35 +/- 5.15% starch, 
5.35 +/- 0.15% sugar, 1.55 +/- 0.24% crude fiber, 1.91 +/- 0.31% ash, 5.65 +/- 0.66% protein, 0.33 +/- 0.02% fat and 34.87 +/- 1.94% amylose content. 
The boiled yam's water absorption and cooking time ranged from 0.35 to 5.17% and 7.00 to 18 min, with an average of 2.74% and 10.64 min, respectively.
"
  
  uri <- "doi:10.25502/pkq1-4c63/d"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "IITA",
                                  publication = NA,
                                  project = NA,
                                  design = "16 elite white yam genotypes, boiled yam quality assessment",
                                  data_type = "trial",
                                  treatment_vars = "variety",
                                  response_vars = "water_absorption; cooking_time",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-08-23",
                                  carob_completion = 100,
                                  carob_effort = 1
  )
  
  f1 <- ff[basename(ff) == "boiled-yam-data.csv"]
  f2 <- ff[basename(ff) == "data-dictionary.csv"]
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  
  r1 <- r1[r1$Clone_ID != "" & !is.na(r1$Clone_ID), ]
  
  d <- data.frame(
    trial_id = "1",                     # single trial
    crop = "yam",
    variety = trimws(r1$Clone_ID),
    country = r2$coverage.country[r2$coverage.country != ""][1],

    water_absorption = r1$Wab,          # %
    cooking_time = r1$CT,               # minutes
  )
  
  carobiner::write_files(path, meta, wide=d)
}
