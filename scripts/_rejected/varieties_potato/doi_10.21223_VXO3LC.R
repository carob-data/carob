# R script for "carob"
# license: GPL (>=3)

## REJECTED
# No data available only code books.

carob_script <- function(path) {
  
  "
Dataset for: Assessment of at least 90 clones sub-Saharan Africa crosses for high yielding under late blight resistance

An assessment of 81 genotypes for yield and late blight resistance was completed in Holetta - Ethiopia. 
It was used alpha lattice design (12*7) with two reps and three local varieties as controls (Gudene, Belete and Jalene).
It was established in June and harvested was in October 2018. The plots were constituted of 10 plants, 
allocated 0.3 m between plants and 0.75m between rows. The selected groups showed a great performance with 
significant difference against the controls. Out of 81 clones tested, 18 were selected to next breeding stage.
"
  
  uri <- "doi:10.21223/VXO3LC"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "CIP",
                                  publication = NA,
                                  project = NA,
                                  design = "Alpha-lattice design (12x7)",
                                  data_type = NA,
                                  treatment_vars = "",
                                  response_vars = "",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-08-28",
                                  carob_completion = 100,
                                  carob_effort = 1
  )
  
  f1 <- ff[basename(ff) == "12538_DataDictionary_ElementDescripti.xlsx"]
  f2 <- ff[basename(ff) == "12538_UniqueIdentifier.xls"]
  f3 <- ff[basename(ff) == "Introduction.csv"]
  
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- read.csv(f3, sep=";")
  
  ## confirmed: r1 is trait NAME definitions with no values, r2 is
  ## keyword tags, r3 is narrative description only - no observational data
  
}
