# R script for "carob"
# license: GPL (>=3)

## NOTES
# 6 site trials, sensory flavor/texture scores (1-9, 3 evaluators/plot),
# potato clones vs Canchan/Unica reference varieties, Peru. Long format:
# one row per plot x evaluator. 

## ISSUES
# r2/r3/r9 have more flavor/texture data (by clone x locality) but aren't
# used - their locality names don't reliably link to the 6 site codes.

## That is not true for r2/r3

# flavor_score/texture_score: no terminag equivalent, suggested new terms.


carob_script <- function(path) {
  
"
Dataset for: Late blight resistant potato varieties for the tropical highlands and mid-elevation
 
Sensory evaluation (flavor and texture, scored by 3 independent tasters) of
8-10 CIP potato clones bred for late blight resistance, alongside reference
varieties Canchan and Unica, across 6 trial sites in Peru (30 plots per
site: ~10 clones x 3 reps). Note: the source dataset's own description also
references tuber yield, dry matter content, and reducing sugar content as
selection criteria, but none of these variables are present in any of the
9 files - only sensory flavor/texture scores are provided.
"
  
  uri <- "doi:10.21223/BJECYK"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "flavor_score;texture_score",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_date = "2026-07-23",
		carob_completion = 75,
		carob_effort = 4
  )
  
  f1 <- ff[basename(ff) == "01_MAJ21_01.xlsx"]
  f2 <- ff[basename(ff) == "02_french_fries.xlsx"]
  f3 <- ff[basename(ff) == "03_Traditional frying.xlsx"]
  f4 <- ff[basename(ff) == "04_HYO21_03.xlsx"]
  f5 <- ff[basename(ff) == "05_HCHO21_02(LICAME).xlsx"]
  f6 <- ff[basename(ff) == "06_HCHO21_03(YANAC).xlsx"]
  f7 <- ff[basename(ff) == "07_CAJ21_01.xlsx"]
  f8 <- ff[basename(ff) == "08_HCO21_03.xlsx"]
  f9 <- ff[basename(ff) == "Summary.xlsx"]
  
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  r4 <- carobiner::read.excel(f4)
  r5 <- carobiner::read.excel(f5)
  r6 <- carobiner::read.excel(f6)
  r7 <- carobiner::read.excel(f7)
  r8 <- carobiner::read.excel(f8)
  #r9 <- carobiner::read.excel(f9)
  ## r2, r3, r9 are not used - see ISSUES (no reliable link to trial_id/site)
  
  ### Reshape one site's plot-level file into long format (one row per plot x
  ### evaluator).
  reshape_site <- function(r, site_code) {
    do.call(rbind, lapply(1:3, function(i) {
      data.frame(
        trial_id = site_code,
        plot_id = as.character(r$Plot),
        variety = r$Clone,
        rep = r$Rep,
        evaluator = i,
        flavor_score = r[[paste0("Evaluator", i, "_Flavor")]],
        texture_score = r[[paste0("Evaluator", i, "_Texture")]]
      )
    }))
  }
  
  d <- rbind(
    reshape_site(r1, "MAJ21_01"),
    reshape_site(r4, "HYO21_03"),
    reshape_site(r5, "HCHO21_02"),
    reshape_site(r6, "HCHO21_03"),
    reshape_site(r7, "CAJ21_01"),
    reshape_site(r8, "HCO21_03")
  )
  
  ### Locality names inferred from site-code correspondence (see ISSUES) -
  ### trial_id keeps the original source code regardless
  locality_lookup <- c(
    MAJ21_01 = "Majes",
    HYO21_03 = "Huancayo",
    HCHO21_02 = "Chota",
    HCHO21_03 = "Yanac",
    CAJ21_01 = "Chugay",
    HCO21_03 = "Huanuco"
  )
  d$location <- unname(locality_lookup[d$trial_id])
  
  ### Coordinates, geocoded via carobiner::geocode(country="Peru", location=...)
  geo_lookup <- data.frame(
    location = c("Majes", "Huancayo", "Chota", "Yanac", "Chugay", "Huanuco"),
    longitude = c(-72.2878, -75.1608, -79.1800, -77.8528, -77.8352, -75.8050),
    latitude = c(-16.3243, -12.1722, -6.3829, -8.6220, -7.8108, -9.4029)
  )
  d <- merge(d, geo_lookup, by = "location", all.x = TRUE)
  d$geo_from_source <- FALSE
  d$country <- "Peru"
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$crop <- "potato"
  d$yield <- NA
  d$yield_part <- NA
  d$yield_moisture <- NA
  d$yield_isfresh <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$K_fertilizer <- NA
  d$planting_date <- NA
  d$harvest_date <- NA
  d$irrigated <- NA
  d$rep <- as.integer(gsub("R", "", d$rep))
  
  carobiner::write_files(path, meta, d)
}
