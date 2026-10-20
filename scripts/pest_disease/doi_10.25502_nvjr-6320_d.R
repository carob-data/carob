# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The source files have different levels of observation, so they are not bound into one table:
#    - d  (written): one record per plot and crop: 24 plantain plots (3 blocks x 4 varieties x 2 cropping systems)
#         and 12 cassava records (the intercropped plots) with the cassava yield.
#    - dd (NOT written): 5686 repeated observations over time (21 dates, Dec 2013 - Sep 2015) of BBTD incidence
#         per plot, aphids and BBTV per plantain plant, aphids per sticky trap, and cassava plant height per plant.
# 2. Cassava yield is entered as it is in the source (grams); there are no data on the harvested area or number of
#    plants harvested, so it cannot be converted to kg/ha. Per-root weights (350-600 g) confirm the unit is grams.
# 3. There is no plantain yield in the raw dataset.
# 4. In yield_data.csv "Treatment" is coded 1-4 without a key and differs between blocks for the same variety, so it
#    appears to be a plot position. All 12 rows are cassava in the intercropped plots.
# 5. "Biom" ("biomass of the plant in gram") is not included: it is not specified whether it is fresh or dry weight.
# 6. bbtd_incidence.csv: BBTD_Incidence does not equal Total_diseased / Total_planted * 100 in 14 rows (block 3, PITA23,
#    from 2014-12-20); incidence is recomputed from the counts. The "death" column is empty.
# 7. "banana bunchy top" is not in the terminag disease vocabulary. The BBTV severity scale (1-3) is not documented.
# 8. Not included (no standard variable): apterous and alate aphid counts, flowering, crop cycle, cassava branching
#    score, number of marketable and non-marketable roots (used only to compute root_infection).
# 9. The species of aphids counted on plants is not given (probably Pentalonia nigronervosa, the banana aphid).

carob_script <- function(path) {
  
  "
Intercropping for BBTD management

Research examining how intercropping affects the spread of Banana Bunchy Top Disease (BBTD) and aphid population dynamics. A 24-month field experiment compared four plantain cultivars (two local varieties: Essong, Ebang; two hybrids: PITA 23, PITA 24) intercropped with improved cassava variety TMS 96/0023 against monocropping controls in Cameroon.
"
  
  uri <- "doi:10.25502/nvjr-6320/d"
  group <- "pest_disease"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "IITA",
                                  publication = NA,
                                  project = NA,
                                  design = "3 blocks; 4 plantain varieties x 2 cropping systems (intercropped with cassava TMS 96/0023, sole plantain); 24 plantain plants per plot",
                                  data_type = "experiment",
                                  treatment_vars = "intercrops",
                                  response_vars = "yield;root_infection",
                                  notes = NA,
                                  carob_contributor = "Mitchelle Njukuya",
                                  carob_date = "2026-09-30",
                                  carob_completion = 100,
                                  carob_effort = 3
  )
  
  f1 <- ff[tolower(basename(ff)) == "bbtd_incidence.csv"]
  f2 <- ff[basename(ff) == "yield_data.csv"]
  f3 <- ff[basename(ff) == "aphid_on_plant.csv"]
  f4 <- ff[basename(ff) == "aphid_trap_data.csv"]
  f5 <- ff[basename(ff) == "cassava_plant_height.csv"]
  
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)[, 1:10]     # drop empty trailing columns
  r3 <- read.csv(f3)
  r4 <- read.csv(f4)[, 1:7]      # drop empty trailing columns
  r5 <- read.csv(f5)[, 1:9]      # drop empty last column
  
  ## plantain plots (list of plots taken from bbtd_incidence.csv)
  d1 <- unique(data.frame(
    block_id = as.character(r1$Block),
    variety = r1$Variety,
    treatment = r1$Treatment,
    crop = "plantain",
    intercrops = ifelse(r1$Treatment == "Intercropping", "cassava", "none")
  ))
  
  ## cassava yield in the intercropped plots
  nroots <- r2$number_marketable_Root + r2$number_non_marketable_root
  d2 <- data.frame(
    block_id = as.character(r2$Block),
    variety = r2$Variety,             # plantain variety of the plot; cassava variety is set below
    treatment = "Intercropping",
    crop = "cassava",
    intercrops = "plantain",
    yield = r2$Weight_of_Marketable_root + r2$weight_of_non_marketable_root,   # g, see ISSUES 2
    yield_marketable = r2$Weight_of_Marketable_root,                          # g
    root_infection = round(100 * r2$RT_rot / nroots, 1)                      # % of roots with root rot
  )
  
  d <- carobiner::bindr(d1, d2)
  
  d$treatment <- tolower(d$treatment)
  d$trial_id <- "1"
  d$yield_part <- ifelse(d$crop == "cassava", "roots", "fruit")
  d$yield_isfresh <- TRUE
  d$country <- "Cameroon"
  d$longitude <- NA
  d$latitude <- NA
  d$geo_from_source <- FALSE
  d$on_farm <- NA
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$planting_date <- as.character(NA)
  d$yield_moisture <- d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- d$harvest_date <- NA
  
  
  ## BBTD incidence per plantain plot
  d3 <- data.frame(
    block_id = as.character(r1$Block),
    variety = r1$Variety,
    treatment = r1$Treatment,
    date = as.character(as.Date(r1$Date, format="%m/%d/%Y")),
    crop = "plantain",
    disease = "banana bunchy top",
    disease_incidence = round(100 * r1$Total_diseased / r1$Total_planted, 2),   # recomputed, see ISSUES 6
    disease_severity = as.character(r1$score)
  )
  
  ## aphids and BBTV on sampled plantain plants
  d4 <- data.frame(
    block_id = as.character(r3$Block),
    variety = r3$Variety,
    treatment = r3$Treatment,
    date = as.character(as.Date(r3$Date, format="%m/%d/%Y")),
    crop = "plantain",
    plant_id = as.character(r3$plant),
    disease = "banana bunchy top",
    disease_incidence = 100 * r3$BBTV_presence_absence,     # 0 = absent, 100 = present on the plant
    disease_severity = as.character(r3$BBTVscore),
    pest_species = "aphids",
    pest_incidence = r3$total_aphids
  )
  
  ## aphids on sticky traps: one row for P. nigronervosa and one for other aphids
  d5 <- data.frame(
    block_id = rep(as.character(r4$Block), 2),
    variety = rep(r4$Variety, 2),
    treatment = rep(r4$Treatment, 2),
    date = rep(as.character(as.Date(r4$Date, format="%m/%d/%Y")), 2),
    crop = "plantain",
    pest_species = rep(c("Pentalonia nigronervosa", "other aphids"), each=nrow(r4)),
    pest_incidence = c(r4$P_nigronervosa, r4$Other_aphids)
  )
  
  ## cassava plant height
  d6 <- data.frame(
    block_id = as.character(r5$Block),
    variety = r5$Variety,
    treatment = r5$Treatment,
    date = as.character(as.Date(r5$Date, format="%m/%d/%Y")),
    crop = "cassava",
    plant_id = as.character(r5$Plant),
    plant_height = r5$PLHT
  )
  
  dd <- carobiner::bindr(d3, d4, d5, d6)
  
  ## plantain variety names are written differently between files (EBANG, ebang, pita 23, PITA23)
  d$variety <- toupper(gsub(" ", "", d$variety))
  d$variety[d$variety == "EBANG"] <- "Ebang"
  d$variety[d$variety == "ESSONG"] <- "Essong"
  d <- unique(d)
  dd$variety <- toupper(gsub(" ", "", dd$variety))
  dd$variety[dd$variety == "EBANG"] <- "Ebang"
  dd$variety[dd$variety == "ESSONG"] <- "Essong"
  
  ## plot_id is made after fixing the variety names so that d and dd match
  d$plot_id <- paste(d$block_id, d$variety, d$treatment, sep="_")
  dd$plot_id <- paste(dd$block_id, dd$variety, tolower(dd$treatment), sep="_")
  dd$block_id <- dd$variety <- dd$treatment <- NULL
  
  d$variety_type <- ifelse(d$variety %in% c("Ebang", "Essong"), "local", "hybrid")
  d$variety[d$crop == "cassava"] <- "TMS 96/0023"
  d$variety_type[d$crop == "cassava"] <- "improved"
  
  carobiner::write_files(path, meta, d)
}
