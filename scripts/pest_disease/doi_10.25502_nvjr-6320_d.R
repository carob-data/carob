# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Two tables are written:
#    - d  (wide): one record per plot and crop: 24 plantain plots (3 blocks x 4 varieties x 2 cropping systems)
#         and 12 cassava records (the intercropped plots).
#    - dd (long): repeated observations over time (21 dates, Dec 2013 - Sep 2015) of BBTD incidence per plot,
#         aphids and BBTV per plantain plant, aphids per sticky trap, and cassava plant height per plant.
#         dd is linked to d with record_id (plot and crop).
# 2. Cassava yield is not included. The source gives the total root weight (g) per plot, but there are no data on the
#    harvested area, plot size, or number of plants harvested (Metadata.csv and the dataset page do not give them),
#    so it cannot be expressed in kg/ha. Can be added if the plot size or cassava spacing is obtained from the authors.
# 3. There is no plantain yield in the raw dataset.
# 4. In yield_data.csv "Treatment" is coded 1-4 without a key and differs between blocks for the same variety, so it
#    appears to be a plot position. All 12 rows are cassava in the intercropped plots.
# 5. "Biom" ("biomass of the plant in gram") in yield_data.csv is not included for the same reason as ISSUES 2: it is
#    one value per plot and the number of plants or area it refers to is not given (nor if it is fresh or dry weight).
# 6. "banana bunchy top" is used as disease name but is not yet in the terminag disease vocabulary.
#    The BBTV severity scale (1-3) is not documented. In aphid_on_plant.csv, 2820 of 2821 plants without BBTV have
#    score 1, so 1 appears to mean "no symptoms". In bbtd_incidence.csv "score" is a plot mean (e.g. 2.33, 2.8) over
#    the diseased plants only, and is empty when no plants are diseased (except 3 rows with score 0, block 1,
#    PITA23, intercropped, Jul-Sep 2014).
# 7. Variables without a terminag term are included under provisional names:
#    aphids_apterous, aphids_alate (counts per plantain plant), flowering, crop_cycle (plantain plants),
#    branching_score (cassava plants), root_number_marketable, root_number_nonmarketable (cassava, per plot).
# 8. The species of aphids counted on plants is not given (probably Pentalonia nigronervosa, the banana aphid).
# 9. "Cycle" ("cropping cycle") is 1 (2108 rows), 2 (908) or 3 (3); assumed to be mother plant and ratoon
#     cycles. One row has Cycle = 0 (2014-12-19, block 2, Ebang, intercropped, plant 5, with 250 aphids); set to NA.


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
                                  response_vars = "disease_incidence;pest_incidence;root_infection",
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
  
  ## cassava roots in the intercropped plots (no yield, see ISSUES 2)
  nroots <- r2$number_marketable_Root + r2$number_non_marketable_root
  d2 <- data.frame(
    block_id = as.character(r2$Block),
    variety = r2$Variety,             # plantain variety of the plot; cassava variety is set below
    treatment = "Intercropping",
    crop = "cassava",
    intercrops = "plantain",
    root_number_marketable = r2$number_marketable_Root,          # provisional, see ISSUES 8
    root_number_nonmarketable = r2$number_non_marketable_root,   # provisional, see ISSUES 8
    root_infection = round(100 * r2$RT_rot / nroots, 1)          # % of roots with root rot
  )
  
  d <- carobiner::bindr(d1, d2)
  
  d$treatment <- tolower(d$treatment)
  d$trial_id <- "1"
  d$yield <- as.numeric(NA)
  d$yield_part <- ifelse(d$crop == "cassava", "roots", "fruit")
  d$yield_isfresh <- TRUE
  d$country <- "Cameroon"
  d$adm1 <- "South"
  d$longitude <- NA
  d$latitude <- NA
  d$geo_from_source <- TRUE
  d$on_farm <- NA
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$planting_date <- "2013-09"   # "A field experiment was planted in September 2013" (dataset description)
  d$yield_moisture <- d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- d$harvest_date <- NA
  
  
  ## BBTD incidence per plantain plot
  d3 <- data.frame(
    block_id = as.character(r1$Block),
    variety = r1$Variety,
    treatment = r1$Treatment,
    date = as.character(as.Date(r1$Date, format="%m/%d/%Y")),
    crop = "plantain",
    disease = "banana bunchy top",
    disease_incidence = round(100 * r1$Total_diseased / r1$Total_planted, 2),   
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
    pest_incidence = r3$total_aphids,
    aphids_apterous = r3$Apterous,    # provisional, see ISSUES 8
    aphids_alate = r3$Alates,         # provisional
    flowering = r3$Flowering == 1,    # provisional; 0/1 "vegetative state of the plant"
    crop_cycle = r3$Cycle             # provisional; 1 = mother plant, 2-3 = ratoons? see ISSUES 10
  )
  d4$crop_cycle[d4$crop_cycle == 0] <- NA   # one row, see ISSUES 10
  d4 <- d4[!is.na(d4$pest_incidence), ]    # one empty row (2013-12-01, block 1, Essong, sole, plant 3)
  
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
    plant_height = r5$PLHT,
    branching_score = r5$Branching_1_3    # provisional, see ISSUES 8 (scale 1-3)
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

  ## link dd to d with record_id (plot and crop, as each intercropped plot has a plantain and a cassava record)
  d$record_id <- 1:nrow(d)
  dd$record_id <- d$record_id[match(paste(dd$plot_id, dd$crop), paste(d$plot_id, d$crop))]
  stopifnot(!any(is.na(dd$record_id)))
  dd$block_id <- dd$variety <- dd$treatment <- dd$plot_id <- dd$crop <- NULL

  d$variety_type <- ifelse(d$variety %in% c("Ebang", "Essong"), "local", "hybrid")
  d$variety[d$crop == "cassava"] <- "TMS 96/0023"
  d$variety_type[d$crop == "cassava"] <- "improved"
  dd$plant_height[dd$plant_height < 10] <- NA  
  dd$disease_incidence <- as.numeric(dd$disease_incidence)

  carobiner::write_files(path, meta, d, dd)
}
