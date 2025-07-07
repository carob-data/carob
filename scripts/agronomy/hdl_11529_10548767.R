# R script for "carob"


carob_script <- function(path) {

"A field experiment was conducted in maize under medium-term conservation agriculture (CA) based maizewheat system at BISA-CIMMYT, Ladhowal, Punjab during kharif 2019 to assess the effect of CA+ practices (CA with sub-surface drip irrigation) with variable N doses on maize. The CA+ treatments were residue retained (WR) permanent bed (PB) with sub-surface drip fertigation (PB-SSD): without N (N0), 120 kg N/ha,150 kg N/ha applied in 4-equal (Eq) and differential splits (Df); CA alone treatment includ PB furrow irrigation with 120 kg N/ha (PBWRFurrow- N120); conventional tillage (CT) involved furrow irrigation with 120 kg N/ha (CTWOR-Furrow-N120) and other treatments were residue removed (WOR) PB: PBWOR-without N (N0), with 120 kg N/ha, and 150 kg N/ha applied in four Eq-splits and Df-splits. The findings of the present experiment showed that the numerical value of yield attributing characters were higher under CA+ plots as compared to CA alone (PBWR-Furrow-N120) and CT (CTWOR-Furrow-N120). Biological yield of maize was significantly higher in all CA+ plots as compared to CA alone and CT plots. Highest biological yield was recorded under PBWR-SSD-N150 Df (23.45 t/ha). Highest no. of cobs (72800/ha), no. of grains/cob (605) and cob length (22.61cm) along with dry matter resulted highest biological yield in PBWR-SSD-N150 plots. The grain N content remained statistically similar across all the N management plots, but in case of total N uptake, PBWR-SSD-N150 Df (CA+) plots dominated due to higher biomass. Besides, CA+ based PBWR-SSD-N120 (average of Df and Eq) registered 23-24% higher total N uptake than CA alone (PBWRFurrow- N120) and conventional (CTWOR-Furrow-N120) plots. Improved agronomic N use-efficiency was also recorded under CA+ plots as compared to CA alone (36.4 kg/kg N) and CT (36.7 kg/kg N) plots. (2021-02-12)"

  uri <- "hdl:11529/10548767"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
    data_organization="ICAR;CIMMYT;IRRI",
    publication='doi:10.56093/ijas.v91i3.112544',
    project=NA,
    data_type = "on-farm experiment",
    response_vars = "yield",
    treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;irrigated;land_prep_method;residue_prevcrop_used;N_splits",
    carob_contributor="Hope Mazungunye",
    carob_date="2023-09-15",
    modified_by="Siyabusa Mkuhlani",
    last_modified = "2024-09-06"
  )
  
  #Process sheet: Yield and Attributing Character
  bn <- basename(ff)

    r1 <- carobiner::read.excel.hdr(ff[bn=="IJAS_Table_Replicated_Data.xlsx"], sheet="Yield and Attributing Character", hdr=1, skip=1)
  d1 <- r1[, -c(2:19)] #Selecting columns of interest, as some parameters cannot be processed.
  
  #Added new column names, as they get mixed up when data set is read. Some of the cells in the raw data set are merged.
  colnames(d1) <- c("treatment", "R1_dmy_total", "R2_dmy_total", "R3_dmy_total", "R1_harvest_index", "R2_harvest_index", "R3_harvest_index")
  
  d1 <-reshape(d1,
              varying=c("R1_dmy_total", "R1_harvest_index",
                        "R2_dmy_total", "R2_harvest_index",
                        "R3_dmy_total", "R3_harvest_index"),
              v.names=c("harvest_index", "dmy_total"),
              timevar="rep",
              times=c("1", "2", "3"),
              direction="long")
  
  #process yield values
  d1$yield_part <- "grain"
  d1$dmy_total <- d1$dmy_total * 1000
  #grain yield computed using Harvest index.
  d1$dm_yield <- d1$dmy_total * d1$harvest_index / 100 
  d1$dmy_residue <- d1$dmy_total - d1$dm_yield
  
  #management
  d1$land_prep_method <- ifelse(grepl("PB", d1$treatment), "raised beds", "conventional")
  d1$residue_prevcrop_used <- grepl("WR", d1$treatment)
  d1$irrigated <- TRUE
  d1$irrigation_method <- ifelse(grepl("SSD", d1$treatment), "sub-surface drip", "furrow")
  
  # fertilizer information (from the data and the paper)
  d1$fertilizer_used <- grepl("N0", d1$treatment)
  d1$fertilization_method <- ifelse(grepl("N0", d1$treatment), NA, "fertigation")
  
  #Type and formulae of the basal dressing applied was not stated but in the publication
  #authors state '....A common dose of 60 kg/ha P2O5 + 30 kg/ha K2O was applied
  #to all plots as basal.'
  #Basal applied. 60kg/ha p205 & 30kg/ha k2O
  d1$P_fertilizer <- 60 * 0.346  
  d1$K_fertilizer <- 30 * 0.831
  
  #The type and formula of the fertilizer applied was not stated in the publication.
  #The authors simply mentioned '...96.5 kg and   126.5 kg N was allotted for 
  #SSD-N120 Eq (both PBWOR and PBWR) and SSD-N150 Eq (both PBWOR and PBWR) plots, respectively
  d1$N_fertilizer <- 0
  d1$N_fertilizer <- ifelse(grepl("N120", d1$treatment), 96.5, d1$N_fertilizer)
  d1$N_fertilizer <- ifelse(grepl("N150", d1$treatment), 126, d1$N_fertilizer)
  d1$N_splits <- ifelse(grepl("-SSD", d1$treatment), 4L, 2L)
  d1$N_splits[d1$N_fertilizer == 0] <- 0L

  #process sheet: N Data (NUE, Uptake)
  r2 <- carobiner::read.excel(ff[bn=="IJAS_Table_Replicated_Data.xlsx"], sheet= "N Data (NUE, Uptake)")
  #selecting columns of interest, as some parameters cannot be processed.
  d2 <- r2[-1, c(1:7)] 
  #Added new column names, as they get mixed up when data set is read. Some of the cells in the raw data set are merged.
  colnames(d2) <- c("treatment", "R1_grain_N", "R2_grain_N", "R3_grain_N", "R1_leaf_N", "R2_leaf_N", "R3_leaf_N")
  
  d2 <- reshape(d2,
              varying=c("R1_grain_N", "R1_leaf_N",
                        "R2_grain_N", "R2_leaf_N",
                        "R3_grain_N", "R3_leaf_N"),
              v.names=c("grain_N", "leaf_N"),
              timevar="rep",
              times=c("1", "2","3"),
              direction='long')
  
    d2[, c("grain_N", "leaf_N")] <- lapply(d2[, c("grain_N", "leaf_N")], as.numeric)
    d <- merge(d1, d2, by=c("treatment", "rep", "id"))
    d$id <- NULL 
    d$rep <- as.integer(d$rep)
    d$trial_id <- "1"
    d$geo_from_source <- TRUE
    d$crop <- "maize"
    d$on_farm <- FALSE
    d$is_survey <- FALSE
    d$irrigated <- TRUE
    d$country <- "India" 
    d$adm1 <- "Punjab"
    d$adm2 <- "Ludhiana"
    d$adm3 <- "Ludhiana West"
    d$location <- "Ladhowal"
    d$site <- tolower("BISA-CIMMYT")
    d$longitude <- 75.44
    d$latitude <- 30.99
    d$elevation <- 229
    d$planting_date <- "2019-06-20"
    d$harvest_date <- "2019-10-13"
    d$variety <- "P3396"

    carobiner::write_files(meta, d, path=path)
}

