
# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
  
  "Conservation agriculture involves reduced tillage, diversification of plant associations, and retention of crop residues to maintain soil cover. However, there is knowledge gap on the appropriate rate of application and interactive effect of residues and nitrogen as in some situations cases of nitrogen lock-up have been reported. This present data set addresses the effects of different nitrogen and residue levels on maize productivity, soil temperature, soil moisture and soil structure in contrasting soil types over 6 seasons. The trials were set across southern Africa i.e. Malawi, Mozambique, Zambia and Zimbabwe. The treatments were as follows: Main treatments: 1. Conventional tillage 2. No-tillage, 0 t/ha residues 3. No-tillage, 2 t/ha residues 4. No-tillage, 4 t/ha residues 5. No-tillage, 6 t/ha residues 6. No-tillage, 8 t/ha residues, Subtreatments: 1. 0 N 2. 30N (200 kg/ha Compound D – 46 kg/ha AN 3. 90N (200 kg/ha Compound D –220 kg/ha AN) The measured attributes are as follows: 1. Maize and grain yields 2. Soil profile temperature 3. Soil profile mositure 4. Normalized difference vegetation index (NDVI)"
  
  uri <- "hdl:11529/10868"
  group <- "agronomy"
  
  
  ff  <- carobiner::get_data(uri, path, group)
  
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
                                  data_organization = "CIMMYT",
                                  publication = NA,
                                  project = NA,
                                  data_type = "experiment",
                                  treatment_vars = "fertilizer_used;land_prep_method;residue_prevcrop_used",
                                  response_vars = "yield", 
                                  completion = 100,
                                  carob_contributor = "Mitchelle Njukuya",
                                  carob_date = "2025-08-29",
                                  notes = NA,
                                  design = NA
  )
  
  f <- ff[basename(ff) == "Residue Level Trial.xlsx"]
  r <- carobiner::read.excel(f)
  
  d <- data.frame(
    country = r$Country,
    crop = "maize",
    yield_part = "grain",
    site = r$Site,
    treatment = r$Treatment,
    yield = r$`Grain (kg/ha)`
  )
  
  #specific site names are from LT conservation agricultural data standardized before
  #they are however not documented in this particular dataset
  #d$site <- gsub("DTC","Domboshava Training Center",d$site)
  #d$site <- gsub("UZ","University of Zimbabwe",d$site)
  #d$site <- gsub("SRS","Sussundenga Research station",d$site)
  #d$site <- gsub("Monze|MFTC","Monze Farmers Training Center",d$site)
  
  d$land_prep_method <- ifelse(d$treatment==1,"conventional","none")
  d$residue_prevcrop_used <- ifelse(d$treatment %in% c(1, 2), FALSE, 
                                    ifelse(d$treatment %in% c(3, 4, 5, 6), TRUE, NA))
  d$residue_prevcrop <- NA
  d$residue_prevcrop[d$treatment==2] <- 0
  d$residue_prevcrop[d$treatment==3] <- 2000
  d$residue_prevcrop[d$treatment==4] <- 4000
  d$residue_prevcrop[d$treatment==5] <- 6000
  d$residue_prevcrop[d$treatment==6] <- 8000
  
  d$fertilizer_used <- ifelse((r$subtreatment == 1), FALSE, 
                              ifelse(r$subtreatment %in% c(2, 3), TRUE, NA))
  
  d$trial_id[d$country=="Zimbabwe"] <- "1"
  d$trial_id[d$country=="Mozambique"] <- "2"
  d$trial_id[d$country=="Zambia"] <- "3"
  d$trial_id[d$country=="Malawi"] <- "4"
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
    
  d$longitude[d$site=="DTC"] <- 31.1337
  d$longitude[d$site=="UZ"] <- 31.0530
  d$longitude[d$site=="SRS"] <- 33.2416
  d$longitude[d$site=="Makoholi"] <- 30.7833
  d$longitude[d$site=="Chitedze"] <- 33.654
  d$longitude[d$site=="Monze"] <- 27.4414
  d$latitude[d$site=="DTC"] <- -17.6091
  d$latitude[d$site=="UZ"] <- -17.7840
  d$latitude[d$site=="SRS"] <- -19.3169
  d$latitude[d$site=="Makoholi"] <- -19.8333
  d$latitude[d$site=="Chitedze"] <- -13.9732
  d$latitude[d$site=="Monze"] <- -16.2402
  d$geo_from_source <- FALSE
  
  d$planting_date <- as.character(NA)
  d$harvest_date  <- as.character(NA)
  
  d$P_fertilizer <- NA
  d$P_fertilizer[r$subtreatment %in% c(2, 3)] <- 12.227 
  d$K_fertilizer <- NA
  d$K_fertilizer[r$subtreatment %in% c(2, 3)] <- 11.617
  d$N_fertilizer <- NA
  d$N_fertilizer[r$subtreatment == 2] <- 29.64
  d$N_fertilizer[r$subtreatment == 3] <- 89.9
  d$fertlizer_type <- ifelse((r$subtreatment == 1), "none", 
                             ifelse(d$treatment %in% c(2, 3), "D-compound;AN", NA))
  
  carobiner::write_files(path, meta, d)
}
