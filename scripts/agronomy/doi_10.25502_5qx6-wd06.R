# R script for "carob"
# license: GPL (>=3)


## ISSUES
# Trt_Code shows a third fertilizer level, NPK2 (16 rows), undefined in both
# the Fertilizer column (blank) and the treatment decoder - left as NA for
# fertilizer_used/N/P/K rather than guessed.
#
# Root fresh-weight columns have no stated units in the source; the value
# distribution (median ~30, max ~95) matches published cassava yields in
# t/ha, so converted to kg/ha (x1000).
#
# No location below country level exists in the source.

# 8 plot_ID values are duplicated (16 rows), triggering carob's duplicate
# check. Only 1 pair (ACPOTZ015182) is a true, complete duplicate; the other
# 7 pairs differ in at least one other column (weight, harvest date, and/or
# Trt_Code) despite sharing the same plot_ID. All 16 rows are kept as-is.

## NOTES
# 47 rows labeled "Sweet potato Monocrop" show clear signs cassava was also
# present (cassava_Density populated, "together wth cassava" noted, real
# cassava yield weight) - treated as intercropped here, contrary to their
# label.
#
# "75:20:90 NPK" treated as elemental kg/ha N:P:K (not oxide P2O5/K2O),
# confirmed via an external citation using the identical phrase.
#
# cassava_Density mapped to seed_density (planted rate), not plant_density,
# since no emergence/survival count exists in the source.

# No planting date exists anywhere in the source - only harvestDate is
# recorded.


carob_script <- function(path) {
  
  "
Cassava sweet potato intercropping in Tanzania

On-farm cassava/sweet potato intercropping (CIS) trials, rounds CIS-3, CIS-4,
and CIS-5, conducted in Tanzania under the ACAI project (IITA). 490 plots
across 108 trials tested sweet potato planting density (10,000/20,000/30,000
plants/ha) and planting time relative to cassava (simultaneous, or 2 weeks
after), with and without NPK fertilizer (75:20:90 kg/ha N:P:K), against
cassava and sweet potato sole-crop checks. Cassava marketable, diseased, and
small root fresh weights were recorded at harvest.
"
  uri <- "doi:10.25502/5qx6-wd06"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
                                  data_organization = "IITA",
                                  publication = NA,
                                  project = "ACAI",
                                  design = NA,
                                  data_type = "on-farm experiment",
                                  treatment_vars = "intercropped;N_fertilizer",
                                  response_vars = "yield;yield_marketable;root_infection",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-07-15",
                                  carob_completion = 80,
                                  carob_effort = 3
  )
  
  f1 <- ff[basename(ff) == "cis_ckan.csv"]
  r1 <- read.csv(f1)
  ## r2 (cis_ckan_treatment.csv) and r3 (column_dictionary_acai_cis_forckan.csv)
  ## are reference/dictionary files, not data.
  
  #Add crop related variable; Crop always cassava as documented yields are for cassava
  r1$crop <- "cassava"
  r1$intercropped <- r1$cropping != "Cassava Monocrop" #SP monocrop als says "together with cassava"
  r1$intercrops <- ifelse(r1$intercropped, "sweetpotato", "none")
  r1$intercrop_type <- ifelse(r1$sweetPotatoPlanting == "together wth cassava", "mixed",
                              ifelse(r1$sweetPotatoPlanting == "2 weeks after cassava", "relay", NA)) # closest to planting timing
  
## Adding the fertilizer variables
  r1$fertilizer_used <- ifelse(grepl("NPK", r1$Fertilizer), TRUE,ifelse(r1$Fertilizer == "control", FALSE, NA))
  
  ##Fertilizer: "control" -> 0 N/P/K; "75:20:90 NPK" assumed to already be elemental kg/ha N:P:K (not oxide form P2O5/K2O)
  r1$N_fertilizer <- ifelse(r1$Fertilizer == "control", 0,ifelse(r1$Fertilizer == "75:20:90 NPK", 75, NA))
  r1$P_fertilizer <- ifelse(r1$Fertilizer == "control", 0,ifelse(r1$Fertilizer == "75:20:90 NPK", 20, NA))
  r1$K_fertilizer <- ifelse(r1$Fertilizer == "control", 0,ifelse(r1$Fertilizer == "75:20:90 NPK", 90, NA))
  
  # assumed the weights provided are in tonne/ha... since planting density reported is per ha
  r1$yield <- rowSums(r1[, c("tuberizedMarketableRootsFW","tuberizedDiseasedRootsFW","tuberizedSmallRootsFW")], na.rm = FALSE) * 1000
  r1$yield_marketable <- r1$tuberizedMarketableRootsFW * 1000
  r1$yield_isfresh <- TRUE
  r1$yield_moisture <- NA
  r1$yield_part <- "roots"
  
  # disease variable; % of total tuberized root weight that was diseased
  r1$root_infection <- (r1$tuberizedDiseasedRootsFW / r1$yield) * 100
  
  d <- data.frame(
    trial_id = r1$trial_ID,
    plot_id = r1$plot_ID,
    treatment = r1$Trt_Code,
    
    country = r1$country,
    
    harvest_date = as.character(as.Date(r1$harvestDate, format = "%d/%m/%Y")),
    
    crop = r1$crop,
    intercropped = r1$intercropped,
    intercrops = r1$intercrops,
    intercrop_type = r1$intercrop_type,
    
    yield = r1$yield,
    yield_marketable = r1$yield_marketable,
    yield_part = r1$yield_part,
    yield_isfresh = r1$yield_isfresh,
    yield_moisture = r1$yield_moisture,
    
    root_infection = r1$root_infection,
    
    seed_density = r1$cassava_Density,   # cassava planting rate, plants/ha
    
    fertilizer_used = r1$fertilizer_used,
    N_fertilizer = r1$N_fertilizer,
    P_fertilizer = r1$P_fertilizer,
    K_fertilizer = r1$K_fertilizer,
    
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = NA,
    
    longitude = NA,
    latitude = NA,
    geo_from_source = NA
  )
  
  d$planting_date <- NA
  
  carobiner::write_files(path, meta, d)
}
