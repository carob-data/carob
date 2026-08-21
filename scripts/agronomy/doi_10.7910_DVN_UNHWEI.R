# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
  "
The dataset includes agronomic and climatic records collected from 2015 to 2025, as well as soil data collected in 2015, 2023, and 2026. Data were collected at the AfricaRice research station in M'Bé, Bouaké, Côte d'Ivoire (7.8528° N, 5.1111° W), from a long-term experiment (LTE) established on upland rice-based cropping systems. The experiment was conducted in large plots, with an elementary plot size of 152 m². The LTE comprises a range of cropping systems differing in: • Soil tillage: conventional tillage (disc ploughing followed by harrowing) or no-till conservation agriculture; • Crop association and rotation: continuous rice cultivation or rice-maize rotations; • Cover crops: annual or perennial legume species; • Cropping season: sowing in March, June, or July; • Upland rice variety: NERICA 4 or WAB 56-50; • Fertilization: no fertilizer application or NPK fertilization; • Seed treatment: with or without fungicide and insecticide treatment. Agronomic data were obtained through field observations and measurements conducted within the LTE plots. 
"
  
  uri <- "doi:10.7910/DVN/UNHWEI"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
                                  data_organization = "AfricaRice; CIRAD",
                                  publication = NA,
                                  project = "",
                                  data_type = "on-farm experiment",
                                  treatment_vars = "variety; N_fertilizer;P_fertilizer;K_fertilizer",
                                  response_vars = "yield", 
                                  carob_completion = 100,
                                  carob_contributor = "Kora Simperegui",
                                  carob_effort = 12,
                                  carob_date = "2025-05-30",
                                  notes = NA, 
                                  design = NA
  )
  
  f <- ff[basename(ff) == "5 Agronomy 2015-2025.xls"]
  r <- carobiner::read.excel(f, na= c("n/a", "no data"))
  
  d <- data.frame(
    country= "Côte d'Ivoire",
    location = "M'be",
    latitude = 7.8528,
    longitude = -5.1111,
    geo_from_source = TRUE,
    crop = r$Main_crop,
    intercrops = r$Crop_Association,
    intercrop_type = "unknown", # Type of intercropping (e.g. mixed, strip)
    variety= r$Main_Crop_Variety,
    emergence_date= r$Emergence,
    flowering_date= r$Flowering,
    maturity_date = r$Maturity,
    # harvest_date= as.character(r$Harvesting),
    pod_density = r$Nbpods_m_2*10000, #Here the number of pods is per meter square. So we timed it per 10,000 to bring it per ha
    weeding_dates = r$Weeding,
    weeding_done = !is.na(r$Weeding),
    plant_height = r$Average_Plant_Height_cm,
    yield_treated = r$`Treated_Plot_Yield_kg_ ha_1`,
    yield_non_treated = r$`Untreated_Plot_Yield_kg_ ha_1`,
    grain_fill = 1-r$`%_Empty_Grains`,
    N_fertilizer= ifelse(r$System_Fertilization=="F0",0,24),
    P_fertilizer= ifelse(r$System_Fertilization=="F0",0,21),
    K_fertilizer= ifelse(r$System_Fertilization=="F0",0,30),
    irrigated = FALSE
    ) 
  #The name of the crops is not standardized. We need to do it in first place (both crop and intercropped)
  d$crop <- gsub("\\(|\\)", "", d$crop) # Remove parentheses from crop
  d$intercrops <- gsub("\\(|\\)", "", d$intercrops)  # Remove parentheses from intercrops
  d$intercrops <- gsub("\\s*\\+\\s*", " + ", d$intercrops) # Standardize spacing around the "+" sign
  
  # Keep only the crop occurring after "+" if there is no "+", return a blank
  d$intercrops <- ifelse(grepl("\\+", d$intercrops), trimws(sub("^[^+]*\\+", "", d$intercrops)), "")
  d$intercrops <- trimws(d$intercrops) # Remove leading and trailing spaces
  
  # Standardize crop names and convert to lowercase
  d$crop <- carobiner::fix_name(d$crop, case = "lower")
  d$intercrops <- carobiner::fix_name(d$intercrops, case = "lower")
  
  # I have noticed that one associated crop still in the format cassava + stylo (main_crop + associated crop). So I manually change it
  d$intercrops <- ifelse(d$intercrops == "cassava + stylo" , "stylo", d$intercrops)
  d$intercropped <- !is.na(d$intercrops) # If the crop is intercropped, the crop_association variable should not be empty
  
  d$fertilizer_used <- !(d$K_fertilizer == 0 & d$N_fertilizer == 0 & d$P_fertilizer == 0) #No application of fertilizer for experiments with K_fertilizer = N_fertilizer = P_fertilizer == 0
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$yield_isfresh <- TRUE
  d$yield_part <- "grain"
  
  # Note that seed treatment was done and no seed treatment was done and yields were recorded for both. Here I will merge 
  # them and create the seed_treatment variable
  yield_cols <- c("yield_treated", "yield_non_treated")
  d$row_id <- seq_len(nrow(d))
  d <-reshape(d, varying = yield_cols, v.names = "yield", timevar = "seed_treatment",times = c("treated","untreated"), idvar = "row_id",direction = "long")
  rownames(d) <- NULL
  d$row_id <- NULL
  
  # Some of the dates are converted to numerical values after loading the data while some were not. I fix it here
    convert_mixed_date <- function(x) {
        x <- as.character(x)
          # Identify values already in YYYY-MM-DD format
          is_date <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
          
            # For values that are not already dates and are not NA, I convert them using Excel's date origin (1899-12-30)
            x[!is_date & !is.na(x)] <- format(as.Date(as.numeric(x[!is_date & !is.na(x)]), origin = "1899-12-30"), "%Y-%m-%d")
            
              # Return the final vector as character
              return(x)
            }
    
  # Now I apply it to all the variables recording date
  date_vars <- c("emergence_date", "flowering_date", "maturity_date", "weeding_dates")
  d[date_vars] <- lapply(d[date_vars], convert_mixed_date)
  
  carobiner::write_files(path, meta, d)
}