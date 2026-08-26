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
                                  carob_date = "2026-08-26",
                                  notes = NA, 
                                  design = NA
  )
  
  f1 <- ff[basename(ff) == "5 Agronomy 2015-2025.xls"]
  f2 <- ff[basename(ff) == "6 Soil  2015.xls"]
  f3 <- ff[basename(ff) == "7 Soil 2023-26.xls"]
  
  r1 <- carobiner::read.excel(f1, na= c("n/a", "no data"))
  r2 <- carobiner::read.excel(f2, na= c("n/a", "no data"))
  r3 <- carobiner::read.excel(f3, na= c("n/a", "no data"))
  
  d <- data.frame(
    country= "Côte d'Ivoire",
    location = "M'be",
    plot_id = r1$Plot_ID,
    date = r1$Year,
    latitude = 7.8528,
    longitude = -5.1111,
    geo_from_source = TRUE,
    crop = r1$Main_crop,
    intercrops = r1$Crop_Association,
    intercrop_type = "unknown", # Type of intercropping (e.g. mixed, strip)
    variety= r1$Main_Crop_Variety,
    planting_date = r1$Main_Crop_Sowing,
    emergence_date= r1$Emergence,
    flowering_date= r1$Flowering,
    maturity_date = r1$Maturity,
    harvest_date= r1$Harvesting,
    pod_density = r1$Nbpods_m_2*10000, #Here the number of pods is per meter square. So we timed it per 10,000 to bring it per ha
    weeding_dates = r1$Weeding,
    weeding_done = !is.na(r1$Weeding),
    plant_height = r1$Average_Plant_Height_cm,
    plot_area_treated = r1$Treated_Plot_Size_m2,
    plot_area_non_treated = r1$Untreated_Plot_Size_m2,
    yield_treated = r1$`Treated_Plot_Yield_kg_ ha_1`,
    yield_non_treated = r1$`Untreated_Plot_Yield_kg_ ha_1`,
    grain_fill = 1-r1$`%_Empty_Grains`,
    # Two fertilization levels were applied for all the systems until 2024:  - F0, without any fertilizer application  - F1, corresponding to N-P-K: 104-21-30 kg ha-1. 
    # In 2025, two additional fertilization levels were added:  - F0+ :  P-S-Ca:8-12-18 kg ha-1  - F1+ : N-P-K: 104-21-30 kg ha-1  + P-S-Ca:8-12-18 kg ha-1 
    N_fertilizer= ifelse(r1$System_Fertilization=="F0",0,104),
    P_fertilizer= ifelse(r1$System_Fertilization=="F0",0,21) + ifelse(r1$Year==2025 ,8,0),
    K_fertilizer= ifelse(r1$System_Fertilization=="F0",0,30) + ifelse(r1$Year==2025 ,12,0),
    S_fertilizer = ifelse(r1$Year==2025 ,12,0),
    Ca_fertilizer = ifelse(r1$Year==2025 ,18,0),
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
  
  # Note that seed treatment was done and no seed treatment was done and yields as well as plot area size were recorded for both. Here I will merge 
  # them and create the seed_treatment variable
  
  d$row_id <- seq_len(nrow(d))
  
  d <- reshape(
    d,
    varying = list(c("yield_treated", "yield_non_treated"), c("plot_area_treated", "plot_area_non_treated")),
    v.names = c("yield", "plot_area"),
    timevar = "seed_treatment",
    times = c("treated", "untreated"),
    idvar = "row_id",
    direction = "long")
  
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
  date_vars <- c("emergence_date", "flowering_date", "maturity_date", "weeding_dates", "planting_date", "harvest_date")
  d[date_vars] <- lapply(d[date_vars], convert_mixed_date)
  
  # Replace the erroneous planting date (1900-01-16) with NA.
  # The date is inconsistent with the experiment timeline, as emergence
  # was recorded on 2018-08-28, which falls within the experiment period.
  d$planting_date[d$planting_date == "1900-01-16"] <- NA
  
  s_2015 <- data.frame(
    date = 2015,
    plot_id = r2$Plot_ID,
    depth_top = ifelse(r2$Horizon=="0-5 cm", 0, ifelse(r2$Horizon=="15-25 cm", 15, ifelse(r2$Horizon=="35-45 cm", 35, NA))),
    depth_bottom = ifelse(r2$Horizon=="0-5 cm", 5, ifelse(r2$Horizon=="15-25 cm", 25, ifelse(r2$Horizon=="35-45 cm", 45, NA))),
    soil_pH = r2$pH_H2O,
    soil_EC = r2$EC25_µs_cm_1, # To convert µS/cm (microsiemens per centimeter) to mS/cm
    soil_SOC = r2$Organic_C_gkg_1_2015,
    soil_clay = r2$`Clay_%`,
    soil_silt = r2$`Silt_%`,
    soil_sand = r2$`Sand_%`,
    soil_bd = r2$Bulk_Density_g_cm_3)
  
  s_2023 <- data.frame(
    date = 2023,
    plot_id = r3$Plot_ID,
    depth_top = ifelse(r3$Horizon=="0 -5cm", 0, ifelse(r3$Horizon=="05-10 cm", 5, ifelse(r3$Horizon=="10 -20 cm", 10, ifelse(r3$Horizon=="20 -30 cm", 20, ifelse(r3$Horizon=="30 -50 cm", 30, NA))))),
    depth_bottom = ifelse(r3$Horizon=="0 -5cm", 5, ifelse(r3$Horizon=="05-10 cm", 10, ifelse(r3$Horizon=="10 -20 cm", 20, ifelse(r3$Horizon=="20 -30 cm", 30, ifelse(r3$Horizon=="30 -50 cm", 50, NA))))),
    soil_pH = r3$pH_H2O,
    soil_SOC = r3$Organic_C_gkg_1_2026,
    soil_clay = r3$`Clay_%`,
    soil_silt = r3$`Silt_%`,
    soil_sand = r3$`Sand_%`,
    soil_bd = r3$Bulk_Density_g_cm_3,   
    #The values below were initially in % and then converted to mg/kg as required by the terminag
    soil_Mn_total	= r3$`Mn_%`*10000, # indicates the percentage of Mn (percentage of total mass) measured by XRF
    soil_Mg_total	= r3$`Mg_%`*10000,
    soil_Mo_total	= r3$`Mo_%`*10000,
    soil_Fe_total	= r3$`Fe_%`*10000,
    soil_Cd_total	= r3$`Cd_%`*10000,
    soil_Pb_total	= r3$`Pb_%`*10000,
    soil_Cr_total	= r3$`Cr_%`*10000,
    soil_Co_total	= r3$`Co _%`*10000,
    soil_K_total	= r3$`K_%`*10000,
    soil_Ni_total	= r3$`Ni_%`*10000,
    soil_Cu_total	= r3$`Cu_%`*10000,
    soil_Zn_total	= r3$`Zn_%`*10000,
    soil_As_total	= r3$`As_%`*10000,
    soil_S_total	= r3$`S_%`*10000,
    soil_P_total	= r3$`P_%`*10000,
    soil_Al_total	= r3$`Al_%`*10000,	#To be added in the terminag
    soil_Ca_total	= r3$`Ca_%`*10000, # To be added in the terminag
    soil_Ti_total	= r3$`Ti_%`*10000, # To be added in the terminag
    soil_Se_total	= r3$`Se_%`*10000, # To be added in the terminag
    soil_Na_total	= r3$`Na_%`*10000 # To be added in the terminag
    )
  
  # Calculate depth
  s_2015$depth <- s_2015$depth_bottom - s_2015$depth_top
  s_2023$depth <- s_2023$depth_bottom - s_2023$depth_top
  
  # Harmonize columns
  soil_vars <- union(names(s_2015), names(s_2023))
  
  s_2015[setdiff(soil_vars, names(s_2015))] <- NA
  s_2023[setdiff(soil_vars, names(s_2023))] <- NA
  
  s_2015 <- s_2015[, soil_vars]
  s_2023 <- s_2023[, soil_vars]
  
  # Combine both soil data
  soil <- rbind(s_2015, s_2023)
 
  # Add soil information to d
  d <- merge(d, soil, by = c("plot_id", "date"), all.x = TRUE, sort = FALSE) # Some rows will be duplicated because different layers of soil were sampled and analyzed for that plot
  
  # Create an unique id
  d$trial_id <- paste(d$plot_id, d$date, d$crop, d$variety, d$seed_treatment, d$depth_top, sep = "_")
  
  carobiner::write_files(path, meta, d)
}