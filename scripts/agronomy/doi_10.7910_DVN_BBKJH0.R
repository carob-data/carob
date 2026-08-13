# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
"
This dataset provides a detailed, plot-level record of rice agronomic practices collected through the validation trial of the RiceAdvice Lite decision-support tool across diverse countries. It captures the full production cycle from field preparation to harvest, including land clearing, tillage intensity, planting density, crop establishment method, and exact sowing and transplanting dates. The dataset comprehensively documents fertilizer management practices such as the type of NPK fertilizer applied, application rates of nitrogen (N), phosphorus (P₂O₅), and potassium (K₂O), and total fertilizer investment cost per hectare alongside crop performance indicators including grain yield at 14% moisture content and nutrient-use efficiency metrics (NUE, PUE, KUE). It also includes spatially referenced production system information (latitude, longitude), variety used, and experimental treatments. Finally, the dataset quantifies economic data such as gross revenue and return on fertilizer investment, enabling rigorous evaluation of profitability and agronomic effectiveness. (2025-10-25)
"
  
  uri <- "doi:10.7910/DVN/BBKJH0"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
   data_organization = "AfricaRice",
   publication = NA,
   project = "EiA",
   data_type = "on-farm experiment",
   treatment_vars = "herbicide_used; variety; planting_method; N_fertilizer;P_fertilizer;K_fertilizer",
   response_vars = "yield", 
   carob_completion = 100,
   carob_contributor = "Blessing Dzuda; Kora Simperegui",
   carob_effort = NA,
   carob_date = "2025-05-30",
   notes = NA, 
   design = NA
  )
  
  f <- ff[basename(ff) == "data.xls"]
  r <- carobiner::read.excel(f, na= c("n/a", "no data"))

    d <- data.frame(
      hhid= as.character(r$HHID),
      country= r$country_name,
      adm1= r$first_level_administrative_unit,
      longitude = round(as.numeric(gsub(",", ".", r$longitude)), 4),
      latitude = round(as.numeric(gsub(",", ".", r$latitude)), 4),
      geo_from_source = TRUE,
      season= r$season,
      variety= r$variety_used,
      rep=as.integer(r$replicate),
      treatment= r$experimental_treatment_name,
      planting_method= tolower(r$planting_method),
      planting_date= as.character(r$sowing_date),
      transplanting_date= as.character(r$transplanting_date),
      harvest_date= as.character(r$harvest_date),
      land_prep_method=as.character(r$number_tillage),
      N_fertilizer= r$n_applied_kg_ha,
      P_fertilizer= r$p_applied_kg_ha,
      K_fertilizer= r$k_applied_kg_ha,
      yield = r$`yield_at_14%_moisture_content_kg_ha`,
      herbicide_used = grepl("herbicide", r$land_preparation_clearing),
      weeding_times = as.integer(r$number_weeding),
      irrigated = grepl("irrigated", tolower(r$production_system)),
      fertilizer_cost = r$total_fertilizer_cost_usd_ha,
      trial_id = paste(r$use_case_name, r$activity_name, r$activity_type, sep = "-")
    ) 
   
    # For the land preparation, the number of tillage was used as a proxy. 0 for "None", NA for "Unknown" and >0 for "Tillage"
    d$land_prep_method[d$land_prep_method == "0"] <- "none"
    d$land_prep_method[is.na(d$land_prep_method)] <- "unknown"
    d$land_prep_method[!d$land_prep_method %in% c("none", "unknown")] <- "tillage"
    
    
    d$fertilizer_used <- !(d$K_fertilizer == 0 & d$N_fertilizer == 0 & d$P_fertilizer == 0) #No application of fertilizer for experiments with K_fertilizer = N_fertilizer = P_fertilizer == 0
    d$fertilizer_type <- ifelse(d$fertilizer_used, "NPK;urea", "none") # In some experiments, no fertilizer was applied. For such, fertilizer_type is set to "none"

    
    d$season <- ifelse(d$season=="dry season","dry","wet")
    d$planting_method <- ifelse(d$planting_method=="direct","direct seeding","transplanted")
    d$country <- gsub("Cote d'Ivoire","Côte d'Ivoire",d$country)
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$yield_isfresh <- TRUE
    d$crop <- "rice"
    d$yield_part <- "grain"
    d$yield_moisture <- 14
    
  carobiner::write_files(path, meta, d)
}