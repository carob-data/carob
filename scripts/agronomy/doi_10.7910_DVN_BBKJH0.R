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
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
   data_organization = "AfricaRice",
   publication = NA,
   project = "EiA",
   data_type = "on-farm experiment",
   treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
   response_vars = "yield", 
   carob_completion = 100,
   carob_contributor = "Blessing Dzuda",
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
      N_fertilizer= r$n_applied_kg_ha,
      P_fertilizer= r$p_applied_kg_ha,
      K_fertilizer= r$k_applied_kg_ha,
      yield = r$`yield_at_14%_moisture_content_kg_ha`,
      herbicide_used = grepl("herbicide", r$land_preparation_clearing),
      weeding_times = as.integer(r$number_weeding),
      irrigated = grepl("irrigated", tolower(r$production_system)),
      fertilizer_cost = r$total_fertilizer_cost_usd_ha,
      fertilizer_used = !grepl("No fertilizer", r$type_npk_fertilizer_used),
      fertilizer_type ="NPK;urea",
      trial_id = paste(r$use_case_name, r$activity_name, r$activity_type, sep = "-")
    ) 
   
    
    #lat_lon data
    
    geo <- data.frame(
      country = c("Burkina Faso", "Mali", rep("Sierra Leone", 2), "Ghana", "Nigeria"),
      adm1 = c("Bama", "Sikasso", "North West", "Southern", "Northern", "Nasarawa"),
      lon = c(-4.4129, -5.667, -12.520, -11.753, -0.181, 7.7115),
      lat = c(11.375, 11.315, 9.367, 8.564,9.549 , 8.547),
      geo_from = FALSE
    )
  
    d <- merge(d, geo, by= c("adm1", "country"), all.x = TRUE)
    
    d$longitude[!is.na(d$lon)] <- d$lon[!is.na(d$lon)]
    d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
    d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
    
    d$lat <- d$lon <- d$geo_from <- NULL
    
    
    
    d$season <- ifelse(d$season=="dry season","dry","wet")
    d$planting_method <- ifelse(d$planting_method=="direct","direct seeding","transplanted")
    d$country <- gsub("Cote d'Ivoire","Côte d'Ivoire",d$country)
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$yield_isfresh <- TRUE
    d$crop <- "rice"
    d$yield_part <- "grain"
    d$yield_moisture <- 14
    
    ### Set ambiguous harvest dates to NA (10 rows)
    d$harvest_date[(as.Date(d$harvest_date) -as.Date(d$planting_date))< 40] <-  NA
    
  carobiner::write_files(path, meta, d)
}
