# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
  "This dataset provides a detailed, plot-level record of rice agronomic practices collected through the validation trial of the RiceAdvice Lite decision-support tool across diverse countries. It captures the full production cycle from field preparation to harvest, including land clearing, tillage intensity, planting density, crop establishment method, and exact sowing and transplanting dates. The dataset comprehensively documents fertilizer management practices such as the type of NPK fertilizer applied, application rates of nitrogen (N), phosphorus (P₂O₅), and potassium (K₂O), and total fertilizer investment cost per hectare alongside crop performance indicators including grain yield at 14% moisture content and nutrient-use efficiency metrics (NUE, PUE, KUE). It also includes spatially referenced production system information (latitude, longitude), variety used, and experimental treatments. Finally, the dataset quantifies economic data such as gross revenue and return on fertilizer investment, enabling rigorous evaluation of profitability and agronomic effectiveness. (2025-10-25)"
  
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
   completion = 100,
   carob_contributor = "Blessing Dzuda",
   carob_date = "2025-05-30",
   notes = NA, 
   design = NA
  )
  
  f <- ff[basename(ff) == "data.xls"]
  r <- carobiner::read.excel(f)

    d <- data.frame(
      hhid=as.character(r$HHID),
      country=r$country_name,
      adm1=r$first_level_administrative_unit,
      season=r$season,
      variety=r$variety_used,
      rep=as.integer(r$replicate),
      treatment=r$experimental_treatment_name,
      planting_method=tolower(r$planting_method),
      planting_date=r$sowing_date,
      transplanting_date=r$transplanting_date,
      harvest_date=r$harvest_date,
      N_fertilizer=r$n_applied_kg_ha,
      P_fertilizer=r$p_applied_kg_ha,
      K_fertilizer=r$k_applied_kg_ha,
      yield=r$`yield_at_14%_moisture_content_kg_ha`
    ) 
    
    #planting date
    d$planting_date[d$planting_date %in% c("n/a", "no data")] <- NA
    d$planting_date <- as.Date(as.numeric(d$planting_date), origin = "1899-12-30")
    d$planting_date <- as.character(d$planting_date)
    
    #transplanting date
    d$transplanting_date[d$transplanting_date %in% c("n/a", "no data")] <- NA
    d$transplanting_date <- as.Date(as.numeric(d$transplanting_date), origin = "1899-12-30")
    d$transplanting_date <- as.character(d$transplanting_date)
    
    #harvesting date
    d$harvest_date[d$harvest_date %in% c("n/a", "no data")] <- NA
    d$harvest_date <- as.Date(as.numeric(d$harvest_date), origin = "1899-12-30")
    d$harvest_date <- as.character(d$harvest_date)
   
    #lat_lon data
    loc <- data.frame(
      adm1 = c("Jigawa", "Kano", "Kebbi", "Nasarawa", 
               "Bama", "Koulikoro", "Sikasso", "Anambra", "Yamoussoukro", 
               "North West", "Southern", "Northern", "Savannah"),
      longitude=c(8.9879, 8.3791, 4.2687, 7.7503, -4.4294, -7.3654, -5.6778, 6.893,
                  -5.2776, -13.2015, -12.178, -0.8419, -0.2663),
      latitude=c(12.6515, 11.7554, 12.4729, 8.3301, 11.3996, 13.2359, 11.3166, 6.3147,
                 6.82, 8.6157, 7.601, 9.4287, 9.1323))
    
    d <- merge(d,loc,by="adm1", all.x = TRUE)
    
    d$season <- ifelse(d$season=="dry season","dry","wet")
    d$planting_method <- ifelse(d$planting_method=="direct","direct seeding","transplanted")
    d$country <- gsub("Cote d'Ivoire","Côte d'Ivoire",d$country)
    d$trial_id <- paste(d$adm1,d$planting_date, sep = "_")
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <- TRUE
    d$geo_from_source <- FALSE
    d$yield_isfresh <- TRUE
    d$crop <- "rice"
    d$yield_part <- "grain"
    d$yield_moisture <- 14
    
  carobiner::write_files(path, meta, d)
}