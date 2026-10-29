
# ============================================================
# CAROB SCRIPT
# Dataset: On-farm validation of RiceAdvice in West Africa
# Repository: Harvard Dataverse (doi:10.7910/DVN/4CFDEG)
# License: GPL (>=3)
# ============================================================

carob_script <- function(path) { 
  
  # ==========================================================
  # Dataset information
  # ==========================================================
  
  uri <- "doi:10.7910/DVN/4CFDEG"
  group <- "agronomy"
  
  # ==========================================================
  # Creating repository folders
  # ==========================================================
  
  #path <- "C:/Users/PKouame/Documents/data_standardization/carob"
  ff  <- carobiner::get_data(uri, path, group)

  # ==========================================================
  # Metadata
  # ==========================================================
  
    meta <- carobiner::get_metadata(
        uri = uri,
        path = path,
        group = group,
        major = 1,
        minor = 0,
        data_organization = "AfricaRice",
        publication = NA,#"doi:10.1016/S0378-4290(03)00153-9; doi:10.1016/S0378-4290(02)00117-X",
        project = "WARDA LTE",
        carob_effort = 8,
        carob_completion = 100,
        carob_date = "2026-10-05",
        data_type = "experiment",
        treatment_vars = "variety;N_fertilizer;P_fertilizer;K_fertilizer",	
        response_vars = "yield",
        #is_survey = FALSE,
        carob_contributor = "Perpetue Kouame;Kora Simperegui"
        )
  
 
  
  # ==========================================================
  # Dataset calling
  # ==========================================================  
  
  file1 <- ff[basename(ff) == "AfricaRice Long Term Trial - Fanaye (dry season).xlsx"]
  file2 <- ff[basename(ff) == "AfricaRice Long Term Trial - Fanaye (wet season).xlsx"]
  file3 <- ff[basename(ff) == "AfricaRice Long Term Trial - Ndiaye (dry season).xlsx"]
  file4 <- ff[basename(ff) == "AfricaRice Long Term Trial - Ndiaye (wet season).xlsx"]
 
  
  runf <- function(f) {
    r <- carobiner::read.excel(f)
    data.frame(
      crop = "rice",
      date = as.character(r[["Year"]]),
      country = r[["Country"]],
      location = r[["Site"]],
      season = tolower(r[["Season"]]),
      treatment = r[["Treatment"]],
      variety = r[["Variety"]],
      fertilizer_type = "",
      planting_date = as.Date(r[["Sowing date"]]),
      transplanting_date = as.character(r[["Transplanting date"]]),
      flowering_date = r[["FLWR"]],
      maturity_date = r[["MATURITY"]],
      grain_fill = r[["FIL"]],		
      yield = r[["YIELD"]],
      on_farm = FALSE,
      geo_from_source = FALSE,
      yield_moisture = 14
    )
  }
  
  d <- do.call(rbind, lapply(c(file1, file2, file3, file4), runf))
  
  d$flowering_date <- as.character(d$planting_date + d$flowering_date)
  d$maturity_date <- as.character(d$planting_date + d$maturity_date)
  d$planting_date <- as.character(d$planting_date)
  
  d$trial_id <- paste0(d$location, d$year, d$season, sep="_")
  
  
  # ==========================================================
  # calling file Dictionnary
  # ==========================================================  
  
  
  npk <- data.frame(rbind(c(0,0,0), c(60,26,50), c(120,0,0), c(120,26,50), c(120,52,100), c(180,26,50)))
  colnames(npk) <- c("N_fertilizer", "P_fertilizer", "K_fertilizer")
  npk$treatment <- paste0("T", 1:6)
  
  d <- merge(d, npk, by="treatment")
  
  # ==========================================================
  # Given Treatment
  # ========================================================== 
 
  d$fertilizer_type[d$treatment == "T1"] <- "none"
  d$fertilizer_type[d$treatment == "T4"] <- "urea"
  d$fertilizer_type[d$treatment %in% c("T2", "T3", "T5", "T6")] <- "NPK"


  
  d$fertilizer_used <- !(d$N_fertilizer == 0 & d$P_fertilizer == 0 & d$K_fertilizer == 0) #No application of fertilizer for experiments with K_fertilizer = N_fertilizer = P_fertilizer == 0
  

  
  
  # ==========================================================
  # Export
  # ==========================================================
  
  carobiner::write_files(path, meta, d)
}

