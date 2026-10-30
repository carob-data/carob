carob_script <- function(path) {
  
  "
Dataset for: Effect of  potato late blight in the field in Oxapampa - Pasco

These datasets correspond to the field experiments in Oxapampa - Pasco.
"
  
  uri <- "doi:10.21223/GKZ7EH"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety;fungicide_frequency_",
    response_vars = "disease_severity;ground_cover;dmy_total",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-10-29",
    carob_completion = 85,
    carob_effort = 2
  )
  
  f1 <- ff[basename(ff) == "01_Data_LateBlightSeverity_Oxapampa1.xlsx"]
  f2 <- ff[basename(ff) == "02_LateBlight_Evaluation_date-Oxapampa.xls"]
  f3 <- ff[basename(ff) == "03-DailyWeather-Data-Oxapampa.xlsx"]
  f4 <- ff[basename(ff) == "04_Total_yield_Oxapampa.xls"]
  f5 <- ff[basename(ff) == "05_Evaluation_date_Total biomass dry matter content.xls"]
  f6 <- ff[basename(ff) == "06_Data_Canopy_cover_Oxapampa.xls"]
  f7 <- ff[basename(ff) == "07_canopy_evaluation_days_Oxapampa.xls"]
  
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  r4 <- carobiner::read.excel(f4)
  r5 <- carobiner::read.excel(f5)
  r6 <- carobiner::read.excel(f6)
  r7 <- carobiner::read.excel(f7)
  
  d <- data.frame(
    trial_id = "GKZ7EH_Oxapampa_1999",
    plot_id = paste(r1$Variety, r1$Frequency, r1$Repetition, sep = "_"),
    rep = as.integer(r1$Repetition),
    variety = r1$Variety,
    treatment = paste(r1$Variety, r1$Frequency, sep = "-"),
    location = "Oxapampa",
    adm1 = "Pasco",
    adm2 = "Oxapampa",
    country = "Peru",
    latitude = -10.5775,
    longitude = -75.4022,
    geo_from_source = FALSE,
    emergence_date = "1999-02-05",
    planting_date = NA,
    harvest_date = NA,
    crop = "potato",
    on_farm = NA,
    is_survey = FALSE,
    irrigated = NA,
    yield = NA,
    yield_part = "tubers",
    yield_moisture =NA,
    yield_isfresh = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    ## NEW VARIABLES
    fungicide_frequency_ = r1$Frequency
    
  )
  
  lb <- paste0("LB", 1:25)
  x <- data.frame(plot_id = d$plot_id, r1[, lb])
  dl1 <- reshape(x, varying = lb, v.names = "disease_severity", timevar = "v", times = lb, direction = "long")
  dl1$date <- as.character(as.Date(r2$Evaluation_date[match(dl1$v, r2$Abbreviation)]))
  dl1 <- dl1[, c("plot_id", "date", "disease_severity")]
  dl1$disease_severity <- as.character(dl1$disease_severity)
  dl1$severity_scale <- "0-100"
  dl1$disease <- "potato late blight"
  
  cc <- grep("^CC_G", names(r6), value = TRUE)
  y <- data.frame(plot_id = paste(r6$Variety, r6$Frequency, r6$Repetition, sep = "_"), r6[, cc], check.names = FALSE)
  dl2 <- reshape(y, varying = cc, v.names = "ground_cover", timevar = "v", times = cc, direction = "long")
  dl2$date <- as.character(as.Date(r7$Evaluation_date_CCG[match(dl2$v, r7$Abbreviation)]))
  dl2 <- dl2[!is.na(dl2$ground_cover), c("plot_id", "date", "ground_cover")]
  
  dl3 <- data.frame(
    plot_id = paste(r4$Variety, r4$Frequency, r4$Repetition, sep = "_"),
    date = as.character(as.Date(r5$Date[match(r4$`Date-evaluation`, r5$Variable_name)])),
    dmy_total = r4$TBDMt_ha * 1000
  )
  dl3 <- dl3[!is.na(dl3$dmy_total), ]
  
  dlong <- carobiner::bindr(dl1, dl2, dl3)
  
  w <- data.frame(
    date = as.character(as.Date(r3$Date)),
    prec = r3$Prec,
    tmin = r3$Tmin,
    tmax = r3$Tmax,
    temp = r3$Tavg,
    rhmx = r3$Rhmax,
    rhmn = r3$Rhmin,
    rhum = r3$Rhavg,
    ETP = r3$ETP
  )
  w$location <- "Oxapampa"
  w$country <- "Peru"
  w$latitude <- -10.5775
  w$longitude <- -75.4022
  
  carobiner::write_files(path, meta, d, long = dlong, wth = w)
}