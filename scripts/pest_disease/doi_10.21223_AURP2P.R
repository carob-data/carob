## ISSUES
# 3 varieties in the data (description says 2); Tomasa only with N160
# No tuber yield, only total biomass dry matter (already kg/ha)
# Location from the title; coordinates from carobiner::geocode
# Weather file has 2 header rows and a "Total" row; only rows with a date are used

## NEW VARIABLES
# fungicide_level_: "Fungicide_rate" in the source (F0, F4, F10), not explained; the spray file has sprays every 7 and 14 days but does not link them to the levels
carob_script <- function(path) {
  
  "
Dataset for: Effect of nitrogen fertilization on potato late blight in the field in Huancayo, Junin Peru

The effect of nitrogen (N) fertilization on the development of potato late blight disease, caused by Phytophthora infestans (Mont.) de Bary, was studied through field experiments in two locations in Peru (Huancayo and Oxapampa). These datasets correspond to the field experiments in Huancayo. Three levels of N (0, 160, and 320 kg/ha) and three levels of fungicide were applied in all combinations on two potato (Solanum tuberosum L.) varieties with different levels of resistance (18 treatments), and in four repetitions. N had no effect on the disease when severity was very low or high. At intermediate levels of disease severity, increased N led to increased disease. In Huancayo, the effect of 0 vs. 160 kg·ha-1 of N on  the more susceptible variety was negligible because of increased late blight.  For the more resistant variety, late blight had little effect on yield when N was applied at 0 or 160 kg·ha-1. Increasing the N fertilization rate to 320 kg·ha-1 did not further increase yield in the absence of late blight, but decreased it sharply in the treatments where the disease was present because no fungicides were applied. These results indicate the importance of considering N fertilization levels in integrated management schemes for potato late blight, and in comparison of results across experiments. If farmers want to increase potato yields with N fertilizer, they may need to use more fungicides or varieties with higher resistance.
"
  
  uri <- "doi:10.21223/AURP2P"
  group <- "pest_disease"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety;N_fertilizer;fungicide_level_",
    response_vars = "disease_severity;ground_cover;dmy_total",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-11-02",
    carob_completion = 85,
    carob_effort = 2
)
  
  f1 <- ff[basename(ff) == "01_data_LateBlightSeverity_Huancayo.xls"]
  f2 <- ff[basename(ff) == "02_LateBlight_Evaluation_date.xls"]
  f3 <- ff[basename(ff) == "03_Data_Canopy_cover_Huancayo.xls"]
  f4 <- ff[basename(ff) == "04_canopy_evaluation_days-1.xls"]
  f5 <- ff[basename(ff) == "05_TotalYield_Huancayo.xls"]
  f6 <- ff[basename(ff) == "06_Evaluation_date_Total biomass dry matter content.xls"]
  f8 <- ff[basename(ff) == "DailyWeatherData_Huancayo.xls"]
  
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  r4 <- carobiner::read.excel(f4)
  r5 <- carobiner::read.excel(f5)
  r6 <- carobiner::read.excel(f6)
  r8 <- carobiner::read.excel(f8)
  
  d <- data.frame(
    trial_id = "AURP2P_Huancayo_2000",
    plot_id = as.character(r1$id),
    rep = as.integer(r1$Repetition),
    variety = r1$Variety,
    treatment = as.character(r1$Treatment),
    N_fertilizer = as.numeric(gsub("N", "", r1$Nitrogen_rate)),
    P_fertilizer = NA,
    K_fertilizer = NA,
    country = "Peru",
    adm1 = "Junin",
    location = "Huancayo",
    latitude = -12.1722,
    longitude = -75.1608,
    geo_from_source = FALSE,
    emergence_date = "2000-01-02",
    planting_date = NA,
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = NA,
    yield_moisture = NA,
    yield_isfresh = NA,
    ## NEW VARIABLES
    fungicide_level_ = r1$Fungicide_rate
    
  )
  
  bm <- grep("^TBDMC", names(r5), value = TRUE)
  z <- data.frame(plot_id = as.character(r5$id), r5[, bm])
  dl3 <- reshape(z, varying = bm, v.names = "dmy_total", timevar = "harvest_date",
                 times = as.character(as.Date(r6$Evaluation_date_TBDMC)), direction = "long")
  dl3 <- dl3[!is.na(dl3$dmy_total), c("plot_id", "harvest_date", "dmy_total")]
  
  d <- merge(dl3, d, by = "plot_id", all.x = TRUE)
  d$record_id <- seq_len(nrow(d))
  
  lb <- grep("^LB", names(r1), value = TRUE)
  x <- data.frame(plot_id = as.character(r1$id), r1[, lb])
  dl1 <- reshape(x, varying = lb, v.names = "disease_severity", timevar = "v", times = lb, direction = "long")
  dl1$date <- as.character(as.Date(r2$Evaluation_date[match(dl1$v, r2$Abbreviation)]))
  dl1 <- dl1[!is.na(dl1$disease_severity), c("plot_id", "date", "disease_severity")]
  dl1$disease_severity <- as.character(dl1$disease_severity)
  dl1$severity_scale <- "0-100"
  dl1$disease <- "potato late blight"
  
  cc <- grep("^CC_G", names(r3), value = TRUE)
  y <- data.frame(plot_id = as.character(r3$id), r3[, cc], check.names = FALSE)
  dl2 <- reshape(y, varying = cc, v.names = "ground_cover", timevar = "v", times = cc, direction = "long")
  dl2$date <- as.character(as.Date(r4$Evaluation_date_CCG[match(dl2$v, r4$Abbreviation)]))
  dl2 <- dl2[!is.na(dl2$ground_cover), c("plot_id", "date", "ground_cover")]
  
  dlong <- carobiner::bindr(dl1, dl2)
  
  w <- r8[grepl("^2000", r8[[2]]), ]
  w <- data.frame(
    date = as.character(as.Date(w[[2]], "%Y%m%d")),
    tmin = as.numeric(w[[3]]),
    tmax = as.numeric(w[[4]]),
    temp = as.numeric(w[[5]]),
    rhum = as.numeric(w[[6]]),
    prec = as.numeric(w[[10]])
  )
  w$location <- "Huancayo"
  w$country <- "Peru"
  w$latitude <- -12.1722
  w$longitude <- -75.1608
  
  carobiner::write_files(path, meta, d, long = dlong, wth = w)
}
