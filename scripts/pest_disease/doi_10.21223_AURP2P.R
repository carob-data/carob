# R script for "carob"
# license: GPL (>=3)

## NOTES
# Huancayo field experiment (1999/2000 season) on nitrogen x fungicide x variety and potato late blight.
# Publication: Juarez HS, Amaro JR, Rivera MD, Parraga A, Hijmans RJ. The effect of nitrogen fertilization
#   on potato late blight in the field. CIP Program Report 1999-2000, pp. 69-72. 
# "the paper" below refers to this publication.
# Design (paper): 2 varieties x 3 N rates x 3 fungicide levels, 4 replicates; RCBD.
# Varieties: the paper reports Amarilis (moderately resistant) and Yungay (moderately susceptible), 36 plots each.
#   The data also have Tomasa (12 plots), only at N160 with all 3 fungicide levels. Tomasa is not in the paper.
#   The Tomasa rows are kept.
# Fungicide: the data file uses codes F0, F4 and F10. In the paper these are the Oxapampa codes; Huancayo had
#   F- (none), F7 (every 7 days) and F14 (every 14 days). Late blight severity in the data (LB26, all plots):
#   F0 >> F10 > F4. So F4 = sprayed every 7 days and F10 = every 14 days. Stored as fungicide_interval.
# Spray dates are from 07_SpraysOfFungicides_Huancayo.xls (columns "7" and "14"): the 7-day treatment had
#   17 sprays (2000-01-07 to 2000-04-28) and the 14-day treatment 5 sprays (2000-01-07 to 2000-03-03).
#   The 14-day sprays stopped early in March; reason not stated. F0 had 0 sprays.
#   fungicide_times (0, 5, 17) is used as the fungicide treatment variable.
# Late blight severity is in % of foliage infected (0-100; values 0-100 in steps of 5), not a fraction as in the paper.
# LB1-LB29 are 29 evaluations (2000-01-11 to 2000-05-30). LB27-LB29 were only scored on N320 plots
#   (all varieties); other plots have no score after LB26 (2000-05-15). Reason not stated; not filled in.
# Emergence is 2000-01-02 (Emergency_date in 02_LateBlight_Evaluation_date.xls).
# TBDMC_kg_ha is total biomass dry matter in kg/ha at five dates (dmy_total, long table). It is not fresh tuber yield.
#   The paper (Table 1) gives tuber dry matter yield only as treatment means, so yield is not included.

## ISSUES
# The paper gives longitude as 75 14' "E"; Peru is west of Greenwich, so it is stored as negative (W).
# P and K: the paper says "phosphorus at 62 kg/ha" and "potassium at 100 kg/ha". Taken as elemental P and K.
#   They could be P2O5 and K2O; to check with the authors.
# P source was Ca(H2PO4)2 (monocalcium phosphate). Mapped to fertilizer_type "TSP" (assumption).
# Irrigation not stated in the paper (irrigated = NA).
# Whether infection was natural or inoculated is not stated (inoculated = NA).
# write_files: missing variables yield, yield_moisture, yield_part. The data have no plot-level tuber yield
#   (05_TotalYield_Huancayo.xls only has total biomass dry matter, TBDMC_kg_ha_Ev1-5). The paper gives tuber
#   dry matter yield only as treatment means (Table 1), so it is not used.
# Daily weather file has 2 header rows and a "Total" row; only rows with a date are used.

## NOT PROCESSED
# HourlyWeatherData_Huancayo.xls: hourly weather (daily weather is processed).
# Ontology id columns in the date files.

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
    publication = "hdl:10568/109461",  # CIP Program Report 1999-2000, pp. 69-72
    project = NA,
    design = "RCBD",  # paper: factorial complete block design, 4 replicates
    data_type = "experiment",
    treatment_vars = "variety;N_fertilizer;fungicide_times",
    response_vars = "disease_severity;ground_cover;dmy_total",
    notes = "Fungicide codes F4 and F10 in the data correspond to spray intervals of 7 and 14 days (see script). Tomasa is not in the publication.",
    carob_contributor = "Maryam Yahya;Bester Mudereri",
    carob_date = "2026-11-02",
    carob_LLM = "Claude Fable 5",
    carob_completion = 90,
    carob_effort = 2
)
  
  f1 <- ff[basename(ff) == "01_data_LateBlightSeverity_Huancayo.xls"]
  f2 <- ff[basename(ff) == "02_LateBlight_Evaluation_date.xls"]
  f3 <- ff[basename(ff) == "03_Data_Canopy_cover_Huancayo.xls"]
  f4 <- ff[basename(ff) == "04_canopy_evaluation_days-1.xls"]
  f5 <- ff[basename(ff) == "05_TotalYield_Huancayo.xls"]
  f6 <- ff[basename(ff) == "06_Evaluation_date_Total biomass dry matter content.xls"]
  f7 <- ff[basename(ff) == "07_SpraysOfFungicides_Huancayo.xls"]
  f8 <- ff[basename(ff) == "DailyWeatherData_Huancayo.xls"]
  
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  r4 <- carobiner::read.excel(f4)
  r5 <- carobiner::read.excel(f5)
  r6 <- carobiner::read.excel(f6)
  r8 <- carobiner::read.excel(f8)

  # fungicide: file codes -> spray interval in days (see NOTES)
  fcode <- trimws(as.character(r1$Fungicide_rate))
  # spray dates from 07_SpraysOfFungicides_Huancayo.xls
  # Excel rows 5-21 hold the dates; column B = 7-day treatment, column D = 14-day treatment
  s <- carobiner::read.excel(f7, col_names=FALSE, range="A5:E21")
  sp7 <- as.character(as.Date(na.omit(s[[2]])))
  sp14 <- as.character(as.Date(na.omit(s[[4]])))
  # check that the expected block was read (17 and 5 sprays)
  stopifnot(length(sp7) == 17, length(sp14) == 5)
  finterval <- c(F0 = NA, F4 = 7L, F10 = 14L)
  fdates <- c(F0 = NA, F4 = paste(sp7, collapse=";"), F10 = paste(sp14, collapse=";"))
  ftimes <- c(F0 = 0L, F4 = length(sp7), F10 = length(sp14))

  nrate <- as.numeric(gsub("N", "", r1$Nitrogen_rate))

  d <- data.frame(
    trial_id = "AURP2P_Huancayo_2000",
    plot_id = as.character(r1$id),
    rep = as.integer(r1$Repetition),
    variety = r1$Variety,
    treatment = as.character(r1$Treatment),
    N_fertilizer = nrate,  # as NH4NO3, banded (paper p. 70-71)
    N_splits = ifelse(nrate > 0, 2L, 0L),  # half at planting, half 32 days after emergence
    P_fertilizer = 62,  # paper: as Ca(H2PO4)2, banded at planting (see ISSUES)
    K_fertilizer = 100,  # paper: as KCl, banded at planting (see ISSUES)
    fertilizer_type = ifelse(nrate > 0, "AN;TSP;KCl", "TSP;KCl"),
    # fungicide from the paper (p. 70): chlorothalonil (Bravo 500), backpack sprayer, from emergence
    fungicide_used = fcode != "F0",
    fungicide_product = ifelse(fcode == "F0", NA, "chlorothalonil"),
    fungicide_interval = unname(finterval[fcode]),
    fungicide_times = unname(ftimes[fcode]),
    fungicide_dates = unname(fdates[fcode]),
    country = "Peru",
    adm1 = "Junin",
    location = "Huancayo",
    # coordinates and elevation from the paper (p. 70): 12 01'45" S, 75 14'0" (paper says E; corrected to W), 3300 m
    latitude = -12.0292,
    longitude = -75.2333,
    elevation = 3300,
    geo_from_source = TRUE,  # from the authors' publication
    # dates from the paper (p. 70); emergence from the dataset
    planting_date = "1999-12-17",
    emergence_date = "2000-01-02",
    harvest_date = "2000-06-02",  # paper: last harvest (the last biomass evaluation in the data is 2000-06-03)
    crop = "potato",
    pathogen = "Phytophthora infestans",
    disease = "potato late blight",
    stress = "disease",
    # plot layout from the paper (p. 70): 4 rows x 14 plants, rows 1 m apart, plants 0.3 m apart
    plot_area = 16.8,  # m2
    row_spacing = 100,  # cm
    plant_spacing = 30,  # cm
    plant_density = 10000 / (1 * 0.3),  # plants/ha
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    inoculated = NA
  )
  
  ## long table: late blight severity (29 dates), canopy cover (21 dates), biomass dry matter (5 dates)
  lb <- grep("^LB", names(r1), value = TRUE)
  x <- data.frame(plot_id = as.character(r1$id), r1[, lb])
  dl1 <- reshape(x, varying = lb, v.names = "disease_severity", timevar = "v", times = lb, direction = "long")
  dl1$date <- as.character(as.Date(r2$Evaluation_date[match(dl1$v, r2$Abbreviation)]))
  dl1 <- dl1[!is.na(dl1$disease_severity), c("plot_id", "date", "disease_severity")]
  dl1$disease_severity <- as.character(dl1$disease_severity)
  dl1$severity_scale <- "0-100"  # % of foliage infected
  
  cc <- grep("^CC_G", names(r3), value = TRUE)
  y <- data.frame(plot_id = as.character(r3$id), r3[, cc], check.names = FALSE)
  dl2 <- reshape(y, varying = cc, v.names = "ground_cover", timevar = "v", times = cc, direction = "long")
  dl2$date <- as.character(as.Date(r4$Evaluation_date_CCG[match(dl2$v, r4$Abbreviation)]))
  dl2 <- dl2[!is.na(dl2$ground_cover), c("plot_id", "date", "ground_cover")]
  
  bm <- grep("^TBDMC", names(r5), value = TRUE)
  z <- data.frame(plot_id = as.character(r5$id), r5[, bm])
  dl3 <- reshape(z, varying = bm, v.names = "dmy_total", timevar = "v", times = bm, direction = "long")
  # r6 labels the evaluations "TBDMC_Cp_gplant_1..5" while the data columns are "TBDMC_kg_ha_Ev1..5";
  # match on the trailing evaluation number
  evnr <- function(x) as.integer(gsub("\\D", "", sub("^TBDMC", "", x)))
  dl3$date <- as.character(as.Date(r6$Evaluation_date_TBDMC[match(evnr(dl3$v), evnr(r6$Abbreviation))]))
  dl3 <- dl3[!is.na(dl3$dmy_total), c("plot_id", "date", "dmy_total")]
  
  dlong <- carobiner::bindr(dl1, dl2, dl3)
  
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
  w$latitude <- -12.0292
  w$longitude <- -75.2333
  
  carobiner::write_files(path, meta, d, long = dlong, wth = w)
}
