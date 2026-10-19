
carob_script <- function(path) {
  
  "
Dataset for: Trait Observation Network (TON) Panel, clones under evaluation for late blight resistance

The assessment of late blight resistance in TON panel genotypes was conducted in experimental trials under field conditions with high endemic disease pressure. The percentage of leaf area affected by late blight infection was recorded by plot throughout the season to subsequently compute the area under the disease progress curve (AUDPC). Since AUDPC is subject to variation from factors that are experiment specific it cannot be used to compare values accross trials. AUDPC values were used to compute the relative AUDPC (rAUDPC) and predicted susceptibility scale value (sAUDPC), which are comparable accross trials
"
  
  uri <- "doi:10.21223/P3/JJJQV0"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 3,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "AUDPC;rAUDPC;disease_severity;yield;yield_marketable",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_LLM = "Claude Opus 4.5",
    carob_date = "2026-10-16",
    carob_completion = 75,
    carob_effort = 3.0
   )

  f1 <- ff[basename(ff) == "PTLate_Blight032016_KUNMING.xlsx"]
  f2 <- ff[basename(ff) == "PTLate_Blight042015_KUNMING.xlsx"]
  f3 <- ff[basename(ff) == "PTLate_blight062016_HOLE.xlsx"]
  f4 <- ff[basename(ff) == "PTLate_blight062017_HOLE.xlsx"]
  
  r1 <- carobiner::read.excel(f1, sheet = "Fieldbook")
  r2 <- carobiner::read.excel(f2, sheet = "Fieldbook")
  r3 <- carobiner::read.excel(f3, sheet = "Fieldbook")
  r4 <- carobiner::read.excel(f4, sheet = "Fieldbook")
  
  ## Add trial_id
  r1$trial_id <- "Kunming_2016"
  r2$trial_id <- "Kunming_2015"
  r3$trial_id <- "Holetta_2016"
  r4$trial_id <- "Holetta_2017"
  
  r1$TTYA <- NA; r1$MTYA <- NA
  r2$TTYA <- NA; r2$MTYA <- NA
  
  r3$LB5 <- NA; r3$LB6 <- NA; r3$LB7 <- NA; r3$LB8 <- NA
  r4$LB6 <- NA; r4$LB7 <- NA; r4$LB8 <- NA
  
  r3$SAUDPC <- NA
  r4$SAUDPC <- NA
  
  r <- carobiner::bindr(r1, r2, r3, r4)
  r$record_id <- seq_len(nrow(r))
  
  geo <- data.frame(
    trial_id = c("Kunming_2016", "Kunming_2015", "Holetta_2016", "Holetta_2017"),
    country = c("China", "China", "Ethiopia", "Ethiopia"),
    adm1 = c("Yunnan", "Yunnan", "Oromia", "Oromia"),
    location = c("Kunming", "Kunming", "Holetta", "Holetta"),
    latitude = c(25.03886, 25.03886, 8.88333, 8.88333),
    longitude = c(102.72257, 102.72257, 38.38333, 38.38333),
    elevation = c(1897, 1897, 2400, 2400),
    geo_from_source = TRUE,
    planting_date = c("2016-03-01", "2015-04-01", "2016-06-27", "2017-06-23"),
    harvest_date = c("2016-10-15", "2015-08-20", "2016-11-01", "2017-10-18")
  )
  
  d <- data.frame(
    record_id = r$record_id,
    trial_id = r$trial_id,
    plot_id = as.character(r$PLOT),
    rep = as.integer(r$REP),
    variety = r$INSTN,
    crop = "potato",
    on_farm = NA,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = r$TTYA * 1000,
    yield_marketable = r$MTYA * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    AUDPC = r$AUDPC / 100,
    rAUDPC = r$rAUDPC,
    sAUDPC = r$SAUDPC,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  
  d <- merge(d, geo, by = "trial_id", all.x = TRUE)
  
  lb_cols <- c("LB1", "LB2", "LB3", "LB4", "LB5", "LB6", "LB7", "LB8")
  lon <- r[, c("record_id", "trial_id", lb_cols)]
  
  d_long <- reshape(lon, varying = lb_cols, v.names = "disease_severity",
                    timevar = "order", times = 1:8, direction = "long")
  d_long <- d_long[!is.na(d_long$disease_severity), ]
  
  days <- data.frame(
    trial_id = rep(c("Kunming_2016", "Kunming_2015", "Holetta_2016", "Holetta_2017"),
                   c(8, 8, 4, 5)),
    order = c(1:8, 1:8, 1:4, 1:5),
    DAP = c(128, 136, 142, 149, 156, 163, 170, 179,
            97, 107, 114, 121, 128, 135, 142, 149,
            45, 52, 58, 67,
            41, 48, 55, 63, 69)
  )
  d_long <- merge(d_long, days, by = c("trial_id", "order"))
  
  d_long$id <- NULL
  d_long$trial_id <- NULL
  d_long$DAP <- as.integer(d_long$DAP)
  d_long$disease_severity <- d_long$disease_severity
  d_long$severity_scale <- "0-100"
  d_long$disease <- "potato late blight"
  
  d_long <- merge(d_long, d[, c("record_id", "planting_date")], by = "record_id")
  d_long$date <- as.character(as.Date(d_long$planting_date) + d_long$DAP)
  d_long$planting_date <- NULL
  
  carobiner::write_files(path, meta, d, long = d_long)
}
