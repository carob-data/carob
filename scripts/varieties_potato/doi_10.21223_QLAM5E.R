carob_script <- function(path) {
  
  "
Replication data for: A case study of potato germplasm enhancement using distant late blight resistant wild relatives 2

'Embryo rescue (ER) diploid hybrid progeny comprised by 548 genotypes, their wild & cultivated parentals, and varieties with known late blight response were evaluated in Oxapampa and Monobamba in 2007. Forty days after planting (DAP), the percentage of leaf area affected by late blight infection was recorded by plot throughout the season. This data was then used to calculate the relative area under the disease progress curve (rAUDPC). Both field trials were arranged in a randomized block design with three replications and experimental units of four hill-plots. ER hybrids were grouped based on their wild parent origin.  The calculated linear mixed model accounted for fixed effects for the environment, experiment, the interaction between environment and experiment, and replications in each experiment, as well as random effects for the genotypes (hybrids and parents) and their interactions with the environments. The model was able to estimate the LB resistance values for both the hybrids and parents using the best linear unbiased predictions (BLUPs) referred to as the BLUPs predicted rAUDPC. The Least Significant Difference (LSD) test was calculated to establish the significance of pairwise comparisons.'
"

uri <- "doi:10.21223/QLAM5E"
group <- "varieties_potato"
ff <- carobiner::get_data(uri, path, group)

meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
  data_organization = "CIP",
  publication = NA,
  project = NA,
  design = "RCBD",
  data_type = "experiment",
  treatment_vars = "variety",
  response_vars = "AUDPC;rAUDPC;disease_severity",
  notes = NA,
  carob_contributor = "Maryam Yahya",
  carob_date = "2026-10-28",
  carob_completion = 70,
  carob_effort = 2.0
)

f1 <- ff[basename(ff) == "01_LB-Fieldbook_ER_mono2007.xlsx"]
f2 <- ff[basename(ff) == "02_LB_Fieldbook_ER_oxa2007.xlsx"]

r1 <- carobiner::read.excel(f1, na = "NA")
r2 <- carobiner::read.excel(f2, na = "NA")

r1$trial_id <- "QLAM5E_Monobamba_2007"
r2$trial_id <- "QLAM5E_Oxapampa_2007"

r <- carobiner::bindr(r1, r2)
r$record_id <- seq_len(nrow(r))

geo <- data.frame(
  trial_id = c("QLAM5E_Monobamba_2007", "QLAM5E_Oxapampa_2007"),
  country = "Peru",
  adm1 = c("Junin", "Pasco"),
  adm2 = c("Jauja", "Oxapampa"),
  adm3 = c("Monobamba", "Chontabamba"),
  location = c("Monobamba", "Oxapampa"),
  latitude = c(-11.357, -10.5775),
  longitude = c(-75.318, -75.4022),
  geo_from_source = FALSE
)

d <- data.frame(
  record_id = r$record_id,
  trial_id = r$trial_id,
  plot_id = as.character(r$plots),
  rep = as.integer(r$rep),
  variety = r$INS,
  crop = "potato",
  on_farm = NA,
  yield_moisture = NA,
  yield_isfresh =NA,
  is_survey = FALSE,
  irrigated = NA,
  AUDPC = r$AUDPC / 100,
  rAUDPC = r$rAUDPC,
  yield_part = "tubers",
  N_fertilizer = NA,
  P_fertilizer = NA,
  K_fertilizer = NA,
  planting_date = "2006-12",
  harvest_date = NA
)

d <- merge(d, geo, by = "trial_id", all.x = TRUE)

lb_cols <- paste0("LB", 1:8)
lon <- r[, c("record_id", lb_cols)]
d_long <- reshape(lon, varying = lb_cols, v.names = "disease_severity", timevar = "order",
                  times = 1:8, direction = "long")
d_long <- d_long[!is.na(d_long$disease_severity), ]
d_long$id <- NULL
d_long$disease_severity <- as.character(d_long$disease_severity)
d_long$severity_scale <- "0-100"
d_long$disease <- "potato late blight"

carobiner::write_files(path, meta, d, long = d_long)
}