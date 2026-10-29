carob_script <- function(path) {
  
  "
Replication data for: A case study of potato germplasm enhancement using distant late blight resistant wild relatives

'Tetraploid hybrid embryo rescue (HER) hybrid progeny coming from interploidy crosses (4x-2x) comprised by testing genotypes, advanced 4x and ER 2x parentals, and varieties with known late blight response were evaluated under natural late blight pressure environments.  First, all HER progeny comprised by 1079 and then a subset of 304 selected HER clones were tested in Monombamba in 2012 and Oxapampa and Monobamba in 2014 respectively. In the three experiments, forty days after planting (DAP), the percentage of leaf area affected by late blight infection was recorded by plot throughout the season. This data was then used to calculate the relative area under the disease progress curve (rAUDPC). All field trials were arranged in alpha design with two and three replications and experimental units of five hill-plots for 2012 and 2014 trials respectively. HER hybrids were grouped based on their wild ancestral origin. The calculated linear mixed model accounted for fixed effects for the environment, experiment, the interaction between environment and experiment, and replications in each experiment, as well as random effects for the genotypes (hybrids and parents) and their interactions with the environments. The model was able to estimate the LB resistance values for both the hybrids and parents using the best linear unbiased predictions (BLUPs) referred to as the BLUPs predicted rAUDPC.'
"

uri <- "doi:10.21223/N7ERPI"
group <- "varieties_potato"
ff <- carobiner::get_data(uri, path, group)

meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
  data_organization = "CIP",
  publication = NA,
  project = NA,
  design = "alpha lattice",
  data_type = "experiment",
  treatment_vars = "variety",
  response_vars = "AUDPC;rAUDPC;disease_severity",
  notes = NA,
  carob_contributor = "Maryam Yahya",
  carob_date = "2026-10-29",
  carob_completion = 85,
  carob_effort = 1.5
)

f1 <- ff[basename(ff) == "03_LB_Fieldbook_HER_mono2012.xlsx"]
f2 <- ff[basename(ff) == "04_LB_Fieldbook_HER_oxa2014.xlsx"]
f3 <- ff[basename(ff) == "05_LB_Fieldbook_HER_mono2014.xlsx"]

r1 <- carobiner::read.excel(f1, na = "NA")
r1 <- r1[!is.na(r1$`breeder code`), ]
r2 <- carobiner::read.excel(f2)
r3 <- carobiner::read.excel(f3)

r1$trial_id <- "N7ERPI_Monobamba_2012"
r2$trial_id <- "N7ERPI_Oxapampa_2014"
r3$trial_id <- "N7ERPI_Monobamba_2014"

r1$plot_id <- paste(r1$exp, r1$plot, sep = "_")
r2$plot_id <- as.character(r2$plots)
r3$plot_id <- as.character(r3$plots)

r <- carobiner::bindr(r1, r2, r3)
r$record_id <- seq_len(nrow(r))

geo <- data.frame(
  trial_id = c("N7ERPI_Monobamba_2012", "N7ERPI_Oxapampa_2014", "N7ERPI_Monobamba_2014"),
  country = "Peru",
  adm1 = c("Junin", "Pasco", "Junin"),
  adm2 = c("Jauja", "Oxapampa", "Jauja"),
  adm3 = c("Monobamba", "Chontabamba", "Monobamba"),
  location = c("Monobamba", "Oxapampa", "Monobamba"),
  latitude = c(-11.357, -10.5775, -11.357),
  longitude = c(-75.318, -75.4022, -75.318),
  geo_from_source = FALSE,
  planting_date = c("2012", "2014", "2014"),
  harvest_date = NA
)

d <- data.frame(
  record_id = r$record_id,
  trial_id = r$trial_id,
  plot_id = r$plot_id,
  rep = as.integer(r$rep),
  variety = r$`breeder code`,
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
  K_fertilizer = NA
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