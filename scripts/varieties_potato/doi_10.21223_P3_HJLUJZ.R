## ISSUES
# Description says 2 seasons and 492 accessions; data has one trial (sheet "raw_trialB") and 482 genotypes
# Coordinates of the CIP Oxapampa station are from the soil file of doi:10.21223/N00MNU 

carob_script <- function(path) {
  
  "
Replication Data for: Late blight severity in potato accessions from the CIP genebank core collection

The assessment of late blight resistance in 492 potato genebank accessions of the CIP core collection under field conditions with high endemic disease pressure located in Oxapampa (1,810 m a.s.l. Pasco-Perú) were conducted. Randomized complete block design (RBDC) with three replications of 8 hill plots which were adjusted to a row-column design in the first season and second season using row-column design with three replications of 5 hill plots were established. After 40 days after planting (DAP), the percentage of leaf area affected by late blight infection was recorded by plot throughout the season to subsequently compute the area under the disease progress curve (AUDPC).
"
  
  uri <- "doi:10.21223/P3/HJLUJZ"
  group <- "varieties_potato"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 3, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "row-column",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "AUDPC;disease_severity",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-11-04",
    carob_completion = 80,
    carob_effort = 2.0
  )
  
  f1 <- ff[basename(ff) == "01_data_CIP_genebank.xlsx"]
  
  r1 <- carobiner::read.excel(f1, na = c("na", "-"))
  r1 <- r1[r1$genotype != "empty", ]
  r1$record_id <- seq_len(nrow(r1))
  
  d <- data.frame(
    record_id = r1$record_id,
    trial_id = "HJLUJZ_Oxapampa",
    plot_id = as.character(r1$plot),
    rep = as.integer(r1$rep),
    variety = r1$genotype,
    AUDPC = r1$audpc / 100,
    country = "Peru",
    adm1 = "Pasco",
    location = "Oxapampa",
    latitude = -10.57745,
    longitude = -75.4043,
    geo_from_source = FALSE,
    elevation = 1810,
    planting_date = NA,
    harvest_date = NA,
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield = NA,
    yield_part = "tubers",
    yield_moisture = NA,
    yield_isfresh = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  
  lb_cols <- paste0("lb", 1:5)
  lon <- r1[, c("record_id", lb_cols)]
  d_long <- reshape(lon, varying = lb_cols, v.names = "disease_severity", timevar = "order",
                    times = 1:5, direction = "long")
  d_long <- d_long[!is.na(d_long$disease_severity), ]
  d_long$id <- NULL
  d_long$disease_severity <- as.character(d_long$disease_severity)
  d_long$severity_scale <- "0-100"
  d_long$disease <- "potato late blight"
  
  carobiner::write_files(path, meta, d, long = d_long)
}
