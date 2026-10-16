carob_script <- function(path) {
  
  "
Dataset for: Advanced clones of group B3 cycle 2, population B (B3C2) in Oxapampa-Peru

B3C2 is the second cycle of recombination of advanced B3C1 potato clones. These clones have high levels of  horizontal resistance to late blight in absence of R-genes, and they also have high tuber yield. These clones  were planted in a randomized complete block design (RCBD) with 2-4 replicates at Oxapampa,  located at 1810 masl in Pasco-Peru in the Eastern mountain ranges facing the Amazon.  The trials were established at Oxapampa due to the high disease pressure of late blight in these areas from 1999 to 2005.
"
  
  uri <- "doi:10.21223/P3/9VMENB"
  group <- "varieties_potato"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 2, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable;AUDPC;rAUDPC;disease_severity;fries_color;chip_color_",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-28",
    carob_completion = 70,
    carob_effort = 8.0
  )
  
  files <- ff[grepl("_processed.xlsx", basename(ff)) &
              !grepl("Data_dictionary", basename(ff))]
  
  r <- lapply(files, function(f) {
    x <- carobiner::read.excel(f, na=".")
    names(x) <- tolower(names(x))
    x$trial_id <- sub("^[0-9]+_", "", basename(f))
    x$trial_id <- sub("_processed\\.xlsx$", "", x$trial_id)
    x
  })
  r <- do.call(carobiner::bindr, r)
  
  xls_files <- ff[grepl("\\.xls$", basename(ff))]
  
  get_dates <- function(f) {
    m <- carobiner::read.excel(f, sheet = "Minimal")
    vals <- setNames(m$Value, m$Factor)
    data.frame(
      trial_id      = sub("\\.xls$", "", basename(f)),
      planting_date = as.character(as.Date(vals[["Begin date"]])),
      harvest_date  = as.character(as.Date(vals[["End date"]]))
    )
  }
  trial_dates <- unique(do.call(rbind, lapply(xls_files, get_dates)))
  
  d <- data.frame(
    trial_id         = r$trial_id,
    plot_id          = as.character(r$plot),
    rep              = as.integer(r$rep),
    variety          = r$variety,
    location         = r$locality,
    adm1             = r$admin1,
    adm2             = r$admin2,
    adm3             = r$admin3,
    country          = "Peru",
    crop             = tolower(r$crop),
    yield_part       = "tubers",
    yield            = r$yield_fresh * 1000,
    yield_marketable = r$mtyna * 1000,
    yield_moisture   = r$avdm,
    yield_isfresh    = TRUE,
    AUDPC            = r$audpc,
    rAUDPC           = r$raudpc,
    latitude         = as.numeric(r$latitude),
    longitude        = as.numeric(r$longitude),
    elevation        = as.numeric(r$elevation),
    geo_from_source  = TRUE,
    N_fertilizer     =NA,
    P_fertilizer     = NA,
    K_fertilizer     =NA,
    soil_texture     = tolower(r$soil_texture),
    on_farm          = FALSE,
    is_survey        = FALSE,
    irrigated        = NA,
    fries_color      = r$ffr,
    LB1 = r$lb1,
    LB2 = r$lb2,
    LB3 = r$lb3,
    LB4 = r$lb4,
    LB5 = r$lb5,
    LB6 = r$lb6,
    LB7 = r$lb7,
    # New variables
    sAUDPC_        = r$saudpc,
    chip_color_    = r$chip_color,
    texture_fries_ = r$texfr       
  )

  d$record_id <- 1:nrow(d)
  
  d <- merge(d, trial_dates, by = "trial_id")
  
  LBvars <- c("LB1", "LB2", "LB3", "LB4", "LB5", "LB6", "LB7")
  
  d_long <- reshape(d[, c("record_id", "planting_date", LBvars)], varying = c(LBvars), v.names = "disease_severity",
    timevar = "DAP", times = c(50, 57, 64, 71, 78, 87, 94), direction = "long"
  )  
  d_long <- d_long[!is.na(d_long$disease_severity), ]
  d_long$date <- as.character(as.Date(d_long$planting_date) + d_long$DAP)
  d_long$disease <- "potato late blight"
  d_long$disease_severity <- as.character(d_long$disease_severity)
  d_long$DAP <- as.integer(d_long$DAP)
  d_long$id <- d_long$planting_date <- NULL
  rownames(d_long) <- NULL

  d[,LBvars] <- NULL
  
  carobiner::write_files(path, meta, d, d_long)
}

