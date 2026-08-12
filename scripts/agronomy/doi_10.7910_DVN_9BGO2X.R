# R script for "carob"
# license: GPL (>=3)
## ISSUES
carob_script <- function(path) {
  "
Replication Data for: Reducing losses but failing to sequester carbon in soils – the case of Conservation Agriculture and Integrated Soil Fertility Management in the humid tropical agro-ecosystem of Western Kenya
Soil organic carbon content of topsoil (0-15 cm depths) of two agronomic long-term trial (CT1 and INM3), collected repeatedly throughout the years
"
  uri <- "doi:10.7910/DVN/9BGO2X"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
                                  data_organization = "CIAT",
                                  publication = "doi:10.1016/j.agee.2017.11.004",
                                  project = NA,
                                  design = "split-split-split plot design",
                                  data_type = "on-farm experiment",
                                  treatment_vars = "N_fertilizer;P_fertilizer;crop_rotation;land_prep_method;residue_prevcrop_used",
                                  response_vars = "soil_SOC;soil_N_total", 
                                  carob_contributor = "Premrose Masunungure",
                                  carob_date = "2026-08-04",
                                  carob_completion = 70,	
                                  carob_effort = 11
  )
  
  #f1 <- ff[basename(ff) == "01.Dictionary.xls"]
  f2 <- ff[basename(ff) == "02. SOC N CT1.xlsx"]
  f3 <- ff[basename(ff) == "03. SOC N INM3.xlsx"]
  #f3 <- ff[basename(ff) == "04.INM3 Layout.jpg"]
  #r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3, sheet="SOC N INM3")
  #r4 <- carobiner::read.excel(f3, sheet="Chart")
  
  r2$crop_rotation <- "soybean;maize"
  r3$crop_rotation <- "Tephrosia;maize"
  
  
  r2$Tillage <- ifelse(r2$Tillage == "CT", "conventional",
                       ifelse(r2$Tillage == "0T", "zero-tillage", r2$Tillage))
  

  r3$FYM <- ifelse(r3$FYM == "PlusFYM", "FYM",
                   ifelse(r3$FYM == "Minus FYM", "no FYM", r3$FYM))
  
  r2_long <- reshape(
    r2,
    varying = list(
      Nitrogen = c("Nitrogen% (01/Aug/06)", "Nitrogen% (01/Aug/09)",
                   "Nitrogen% (06/Aug/12)", "Nitrogen% (06/Aug/15)"),
      Carbon   = c("Carbon % (01/Aug/06)", "Carbon % (01/Aug/09)",
                   "Carbon % (06/Aug/12)", "Carbon % (06/Aug/15)")
    ),
    v.names = c("Nitrogen", "Carbon"),
    timevar = "date",
    times = c("01/Aug/06", "01/Aug/09", "06/Aug/12", "06/Aug/15"),
    direction = "long"
  )
  r2_long$date <- as.Date(r2_long$date, format = "%d/%b/%y")
  r2_long$id <- NULL
  

  r3_long <- reshape(
    r3,
    varying = list(
      Nitrogen = c("Nitrogen% (16/Sep/05)","Nitrogen% (11/Sep/07)","Nitrogen% (31/Aug/09)",
                   "Nitrogen% (30/Aug/11)","Nitrogen% (13/Sep/13)","Nitrogen% (01/Sep/15)"),
      Carbon   = c("Carbon% (16/Sep/05)","Carbon% (11/Sep/07)","Carbon% (31/Aug/09)",
                   "Carbon% (30/Aug/11)","Carbon% (13/Sep/13)","Carbon% (01/Sep/15)")
    ),
    v.names = c("Nitrogen", "Carbon"),
    timevar = "date",
    times = c("16/Sep/05","11/Sep/07","31/Aug/09","30/Aug/11","13/Sep/13","01/Sep/15"),
    direction = "long"
  )
  r3_long$date <- as.Date(r3_long$date, format = "%d/%b/%y")
  r3_long$id <- NULL
  
  d1 <- data.frame(
    country = "Kenya",
    trial_id = "CT1",
    plot_id = as.character(r2_long$Plot),
    treatment = as.character(r2_long$`Treatm/ Cluster`),
    rep = as.integer(r2_long$Rep),
    land_prep_method = r2_long$Tillage,
    N_fertilizer = r2_long$N,
    P_fertilizer = r2_long$P,
    residue_prevcrop_used = r2_long$Stover,
    crop_rotation = r2_long$crop_rotation,
    date = as.Date(r2_long$date),
    soil_N_total = as.numeric(r2_long$Nitrogen),
    soil_SOC = as.numeric(r2_long$Carbon)
  )
  d2 <- data.frame(
    country = "Kenya",
    trial_id = "INM3",
    plot_id = as.character(r3_long$Plot),
    treatment = as.character(r3_long$`Trt#`),
    rep = as.integer(r3_long$Rep),
    OM_type = r3_long$FYM,
    residue_prevcrop_used = r3_long$Residues,
    crop_rotation = r3_long$crop_rotation,
    N_fertilizer = r3_long$N,
    P_fertilizer = r3_long$P,
    date = as.Date(r3_long$date),
    soil_N_total = as.numeric(r3_long$Nitrogen),
    soil_SOC = as.numeric(r3_long$Carbon)
  )
  d <- carobiner::bindr(d1, d2)
  d <- unique(d)
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- NA
  
  d$latitude  <- ifelse(d$trial_id == "CT1", 0.1297, 0.144)
  d$longitude <- ifelse(d$trial_id == "CT1", 34.405, 34.404)
  d$geo_from_source <- TRUE
  
  d$planting_date <- as.character(as.Date(NA))
  d$harvest_date  <- as.character(as.Date(NA))
  
  d$fertilizer_type <- "urea; MOP; triple super phosphate"   #information obtained from publication
  
  d$yield_part <- "grain"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- NA
  
  carobiner::write_files(path, meta, d)
}
