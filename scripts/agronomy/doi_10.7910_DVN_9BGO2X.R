# R script for "carob"
# license: GPL (>=3)

## ISSUES
# land_prep_method is not is d2 dataframe so it created NA value for d dataframe after binding to d

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
		treatment_vars = "N_fertilizer;P_fertilizer;crop_rotation;land_prep_method",
		response_vars = "soil_SOC;soil_N_total", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-08-04",
		carob_completion = 85,	
		carob_effort = 15
  )
  
  #f1 <- ff[basename(ff) == "01.Dictionary.xls"]
  f2 <- ff[basename(ff) == "02. SOC N CT1.xlsx"]
  f3 <- ff[basename(ff) == "03. SOC N INM3.xlsx"]
  #f3 <- ff[basename(ff) == "04.INM3 Layout.jpg"]
  #r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3, sheet="SOC N INM3")
  #r4 <- carobiner::read.excel(f3, sheet="Chart")
  
  
  r2 <- unique(carobiner::read.excel(f2))
  r2$record_id <- as.integer(1:nrow(r2))
  
  r3 <- unique(carobiner::read.excel(f3, sheet = "SOC N INM3"))
  r3$record_id <- as.integer(nrow(r2)+1:nrow(r3))
  
  
  col <- names(r2)[grepl("record_id|Nitrogen|Carbon", names(r2))]
  r2_long <- r2[, col]
  
  
  r2_long <- reshape(
    r2_long,
    varying = list(
      Nitrogen = c("Nitrogen% (01/Aug/06)", "Nitrogen% (01/Aug/09)",
                   "Nitrogen% (06/Aug/12)", "Nitrogen% (06/Aug/15)"),
      Carbon = c("Carbon % (01/Aug/06)", "Carbon % (01/Aug/09)",
                 "Carbon % (06/Aug/12)", "Carbon % (06/Aug/15)")
    ),
    v.names = c("soil_N_total", "soil_SOC"),
    timevar = "date",
    times = c("01/Aug/06", "01/Aug/09", "06/Aug/12", "06/Aug/15"),
    direction = "long"
  )
  
  r2_long$date <- as.Date(r2_long$date, format = "%d/%b/%y")
  r2_long$date <- format(r2_long$date, "%Y-%m-%d")
  r2_long$id <- NULL
  
  r2_long$soil_N_total = as.numeric(r2_long$soil_N_total)
  r2_long$soil_SOC = as.numeric(r2_long$soil_SOC)
  
  
  col1 <- names(r3)[grepl("record_id|Nitrogen|Carbon", names(r3))]
  r3_long <- r3[, col1]
  
  r3_long <- reshape(
    r3_long,
    varying = list(
      Nitrogen = c("Nitrogen% (16/Sep/05)", "Nitrogen% (11/Sep/07)",
                   "Nitrogen% (31/Aug/09)", "Nitrogen% (30/Aug/11)",
                   "Nitrogen% (13/Sep/13)", "Nitrogen% (01/Sep/15)"),
      Carbon = c("Carbon% (16/Sep/05)", "Carbon% (11/Sep/07)",
                 "Carbon% (31/Aug/09)", "Carbon% (30/Aug/11)",
                 "Carbon% (13/Sep/13)", "Carbon% (01/Sep/15)")
    ),
    v.names = c("soil_N_total", "soil_SOC"),
    timevar = "date",
    times = c("16/Sep/05", "11/Sep/07", "31/Aug/09",
              "30/Aug/11", "13/Sep/13", "01/Sep/15"),
    direction = "long"
  )
  

  r3_long$date <- as.Date(r3_long$date, format = "%d/%b/%y")
  r3_long$date <- format(r3_long$date, "%Y-%m-%d")
  r3_long$id <- NULL
  
  r3_long$soil_N_total[r3_long$soil_N_total=="Missing"]  <- NA
  r3_long$soil_SOC[r3_long$soil_SOC=="Missing"] <- NA
  r3_long$soil_N_total = as.numeric(r3_long$soil_N_total)
  r3_long$soil_SOC = as.numeric(r3_long$soil_SOC)
  
  d_lon <- carobiner::bindr(r2_long, r3_long)
  
  
  d1 <- data.frame(
    country = "Kenya",
    trial_id = "CT1",
    record_id = r2$record_id,
    plot_id = as.character(r2$Plot),
    treatment = as.character(r2$`Treatm/ Cluster`),
    rep = as.integer(r2$Rep),
    N_fertilizer = r2$N,
    P_fertilizer = r2$P,
    K_fertilizer = 60,   #60kg/ha information from the publication
    residue_prevcrop_used = grepl("R\\+", r2$Stover),
    crop_rotation = ifelse(r2$Rotation == "M-M", "maize; maize",
                               ifelse(r2$Rotation == "S-M", "soybean; maize",
		    ifelse(r2$Rotation == "M-S", "maize; soybean",
		           ifelse(r2$Rotation == "Inter", "maize;soybean", r2$Rotation)))),
    land_prep_method = ifelse(r2$Tillage == "CT", "conventional",
                              ifelse(r2$Tillage == "0T", "none", r2$Tillage))
  ) 
  
  d1$crop = "maize"   #information from publication
  
  
  d2 <- data.frame(
    country = "Kenya",
    trial_id = "INM3",
    plot_id = as.character(r3$Plot),
    record_id = r3$record_id,
    treatment = as.character(r3$`Trt#`),
    rep = as.integer(r3$Rep),
    crop_rotation = ifelse(r3$Rotation == "M-M", "maize; maize",
                               ifelse(r3$Rotation == "T-M", "tephrosia; maize",
		    ifelse(r3$Rotation == "M-T", "maize; tephrosia",
		           ifelse(r3$Rotation == "Intercr", "maize;tephrosia", r3$Rotation)))),
    N_fertilizer = r3$N,
    P_fertilizer = r3$P,
    K_fertilizer = 60,   #60kg/ha information from the publication
    OM_type = ifelse(r3$FYM == "PlusFYM", "farmyard manure",
                     ifelse(r3$FYM == "Minus FYM", "none", r3$FYM))
  )
  
  d2$crop = "maize"     #information from publication
  
  d <- carobiner::bindr(d1, d2)
  d <- unique(d)
  
  d$on_farm <- TRUE   #information from the publication
  d$is_survey <- FALSE
  d$irrigated <- NA
  
  d$latitude  <- ifelse(d$trial_id == "CT1", 0.1297, 0.144)
  d$longitude <- ifelse(d$trial_id == "CT1", 34.405, 34.404)
  d$geo_from_source <- TRUE
  
  d$planting_date <- as.character(as.Date(NA))
  d$harvest_date  <- as.character(as.Date(NA))
  
  d$fertilizer_type <- "urea; MOP; TSP"    #information obtained from publication but KCL was applied as muriate of potash.
  d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type) 
  
  d$yield <- NA
  d$yield_part <- "grain"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- NA
  
  
  carobiner::write_files(path, meta, d, long = d_lon)
} 


