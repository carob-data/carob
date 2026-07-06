# R script for "carob"
# license: GPL (>=3)

## ISSUES/NOTES
# - join_key added due to mismatch (inconsistent zero padding) in the raw PlotID/FieldID strings in r3 and r4;
# - "Season" set as "first" as confirmed from publication that Pampaida had one season
# - r4$Season (values "d1"/"d2"/"d3") not a real cropping season, just "field" with a "d"; no documentation in metadata
# - planting_date not given directly; back-calculated as r3$Date_Mac (measurement date) minus r3$Period (days after planting), using period=14 (earliest); converges to 2010-07-01 to 2010-07-17
# - r3$Date is 4 years later than Date_Mac for every record (e.g. 7/31/2014 vs 7/30/2010), assumed a digitization date, not used
# - fertilizer rates (N/P/K/Ca/Mg/S/Zn, manure amount) not in raw data, only treatment codes; taken from Kihara et al. 2016 (Table 2): 100N/30P/60K kg/ha for maize, +MN adds 10Ca/5Mg/5S/3Zn kg/ha + trace B, +MA adds 10 t/ha manure (dry matter basis), no lime used at Pampaida
# - grain yield is on 12.5% moisture basis per the publication
# - plant_spacing/row_spacing/plot_area taken from the publication's trial design (constant, not in raw data)
# - Adj_StoverYld labelled "t/ha" in the metadata but confirmed (vs TStoverYld, and TGrainYld_adj + Adj_StoverYld/1000 ~= TPlntBiom) to already be kg/ha; not converted further
# - cob fresh weight (CobFW/SSCobFW/AdjCobFW.kg.ha.) skipped - overlaps with grain/stover already captured
# - harvest_date not set - growth measurements only go to 70 days after planting; too early for maize harvest.

carob_script <- function(path) {
  
"Africa Soil Information System - Phase 1, Pampaida

The AfSIS project aimed to establish an Africa Soil Information system. Data 
was collected in sentinel sites across sub-Saharan Africa using the Land 
Degradation Surveillance framework, and included multi-location diagnostic 
trials in selected sentinel sites to determine nutrient limitations and 
response to improved soil management practices (soil amendments). Pampaida 
is a maize-growing sentinel site in Nigeria. The trial used a modified 
nutrient omission design: Control, NPK, three treatments omitting one of 
N/P/K from NPK, and three treatments adding multi-nutrients, manure or lime 
to NPK (no lime treatment at Pampaida)."
  
  uri <- "doi:10.25502/20180814/1415/HJ"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
      data_organization = "IITA",
      publication = "doi:10.1016/j.agee.2016.05.012",
      project = "AfSIS",
      design = "nutrient omission trial",
      data_type = "on-farm experiment",
      treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;OM_used",
      response_vars = "yield;dmy_residue;dmy_total",
      notes = "Fertilizer N/P/K/Ca/Mg/S/Zn rates and manure amount per treatment code 
      were not in the raw data; they were taken from the associated publication (Kihara et al. 2016, Table 2) for maize. 
      Grain yield is on a 12.5% moisture basis per the publication. 
      Planting date was back-calculated from measurement dates minus days-after-planting.",
      carob_contributor = "Stella Muthoni",
      carob_date = "2026-06-30",
      carob_completion = 90,
      carob_effort = 5 #most time spent decoding the uniqueID linkage and other definition in publication
  )
  
  f1 <- ff[basename(ff) == "pampaida_dt2010_field_plant_and_plot_metadata.xlsx"]
  f2 <- ff[basename(ff) == "pampaida_dt2010_field.csv"]
  f3 <- ff[basename(ff) == "pampaida_dt2010_plant.csv"]
  f4 <- ff[basename(ff) == "pampaida_dt2010_plot.csv"]
  
  r2 <- read.csv(f2)   # field-level: 32 fields, location, rotation history
  r3 <- read.csv(f3)   # plant growth + treatment code: 9216 rows (multiple plants/periods per plot)
  r4 <- read.csv(f4)   # plot-level yield: 270 rows
  
  ## ---- build standardized join keys ----
  
  r4$Field <- as.integer(substr(r4$FieldID, nchar(r4$FieldID), nchar(r4$FieldID)))
  
  r3$join_key <- paste(r3$Cluster, r3$Field, r3$Plot, sep="_")
  r4$join_key <- paste(r4$Cluster, r4$Field, r4$Plot, sep="_")
  
  r2$join_key_field <- paste(r2$Cluster, r2$Field, sep="_")
  r4$join_key_field <- paste(r4$Cluster, r4$Field, sep="_")
  
  ## ---- planting date, treatment code, plot/rep, one row per plot (period=14, earliest) ----
  
  r3$Date_Mac <- as.Date(r3$Date_Mac, format="%m/%d/%Y")
  r3$planting_date_calc <- r3$Date_Mac - r3$Period
  
  plot_info <- unique(r3[r3$Period == 14, c("join_key", "TRT.Code", "planting_date_calc", "Rep")])
  plot_info$TRT.Code <- trimws(plot_info$TRT.Code)
  
  ## ---- maximum plant height per plot, across all measurement periods ----
  
  height_max <- aggregate(Plant.height..cm. ~ join_key, data=r3, FUN=max, na.rm=TRUE)
  names(height_max) <- c("join_key", "plant_height")
  
  ## ---- fertilizer rates by treatment code (Kihara et al. 2016, Table 2, maize rates) ----
  ## macronutrients: 100 kg N/ha, 30 kg P/ha, 60 kg K/ha for maize
  ## +MN (micronutrients): 10 kg Ca/ha, 5 kg Mg/ha, 5 kg S/ha, 3 kg Zn/ha, trace B (added to NPK)
  ## +MA (manure): 10 t/ha on a dry matter basis (added to NPK)
  ## lime: not used at Pampaida (pH already >5.5)
  
  fert_lookup <- data.frame(
    TRT.Code = c("Control", "NK", "NP", "PK", "NPK", "NPK+Manure", "NPK+MN"),
    N_fertilizer = c(0, 100, 100, 0, 100, 100, 100),
    P_fertilizer = c(0, 0, 30, 30, 30, 30, 30),
    K_fertilizer = c(0, 60, 0, 60, 60, 60, 60),
    Ca_fertilizer = c(0, 0, 0, 0, 0, 0, 10),
    Mg_fertilizer = c(0, 0, 0, 0, 0, 0, 5),
    S_fertilizer = c(0, 0, 0, 0, 0, 0, 5),
    Zn_fertilizer = c(0, 0, 0, 0, 0, 0, 3),
    OM_amount = c(0, 0, 0, 0, 0, 10000, 0)
  )
  
  ## ---- merge everything: yield (r4) + treatment/date (plot_info) + field/location (r2) + fertilizer rates ----
  
  master <- merge(r4, plot_info, by="join_key", all.x=TRUE)
  master <- merge(master, height_max, by="join_key", all.x=TRUE)
  master <- merge(master, r2, by="join_key_field", all.x=TRUE, suffixes=c("", "_field"))
  master <- merge(master, fert_lookup, by="TRT.Code", all.x=TRUE)
  
  ## ---- standardize crop rotation history columns to valid terminag crop names ----
  crop_lookup <- c(
    "maize"="maize", "sorghum"="sorghum", "millet"="millet", "rice"="rice",
    "cassava"="cassava", "soybean"="soybean", "groundnut"="groundnut",
    "cowpea"="cowpea", "beans"="common bean", "onions"="onion", "onion"="onion",
    "pepe"="chili pepper")
  
  clean_crop <- function(x) {
    x <- tolower(trimws(x))
    x <- unlist(strsplit(x, "-"))
    x <- trimws(x)
    x <- x[!x %in% c("", ".", "fallow")]
    x <- crop_lookup[x]
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    paste(unique(x), collapse=";")
  }
  
  master$PCrop1_clean <- sapply(master$PCrop1, clean_crop)
  master$P2Crop_clean <- sapply(master$P2Crop, clean_crop)
  master$P3Crop_clean <- sapply(master$P3Crop, clean_crop)
  master$P4Crop_clean <- sapply(master$P4Crop, clean_crop)
  
  #Add crops rotated with
  build_rotation <- function(c1, c2, c3, c4) {
    parts <- cbind(c1, c2, c3, c4)
    apply(parts, 1, function(row) {
      row <- row[!is.na(row)]
      if (length(row) == 0) return(NA)
      paste(row, collapse=";")
    })
  }
  master$crop_rotation <- build_rotation(master$PCrop1_clean, master$P2Crop_clean, master$P3Crop_clean, master$P4Crop_clean)
  
  ## ---- build final carob data frame ----
  
  d <- data.frame(
    trial_id = as.character(as.integer(as.factor(master$join_key_field))),
    plot_id = as.character(master$Plot),
    rep = master$Rep,
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = FALSE,
    crop = "maize",
    variety = "Oba Super 2",
    previous_crop = master$PCrop1_clean,
    crop_rotation = master$crop_rotation,
    season = "first",
    plant_height = master$plant_height,
    treatment = master$TRT.Code,
    location = master$Village,
    country = "Nigeria",
    adm1 = "Kaduna",
    adm2 = "Ikara",
    longitude = master$Flong,
    latitude = master$Flat,
    elevation = master$Elevation,
    geo_from_source = TRUE,
    planting_date = as.character(master$planting_date_calc),
    fertilizer_used = master$TRT.Code != "Control",
    N_fertilizer = master$N_fertilizer,
    P_fertilizer = master$P_fertilizer,
    K_fertilizer = master$K_fertilizer,
    Ca_fertilizer = master$Ca_fertilizer,
    Mg_fertilizer = master$Mg_fertilizer,
    S_fertilizer = master$S_fertilizer,
    Zn_fertilizer = master$Zn_fertilizer,
    fertilizer_type = "NPK",
    OM_used = master$TRT.Code == "NPK+Manure",
    OM_type = ifelse(master$TRT.Code == "NPK+Manure", "farmyard manure", NA),
    OM_amount = master$OM_amount,
    plant_spacing = 25,
    row_spacing = 75,
    plot_area = 25,
    yield_part = "grain",
    yield = master$TGrainYld_adj * 1000,
    yield_moisture = 12.5,
    dmy_residue = master$Adj_StoverYld,
    dmy_total = master$TPlntBiom * 1000
  )
  
  d$location[d$location == ""] <- NA
##  d <- d[!is.na(d$yield), ]
  
  carobiner::write_files(path, meta, d)
}

