# R script for "carob"
# license: GPL (>=3)


## ISSUES
# Two soybean records in the cleaned data do not have a matching X_uuid in
# the raw survey data. Their coordinates and province are therefore missing.
# The cleaned data describe yield as grain dry matter yield (kg/ha).
# Planting and harvest dates in the cleaned sheets are Julian day numbers.
# They are converted to calendar dates using the 2022/2023 growing season.

carob_script <- function(path) {
  
  "
  Maize and soybean farming practices and yields during the 2022-2023 season in Malawi, Mozambique and Zambia
  
  This dataset describes farming practices in soybean and maize monocrop fields in Malawi, Mozambique and Zambia for the 2022-2023 growing season. Crop cuts were carried out on the fields of farmers participating in the EiA use case, as well as neighbouring farms and farms in non-implementing communities for representativeness. Data are provided as Excel spreadsheets and an .rds file (compatible with R 4.4.1). There is one table with the raw data (as recorded through KoboCollect) and one with the cleaned data used for the related manuscript. Survey data are reproduced as closely as possible to how they were recorded. Secondary data were used as indicated in the codebook.
  "
      
  uri <- "doi:10.71682/10549347"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(
    uri, path, group, major = 1, minor = 0,
    data_organization = paste(
      "CIMMYT; IITA; CIFOR-ICRAF; ZARI;",
      "Department of Agricultural Research Services (DARS), Chitedze, Malawi;",
      "IIAM"
    ),
    publication = "doi.org/10.1016/j.agsy.2026.104740",
    project = NA,
    design = "unitOfAnalysis: Field; timeMethod: Cross-sectional; responseRate: 100%",
    data_type = "survey",
    treatment_vars = "",
    response_vars = "yield",
    notes = paste(
      "Farm-level survey and crop-cut data for maize and soybean during the",
      "2022-2023 growing season in Malawi, Mozambique and Zambia.",
      "The cleaned maize and soybean tables were used for standardized",
      "agronomic observations. Coordinates and province names were joined",
      "from the raw survey table using X_uuid. Two soybean records had no",
      "matching raw survey record and consequently have missing coordinates.",
      "Planting and harvest dates in the cleaned tables are Julian day numbers.",
      "Days 300 to 365 are assigned to 2022, and days 1 to 299 to 2023.",
      "Yield is reported as grain dry matter yield in kg/ha.",
      "The source codebook identifies pfert_kgha as P2O5 and kfert_kgha",
      "as K2O. These are converted to elemental P and K, respectively."
    ),
    carob_contributor = "Emmanuel Ayodele Jolaiya",
    carob_date = "2026-11-10",
    carob_completion = 85,
    carob_effort = 0.5
  )

  f1 <- ff[basename(ff) == "EiA_2022_survey_2025-09-26_v0.xlsx"]
  if (length(f1) != 1) {
    stop("Expected exactly one EiA_2022_survey_2025-09-26_v0.xlsx file")
  }
  
  raw <- carobiner::read.excel(f1, sheet = "raw_survey_data")
  maize <- carobiner::read.excel(f1, sheet = "maize_clean_data")
  soybean <- carobiner::read.excel(f1, sheet = "soybean_clean_data")
  
  # Combine the two cleaned crop tables. The source tables contain different
  # crop-specific columns, so rbind is performed after selecting common fields.
  common <- c(
    "X_uuid", "country", "farmer_id", "district", "epa",
    "plot_geotraced_area_m2", "crop_condition", "slope",
    "date_planting", "date_harvest", "seed_kgha", "seed_type",
    "orgfert_yn", "weed_mgt", "weeding_1st_das", "weeding_2nd_das",
    "diasease_damage", "pest_damage", "season_days"
  )
  
  maize_part <- maize[, common]
  maize_part$crop <- "maize"
  maize_part$yield <- maize$maize_yield_kgha
  maize_part$N_fertilizer <- maize$nfert_kgha
  maize_part$P2O5_fertilizer <- maize$pfert_kgha
  maize_part$K2O_fertilizer <- maize$kfert_kgha
  
  soybean_part <- soybean[, common]
  soybean_part$crop <- "soybean"
  soybean_part$yield <- soybean$soy_yield_kgha
  soybean_part$N_fertilizer <- NA_real_
  soybean_part$P2O5_fertilizer <- NA_real_
  soybean_part$K2O_fertilizer <- NA_real_
  
  # Soybean clean data do not include mineral fertilizer rates.
  d0 <- rbind(maize_part, soybean_part)
  
  # Add source coordinates and province from the raw survey records.
  raw_geo <- raw[, c(
    "X_uuid", "province", "_field_gps_latitude",
    "_field_gps_longitude", "_field_gps_altitude", "variety_name"
  )]
  raw_geo <- raw_geo[!duplicated(raw_geo$X_uuid), ]
  d0 <- merge(d0, raw_geo, by = "X_uuid", all.x = TRUE, sort = FALSE)
  
  # Keep the original order from the cleaned tables after the merge.
  d0 <- d0[match(c(maize_part$X_uuid, soybean_part$X_uuid), d0$X_uuid), ]
  
  # Convert Julian day numbers to dates for the 2022-2023 season.
  julian_to_date <- function(x) {
    out <- rep(as.Date(NA), length(x))
    ok <- !is.na(x) & is.finite(as.numeric(x)) &
      as.numeric(x) >= 1 & as.numeric(x) <= 366
    yr <- ifelse(as.numeric(x[ok]) >= 300, 2022, 2023)
    out[ok] <- as.Date(as.numeric(x[ok]) - 1, origin = paste0(yr, "-01-01"))
    as.character(out)
  }
  
  d <- data.frame(
    country = carobiner::fix_name(d0$country, "title"),
    adm1 = carobiner::fix_name(d0$province, "title"),
    adm2 = carobiner::fix_name(d0$district, "title"),
    adm3 = carobiner::fix_name(d0$epa, "title"),
    hhid = d0$farmer_id,
    crop = tolower(d0$crop),
    latitude = as.numeric(d0$`_field_gps_latitude`),
    longitude = as.numeric(d0$`_field_gps_longitude`),
    elevation = as.numeric(d0$`_field_gps_altitude`),
    plot_id = d0$X_uuid,
    variety = as.character(d0$variety_name),
    planting_date = julian_to_date(d0$date_planting),
    harvest_date = julian_to_date(d0$date_harvest),
    yield = as.numeric(d0$yield),
    yield_part = "grain",
    yield_isfresh=NA_real_,
    yield_moisture = NA_real_,
    plot_area = as.numeric(d0$plot_geotraced_area_m2),
    N_fertilizer = as.numeric(d0$N_fertilizer),
    P_fertilizer = as.numeric(d0$P2O5_fertilizer) / 2.29,
    K_fertilizer = as.numeric(d0$K2O_fertilizer) / 1.2051,
    S_fertilizer = NA_real_,
    lime = NA_real_,
    fertilizer_type = NA_character_,
    inoculated = NA,
    inoculant = NA_character_,
    on_farm = TRUE,
    is_survey = TRUE,
    irrigated = NA,
    crop_rotation = NA_character_,
    geo_from_source = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Every surveyed field is treated as an individual survey trial.
  d$trial_id <- as.character(seq_len(nrow(d)))
  
  # Remove records without a reported yield, if any.
  d <- d[!is.na(d$yield), ]
  
  carobiner::write_files(path, meta, d)
  }
  
  ## Test in a clean R session:
  # carob_script(path = "path/to/carob/data")
carob_script(path = "/Users/jeff/Documents/2026/IWMI/Workshop/carob")

