# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {
  
  "
Dataset for: Potato NPT Trials Amidst Prolonged Rains in Kenyan Regions - October 2009

The Potato NPT Trials Amidst Prolonged Rains in Kenyan Regions took place
in October 2009 in various locations in Kenya, including Baraka,
Kibirichia, Kisima, Limuru and Narok. The study was conducted under a
randomized complete block design (RCBD) with eight potato clones and
four replications, aiming to assess the performance of these clones
under prolonged rainfall.
"
  
  uri <- "doi:10.21223/FBZ6JS"
  group <- "agronomy"
  
  ff <- carobiner::get_data(uri, path, group)
  
  
  meta <- carobiner::get_metadata(
    uri,
    path,
    group,
    major = 2,
    minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "randomized complete block design",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = paste(
      c(
        "yield",
        "number_tubers_planted",
        "number_plants_emerged",
        "percentage_plants_emerged",
        "number_plants_harvested",
        "percentage_plants_harvested",
        "number_marketable_tubers_plot",
        "number_marketable_tubers_plant",
        "marketable_tuber_weight_plot",
        "marketable_tuber_weight_plant",
        "marketable_tuber_yield_not_adjusted",
        "marketable_tuber_yield_adjusted",
        "average_marketable_tuber_weight"
      ),
      collapse = ";"
    ),
    notes = paste(
      "Potato NPT trials conducted in Kenya in October 2009.",
      "Five Fieldbook files are available for Baraka, Kibirichia,",
      "Kisima, Limuru and Narok.",
      "The trials used eight potato clones and four replications",
      "in a randomized complete block design.",
      "The Minimal sheets provide source locality and geographic",
      "information for the trial sites.",
      "MTYA is the adjusted marketable tuber yield reported in",
      "the Fieldbook and is standardized to kg/ha."
    ),
    carob_contributor = "MARYAM YAHYA",
    carob_date = "2026-08-24",
    carob_completion = 100,
    carob_effort = 1
  )
  
  
  ## Source files
  
  f1 <- ff[basename(ff) == "PTYL200910_BARAKA.xls"]
  f2 <- ff[basename(ff) == "PTYL200910_KIBRCH.xls"]
  f3 <- ff[basename(ff) == "PTYL200910_KISIMA.xls"]
  f4 <- ff[basename(ff) == "PTYL200910_LIMURU.xls"]
  f5 <- ff[basename(ff) == "PTYL200910_NAROK.xls"]
  
  
  ## Read the Fieldbook sheets
  
  r1 <- carobiner::read.excel(f1, sheet = "Fieldbook")
  r2 <- carobiner::read.excel(f2, sheet = "Fieldbook")
  r3 <- carobiner::read.excel(f3, sheet = "Fieldbook")
  r4 <- carobiner::read.excel(f4, sheet = "Fieldbook")
  r5 <- carobiner::read.excel(f5, sheet = "Fieldbook")
  
  
  ## Read Minimal sheets for site information
  
  min1 <- carobiner::read.excel(f1, sheet = "Minimal")
  min2 <- carobiner::read.excel(f2, sheet = "Minimal")
  min3 <- carobiner::read.excel(f3, sheet = "Minimal")
  min4 <- carobiner::read.excel(f4, sheet = "Minimal")
  min5 <- carobiner::read.excel(f5, sheet = "Minimal")
  
  
  ## Add trial location to each Fieldbook
  
  r1$location <- "Baraka"
  r2$location <- "Kibirichia"
  r3$location <- "Kisima"
  r4$location <- "Limuru"
  r5$location <- "Narok"
  
  
  ## Combine the five trials
  
  d <- carobiner::bindr(
    r1,
    r2,
    r3,
    r4,
    r5
  )
  
  
  ## Basic trial information
  
  d$country <- "Kenya"
  
  d$crop <- "potato"
  
  d$crop_rotation <- "potato"
  
  d$variety <- as.character(d$INSTN)
  
  d$trial_id <- match(
    d$location,
    c(
      "Baraka",
      "Kibirichia",
      "Kisima",
      "Limuru",
      "Narok"
    )
  )
  
  d$trial_id <- as.character(d$trial_id)
  
  d$plot_id <- as.character(d$PLOT)
  
  d$rep <- as.integer(d$REP)
  
  d$design <- "randomized complete block design"
  
  d$on_farm <- FALSE
  
  d$is_survey <- FALSE
  
  d$irrigated <- NA
  
  
  ## Geographic information from Minimal sheets
  
  get_minimal_value <- function(x, factor) {
    
    z <- x[x[["Factor"]] == factor, "Value"]
    
    if (length(z) == 0) {
      return(NA_character_)
    }
    
    as.character(z[1])
  }
  
  
  min_location <- c(
    get_minimal_value(min1, "Locality"),
    get_minimal_value(min2, "Locality"),
    get_minimal_value(min3, "Locality"),
    get_minimal_value(min4, "Locality"),
    get_minimal_value(min5, "Locality")
  )
  
  min_latitude <- c(
    get_minimal_value(min1, "Latitude"),
    get_minimal_value(min2, "Latitude"),
    get_minimal_value(min3, "Latitude"),
    get_minimal_value(min4, "Latitude"),
    get_minimal_value(min5, "Latitude")
  )
  
  min_longitude <- c(
    get_minimal_value(min1, "Longitude"),
    get_minimal_value(min2, "Longitude"),
    get_minimal_value(min3, "Longitude"),
    get_minimal_value(min4, "Longitude"),
    get_minimal_value(min5, "Longitude")
  )
  
  
  ## Match the geographic information to each trial
  
  location_index <- match(
    d$location,
    c(
      "Baraka",
      "Kibirichia",
      "Kisima",
      "Limuru",
      "Narok"
    )
  )
  
  d$latitude <- as.numeric(
    min_latitude[location_index]
  )
  
  d$longitude <- as.numeric(
    min_longitude[location_index]
  )
  
  d$geo_from_source <- TRUE
  
  
  ## Trial dates
  
  d$planting_date <- "2009-10"
  
  d$harvest_date <- "2010-02"
  
  
  ## Fertilizer and management information
  
  # No standardized fertilizer application information was identified
  # from the Fieldbook variables used for the trial-level dataset.
  
  d$P_fertilizer <- NA_real_
  d$K_fertilizer <- NA_real_
  d$N_fertilizer <- NA_real_
  d$S_fertilizer <- NA_real_
  d$lime <- NA_real_
  
  d$fertilizer_type <- NA_character_
  
  
  ## Inoculation
  
  d$inoculated <- NA
  
  d$inoculant <- NA_character_
  
  
  ## Yield
  
  # MTYA is the adjusted marketable tuber yield.
  # The Fieldbook reports this as tonnes/ha, therefore it is
  # converted to kg/ha for the standardized CAROB yield variable.
  
  d$yield_marketable <- suppressWarnings(
    as.numeric(d$MTYA) * 1000
  )
  
  d$yield_part <- "tubers"
  d$yield <- NA_real_ 
  
  d$yield_moisture <- NA_real_
  
  d$yield_isfresh <- NA
  
  d$fwy_storage <- NA_real_
  
  d$dmy_storage <- NA_real_
  
  d$dmy_total <- NA_real_
  
  
  ## Standardize Fieldbook variables
  
  d$number_tubers_planted <-
    suppressWarnings(as.numeric(d$NTP))
  
  d$number_plants_emerged <-
    suppressWarnings(as.numeric(d$NPE))
  
  d$percentage_plants_emerged <-
    suppressWarnings(as.numeric(d$PPE))
  
  d$number_plants_harvested <-
    suppressWarnings(as.numeric(d$NPH))
  
  d$percentage_plants_harvested <-
    suppressWarnings(as.numeric(d$PPH))
  
  d$number_marketable_tubers_plot <-
    suppressWarnings(as.numeric(d$NMTP))
  
  d$number_marketable_tubers_plant <-
    suppressWarnings(as.numeric(d$NMTPL))
  
  d$marketable_tuber_weight_plot <-
    suppressWarnings(as.numeric(d$MTWP))
  
  d$marketable_tuber_weight_plant <-
    suppressWarnings(as.numeric(d$MTWPL))
  
  d$marketable_tuber_yield_not_adjusted <-
    suppressWarnings(as.numeric(d$MTYNA))
  
  d$marketable_tuber_yield_adjusted <-
    suppressWarnings(as.numeric(d$MTYA))
  
  d$average_marketable_tuber_weight <-
    suppressWarnings(as.numeric(d$ATMW))
  
  
  ## Keep only standardized CAROB variables
  
  keep <- c(
    "trial_id",
    "country",
    "location",
    "crop",
    "crop_rotation",
    "plot_id",
    "rep",
    "variety",
    "design",
    "on_farm",
    "is_survey",
    "irrigated",
    "longitude",
    "latitude",
    "geo_from_source",
    "planting_date",
    "harvest_date",
    "P_fertilizer",
    "K_fertilizer",
    "N_fertilizer",
    "S_fertilizer",
    "lime",
    "fertilizer_type",
    "inoculated",
    "inoculant",
    "yield",
    "yield_marketable",
    "yield_part",
    "yield_moisture",
    "yield_isfresh",
    "fwy_storage",
    "dmy_storage",
    "dmy_total",
    "number_tubers_planted",
    "number_plants_emerged",
    "percentage_plants_emerged",
    "number_plants_harvested",
    "percentage_plants_harvested",
    "number_marketable_tubers_plot",
    "number_marketable_tubers_plant",
    "marketable_tuber_weight_plot",
    "marketable_tuber_weight_plant",
    "marketable_tuber_yield_not_adjusted",
    "marketable_tuber_yield_adjusted",
    "average_marketable_tuber_weight"
  )
  
  d <- d[, keep]
  
  
  ## Metadata
  
  meta$data_type <- "experiment"
  
  meta$design <- "randomized complete block design"
  
  meta$treatment_vars <- "variety"
  
  meta$response_vars <- paste(
    c(
      "yield",
      "number_tubers_planted",
      "number_plants_emerged",
      "percentage_plants_emerged",
      "number_plants_harvested",
      "percentage_plants_harvested",
      "number_marketable_tubers_plot",
      "number_marketable_tubers_plant",
      "marketable_tuber_weight_plot",
      "marketable_tuber_weight_plant",
      "marketable_tuber_yield_not_adjusted",
      "marketable_tuber_yield_adjusted",
      "average_marketable_tuber_weight"
    ),
    collapse = ";"
  )
  
  
  meta$notes <- paste(
    "Potato NPT trials conducted in Kenya in October 2009.",
    "Five trial locations are represented: Baraka, Kibirichia,",
    "Kisima, Limuru and Narok.",
    "The trials used eight potato clones and four replications",
    "under a randomized complete block design.",
    "Geographic information was obtained from the source Minimal",
    "sheets.",
    "MTYA is the adjusted marketable tuber yield and is converted",
    "from tonnes per hectare to kg per hectare for the standardized",
    "yield variable.",
    "Spreadsheet error values such as #N/D and #DIV/0! are treated",
    "as missing values during numeric conversion."
  )
  
  
  meta$carob_contributor <- "MARYAM YAHYA"
  
  meta$carob_date <- "2026-08-24"
  
  meta$carob_completion <- 100
  
  meta$carob_effort <- 1
  
  
  ## Write CAROB files
  
  carobiner::write_files(path, meta, d)
}