
carob_script <- function(path) {
  
  "
  Dataset for: Releasing at least one new potato variety in the highlands of Peru, focusing on quality for French fries and high level of resistance to late blight using participatory varietal selection during period from 2020-2021 in Cajamarca and Huamachuco

  During the 2020-2021 season, 8 potato clones selected in the previous 2019-2020 season were evaluated, previously selected for their high levels of resistance to late blight and their excellent quality for french fries tested under high and low-temperature conditions (important conditions for the content of reducing sugars such as glucose and fructose, which cause the dark color in frying) were used together with two control varieties Canchan and Única, widely adopted by farmers and final consumers. The experiments were planted using tuber seeds from in vitro (basic) plants, in 2 locations in the north of Peru using the statistical design of randomized complete blocks with three replications of 150 plants each. The fertilization rate was 200-220-180 NPK per hectare, using as sources ammonium nitrate 33% N; di-ammonium phosphate 46% P2O5, 18% N; and potassium sulfate 50% K2O. Pest and disease control was carried out in a timely and adequate manner. In all experiments, late blight control was carried out on Canchan and Unica varieties planted as susceptible controls. Clone selection was planned to be carried out using the Participatory Varietal Selection (PVS) methodology, at flowering, harvest, and post-harvest stages. At harvest, the number and weight of marketable and unmarketable tubers per plot were recorded, then the tuber yield per hectare in t/ha was calculated, tuber samples were taken to determine the dry matter content using the hydrometer method and the dry weight/fresh weight, The tubers were also stored at room temperature (15-16oC) for frying after three months to see if they maintain their frying quality. The frying quality of the potato chips was evaluated based on the frying color, using the scale in grades from 1 to 5, developed by the Potato chip- 'Snack Food Association' (www.sfa.org), the color grade of the selected clones should be 1 or 2. Three clones were selected as potential varieties with resistance to late blight, quality for french fries and / or baked. These clones were selected based on their high yield, good quality for frying, low content of reducing sugars, high content of dry matter, and information from the PVS methodology.
  "
  
  uri <- "doi:10.21223/H8E8KL"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 2,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-07",
    carob_completion = 85,
    carob_effort = 3.5
)
  
  ## Source files
  f1 <- ff[basename(ff) == "01_PTYield112020_CAJ_exp1_data.xlsx"]
  f2 <- ff[basename(ff) == "02_PTYield112020_HCHO_exp2_data.xlsx"]
  f3 <- ff[basename(ff) == "03_PTYield112020_HCHO_exp3_data.xlsx"]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  
  ## Add location
  r1$location <- "Cajamarca"
  r2$location <- "Huamachuco"
  r3$location <- "Huamachuco"
  
  ## Add trial ID for Huamachuco (two separate experiments)
  r2$trial <- "exp2"
  r3$trial <- "exp3"
  
  ## Combine
  r <- carobiner::bindr(r1, r2, r3)
  
  ## Create trial_id with location and experiment
  r$trial_id <- ifelse(is.na(r$trial), 
     paste0("H8E8KL_", r$location),
     paste0("H8E8KL_", r$location, "_", r$trial))

  ## Coordinates estimated from Google Maps (September 2026)
  geo <- data.frame(
    location = c("Cajamarca", "Huamachuco"),
    latitude = c(-7.1638, -7.8133),
    longitude = c(-78.5000, -78.0500),
    geo_source = "Google Maps",
    geo_from_source = FALSE
  )
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = r$trial_id,
    plot_id = as.character(r$PLOT),
    rep = as.integer(r$REP),
    variety = r$INSTN,
    location = r$location,
    country = "Peru",
    crop = "potato",
    crop_rotation = NA,
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = r$TTYA * 1000,
    yield_marketable = r$MTYA * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    planting_date = "2020",
    harvest_date = "2021",
    N_fertilizer = 200,
    P_fertilizer = 220 / 2.29,
    K_fertilizer = 180 / 1.2051,
    fertilizer_type = NA_character_,
    lime = NA_real_,
    soil_texture = NA_character_,
    elevation = NA_real_
  )
  
  ## Merge coordinates
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}
