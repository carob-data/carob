
carob_script <- function(path) {

"
Dataset for: SET 2: Participatory Varietal Selection of Late Blight Resistant Potato Clones

Experiments were installed in Cajamarca, with the objective of identifying clones with high potential for being varieties applying the Participatory Varietal Selection methodology. For the period 2016-2017, 18 clones with high resistance to late blight were planted, belonging to the B population and developed in the International Potato Center together with Two control varieties, Amarilis and Yungay (susceptible). Finally, in the harvest 5 clones with high yield, low glycoalkaloid content and good organoleptic quality were selected as a result of the Participatory Variety Selection of the farmers and the analysis of mixed models and BLUPs for the yield data. The 5 selected clones were planted again in the period 2017-2018 and through the Participatory Varietal Selection, three promising clones were selected (CIP308488.92, CIP308495.227 and CIP308478.59).
"
  uri <- "doi:10.21223/UGHHPK"
  group <- "varieties_potato"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 2,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-10-19",
    carob_completion = 80,
     carob_effort = 3.0
  )
  
  f1 <- ff[basename(ff) == "PTPVS122016_CHAQUIL_exp1.xlsx"]
  f2 <- ff[basename(ff) == "PTPVS122016_CHUCMAR_exp2.xlsx"]
  f3 <- ff[basename(ff) == "PTPVS122017_CHUCMAR_exp3.xlsx"]
  
  r1 <- carobiner::read.excel(f1, sheet = "F4_harvest_mother")
  r2 <- carobiner::read.excel(f2, sheet = "F4_harvest_mother")
  r3 <- carobiner::read.excel(f3, sheet = "F4_harvest_mother")
  
  r1$trial_id <- "UGHHPK_Chaquil_2016"
  r2$trial_id <- "UGHHPK_Chucmar_2016"
  r3$trial_id <- "UGHHPK_Chucmar_2017"
  
  r <- carobiner::bindr(r1, r2, r3)
  
  geo <- data.frame(
    trial_id = c("UGHHPK_Chaquil_2016", "UGHHPK_Chucmar_2016", "UGHHPK_Chucmar_2017"),
    country = "Peru",
    adm1 = "Cajamarca",
    adm2 = c("Cajamarca", "Chota", "Chota"),
    adm3 = c("Banos del Inca", "Tacabamba", "Tacabamba"),
    location = c("Chaquil", "Chucmar", "Chucmar"),
    latitude = c(-7.12917, -6.41389, -6.41389),
    longitude = c(-78.3575, -78.5525, -78.5525),
    elevation = c(2981, 3058, 3058),
    geo_from_source = TRUE,
    planting_date = c("2016-12-09", "2016-12-20", "2017-12-05"),
    harvest_date = c("2017-04-21", "2017-05-09", "2018-05-15"),
    plot_area = c(3, 3, 6),
    fungicide_used = c(TRUE, TRUE, NA)
  )
  
  r <- merge(r, geo, by = "trial_id", all.x = TRUE)
  
  d <- data.frame(
    trial_id = r$trial_id,
    plot_id = as.character(r$PLOT),
    rep = as.integer(r$REP),
    variety = r$INSTN,
    crop = "potato",
    on_farm = NA,
    is_survey = FALSE,
    irrigated = NA,
    seed_density = r$NTP / r$plot_area * 10000,
    plant_density = r$NPH / r$plot_area * 10000,
    yield_part = "tubers",
    yield = r$TTWP / r$plot_area * 10000,
    yield_marketable = r$MTWP / r$plot_area * 10000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
  )
  
  d$plant_density[d$plant_density == 0] <- NA
  
  d <- merge(d, geo, by = "trial_id", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}