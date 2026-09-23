

carob_script <- function(path) {

"
Dataset for:Environmental impact and stability of potato clones resistant to late blight Phytophthora infestans (Mont) de Bary, resilient to climate change in Peru

Potato cultivation is one of the three most important foods in the world's diet. In Peru, it is the basis of the diet of the high Andean population. This crop is affected by late blight, a disease that can decimate production if not controlled in time. The oomycete (Phytophthora infestans) causing this disease is controlled using fungicides, which affect the environment and human health, another form of control is the use of resistant cultivars. Hence, the objective of this study was to evaluate 30 potato clones from the LBHTC2 population, to select clones with high levels of resistance to this disease, stable for tuber yield, low environmental impact, and high economic profitability. Three experiments were installed in the 2021–2022 agricultural campaign, two experiments with and without late blight control in Oxapampa and Huánuco and one experiment under normal conditions of a potato crop in Huancayo, using randomized complete blocks with three replications. The cultivars Yungay, Amarilis, and Kory were used as controls. Late blight resistance and environmental impact were determined based on experiments with and without control in Huánuco and Oxapampa. Yield stability and economic profitability were evaluated based on information from the three experiments. Fourteen clones with high Late blight resistance were identified, phenotypically stable for tuber yield, with low environmental impact and high economic profitability, superior to control cultivars with phenotypic stability and low genotype-environment interaction. These clones have a high potential for sustainable production systems that allow for reducing environmental impact, increasing economic profitability, and improving producers' living standards
"
  uri <- "doi:10.21223/BW8LWJ"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-22",
    carob_completion = 90,
    carob_effort = 2.5
  )
  
  f1 <- ff[basename(ff) == "01_Experiments without control LB.xlsx"]
  f2 <- ff[basename(ff) == "02_Experiments with LB control.xlsx"]
  f3 <- ff[basename(ff) == "03_Experiments Phenotypic Stabilit.xlsx"]
  
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  
  r1$trial_type <- "without_LB_control"
  r2$trial_type <- "with_LB_control"
  r3$trial_type <- "phenotypic_stability"
  
  r <- carobiner::bindr(r1, r2, r3)
  
  geo <- data.frame(
    location = c("Oxapampa", "Huanuco", "El Mantaro, Junin"),
    latitude = c(-10.5775, -9.9306, -11.8550),
    longitude = c(-75.4022, -76.2421, -75.3233),
    geo_from_source = FALSE
  )
  
  d <- data.frame(
    trial_id = paste0("BW8LWJ_", r$trial_type, "_", r$Site),
    plot_id = as.character(r$Num),
    rep = as.integer(r$Rep),
    variety = r$Clone,
    location = r$Site,
    country = "Peru",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = r$TTY_Ha * 1000,
    yield_marketable =r$MTY_ha * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    AUDPC = r$AUDPC,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    planting_date = "2021",
    harvest_date = "2022"
  )
    
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}
