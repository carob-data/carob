
carob_script <- function(path) {

"
Replication Data for: Phenotypic Stability and Correlation for Late Blight Resistance in Advanced Potato Clones Under Field and Controlled Conditions

Late blight (LB) is the main potato disease worldwide and one of the most important ways to control it is the use of resistant varieties. Twenty-two potato clones belong to the B3 breeding population developed by the International Potato Center (CIP) with high resistance to the disease and two susceptible controls were inoculated with four Peruvian complex isolates (POX67, PPA61, PLL69, and PPI112) of Phytophthora infestans, with complex virulence on potato. Whole plant inoculation assays were carried out under greenhouse and humid chamber conditions in Lima, Peru, and data obtained were correlated with data from field assays carried out in Oxapampa (Pasco), a CIP breeding site in the Peruvian rain forest. High significant correlations (α = 0.01) were found in the resistance to LB shown by potato clones, the values of the correlations under greenhouse conditions between the isolates POX67, PPA61, and PLL69 with the resistance in the field were r = 0.93, 0.92 and 0.80, respectively and under humid chamber conditions were r = 0.94, 0.93 and 0.94, respectively. Moderate correlations were found between PPI112 insulation with resistance in the field, in greenhouse (r = 0.69) and in humid chamber conditions (r = 0.77). The twenty-four clones tested in this study showed phenotypic stability for LB resistance according to non-parametric analysis.
"
  uri <- "doi:10.21223/JPWA9G"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "lb_resistance_",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-24",
    carob_completion = 90,
    carob_effort = 2.0
  )
  
  f1 <- ff[basename(ff) == "Table 1.- Potato clones used in the phenotypic stability and correlation study.xlsx"]
  
  r1 <- carobiner::read.excel(f1)
  
  d <- data.frame(
    trial_id = "JPWA9G",
    variety = r1$Clone,
    variety_pedigree = paste(r1$"Female Parent", r1$"Male Parent", sep = " x "),
    variety_type = r1$Group,
    location = "Oxapampa",
    country = "Peru",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield_moisture = NA,
    yield_isfresh=NA,
    planting_date = NA,
    harvest_date = NA,
    latitude = -10.5775,
    longitude = -75.4022,
    geo_from_source = FALSE,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer=NA,
    #New variable
    lb_resistance_ = r1$"Resistance to Late Blight"
    
  )
  
  carobiner::write_files(path, meta, d)
}