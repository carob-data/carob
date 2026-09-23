
carob_script <- function(path) {

"
Dataset for: Genomic selection for resistance to LB in population LBHTC2  Intermediate Trials

In the 2020-2021 season, these 500 clones were sown in intermediate trials in the localities of Oxapampa, Huanuco, 9°48′06″S, 2500 masl and Huancayo, 12º07'S, 3280 masl, under the 30x20 row-column design (John 1989), with two replications of 10 plants each. The late blight control was with two applications of the contact fungicide Mancozeb, up to 100% of the emergence of the plants, the damage by the disease was evaluated from 35 days after sowing to 85 days, with intervals of 7 days, carrying out 7 evaluations, then the rAUDPC values were calculated https://doi.org/10.4160/9789290603917, at harvest the number and weight of commercial and non-commercial tubers were taken (https: //research.cip.cgiar .org / potato knowledge / yield.php). These experiments are within the Excellence platform in Breeding (https://excellenceinbreeding.org/module1), product design, and management module. The data analysis was performed using the mixed model analysis, which makes spatial corrections to obtain the BLUPs (best linear unbiased predictions) of late blight resistance measured through the area under the disease progress curve relative (rAUDPC) and Marketable tuber yield unadjusted yield per hectare (MTYNA). The combined analysis of the three localities was carried out, estimating the predicted values and the selection indices of the clones. The combined analysis of the three locations shows us rAUDPC values ranged between 0.000 and 0.263 for tested clones. 81.708% of the clones presented rAUDPC values less than 0.037 (Kory resistant control value) (Figure 1), while the susceptible check Yungay had an rAUDPC value of 0.257. MTYNA means varied from 3.85 to 35.73 t / ha, with an average of 20.22 t / ha higher than those of the controls, which varied from 7.29 to 19.55 t / ha. 150 clones with a high level of late blight resistance, tuber yield larger than Yungay's, and good agronomic characteristics (skin color and tuber shape) were selected.  The rAUDPC values of the selected clones varied from 0.000 to 0.064, with an average of 0.015, the yields under the pressure of the disease ranging between 14.76 and 35.73 t / ha with an average of 25.34 t / ha.
"

  uri <- "doi:10.21223/B7HWWH"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 2, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "row-column",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-23",
    carob_completion = 80,
    carob_effort = 3.0
  )
  
  f2 <- ff[basename(ff) == "01_LBHTC2-HUANCAYO 2020-2021_data.xlsx"]
  f5 <- ff[basename(ff) == "02_LBHTC2-HUANCAYO 2020-2021_data.xlsx"]
  f8 <- ff[basename(ff) == "03_LBHTC2-OXAPAMPA 2020-2021_data .xlsx"]
  
  r2 <- carobiner::read.excel(f2)
  r5 <- carobiner::read.excel(f5)
  r8 <- carobiner::read.excel(f8)
  
  ## Add location
  r2$location <- "Huancayo"
  r5$location <- "Huanuco"
  r8$location <- "Oxapampa"
  
  r <- carobiner::bindr(r2, r5, r8)
 
  geo <- data.frame(
    location = c("Huancayo", "Huanuco", "Oxapampa"),
    latitude = c(-12.0651, -9.9306, -10.5775),
    longitude = c(-75.2049, -76.2421, -75.4022),
    geo_from_source = FALSE
  )
  
  d <- data.frame(
    trial_id = paste0("B7HWWH_", r$location),
    plot_id = as.character(r$ID),
    rep = as.integer(r$Rep),
    variety = r$Genotype,
    location = r$location,
    country = "Peru",
    crop = "potato",
    on_farm = FALSE,
    is_survey = FALSE,
    irrigated = NA,
    yield_part = "tubers",
    yield = r$TTYA * 1000,
    yield_marketable = r$MTYA * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA
# fix, see files    planting_date = "2020",
# fix, see files   harvest_date = "2021"
  )
  
  d <- merge(d, geo, by = "location", all.x = TRUE)
 
 # record 324
 d$yield[d$yield == 574494.255] <- 5744.94255
 # record 2488
 d$yield[d$yield == 371329.62] <- 3713.2962
 d$yield_marketable[d$yield_marketable == 3666.63] <- 3666.63

 carobiner::write_files(path, meta, d)
}
