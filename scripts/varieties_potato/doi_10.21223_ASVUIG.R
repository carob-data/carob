carob_script <- function(path) {
  
  "
  Dataset for: Breeding for resistance to late blight - LBHTC2 population - Phenotypic and Genomic Selection (2020-2021)

  The production of potato (Solanum tuberosum) in farmers' fields is heavily affected by the late blight [Phythophthora infestans (Mont) de Bary] disease. The International Potato Center (CIP) has a breeding population with resistance to late blight, heat tolerance and high yields of tubers called LBHT (Gastelo et al 2015). In 2020-2021 season these 500 clones were sown in intermediate trials in the localities of Oxapampa, Huanuco, 9°48′06″S, 2500 masl and Huancayo, 12º07'S, 3280 masl, under the 30x20 row-column design (John 1989), with two replications of 10 plants each. The late blight control was with two applications of the contact fungicide Mancozeb, up to 100% of the emergence of the plants, the damage by the disease was evaluated from 35 days after sowing to 85 days, with intervals of 7 days, carrying out 7 evaluations, then the rAUDPC values were calculated https://doi.org/10.4160/9789290603917, at harvest the number and weight of commercial and non-commercial tubers were taken. These experiments are within the Excellence platform in Breeding (https://excellenceinbreeding.org/module1), product design, and management module. The data analysis was performed using the mixed model analysis, which makes spatial corrections to obtain the BLUPs (best linear unbiased predictions) of late blight resistance measured through the area under the disease progress curve relative (rAUDPC) and Marketable tuber yield unadjusted yield per hectare (MTYNA). The combined analysis of the three localities was carried out, estimating the predicted values and the selection index of the clones. The combined analysis of the three locations shows us rAUDPC values ranged between 0.000 and 0.263 for tested clones. 81.708% of the clones presented rAUDPC values less than 0.037 (Kory resistant control value), while the susceptible check Yungay had an rAUDPC value of 0.257. MTYNA means varied from 3.85 to 35.73 t/ha, with an average of 20.22 t / ha higher than those of the controls, which varied from 7.29 to 19.55 t/ha. 150 clones with a high level of late blight resistance, tuber yield larger than Yungay's, and good agronomic characteristics (skin color and tuber shape) were selected using the selection index. The rAUDPC values of the selected clones varied from 0.000 to 0.064, with an average of 0.015, the yields under the pressure of the disease ranging between 14.76 and 35.73 t / ha with an average of 25.34 t / ha. These clones will be planted in the 2021-2022 season in four locations in advanced trials.
  "
  
  uri <- "doi:10.21223/ASVUIG"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 5,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "row-column",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "rAUDPC;yield_marketable",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-10",
    carob_completion = 80,
    carob_effort = 2.5
  )
  
  ## Source files
  f1 <- ff[basename(ff) == "01_Combined_genomic_selection.xlsx"]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1)
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = "ASVUIG",
    variety = r1$Genotype,
    variety_pedigree = paste(r1$Female, "x", r1$Male),
    rAUDPC = r1$`PREDICTED   BLUPs rAUDPC`,
    yield_marketable = r1$`PREDICTED   BLUPs MTYNA` * 1000,  # t/ha to kg/ha
    variety_type = r1$Type,         
    location = "Oxapampa; Huanuco; Huancayo",
    latitude = -10.58,
    longitude = -75.40,
    elevation = 2500,
    geo_from_source = FALSE,
    country = "Peru",
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    yield_part = "tubers",
    yield = NA,
    yield_moisture = NA,
    yield_isfresh = NA,
    irrigated = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    planting_date =  "2020",
    harvest_date =  "2021",
    #new variable
    selection_index_ = r1$`Selection Index`,
    selected_ = r1$`Selected by INDEX SELECTION`,
    molecular_marker_ = r1$`MOLECULAR MARKER`,
    snp_st0020_ = r1$`SNP St0020`,
    snp_st0023_ = r1$`SNP St0023`
  )
  
  carobiner::write_files(path, meta, d)
  
}