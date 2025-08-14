# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. Name of University not available in the values_organisations.
#2. there are 2 entries with NA's throughout, not sure how to proceed

carob_script <- function(path) {
  
  
  "Evaluating sustainable cropping systems: A comparative study of agricultural practices in Central Malawi
  
  This database contains grain, biomasss and nutritional yield data from on-farm trials in Kasungu, Mchinji, and Lilongwe districts of central Malawi, conducted over three cropping seasons (2014–15 to 2016-17). The trials tested sustainable and resilient cropping systems, including Conservation Agriculture (CA) with minimum tillage, glyphosate herbicide, and maize-legume rotations, compared to conventional ridge-and-furrow sole maize (True farmer practice). The experiments were conducted on 24 farms, with each farm serving as one replicate. Data collected included grain yield, biomass, protein, and energy yields, providing insights into system performance and sustainability."
  
  
  ## Identifiers
  uri <- "doi:10.71682/10549300"
  group <- "agronomy"
  
  ## Download data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ## metadata 
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
                                  data_organization = "CIMMYT:Hebei Agriculture University",
                                  publication = "doi.org/10.1016/j.fcr.2024.109565",
                                  project = NA,
                                  data_type = "on-farm experiment",
                                  treatment_vars = "land_prep_method;crop_rotation",
                                  response_vars = "yield;dmy_total;grain_protein", 
                                  completion = 100,
                                  carob_contributor = "Blessing Dzuda",
                                  carob_date = "2025-08-14",
                                  notes = NA, 
                                  design ="Randomized Block"
  )
  
  ## read data 
  f <- ff[basename(ff) == "grain_and_nutritional_yields_v00.xlsx"]
  r <- read_excel(f, sheet ="Rotation.Data")

  
  #converting season years to planting dates
  r$year <- gsub("2014-15","2014",r$year)
  r$year <- gsub("2015-16","2015",r$year)
  r$year <- gsub("2016-17","2016",r$year)
  
  ## select the variables of interest and assign them to the correct name
  d <- data.frame(
    country = "Malawi",
    adm1="Central Region",
    adm2=r$district,
    planting_date=as.character(as.Date(paste(r$year, "01", "01", sep = "-"))),
    season="wet",
    treatment=r$treatment_name,
    plot_id=r$plot_id,
    land_prep_method=r$tillage,
    crop=tolower(r$crop),
    plant_density=as.numeric(r$plant_population),
    yield=as.numeric(r$grain_yield),
    dmy_total=as.numeric(r$biomass),
    grain_protein=(as.numeric(r$crop_protein)/as.numeric(r$grain_yield))*100,
    yield_part="grain",
    crop_rotation=r$treatment_name
  )
  
  coords <- data.frame(
    adm2 = c("Kasungu", "Mchinji", "Mitundu"),
    longitude = c(33.48333, 32.88019, 33.77885),
    latitude = c(-13.03333, -13.79841, -14.24695),
    elevation= c(1048, 1182, 1207)
  )
  
  #merging with the main data
  d <- d %>%
    left_join(coords, by = "adm2")
  
  #correcting values in crop rotation
  d$crop_rotation <- recode(d$crop_rotation,
                            "CA_Maize_Cowpea_Rotation" = "maize;cowpea",
                            "CA_Maize_Groundnut_Rotation" = "maize;groundnut",
                            "CA_Maize_Soybean_Rotation" = "maize;soybean",
                            "CA_Sole_Maize_Herbicide" = "maize",
                            "CA_Sole_Maize_No_Herbicide"= "maize",
                            "True_Farmer_practice"= "maize")
  varieties <- c(
    maize = "MH26",
    soyabean = "Nasoko",
    groundnut = "Chitala groundnut7",
    cowpea = "IT82E16"
  )
  
  d$variety <- varieties[d$crop]
  d$land_prep_method<- gsub("CA","none",d$land_prep_method)
  d$land_prep_method<- gsub("CP","conventional",d$land_prep_method)
  d$crop<- gsub("soyabean","soybean",d$crop)
  d$trial_id <- as.character(paste0(d$adm2,"_",d$planting_date))
  
  d$on_farm <-TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$geo_from_source <-FALSE
  d$inoculated <- FALSE
  d$yield_moisture <- 12.5
  d$P_fertilizer <- 21/2.29
  d$K_fertilizer <- as.numeric(NA)
  d$N_fertilizer <- 92
  d$S_fertilizer <-  4
  d$herbicide_used <- TRUE
  d$herbicide_product <- "glyphosate"
  d$herbicide_amount <- 3
  d$weeding_implement <- "hoe"
  d$weeding_times <- as.integer(3)
  d$row_spacing <- 75
  d$plant_spacing <- 25
  d$pest_species <- "Busseola fusca;Phyllophaga spp."
  
  #removing duplicates
  d <- d[!duplicated(d), ]
   
  # all scripts must end like this
  carobiner::write_files(path, meta, d)
}
