# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. In the "Abbreviation" column from the original dataset, there are values called "NT and TR". It is not clear on what they mean because a single entry cannot be Conventional and No Tillage at the same time as it contradicts each other. 
# 2. No specific experimental site was mentioned, therefore coodinates are for Mexico.
# 3. 
carob_script <- function(path) {
  
  ## when done, remove all the default comments, such as this one, from the script
  ## only keep the comments you added that are specific to this dataset
  
  
  "This dataset includes experimental data collected from a long-term field experiment conducted over 20 years (1999-2019) in a semiarid region of central Mexico. The study focused on evaluating the effects of various soil management practices—including the use of permanent beds, crop residue management, and tied ridges—on the yield and profitability of maize (Zea mays L.) and wheat (Triticum aestivum L.) under rainfed conditions. The dataset includes: Grain yield data for maize and wheat, Biomass production measurements, Information on soil management practices applied during the experiment."
  
  
  ## Identifiers
  uri <- "hdl:11529/10549127"
  group <- "agronomy"
  
  ## Download data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ## metadata 
  # change the major and minor versions if you see a warning
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
                                  data_organization = "CIMMYT",
                                  publication = NA,
                                  project = NA,
                                  data_type = "on-farm experiment",
                                  treatment_vars = "land_prep_method",
                                  response_vars = "yield", 
                                  completion = 90,
                                  carob_contributor = "Blessing Dzuda",
                                  carob_date = "2025-08-11",
                                  notes = NA,
                                  design = NA
  )
  
  ## read data 
  
  f <- ff[basename(ff) == "DAT-H9Texcoco-1999-2019_V2.xlsx"]
  r <- read_excel(f, sheet = "Data field experiment")
  # or  r <- carobiner::read.excel(f)
  
  ## select the variables of interest and assign them to the correct name
  d <- data.frame(
    country = "Mexico",
    planting_date= as.character(as.Date(paste0(r$Year, "-01-01"))),
    crop=tolower(r$Crop),
    rep=as.integer(r$Repetition),
    treatment=as.character(r$Treatment),
    land_prep_method=r$Till,
    dmy_residue=r$`Biomass (kg/ha)`,
    yield=r$`Yield (kg/ha)`,
    grain_protein=as.numeric(r$`PGr (%)`),
    yield_moisture=12
    )
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)
  d$trial_id <- paste(d$planting_date, d$crop, sep = ",")
  d$yield_part<- "grain"
  d$land_prep_method <- gsub("CB","conventional tilled beds",d$land_prep_method)
  d$land_prep_method <- gsub("PB","permanent beds", d$land_prep_method)
 
  d$on_farm <- FALSE
  d$is_survey <- FALSE 
  d$irrigated <- FALSE
  d$longitude <- -102.57867
  d$latitude <- 23.624813
  d$geo_from_source <- FALSE


  # all scripts must end like this
  carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)