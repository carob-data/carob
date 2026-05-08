# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. Higher yield recorded for beans, probably as a result of both grain and pod weight
carob_script <- function(path) {-

  "56 bean accessions were planted in a randomised block design, with three replications. All blocks received the same treatment. The block sizes were 2m by 2m. The phenotypic data was collected using the descriptors from the International Board for Plant Genetic Resources. (2019-07-01)"
  
  uri <- "doi:10.7910/DVN/4W2G0I"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
   data_organization = "NARO",
   publication = NA,
   project = NA,
   data_type = "on-station experiment",
   treatment_vars = "variety",
   response_vars = "yield", 
   completion = 100,
   carob_contributor = "Blessing Dzuda",
   carob_date = "2026-05-03",
   notes = "NA", 
   design = "Randomised Block"
  )
  
  f <- ff[basename(ff) == "Hoima beans onstation.xlsx"]
  
  #fixing column names first
  f1 <- carobiner::read.excel(
    f, n_max = 2, col_names = FALSE)
  
  col_names <- apply(f1, 2, function(x) {
    x <- x[!is.na(x) & x != ""]
    paste(x, collapse = "_")})
  
  #readng actual data
  r <- carobiner::read.excel(f, skip = 3, col_names = FALSE)
  
  names(r) <- col_names
  
  r <- r[-c(1,2), ]

  
  d <- data.frame(
    country = "Uganda",
    adm2="Hoima",
    planting_date="2019",
    latitude=1.43,
    longitude=31.35,
    crop="common bean",
    treatment=r$`Name of variety`,
    variety=r$`Name of variety`,
    emergence_days=as.integer(r$`Days from sowing_to germination`),
    flowering_days=as.integer(r$`Days from sowing_to flowering`),
    seed_weight=as.numeric(r$`100% seed_weight (g)`),
    yield=as.numeric(r$Total_Kgs)*2500,#converting to kg/ha since weight recorded was from 2m by 2m plots
    yield_part="grain")
    
    

    d$trial_id <- paste(d$adm2, as.character(d$planting_date), sep = "_")
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <- FALSE
    d$geo_from_source <- FALSE
    d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
    d$yield_moisture <- as.numeric(NA)

  carobiner::write_files(path, meta, d)
}