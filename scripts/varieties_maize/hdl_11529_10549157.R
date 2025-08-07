# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Data has 2 sheets, namely "Data" and "Subdataset".I worked on the two separately then bound them but seems there are duplicates, so i removed the duplicates.
# 2. NA's are arising from the fertilizer variables, in which no fertilizer was applied.
# 3. Used year of the experiments as no planting dates were explicitly stated.


carob_script <- function(path){
  
  
  "Every year, the MasAgro maize component of the Sustainable Modernization of Traditional Agriculture project (MasAgro) established a seed evaluation network in the three mega-environments of maize production in Mexico (Subtropic, Tropic and Highlands) to evaluate all materials developed by CIMMYT breeders for MasAgro, and assess their performance vis-à-vis public and private materials under development or readily available in the market. This dataset contains grain yield (GY) information (N=897) of subtropical, tropical and highlands’ white (n=582) and yellow (n=315) hybrids evaluated from 2011 to 2019. Yield data were previously processed and analyzed by CIMMYT's Subtropical, Tropical and Highlands maize breeding programmes. Open-pollinated varieties (OPVs) were excluded to compare GY of only hybrids. Hybrids were classified into five categories based on the entity responsible for their development or the proprietary holder. These categories are: CIMMYT MasAgro (hybrids developed by CIMMYT for MasAgro, Public (hybrids developed by public NARS), Private national (hybrids developed by national companies using proprietary germplasm); and Multinational (hybrids developed by multinational companies). The dataset is used to compare GY of MasAgro, public, private national and multinational hybrids across all years (2011-2019), for each colour – yellow and white maize – and mega-environment – subtropic, tropics and highlands.
"
  
  ## Identifiers
  uri <- "hdl:11529/10549157"
  group <- "maize"
  
  ## Download data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ## metadata 
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
                                  data_organization = "CIMMYT",
                                  publication = NA,
                                  project = "MasAgro",
                                  data_type = "on-farm experiment",
                                  treatment_vars = "variety_type",
                                  response_vars = "yield", 
                                  completion = 100,
                                  carob_contributor = "Blessing Dzuda",
                                  carob_date = "2025-08-05",
                                  notes =NA, 
                                  design = NA
  )
  
  ## read data 
  
  f <- ff[basename(ff) == "MasAgro_maize_seed_evaluation_network_2011-2019 yields.xlsx"]
  r <- read_excel(f, sheet="Data")
  r2<- read_excel(f, sheet="Subdataset n=341")
  
  d1 <- data.frame(
    country = "Mexico",
    adm1="Texcoco",
    adm2="El Batan",
    locality=r$mega_env,
    variety_code=r$var_code,
    planting_date=r$yr,
    crop="maize",
    yield_part="grain",
    yield=r$GY*1000,
    elevation ="2200")
  
  d2 <- data.frame(
    country = "Mexico",
    adm1="Texcoco",
    adm2="El Batan",
    locality=r2$mega_env,
    variety_code=r2$var_code,
    planting_date=r2$yr,
    crop="maize",
    yield_part="grain",
    yield=r2$GY*1000,
    elevation ="2200")
  
  #combining the two
  d<- rbind(d1,d2)
  
  #removing duplicates from combined dataframes
  d <- d[!duplicated(d), ]
  
  
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$longitude <- "-100.8415"
  d$latitude <- "20.8242"
  d$geo_from_source <- FALSE
  d$P_fertilizer <- NA
  d$K_fertilizer <- NA
  d$N_fertilizer <- NA
  d$S_fertilizer <- NA
  d$lime <- NA
  d$trial_id <- ifelse(d$locality == "Highlands", 1,
                       ifelse(d$locality == "Subtropic", 2,
                              ifelse(d$locality == "Tropic", 3, NA)))
  
  #correcting bad data types
  d$planting_date <- as.character(as.Date(paste0(d$planting_date, "-01-01")))
  d$elevation <- as.numeric(d$elevation)
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)
  d$trial_id <- as.character(as.integer(as.factor(d$trial_id)))
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  
  # ending the script
  carobiner::write_files(path, meta, d)
}

