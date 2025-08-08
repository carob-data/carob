# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. The composition of NPK fertilizer from the dataset has not been specified, cant determine N_fertilizer used in the trial.
#2. Grain moisture content was not mentioned.
#3. 
carob_script <- function(path) {
  
  "Lower yield of wheat in eastern India can be attributed to several input factors i.e., inappropriate crop management practices including other social and environmental factors. Among them, sub-optimal irrigation is one of the important factors. Recommended number of irrigations for optimal wheat harvest is 4-5 depending upon crop growth stages and availability of water. Observations by the project team and a recent survey conducted by CSISA at a landscape-level found that majority of farmers in this part of India apply 2-3 irrigations. Moreover, several studies have suggested to apply irrigation to wheat at its grain filling stage to protect the crop from terminal heat stress thereby safeguarding yield. To validate the effect of number of irrigations and irrigation at the grain-filling stage of wheat, multi-location on-farm trials were conducted continuously over six years starting from 2016-17. Krishi Vigyan Kendras, district-level extension center of national agriculture research and extension system were involved in this process. Ten districts were selected in a way that all agro-climatic zones of this area are covered. The treatment in this trial was application of additional irrigation at maturity/grain-filling stage of wheat crop against general farmer practice. Number of irrigations with farmers vary from 1 to 4, so our treatments included 1+1, 2+1, 3+1, and 4+1 irrigations. We applied these treatments in two sets of wheat crop establishment i.e., zero-tillage and conventional tillage methods. There is asymmetry in distribution of samples within treatments and over years. That happened as trial was in farmer’s participatory mode and numbers were dependent completely on willingness of farmers to participate. Altogether, the trial was conducted at 1810 sites, and we captured 63 variables including yield and yield attributing traits. (2023-08-17)"
  
  
  ## Identifiers
  uri <- "hdl:11529/10548945"
  group <- "varieties_wheat"
  
  ## Download data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ## metadata 
  # change the major and minor versions if you see a warning
  meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
                                  # include the data provider and/or all institutes listed as authors (if any)
                                  data_organization = "CIMMYT;IRRI;CU",
                                  publication =NA,
                                  project = "CSISA",
                                  data_type = "on-farm experiment",
                                  treatment_vars = "irrigation_number;land_prep_method",
                                  response_vars = "yield", 
                                  completion = 100,
                                  carob_contributor = "Blessing Dzuda",
                                  carob_date = "2025-08-07",
                                  notes = NA, 
                                  design = NA
  )
  
  ## read data 
  
  f <- ff[basename(ff) == "CSISA_IND_Irrigation Trial_Data_2017-22_Final.csv"]
  #r <- read_csv(f, locale = locale(encoding = "UTF-8"))
  r <- read_csv(f, locale = locale(encoding = "Latin1"))
  #fixing the column values
  r[[62]] <- gsub("\u00A0", "", r[[62]])
  r[[62]] <- as.numeric(r[[62]])
  
  #converting planting seasons to years
  r$Year <- gsub("2016-17","2016",r$Year)
  r$Year <- gsub("2017-18","2017",r$Year)
  r$Year <- gsub("2018-19","2018",r$Year)
  r$Year <- gsub("2019-20","2019",r$Year)
  r$Year <- gsub("2020-21","2020",r$Year)
  r$Year <- gsub("2021-22","2021",r$Year)
  
  ## select the variables of interest and assign them to the correct name
  d <- data.frame(
    year=r$Year,
    country="India",
    adm1=r$State,
    adm2=r$District,
    adm3=r$Village,
    adm4=r$Block,
    latitude=r$Latitude,
    longitude=r$Longitude,
    soil_type=r$SoilType,
    previous_crop=tolower(r$PreviousCrop),
    land_prep_method=r$CropEstablishment,
    variety=r$Variety,
    seed_rate=r$SeedRate,
    planting_date=as.character(as.Date(r$SowingDate, format = "%Y.%m.%d")),
    irrigation_number=as.integer(r$IrrigationNumber),
    herbicide_products=r$HerbicideName,
    herbicide_amount=as.numeric(r$HerbicideDose)*0.00112,
    herbicide_dates=as.character(as.Date(r$HerbicideDate, format = "%Y.%m.%d")),
    weeding_implement=ifelse(r$ManualWeeding=="Yes","manual","none"),
    harvest_date=as.character(as.Date(r$HarvestDate, format = "%Y.%m.%d")),
    dmy_total=r$BiomassYield*1000,
    yield=r$GrainYield*1000,
    yield_moisture=0,
    seed_weight=r$TestWeight,
    crop="wheat",
    yield_part="grain"
  )
    d$trial_id=paste0(d$adm4, "_", d$year)

  ## about the data (TRUE/FALSE)
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <-TRUE
    d$herbicide_used<-TRUE
    d$weeding_done <- TRUE
    d$geo_from_source <- TRUE
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)

    herbicide_map <- c(
      "Carfentrazone Ethyl 40DF \\(Affinity\\)"                      = "carfentrazone-ethyl",
      "Sulfosulfuron\\+Metsulfuron"                                  = "sulfosulfuron+metsulfuron",
      "Metsulfuron\\+Sulfosulfuron \\(Total\\)"                      = "metsulfuron+sulfosulfuron",
      "Sulfosulfuron 75% WG \\(Leader\\), Carfentrazone Ethyl 40DF \\(Affinity\\)" = "sulfosulfuron+carfentrazone-ethyl",
      "Metsulfuron\\+Sulphosulfuron \\(Total\\)\\+Carfentrazone Ethyl 40DF \\(Affinity\\)" = "metsulfuron+sulphosulfuron+carfentrazone-ethyl",
      "Carfentrazone\\+Sulphosulfuron 45% \\(Broadway\\)"            = "carfentrazone+sulphosulfuron",
      "Clodinafop 15 W\\.P\\. \\(Topik\\)\\+Metsulfuron 20 W\\.P\\. \\(Algrip\\)" = "clodinafop+metsulfuron",
      "Sulfosulfuron 75% WG \\(Leader\\)"                            = "sulfosulfuron",
      "Sulphosulfuron 75% \\(Leader\\)\\+Carfentrazone Ethyl 40DF \\(Affinity\\)" = "sulphosulfuron+carfentrazone-ethyl",
      "2,4-dichlorophenoxyacetic acid \\(2,4-D\\)"                   = "2,4-D",
      "Clodinafop Propargyl 15% \\+ Metsulfuron Methyl 1% WP \\(Vesta\\)" = "clodinafop+metsulfuron",
      "2,4-Dichlorophenoxyacetic acid \\(2,4-D\\) \\(EC\\)"           = "2,4-D",
      "Metsulfuron 20 W\\.P\\. \\(Algrip\\)"                         = "metsulfuron",
      "Carfentrazone \\(Naboodh\\)"                                  = "carfentrazone",
      "Clodinofop 15w\\.p\\. \\(Topic\\)"                            = "clodinafop",
      "Sulphosulfuron \\(Total\\)"                                   = "sulphosulfuron",
      "Metsulfuron"                                                  = "metsulfuron",
      "Sulphosulfuron 75% \\(Leader\\)"                              = "sulphosulfuron",
      "2,4-Dichlorophenoxyacetic acid \\(2,4-D\\) \\(EC\\)"          = "2,4-D"
    )
    
    
    
    d$herbicide_product <- str_replace_all(d$herbicide_products, herbicide_map)
    d$previous_crop <- gsub("paddy|fallow","none", d$previous_crop)
    d$land_prep_method <- gsub("CT","conventional",d$land_prep_method)
    d$land_prep_method <- gsub("ZT","none",d$land_prep_method)
    d$herbicide_product <- gsub("\\+", ",", d$herbicide_product)
    d$herbicide_products <- NULL
    
  # all scripts must end like this
  carobiner::write_files(path, meta, d)
}

