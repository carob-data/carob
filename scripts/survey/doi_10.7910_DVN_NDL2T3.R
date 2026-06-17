# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
  
"
Current rice management  practices in Nigeria, West Africa

This database provides a comprehensive, field-level record of rice management practices currently used by farmers in Nigeria, covering the entire production cycle from land preparation to harvest. For each plot, it documents land preparation and leveling, number and method of tillage, variety and seed source, planting date and adherence to optimum sowing windows, quantity of seed applied, and crop establishment method. It further details nutrient management including organic inputs, number and timing of inorganic fertilizer applications, and the quantities of nitrogen (N), potassium (K₂O), and phosphorus (P₂O₅) applied as well as weed, pest, and disease control measures (number of weedings, herbicide and insecticide use, number of insecticide, disease, and bird control interventions). Additional variables include crop duration and precise timing of fertilizer operations. This dataset offers a robust foundation for yield-gap analysis, nutrient-use efficiency and management practices assessments aimed at improving rice productivity and sustainability in Nigeria.
"

  uri <- "doi:10.7910/DVN/NDL2T3"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
    
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
     data_organization = "AfricaRice",
     publication = NA,
     project = NA,
     carob_date = "2026-06-15",
     carob_effort = NA,
     design = NA,
     data_type = "survey",
     treatment_vars = "none",
     response_vars = "none", 
     carob_contributor = "Blessing Dzuda",
     carob_completion = 100,	
     notes = NA
  )
  
  f <- ff[basename(ff) == "Data.xls"]
  r <- carobiner::read.excel(f)
  
  d <- data.frame(
    hhid = r$barcode_household,
    date = as.character(r$`survey date`),
    country = r$Country,
    adm1 = r$Region,
    adm2=r$State,
    latitude = r$Field_latitude,
    longitude = r$Field_longitude,
    field_size=r$`Field size (ha)`,
    season = r$Season,
    age=as.numeric(r$`Farmer age`),
    sex=r$`Gender`,
    education=r$`Farmer education`,
    land_ownedby=r$`Land ownership`,
    crop="rice",
    yield=r$`Paddy yield (kg/ha)`,
    previous_crop_residue_management=r$`Crop residue management`,
    variety=r$`Variety used`,
    seed_source=r$`Seed source`,
    planting_date=as.character(r$`planting date`),
    seed_density=r$`Quantity of seed used (kg/ha)`,
    planting_method=r$`Crop establishement method`,
    land_prep_traction=r$`Tillage method`,
    OM_amount=r$`Quantity of organic input applied (kg/ha)`,
    weeding_times=as.integer(r$`Number of weeding`),
    maturity_days=r$`Crop duration`,
    N_fertilizer=r$`Quanity of N applied (kg/ha)`,
    P_fertilizer=r$`Quantity of P2O5 applied (kg/ha)`/2.29,
    K_fertilizer=r$`Quantity of K2O applied (kg/ha)`/1.2051
  )

  d$trial_id <- paste(d$hhid,d$adm2,sep = "_")
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  d$irrigated <- r$`Production system`=="irrigated"
  d$geo_from_source <- TRUE
  d$yield_part <- "grain"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- TRUE
  d$season <- ifelse(d$season=="Wet season","wet","dry")
  d$longitude[d$adm2=="Kano"] <- 11.992
  d$geo_from_source[d$adm2=="Kano"] <- FALSE
  
  d$insecticide_used <- r$`Insecticide use`=="Yes"
  d$herbicide_used <- r$`Herbicide use`=="Yes"
  d$OM_used <- r$`Organic input use`=="yes"
  
  d$insecticide_times <- as.integer(r$`Number of insecticide application`)
  d$farm_labour <- r$`Labor input (person day/ha)` *d$field_size #converting to labour days/year basing on field size
  ## leaving out because not clear what bird control refers to
  ## d$bird_control_times <- r$`Number of bird control` 
  d$fungicide_times <- as.integer(r$`Number of disease control`)
  ## leaving out because it is not clear what that means. Did the farmer say planting was on-time or not?
  ## d$ontime_planting <- r$`Planting at optimum sowing date`
  d$N_splits <- as.integer(r$`Number of inorganic fertilizer application`)
  d$fertilizer_dap <- apply(r[, c("Days to first fertilizer application", "Days to second fertilizer application")], 1, \(i) paste(i, collapse=";"))
  d$land_prep_times <- as.integer(r$`Number of tillage`)
  
  d$planting_method <- gsub("line_seeding","line sowing", d$planting_method)
  d$land_prep_traction <- gsub("4wheel_tractor","4 wheel tractor", d$land_prep_traction)
  d$land_prep_traction <- gsub("2wheel_tractor","2 wheel tractor", d$land_prep_traction)
  
  carobiner::write_files(path, meta, d)
}
