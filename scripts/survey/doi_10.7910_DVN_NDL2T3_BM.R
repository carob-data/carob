# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {
  
  "
This database provides a comprehensive, field-level record of rice management practices currently used by farmers in Nigeria, covering the entire production cycle from land preparation to harvest.
"
  
  uri <- "doi:10.7910/DVN/NDL2T3"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
                                  data_organization = "AfricaRice",
                                  publication = NA,
                                  project = "AfricaRice",
                                  carob_date = "2026-10-11",
                                  carob_effort = 12,
                                  design = NA,
                                  data_type = "survey",
                                  treatment_vars = "NA",
                                  response_vars = "NA", 
                                  carob_contributor = "Bagri Moukaila",
                                  carob_completion = 100,	
                                  notes = NA
  )
  
  
  f <- ff[basename(ff) == "data.xls"]
  
  r <- carobiner::read.excel(f)
  
  
  d <- data.frame(
    country = r$Country,
    adm1 = r$Region,
    adm2 = r$State,
    date = as.character(r$`survey date`),
    latitude = r$Field_latitude,
    longitude = r$Field_longitude,
    field_size = r$`Field size (ha)`,
    season = ifelse(r$Season=="Wet season", "wet", "dry"),
    sex = r$Gender,
    age = r$`Farmer age`,
    education = r$`Farmer education`,
    land_ownedby = r$`Land ownership`,
    yield = r$`Paddy yield (kg/ha)`,
    yield_part = "grain",
    labour = r$`Labor input (person day/ha)`,
    previous_crop_residue_management = r$`Crop residue management`,
    land_prep_method = ifelse(
      r$`Land preparation` == "Yes", "tillage",
      ifelse(
        r$`Land preparation` == "No", "none",
        NA_character_
      )
    ),
    variety = r$`Variety used`,
    seed_source = r$`Seed source`,
    planting_date = format(
      as.Date(r$`planting date`),
      "%Y-%m-%d"
    ),
    seed_rate = r$`Quantity of seed used (kg/ha)`,
    planting_method = ifelse(
      r$`Crop establishement method` == "broadcasting", "broadcasting",
      ifelse(
        r$`Crop establishement method` == "dibbling", "dibbling",
        ifelse(
          r$`Crop establishement method` == "line_seeding", "line sowing",
          ifelse(
              r$`Crop establishement method` == "transplanting","transplanting",
              NA_character_
            )
          )
        )
    ),
    OM_used = ifelse(
      r$`Organic input use` %in% c("no", "No"), FALSE, TRUE),
    weeding_times = as.integer(r$`Number of weeding`),
    insecticide_used = ifelse(
      r$`Insecticide use` %in% c("no", "No"), FALSE, TRUE),
    insecticide_times = as.integer(r$`Number of insecticide application`),
    herbicide_used = ifelse(
      r$`Herbicide use` %in% c("no", "No"), FALSE, TRUE),
    land_prep_implement = ifelse(
      r$`Tillage method` == "2wheel_tractor", "2 wheel tractor",
      ifelse(
        r$`Tillage method` == "4wheel_tractor", "4 wheel tractor",
        ifelse(
          r$`Tillage method` == "animal", "animal",
          ifelse(
            r$`Tillage method` == "manual", "manual",
            ifelse(
              r$`Tillage method` == "mechanical", "chisel plough",
              NA_character_
            )
          )
        )
      )
    ),
    OM_amount = r$`Quantity of organic input applied (kg/ha)`,
    fertilizer_dap = paste0(r$`Days to first fertilizer application`, ',', r$`Days to second fertilizer application`),
    N_fertilizer = r$`Quanity of N applied (kg/ha)`,
    K_fertilizer = r$`Quantity of K2O applied (kg/ha)`*0.8301,
    P_fertilizer = r$`Quantity of P2O5 applied (kg/ha)`*0.4364,
    is_survey = TRUE,
    crop = "rice",
    on_farm = FALSE,
    geo_from_source = TRUE,
    hhid = as.character(r$barcode_household))
   
  
  ### remove duplicate rows
  d <- unique(d)
  
  carobiner::write_files(path, meta, d)
  } 


View(d)

