# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"Context or problem: The associations among soil health, management practices, and environmental conditions are complex, and research often focuses on specific practices or regional contexts. This have led to varying results regarding which soil health parameters are most influential for soybean yield. Objective: 
   
   In this study, we investigated the effects of soil health measurements, agricultural management practices (4 - 40 years), inherent soil properties, location-specific factors, and soil fertility analytical results on soybean (Glycine max L. Merr.) seed yield. 
   
   Methods: Soil samples (0–15 cm) were collected in 2023 from 17 agricultural research trials across the US. Soil health measurements, inherent soil properties, and soil fertility analytical results were assessed. Field management history and yield data were reported by the collaborators, and publicly available weather data (precipitation and temperature) were retrieved. Conditional inference trees were used to identify soybean yield influential factors.
   
   Results: Soybean seed yield was mainly driven by planting date. Trials planted before 26 May averaged 4,809 kg ha⁻¹, 55% greater yields than planting after 26 May (2,649 kg ha⁻¹). Longitude, along with soil organic carbon (SOC), autoclaved citrate extractable N (ACE-N), and soil test potassium (STK) were also important factors explaining yield variability
   
   .Conclusions: Our results demonstrated that planting date was the most critical factor driving soybean seed yield, yet yield responses are modulated to a lesser extent by longitude, SOC, ACE-N, and STK.
   
   Implications: To optimize soybean yield, conservation practices should prioritize early planting and soil health improvement. These findings can help identify soil health parameters associated with soybean seed yield for future long-term research.
"
   
   uri <- "doi:10.5061/dryad.hx3ffbgrk"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
            data_organization = "UWM", # University of Wisconsin–Madison
            publication="doi:10.1016/j.fcr.2025.109959", 
            project=NA, 
            data_type= "experiment", 
            treatment_vars= "P_fertilizer; K_fertilizer; land_prep_method; crop_rotation", 
            response_vars = "yield", 
            carob_contributor= "Cedric Ngakou", 
            carob_date="2025-06-23",
            completion=100,
            notes=NA
   )
   
   
   f <- ff[basename(ff) == "SeveroSilvaEtal_FCR_Dryad.xlsx"]
   rr <- carobiner::read.excel(f)
   
   ### process file
   r <- carobiner::read.excel(f,  sheet = "data", na= ("."))
   
   d <- data.frame(
      location= r$location,
      adm1= r$state,
      rep= as.integer(r$rep),
      #year= r$year_stablished,
      latitude= r$lat,
      longitude= r$long,
      crop_rotation= r$rotation,
      #crop_rotation_count= r$n_crops_in_rotation,
      land_prep_method= r$tillage,
      #drainage= r$drainage,
      previous_crop= ifelse(grepl("Corn", r$previous_crop), "maize", 
                     ifelse(grepl("Grain_sorghum", r$previous_crop), "sorghum", tolower(r$previous_crop))),
      crop= tolower(r$crop_in_2023),
      planting_date=  as.character(as.Date(paste0("2023", "-01-01")) + r$PD - 1),
      #$MG,
      seed_rate= r$SR,
      row_spacing= r$RS,
      irrigated= ifelse(grepl("yes", r$irrigation), TRUE, FALSE),
      N_fertilizer= 0,
      P_fertilizer= r$p_fert_kg_ha,
      K_fertilizer= r$k_fert_kg_ha,
      fungicide_used= ifelse(grepl("yes", r$STF), TRUE, FALSE),
      insecticide_used= ifelse(grepl("yes", r$STI), TRUE, FALSE),
      herbicide_used= ifelse(grepl("yes", r$pre_herb), TRUE, FALSE),
      yield_moisture= r$crop_moisture,
      yield= r$yield_kg_ha,
      soil_pH= as.numeric(r$ph),
      soil_SOM= as.numeric(r$`OM-LOI`),
      soil_P_total= as.numeric(r$STP),
      soil_K= as.numeric(r$STK),
      soil_SOC= as.numeric(r$SOC),
      soil_N= r$TN,
      soil_type= r$texture_class,
      soil_sand= r$sand,
      soil_silt= r$silt,
      soil_clay= r$clay,
      rain= r$prcp_2023_mm,
      temp= r$mean_2023_temp_c,
      country= "United States",
      yield_part= "grain",
      is_survey= FALSE,
      on_farm= TRUE,
      trial_id="1" , 
      geo_from_source= TRUE
      
   )
   
 d$adm1 <- carobiner::replace_values(d$adm1, c("AR", "IA", "MN", "NC", "ND", "OH", "WI"),  
            c("Arkansas","Iowa", "Minnesota", "North Carolina","North Dakota", "Ohio", "Wisconsin"))
   
 d$land_prep_method <- carobiner::replace_values(d$land_prep_method, c("NT", "CT", "CP", "MP", "ST", "RT"),  
                  c("minimum tillage", "conventional", "tillage", "tillage","strip tillage", "reduced tillage"))
 
 
 P <- carobiner::fix_name(d$crop_rotation) 
 P <- gsub("Continuous soybean", "soybean; soybean", P)
 P <- gsub("Corn-wheat-double crop soybean", "maize; wheat; soybean", P)
 P <- gsub("Grain sorghum-wheat-double crop soybean", "sorghum; wheat; soybean", P)
 P <- gsub("Grain sorghum-soybean", "sorghum; soybean", P)
 P <- gsub("Cotton-peanut-corn-wheat-soybean", "cotton; groundnut; maize; wheat; soybean", P)
 P <- gsub("Corn-soybean-wheat", "maize; soybean; wheat", P)
 P <- gsub("1st year soybean", "soybean", P) # incomplete
 P <- gsub("Corn-soybean", "maize; soybean", P)
 P <- gsub("Wheat-double crop soybean", "wheat; soybean", P)
 P <- gsub("1-yr-soybean after continuous corn", "soybean; maize", P)
 P <- gsub("Soybean-wheat-double crop grain sorghum", "soybean; wheat; sorghum", P)
 P <- gsub("Soybean-wheat", "soybean; wheat", P)
 P <- gsub("5th year corn", "maize; maize; maize; maize; maize", P)
 d$crop_rotation <- P
   
d$year <- NULL  
 
carobiner::write_files(path, meta, d)
   
}


