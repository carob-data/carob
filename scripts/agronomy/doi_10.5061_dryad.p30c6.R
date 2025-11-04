# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"Nutrient management recommendations may change as yield levels and efficiency of crop production increase. Recommendations for P, K, and S were evaluated using results from 34 irrigated corn (Zea mays L.) trials conducted in diverse situations across Nebraska. The mean yield was 14.7 Mg ha−1 with adequate fertilizer applied. The median harvest index values were 0.52, 0.89, 0.15, and 0.56 for biomass, P, K, and S, respectively. Median grain yields were 372, 49, and 613 kg kg−1 of aboveground plant uptake of P, K, and S, respectively. The estimated critical Bray-1 P level for corn response to 20 kg P ha−1 was 20 mg kg−1 when the previous crop was corn compared with 10 mg kg−1 when corn followed soybean [Glycine max (L.) Merr.]. Soil test K was generally high with only three site-years <125 mg kg−1 Over all trials, application of 40 kg K ha−1 resulted in a 0.2 Mg ha−1 mean grain yield decrease. Application of 22 kg S ha−1 did not result in significant yield increase in any trial. Soil test results accounted for twice as much variation in nutrient uptake when soil organic matter (SOM) and pH were considered in addition to the soil test nutrient values. The results indicate a need to revise the current recommendation for P, to maintain the current K and S recommendations, and to use SOM and pH in addition to soil test nutrient values in estimating applied nutrient requirements for irrigated high yield corn production."

   
   uri <- "doi:10.5061/dryad.p30c6"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=NA,
		data_organization = "UNL", 
		publication="doi:10.2134/agronj2008.0103x", 
		project=NA, 
		data_type= "experiment", 
		treatment_vars= "N_fertilizer; P_fertilizer; K_fertilizer; S_fertilizer; land_prep_method", 
		response_vars = "fw_yield; dm_yield; dmy_total; dmy_residue", 
		carob_contributor= "Cedric Ngakou", 
		carob_date="2025-06-12",
		completion=75,
		notes=NA
   )
   
   f <- ff[basename(ff) == "Nebr_HiYieldCornNPKS_Feb_2021.xls"]
   
   ##### Process site information
   r1 <- carobiner::read.excel(f, sheet = "Site-year information")
   
   d1 <- data.frame(
      date= r1$Year,
      location= r1$Site,
      latitude= r1$Latitude,
      longitude= r1$Longitude,
      soil= r1$Soil,
      soil_type= ifelse(grepl("lS|ls", r1$Texture), "loamy sand",
                        ifelse(grepl("siL", r1$Texture), "silt loam",
                        ifelse(grepl("sL",r1$Texture), "sandy loam", "silt clay loam"))),
      previous_crop= ifelse(grepl("corn", r1$`prev. Crop`), "maize", 
                     ifelse(grepl("drybean", r1$`prev. Crop`), "common bean", r1$`prev. Crop`)),
      soil_SOM= r1$`SOM (%)`,
      soil_pH= r1$pH,
      soil_P= r1$`Br.P (ppm)`,
      soil_K= r1$`K (ppm)`,
      irrigated= ifelse(grepl("yes" , r1$`Irrig. Amount`), TRUE, FALSE),
      irrigation_amount=  r1$`Irrigation mm`,
      planting_date= as.character(as.Date(r1$Planting))
   ) 
   
   ### Process yield data 
   r2 <- suppressWarnings(carobiner::read.excel(f, sheet = "Data", fix_names = TRUE))
   
   d2 <- data.frame(
      date= r2$Year,
      adm1= "Nebraska",
      country= "United States",
      location= r2$Site,
      soil= r2$Soil,
      soil_type= ifelse(grepl("lS|ls", r2$Texture), "loamy sand",
         ifelse(grepl("siL", r2$Texture), "silt loam",
         ifelse(grepl("sL",r2$Texture), "sandy loam", "silt clay loam"))),
      crop= "maize",
      previous_crop= ifelse(grepl("corn", r2$prevCrop), "maize", 
            ifelse(grepl("drybean", r2$prevCrop), "common bean", r2$prevCrop)),
      land_prep_method= ifelse(grepl("RT", r2$Till), "ridge tillage", 
                  ifelse(grepl("CT", r2$Till), "conventional", "minimum tillage")),
      treatment= r2$Treatment,
      rep= as.integer(r2$Rep),
      N_fertilizer= r2$FN,
      P_fertilizer= r2$FP,
      K_fertilizer= r2$FK,
      S_fertilizer= r2$FS,
      plant_density= r2$POP.ha,
      fw_yield= r2$GY.Mg*1000, #kg/ha
      yield_moisture= 15.5,
      dm_yield= r2$GDM,
      #cob_dmy= r2$CDM,
      dmy_residue= r2$VDM,
      dmy_total= r2$TDM,
      harvest_index= r2$HI.30,
      #r2$EARS,
      grain_N= r2$gN*10, # from % to mg/g
      grain_P= r2$gP*10,
      grain_K= r2$gK*10,
      grain_Ca= r2$gCa*10,
      grain_Mg= r2$gMg*10,
      grain_S= r2$gS*10,
      residue_N= r2$vN*10,
      residue_Ca= r2$vCa*10,
      residue_K= r2$vK*10,
      residue_Mg= r2$vMg*10,
      residue_P= r2$vP*10,
      residue_S= r2$vS*10,
      geo_from_source= TRUE, 
      trial_id= paste0(r2$Year, "_", r2$Site) , 
      on_farm= TRUE, 
      is_survey= FALSE, 
      yield_part= "grain",
      record_id= 1:nrow(r2)
   )
   
   ### merge d1 and d2
   d <- merge(d2, d1, by= c("date","location","soil","soil_type","previous_crop"), all.x = TRUE) 
   d$date <- d$soil <- NULL
   
   ###  process soil data at different dephs
   r2$Mn.8.16 <- r2$Zn.8.16 <- r2$Fe.8.16 <- r2$Cu.8.16 <- NA
   r2$S.8.16 <- NA
   r2$BD.0.4 <-  r2$BD.4.8 <-  r2$BD.8.16 <- NA
   si <- names(r2)[grepl("SOM|pH|Bray|^K.0.4$|^K.4.8$|^K.0.8$|^K.8.16$|Zn|Fe|Mn|Cu|^S.0.4$|^S.4.8$|^S.0.8$|^S.8.16$|BD", names(r2))]
   SI <- r2[, si]
   cols <- paste0(rep(c("Fe", "Zn", "Mn", "Cu", "S", "BD", "SOM", "pH", "Bray.P", "K"), each=4), 
				   c(".0.4", ".4.8", ".0.8", ".8.16"))
 
   ds <- SI[, cols]

   ds$record_id <- as.integer(1:nrow(ds))
   
   x <- reshape(ds, direction="long", varying =cols , v.names="value", timevar="step")
   
   x$soil_depth <- c(rep(c("0-4", "4-8", "0-8", "8-16"), times = 10))[x$step]

   x$soil_variable <- c(rep(c("soil_Fe", "soil_Zn", "soil_Mn", "soil_Cu", "soil_S", "soil_bd", "soil_SOM", "soil_pH", "soil_P_available", "soil_K"), each= 4))[x$step]
   x$step <- x$id <- NULL 
   

   carobiner::write_files(path, meta, d, long = x)

}
