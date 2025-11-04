carob_script <- function(path) {
   
"Reducing soil disturbance may limit erosion, but many still consider tillage essential for seedbed preparation, particularly on poorly drained soils. Our objective was to quantify tillage and fertilizer management effects after 45 yr {21 in continuous corn [Zea mays L.] [CC] and 24 in corn–soybean [Glycine max (L.) Merr.] [CS] rotation} on a somewhat poorly drained silt loam near Belleville, IL. Four tillage (moldboard plow [MP], chisel tillage [ChT], alternate tillage [AT], and no-till [NT]) and five fertilizer (no fertilization, N-only, N+NPK starter, NPK+NPK starter, and NPK broadcast) treatments were evaluated. With N, P, and K fertilizer, yields were similar for tilled and NT treatments, averaging 8.73 Mg ha–1 for CC and 11.93 Mg ha–1 and 3.70 Mg ha–1 for rotated corn and soybean. Below recommended soil-test values resulted in NT yielding less than tilled treatments even though soil test P, K, and pH were similar. No-till with N, P, and K increased soil organic matter (OM) to 27.6 g kg–1 (20.5 g kg–1 in all other treatments), with the greatest increase from 0- to 5-cm. No-till treatments showed stratification of P and K, but it had no effect on yield. No excessive pH stratification was observed. Overall, fertilizer management predominantly influenced crop yield and with complete NPK management non-tilled yields were similar to tilled, even on flat, somewhat-poorly drained soils. No-till with NPK management therefore may allow farmers to maintain high yields while reducing soil and nutrient losses."
   
   uri <- "doi:10.5061/dryad.7920c"
   group <- "agronomy"
   ff	 <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA, 
      data_organization = "SIU;BOU",
      publication="doi:10.2134/agronj2015.0397", 
      project=NA, 
      data_type= "experiment", 
      treatment_vars= "land_prep_method; N_fertilizer; P_fertilizer; K_fertilizer", 
      response_vars = "yield", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-06-06",
      #completeness=75, # files f3
      notes=NA
   )
   
   f1 <- ff[basename(ff) == "BRC_TxF_Plant_tissue_1990_2014 Corn.csv"]
   f2 <- ff[basename(ff) == "BRC_TxF_soils_composite_78 83 90 99 2011 2013.csv"]
   f3 <- ff[basename(ff) == "BRC_TxF_yield_1970-2015.csv"]
   #f4 <- ff[basename(ff) == "BRC_TxF_soils_increments_1990_2013.csv"]
   
   
   ##### Process plant tissue
   
   r1 <- read.csv(f1)
   
    d1 <- data.frame(
       trial_id= as.character(r1$Plot.Idnum),
       date= r1$Year,
       rep= r1$Rep,
       treatment= r1$Treatment_st,
       fertilizer_type= r1$Fert_st,
       land_prep_method= r1$Tillage_st,
       leaf_N= r1$Leaf_N_per*10,
       leaf_P= r1$Leaf_P_per*10,
       leaf_K= r1$Leaf_K_per*10,
       leaf_Ca= r1$Leaf_Ca_per*10,
       leaf_Mg= r1$Leaf_Mg_per*10,
       leaf_S= r1$Leaf_S_per*10,
       leaf_B= r1$Leaf_B_ppm*1000,
       leaf_Fe= r1$Leaf_Fe_ppm*1000,
       leaf_Cu= r1$Leaf_Cu_ppm*1000,
       leaf_Mn= r1$Leaf_Mn_ppm*1000,
       leaf_Zn= r1$Leaf_Zn_ppm*1000,
       leaf_Al= r1$Leaf_Al_ppm*1000, # mg/g
       leaf_Na= r1$Leaf_Na_ppm*1000,
       crop= "maize"
    )
   
    ##### Process soils composite
    
    r2 <- read.csv(f2)
    
    d2 <- data.frame(
       trial_id= as.character(r2$Plot_Idnum),
       date= r2$Year,
       rep= r2$Rep,
       treatment= gsub("-", "_", r2$Treatment_st),
       fertilizer_type= gsub("-", "_", r2$Fert_st),
       land_prep_method= gsub("-", "_", r2$Tillage_st),
       soil_EC= r2$TEC..meq.100g.,
       soil_pH= r2$pH_water,
       soil_SOM= r2$OM_percent*10,
       soil_P_Bray = r2$Bray.I.P_ppm*1000, ##mg/g
       soil_P = r2$M3.P_ppm*1000,
       soil_K= r2$M3.K_ppm*1000,
       soil_S= r2$M3.S_ppm*1000,
       soil_Ca= r2$M3.Ca_ppm*1000,
       soil_Mg= r2$M3.Mg_ppm*1000,
       
       soil_Na= r2$Na_ppm*1000,
       soil_B= r2$M3.B_ppm*1000,
       soil_Fe= r2$M3.Fe_ppm*1000,
       soil_Mn= r2$M3.Mn_ppm*1000,
       soil_Cu= r2$M3.Cu_ppm*1000,
       soil_Zn= r2$M3.Zn_ppm*1000,
       soil_Al= r2$M3.Al_ppm*1000
    )
	
	soilmeta <- data.frame(variable=c("P", "K", "S", "Ca", "Mg", "Na", "B", "Fe", "Mn", "Cu", "Zn", "Al"), method="Mehlich")
    
##### Process yield data
    r3 <- read.csv(f3)
    
    d3 <- data.frame(
       trial_id= as.character(r3$Plot_Idnum),
       date= r3$Year,
       rep= r3$Rep,
       country= "United States",
       adm1= "Southern Illinois",
       location= "University Belleville Research Center",
       latitude= 38.519179, ## from 
       longitude=  - 89.843248,
       geo_from_source= TRUE,
       treatment= r3$Treatment_st,
       fertilizer_type= r3$Fert_st,
       land_prep_method= r3$Tillage_st,
       yield= r3$Yield_kg_ha,
       crop= ifelse(grepl("Corn", r3$Crop), "maize", "soybean"),
       crop_rotation= ifelse(grepl("CC", r3$Rotation) & grepl("Corn", r3$Crop), "maize;maize",
                             ifelse(grepl("CC", r3$Rotation) & grepl("Soy", r3$Crop), "soybean;soybean", "maize;soybean")),
       plant_height= r3$Height_in,
       plant_density= r3$Pop2_EOS_no_ac/2.471, # plant/ha
       plot_area= 6*8/10000, # ha 
       #r3$Lodging_pct,
       yield_moisture= r3$Moisture_pct,
       on_farm= TRUE,
       is_survey= FALSE, 
       yield_part= "grain",
       irrigated= NA
    )
   
    ### merge soil data crop nutrient content and yield data
    
    d <- merge(d3, d1, by= c("date","treatment","fertilizer_type", "land_prep_method", "trial_id", "rep", "crop"), all.x = TRUE)
    dd <- merge(d, d2, by= c("date","treatment","fertilizer_type", "land_prep_method", "trial_id", "rep"), all.x = TRUE)
    
    c1 <- (dd$date >= 1970 & dd$date <=1973)
    c2 <- (dd$date >= 1974 & dd$date <=1999)
    c3 <- dd$date > 1999
    
    dd$N_fertilizer <-ifelse(grepl("N_only", dd$fertilizer_type) & c1, 140,
         ifelse(grepl("N_only", dd$fertilizer_type) & c2, 196,
         ifelse(grepl("N_only", dd$fertilizer_type) & c3, 196,
         ifelse(grepl("NplusNPKst", dd$fertilizer_type) & c1, 129+11,
         ifelse(grepl("NplusNPKst", dd$fertilizer_type)& c2, 179+17,
         ifelse(grepl("NplusNPKst", dd$fertilizer_type)& c3, 196,
         ifelse(grepl("NPK", dd$fertilizer_type)& c1 ,140 , 
         ifelse(grepl("NPK", dd$fertilizer_type)& c2, 196 , 
         ifelse(grepl("NPK", dd$fertilizer_type)& c3,196 ,
         ifelse(grepl("NPKplusNPKst", dd$fertilizer_type)& c1, 129+11 ,
         ifelse(grepl("NPKplusNPKst", dd$fertilizer_type)& c2, 179+17 , 
         ifelse(grepl("NPKplusNPKst", dd$fertilizer_type)& c3, 196, 0)))))))))))) 
                                                                                      
    dd$P_fertilizer <- ifelse(grepl("NplusNPKst|NPK", dd$fertilizer_type) & c1, 25,
         ifelse(grepl("NplusNPKst|NPK", dd$fertilizer_type)& c2, 39,
         ifelse(grepl("NplusNPKst|NPK", dd$fertilizer_type)& c3, 24,
         ifelse(grepl("NPKplusNPKst", dd$fertilizer_type)& c1, 14+10 ,
         ifelse(grepl("NPKplusNPKst", dd$fertilizer_type)& c2, 25+14 , 
         ifelse(grepl("NPKplusNPKst", dd$fertilizer_type)& c3, 24, 0)))))) 
    
    
    dd$K_fertilizer <- ifelse(grepl("NplusNPKst", dd$fertilizer_type) & c1, 24,
         ifelse(grepl("NplusNPKst", dd$fertilizer_type)& c2, 112,
         ifelse(grepl("NplusNPKst", dd$fertilizer_type)& c3, 140,
         ifelse(grepl("NPK", dd$fertilizer_type)& c1 ,116 , 
         ifelse(grepl("NPK", dd$fertilizer_type)& c2, 168 , 
         ifelse(grepl("NPK", dd$fertilizer_type)& c3,140 ,
         ifelse(grepl("NPKplusNPKst", dd$fertilizer_type)& c1, 93+24 ,
         ifelse(grepl("NPKplusNPKst", dd$fertilizer_type)& c2, 112+140 , 
         ifelse(grepl("NPKplusNPKst", dd$fertilizer_type)& c3, 140, 0))))))))) 
    
    dd$land_prep_method <- ifelse(grepl("Conv", dd$land_prep_method), "conventional", 
                           ifelse(grepl("Chisel", dd$land_prep_method), "reduced tillage", 
                           ifelse(grepl("No_till", dd$land_prep_method), "minimum tillage", "strip tillage")))
    
   
    dd$fertilizer_type <- ifelse(dd$date > 2006 & grepl("NPKwNPK|NwNPK|NPK", dd$treatment), "urea;TSP;KCl",
                          ifelse(dd$date <= 2006 & grepl("NPKwNPK|NwNPK|NPK", dd$treatment), "AN;TSP;KCl", 
                          ifelse(dd$date <= 2006 & grepl("^AT_N$|CT_N$|^NT_N$|^RT_N$", dd$treatment), "AN", 
                          ifelse(dd$date > 2006 & grepl("^AT_N$|^CT_N$|^NT_N$|^RT_N$", dd$treatment), "urea", "none"))))
    
    dd$planting_date <- as.character(dd$date)
    dd$date <- NULL
    
	carobiner::write_files(meta, dd, path=path, var_meta=soilmeta)
}