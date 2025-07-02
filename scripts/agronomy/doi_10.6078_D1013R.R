# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"Cropland management practices that restore soil organic carbon (SOC) are increasingly presented as climate solutions that also enhance yields. But how often these benefits align at the farm level — the scale of farmers’ decision-making — remains uncertain. We examined concurrent SOC and yield responses to cover cropping, including their direct connection, with a global meta-analysis. Cover cropping simultaneously increased yields and SOC in 59.7% of 434 paired observations. Increases in SOC helped increase crop yields in soils with initial SOC concentrations below 11.6 g kg-1; for example, a change from 5 g kg-1 to 6 g kg-1 increased yields by 2.4%. These yield benefits of SOC did not decline as nitrogen inputs increased or when legume cover crops were used, suggesting fertility inputs cannot substitute for SOC effects. Integrating legume cover crops into systems with simplified rotations or with nitrogen inputs < 157 kg N ha-1 season-1 led to the largest yield increases (up to 24.3%), with legumes also increasing SOC more than non-legumes (up to 1.5 g C kg-1). By simultaneously increasing yields and SOC, targeting cover crops on low carbon soils is an opportunity to benefit both food security and climate."
  
   uri <- "doi:10.6078/D1013R"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=9, minor=NA,
		#University of California, Berkeley, Lawrence Livermore National Laboratory; University of Oregon
         data_organization = "UCB; LLNM; UO",
         publication="doi:10.1038/s41893-023-01131-7", 
         project=NA, 
         data_type= "compilation", 
         treatment_vars= "N_fertilizer; P_fertilizer; land_prep_method", 
         response_vars = "yield", 
         carob_contributor= "Cedric Ngakou", 
         carob_date="2025-06-02",
         completion=75,
         notes="The Encyclo.csv file has not been processed as it does not contain useful information."
   )
   
   f1 <- ff[basename(ff) == "study.csv"]
   f2 <- ff[basename(ff) == "yield.csv"]
   f3 <- ff[basename(ff) == "Plotlvl.csv"]
   f4 <- ff[basename(ff) == "soilraw.csv"]
   f5 <- ff[basename(ff) == "Encyclo.csv"]
   
   ## process study file 
   r1 <- read.csv(f1)
   
   d1 <- data.frame(
      trial_id= r1$Study_Site.ID,
      id= r1$Study.Level.Data.Source.ID,
      location= r1$Location,
      latitude= r1$Lat.Decimal,
      longitude= r1$Long.Decimal,
      elevation= r1$Altitude..m.,
      temp= r1$Mean.Temp.C,
      rain= r1$Mean.Precip.mm,
      soil_type= r1$Soil.Texture,
      soil_sand= r1$Sand..,
      soil_silt= r1$Silt..,
      soil_clay= r1$Clay..,
      #soil_SOC= r1$SOC.g.kg,
      soil_bd= r1$Bulk.Density.Mg.m.3,
      soil_pH= r1$Soil.pH,
      plot_area= r1$Plot.Size..m.2./10000, # ha
      geo_from_source= TRUE
   )
   ## process yield file 
   
   r2 <- read.csv(f2)
   
   ##### Fallow system
   d21 <- data.frame(
      id= r2$Yield.Data.Source.ID,
      plot_id= r2$Plot.ID,
      planting_date= as.character(r2$Year.Yield),
      crop= r2$Yield.Level.Cash.Crop.Species,
      yield= r2$Fallow.Yield.kg.ha,
      treatment= "fallow"
   )
   
   ##### No Fallow system
   d22 <- data.frame(
      id= r2$Yield.Data.Source.ID,
      plot_id= r2$Plot.ID,
      planting_date= r2$Year.Yield,
      crop= r2$Yield.Level.Cash.Crop.Species,
      yield= r2$CC.Yield.kg.ha,
      treatment= "No fallow"
   )
   
   d2 <- rbind(d22, d21)
   ### merge d1 and d2
   d <- merge(d2, d1, by="id", all= TRUE)
   cols <- c("latitude", "longitude")
   d <- d[!apply(d[cols], 1, function(row) all(is.na(row))), ]
   d <- d[!is.na(d$crop),]
   
   ## process plot file 
   r3 <- read.csv(f3)
   d3 <- data.frame(
      trial_id= r3$Study_Site.ID,
      plot_id= r3$Plot.ID,
      id= r3$Plot.Level.Data.Source.ID,
      OM_used= ifelse(grepl("Yes", r3$CC.Incorporation..Green.Manure.), TRUE, FALSE),
      land_prep_method= r3$Tillage,
      irrigated= ifelse(grepl("Yes", r3$Irrigation), TRUE, FALSE),
      N_fertilizer= r3$N.kg.ha,
      P_fertilizer= r3$P.kg.ha,
      rep= r3$Reps
   )
   
   ### merge d and d3
   
   dd <- merge(d, d3, by=c("id","trial_id", "plot_id"), all.x= TRUE)
   
   ### process soil raw data
   r4 <- read.csv(f4)
   
   d4 <- data.frame(
      plot_id= r4$Plot.ID,
      id= r4$SOC.Data.Source.ID,
      ### Soil SOC
      d1= r4$SOC.First.Depth,
      soil_SOC_1= r4$CC.SOC.d1.g.kg,
      soil_SOC_F1= r4$Fallow.SOC.d1.g.kg,
      d2= r4$SOC.2nd.Depth.cm,
      soil_SOC_2= r4$CC.SOC.d2.g.kg,
      soil_SOC_F2= r4$Fallow.SOC.d2.g.kg,
      d3= r4$SOC.3rd.Depth.cm,
      soil_SOC_3= r4$CC.SOC.d3.g.kg,
      soil_SOC_F3= r4$Fallow.SOC.d3.g.kg,
      ### soil NO3
      d4= r4$Soil.Nitrate.Depth.1,
      soil_NO3_1= r4$CC.Nitrate..mg.NO3.N.kg..D1,
      soil_NO3_F1= r4$Fallow.Nitrate..mg.NO3.N.kg..D1,
      d5= r4$Soil.Nitrate.Depth.2,
      soil_NO3_2= r4$CC.Nitrate..mg.NO3.N.kg..D2,
      soil_NO3_F2= r4$Fallow.Nitrate..mg.NO3.N.kg..D2,
      
      ## soil NH4
      d6= r4$Soil.Ammonium.Depth.1,
      soil_NH4_1= r4$CC.Ammonium..mg.NH4.N.kg..D1,
      soil_NH4_F1= r4$Fallow.Ammonium..mg.NH4.N.kg..D1,
      d7=  r4$Soil.Ammonium.Depth.2,
      soil_NH4_2= r4$CC.Ammonium..mg.NH4.N.kg..D2,
      soil_NH4_F2= r4$Fallow.Ammonium..mg.NH4.N.kg..D2,
      
      ## soil N
      d8= r4$Total.Nitrogen.Depth.1,
      soil_N_1= r4$CC.TN..g.kg..D1,
      soil_N_F1 = r4$Fallow.TN..g.kg..D1,
      d9= r4$Total.Nitrogen.Depth.2,
      soil_N_2= r4$CC.TN..g.kg..D2,
      soil_N_F2= r4$Fallow.TN..g.kg..D2
   )
   
   ### No fallow
   i1 <- names(d4)[!grepl("F1|F2|F3", names(d4))]
   d41 <- d4[, i1]
   d41$treatment <- "No fallow"
   ### Fallow
   i2 <- names(d4)[!grepl("_1|_2|_3", names(d4))]
   d42 <-d4[, i2]
   names(d42) <- gsub("F", "", names(d42))
   d42$treatment <- "fallow"
   dS <- rbind(d41, d42)
   
   ### merge d an dS
   df <- merge(dd, dS, by= c("id", "plot_id","treatment"), all.x = TRUE)
   df$record_id <- as.integer(1: nrow(df))
   mns <- names(df)[grepl("SOC|NO3|NH4|_N", names(df))]
   dph <- names(df)[grepl("^d[1-9]$", names(df))] 
   ds <- df[, c("record_id", mns, dph)]
   
   #### long records 
   ds <- reshape(ds, direction="long", varying =list(mns, dph) , v.names=c("value", "depth"), timevar="step")
   ds$variable <- c(rep("soil_SOC",3), rep("soil_NO3", 2), rep("soil_NH4", 2), rep("soil_N", 2))[ds$step]
   ds <- na.omit(ds)
   ds$step <- ds$id <- NULL
   depth <- do.call(rbind, lapply(strsplit(ds$depth, "_"), \(x) as.numeric(x[1:2])))
   ds$depth_top <- depth[,1]
   ds$depth_bottom <- depth[,2] 
   ds$depth <- NULL
   
   d <- df[, names(df)[!grepl("SOC|NO3|NH4|_N|^d[1-9]$", names(df))]]
   
   ### Fixing 
   d$crop <- tolower(gsub("Corn", "maize", d$crop))
   P <- carobiner::fix_name(d$crop)
   P <- gsub("baby maize", "maize", P)
   P <- gsub("casava", "cassava", P)
   P <- gsub("garbanzo", "chickpea", P)
   P <- gsub("grain sorghum", "sorghum", P)
   P <- gsub("pinto bean", "common bean", P)
   P <- gsub("red pepper", "pepper", P)
   P <- gsub("russet potato", "potato", P)
   P <- gsub("sweet potato", "sweetpotato", P)
   #P <- gsub("zucchini", "zucchini", P)
   d$crop <- P
   
   d$land_prep_method <- ifelse(grepl("No-Till", d$land_prep_method), "minimum tillage", 
                         ifelse(grepl("Strip-Till", d$land_prep_method), "strip tillage", 
                         ifelse(grepl("Standard|Chisel", d$land_prep_method), "tillage",
                         ifelse(grepl("Reduced", d$land_prep_method), "reduced tillage",
                         ifelse(grepl("Hoe", d$land_prep_method), "hoeing", d$land_prep_method)))))
   
   d$N_fertilizer <- as.numeric(ifelse(grepl("Unspecified|Factor", d$N_fertilizer), NA, d$N_fertilizer))
   d$P_fertilizer <- as.numeric(ifelse(grepl("Unspecified|Factor", d$P_fertilizer), NA, d$P_fertilizer))
   d$country <- ifelse(grepl("Spain", d$location), "Spain", 
                ifelse(grepl("USA|Michigan|Logan County|Kentucky|Florida|Mississippi|Bellwood|Missouri|Bledsoe|Arkansas|Payallup|Beltsville|Holtwood|Keedsyville|LTRAS|Fort Valley|Napa Valley|Ames|Clovis|Jackson", d$location), "United States",
                ifelse(grepl("France", d$location), "France",
                ifelse(grepl("China", d$location), "China",
                ifelse(grepl("Brazil|Quirinópolis|Chapadão|Quatá", d$location), "Brazil",
                ifelse(grepl("Poland", d$location), "Poland",
                ifelse(grepl("India", d$location), "India", 
                ifelse(grepl("Korea", d$location), "South Korea",
                ifelse(grepl("Estonia", d$location), "Estonia", 
                ifelse(grepl("Canada|Swift", d$location), "Canada", 
                ifelse(grepl("Japan", d$location), "Japan", 
                ifelse(grepl("Ghana", d$location), "Ghana", 
                ifelse(grepl("Bangladesh", d$location), "Bangladesh", 
                ifelse(grepl("Argentina", d$location), "Argentina", 
                ifelse(grepl("Nigeria", d$location), "Nigeria", 
                ifelse(grepl("Moldova", d$location), "Moldova",
                ifelse(grepl("Tuscia", d$location), "Italy",
                ifelse(grepl("Askov Experimental", d$location), "Denmark", d$location))))))))))))))))))
   d$location <- gsub(" ", "", d$location)
   d$id <- NULL
   
   d$K_fertilizer <- as.numeric(NA) 
   d$on_farm <- TRUE 
   d$is_survey <- FALSE 
   d$yield_part <- "none"
   
   carobiner::write_files(path, meta, d, long = ds)
}

