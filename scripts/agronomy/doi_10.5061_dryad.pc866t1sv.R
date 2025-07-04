# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"Organic nutrient management through the application of compost and/or cover crops provides mineralizable sources of nutrients for plants while often building soil organic matter (SOM) and various aspects of soil health. Variability in nutrient acquisition strategies between crop genotypes may confer advantages under different soil health contexts and could be important for crop selection and breeding, but crop response under field conditions remains unexplored. We investigated the ability of different genotypes of winter wheat (Triticum aestivum L.) to access nitrogen (N) from newly added cover crop residues in two soils with contrasting levels of SOM and biological activity. We planted three previously characterized wheat genotypes in a long-term dryland compost amendment field trial: 1) Byrd (modern, deep roots, low exudation), 2) Cheyenne (historic, drought susceptible, intermediate exudation), and 3) Snowmass (modern, drought-susceptible, high exudation). 15N-labelled cover crop residue was added to each plot and traced into wheat tissue. In the low SOM soil, the high exudate genotype Snowmass and historic genotype Cheyenne took up the most residue-derived N (6.4-8.1 kg N ha-1) compared to the low-exudate genotype Byrd (4.4 kg N ha-1), suggesting a strong exudate effect in the more carbon-limited soil. However, the low-exudate, deep rooted genotype, Byrd, took up the most residue N in the high SOM soils (4.6 kg N ha-1 vs. 2.8 and 3.3 hg N ha-1 for Cheyenne and Snowmass, respectively), which indicated higher native N cycling activities and great importance of drought resistance. Enzyme activity, inorganic N, and microbial communities were not influenced by genotype, though did show strong effects of compost application legacy. Our results show that belowground allocation strategies that favor microbial stimulation may be less successful under water limitation, especially when high SOM can support mineralization of residue N without added investment in root inputs. Increased soil health through SOM-building management likely enhances nutrient cycling, and may better support root strategies that invest less in microbial stimulation in favor of other limiting resources"
   
   uri <- "doi:10.5061/dryad.pc866t1sv"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
       data_organization = "COLSU", # Colorado State University
       publication="doi.org/10.1016/j.agee.2022.108336", 
       project=NA, 
       data_type= "on-station experiment", 
       treatment_vars= "OM_type; variety", 
       response_vars = "fw_yield; fwy_total", 
       carob_contributor= "Cedric Ngakou", 
       carob_date="2025-06-18",
       completion=75,
       notes=NA
   )
   
   
   f1 <- ff[basename(ff) == "Akron_Compost_Yield_Data.csv"]
   f2 <- ff[basename(ff) == "Akron_Compost_Soil_Data.csv"]
   
   ### Process yield data 
   
   r1 <- read.csv(f1, na= "na")
   
   d1 <- data.frame(
      planting_date= as.character(r1$Year),
      harvest_date= as.character(as.Date(r1$Sample.date, format ="%m/%d/%y")),
      rep= as.integer(gsub("b-", "", r1$Block)),
      treatment=ifelse(grepl("5x", r1$Soil.trt), "compost", "control"),
      OM_type= ifelse(grepl("5x", r1$Soil.trt), "compost", "none"),
      OM_amount= ifelse(grepl("5x", r1$Soil.trt), 109*1000, 0), # kg/ha 
      OM_used= ifelse(grepl("5x", r1$Soil.trt), TRUE, FALSE), # kg/ha
      variety= r1$Variety,
      fw_yield= r1$wheat.grain.yield.kg.ha,
      fwy_total= r1$wheat.biomass.yield.kg.ha,
      seed_weight= r1$X100.grain.wt.g*10,
      harvest_index= r1$harvest.index,
      grain_N= as.numeric(r1$grain.perc.N)*10, # mg/g
      residue_N= as.numeric(r1$straw.perc.N)*10, # mg/g
      country= "United States",
      location= "Akron",
      crop= "wheat",
      longitude= -103.1333 , ##from paper
      latitude= 40.15 ,
      elevation= 1384,
      geo_from_source= TRUE,
      on_farm= FALSE,
      is_survey= FALSE,
      trial_id= ifelse(grepl("5x", r1$Soil.trt), "1", "2") , 
      irrigated= NA,
      yield_part= "grain"
      
   )
   
   ### Process soil activity data 
   
   r2 <- read.csv(f2, na="na")

   d2 <- data.frame(
      planting_date= as.character(r2$Year),
      date=  as.character(as.Date(r2$Sample.date, format ="%m/%d/%y")),
      rep= as.integer(gsub("b-", "", r2$Block)),
      #growth_stage= r2$Sampling.Period,
      treatment= ifelse(grepl("5x", r2$Soil.trt), "compost", "control"),
      variety= r2$Variety,
      tap_enzyme= r2$TAP.activity,
      NAG_enzyme= r2$NAG.activity,
      bg_enzyme= r2$BG.activity,
      cb_enzyme= r2$CB.activity,
      lap_enzyme= r2$LAP.activity,
      phos_enzyme= r2$PHOS.activity,
      #r2$tot.enz,
      soil_NO3= r2$no3.N.mg.kg,
      soil_NH4= r2$nh4.N.mg.kg,
      #r2$tot.inorg.n.mg.kg,
      #DNA= r2$dna.con.ng.ul,
      soil_GWC= r2$GWC # gravimetric water content

   )
   
   dd <- merge(d2, d1, by= c("rep", "treatment", "variety", "planting_date"), all.x = TRUE)
   dd$record_id <- as.integer(1: nrow(dd))
   i <- grep("enzyme|soil|^date", names(dd))
   nms <- names(dd)[i]
   d <- dd[, -i] 
   ds <- dd[, c("record_id", names(dd)[i])]
   nms <- names(ds)[grep("enzyme", names(ds))]
   ds <- reshape(ds, direction="long", varying=nms, v.names="soil_microbial_activity_value", timevar="soil_microbial_activity_category")
   ds$soil_microbial_activity_category <- nms[ds$soil_microbial_activity_category]
   
   ds$id  <-  NULL
   ds <- na.omit(ds)
	
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 
   
  carobiner::write_files(path, meta, d, long = ds)
   
}


