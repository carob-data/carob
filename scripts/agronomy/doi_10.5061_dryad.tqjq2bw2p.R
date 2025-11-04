# R script for "carob"


carob_script <- function(path) {
   
"Ensuring food security in small-scale farming requires improvement of productivity of food crops to ensure access of food in local communities. the objective of the sorghum-legume demonstration trial was to demonstrate the performance of proven Sustainable Agricultural Intensification (SAI) practices in terms of yield, soil quality and labour efficiency. A farmers-led field trails were conducted in Nachunyu and Mmumbu villages in Lindi district, Tanzania. The SAI technologies validated include:  

i) Treatment 1: Conventional tillage – farmers practice, consisting of hand hoe cultivation, residue were removed, no herbicides applied (hand hoe weeding) and continuous sorghum  
ii) Treatment 2: Conservation tillage dibble stick, retention of crop residues but no herbicide use (traditional weeding), and cropping system of continuous sorghum, applied fertilizers and no burning of residues; 
iii) Treatment 3: Conservation tillage using dibble stick, retention of crop residues, applied herbicide immediately after planting , but continuous sorghum, applied fertilizer (NPS) and no burning of residues; 
iv) Treatment 4: Conservation agriculture with legume-sorghum rotation, tillage dibble stick, retention of crop residues, apply herbicide roundup soon after planting followed by traditional hand hoe weeding when needed after crop germination, apply fertilizers (NPS), started with sorghum in  season one (1) 2017/18. 

Soil samples were collected before the trials and after the trials. Data collected includes grain yields and residue biomass."
  
    uri <- "doi:10.5061/dryad.tqjq2bw2p"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=NA,
      data_organization = "SUA", #"Sokoine University of Agriculture", 
      publication=NA, 
      project="InnovAfrica", 
      data_type= "experiment", 
      treatment_vars= "herbicide_used; crop_rotation; planting_method; land_prep_method; residue_prevcrop; residue_prevcrop_used", 
      response_vars = "yield; fwy_residue", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-05-30",
      notes=NA
   )
   
   
   f1 <- ff[ basename(ff)=="2806_-_2868_Soil_data_LINDI-DRYAD_2018-19.csv"]
   f2 <- ff[ basename(ff)=="Yield-Legume_Harvest_all_sites2019-2020.csv"]
   f3 <- ff[ basename(ff)=="Lindi_TZ_YldLEGU_All2018.csv"]
   f4 <- ff[ basename(ff)=="sas_input_tile_sorghum-2017-18.xlsx"]
   f5 <- ff[ basename(ff)=="1593-1599_initial_soil_data-LINDI_SOIL_DRYAD.csv"]
   
   ## Processing data soil data 2018-2019 season
   
   r1 <- read.csv(f1, fileEncoding = "latin1", na=c("nd"))
   
   d1 <- data.frame(
      location= r1$FIELD.REFERENCE,
      soil_pH= r1$Soil.pH..1.2.5.,
      soil_EC= r1$EC.æS.cm*0.001,
      soil_N= r1$TN.Kjeld....,
      soil_P_total= r1$Ext.P..mg.kg.,
      soil_S= r1$S...mg.kg.,
      soil_CEC=  as.numeric(r1$CEC.cmolKg.1),
      soil_ex_Ca= as.numeric(r1$Exch..Ca...cmolKg.1),
      soil_ex_Mg= as.numeric(r1$Exch..Mg...CmolKg.1),
      soil_ex_K= as.numeric(r1$Exch..K..CmolKg.1),
      soil_ex_Na= as.numeric(r1$Exch..Na..CmolKg.1)
      
   )
   split_col <- strsplit(d1$location, "/")
   nbr_cols <- lapply(split_col, function(x) {
      length(x) <- 4 
      x})
   New_cols <- do.call(rbind,  nbr_cols)
   colnames(New_cols)  <- c("location","treatment","rep","loc")
   d1$location <- NULL
   d1 <- cbind(New_cols, d1)
   d1$loc <-  NULL
   d1$treatment <- as.integer(gsub("TRT", "", d1$treatment))
   d1$rep <- as.integer(gsub("BLOCK", "", d1$rep))
   d1$location <- tolower(d1$location) 

# RH: to make d1 match with d2
	d1$location[d1$location=="jitahidi"] <- "hiari"
   
   ### processing yield from survey 2018-2019
   r2 <- read.csv(f2)  # 
   d2 <- data.frame(
      trial_id= "1",
      location= tolower(r2$site),
      crop= tolower(gsub("GNUT|Gnut", "groundnut", r2$Crop)),
      rep= as.integer(r2$block),
      treatment= r2$trt,
      yield= r2$Kgperha,
      planting_date= "2018",
      harvest_date= "2019",
      is_survey= TRUE,
      on_farm= TRUE,
      irrigated= NA,
      yield_part= "grain"
   )
   
   # merge yield with soil data 2018-2019
   
  ##RH  d <- merge(d2, d1, by= c("location","treatment", "rep"))
   
   
   ###  processing average yield for famer groups  
   ### RH: this does not seem to related to the experiment
   ##CN: I think this is the average yield collected from published agricultural statistics, as stated in the abstract.
#   r3 <- read.csv(f3)   
#   d3 <- data.frame(
#      trial_id= "2",
#      location= tolower(r3$site.farmer.group),
#      crop= tolower(gsub("G'NUT","groundnut", r3$TRT)),
#      rep= as.integer(r3$REP),
#      yield= r3$legugrain.t.ha * 1000,
#      fwy_residue= r3$residue.t.ha *1000,
#      on_farm= FALSE,
#      is_survey= FALSE,
#      irrigated= NA,
#      yield_part= "grain",
#      season= "s1"
#   )
#   d3$crop <- gsub("pegeonpea", "pigeon pea", d3$crop)
  

   ### residues from prior crop 2017-2018
   r4 <- carobiner::read.excel(f4)
 
	herb_used= ifelse(grepl("Gly", r4$TRT), TRUE, FALSE)
   d4 <- data.frame(
      location= tolower(r4$site),
	treatment= r4$trtNo.,
	crop_rotation = ifelse(grepl("LeSor", r4$TRT), "legume;sorghum",
						ifelse(grepl("SorLe", r4$TRT), "sorghum;legume",
						ifelse(grepl("Sor", r4$TRT), "sorghum", NA))),
   intercrops= ifelse(grepl("3CA_GlySor", r4$TRT), "maize", NA),
      herbicide_used= herb_used,
      herbicide_product= ifelse(herb_used, "glyphosate", NA),
      rep= as.integer(r4$REP),
      residue_prevcrop= r4$`residueKg/ha`
   )  

### RH rotation is not an intercrop
   ##CN: the intercrops is with maize 
  d4$trt <- NULL
 
  ## joint d3 and d4 from s1 and s2
  ## dy <- carobiner::bindr(d3, d4) 
   
## RH merge d2 and d4 
## not using all=TRUE because there is not much value in the data if there is no record in d2
   m <- merge(d2, d4, by=c("location", "treatment", "rep"))   
   
   
   ### processing initial soil data
   
   r5 <- read.csv(f5)
   d5 <- data.frame(
      location= r5$FIELD.REFERENCE,
      rep= ifelse(grepl("Block1|s1", r5$FIELD.REFERENCE), 1L, 
                     ifelse(grepl("Block2|s2", r5$FIELD.REFERENCE), 2L, 3L)),					 
      soil_pH= r5$Soil.Ph.WATER..1.2.5.,
      soil_EC= r5$EC.uS.cm*0.001,
      soil_clay= r5$X..Clay,
      soil_silt= r5$X..Silt,
      soil_sand= r5$X..Sand,
      soil_type= r5$Texture.class,
      soil_Cu= r5$Cu.mg.kg,
      soil_Zn= r5$Zn.mg.kg,
      soil_Mn= r5$Mn.mg.kg,
      soil_Fe= r5$Fe.mg.kg,
      soil_N= r5$TN.Kjeld..,
      soil_SOC= r5$OC.BlkW..,
      soil_P= r5$P.Olsen.Bray.mg.kg,
      soil_S= r5$S.mg.kg,
      soil_CEC= r5$CEC.cmolc.kg,
      soil_ex_Ca= r5$exch..Ca2.,
      soil_ex_Mg= r5$Exch..Mg2.,
      soil_ex_Na= r5$Exch..Na.,
      soil_ex_K= r5$Exch..K.
   )
   
	## who knows, but this follows the "Nachunyu-Napande Mkombozi" logic
   	d5$rep[r5$FIELD.REFERENCE == "Mmumbu - Chundu (Down)    s1"] <- 2L

  # please explain how you know that Nachunyu-Napande is Hamasa and not Hiari
	d5$location <- ifelse(grepl("Block", d5$location),"hamasa" ,
                  ifelse(grepl("Mkombozi", d5$location), "mkombozi", "hiari"))
   
	##dd <- merge(dy, d5, by=c("rep","location"), all.x=TRUE)
   
   ### combining all data 
   ##d <- carobiner::bindr(d, dd)
   ## RH that made no sense. They would have to be merged. 
   ## RH but the soil data is now a time-variable (before and after)
   ## RH we need a better way to capture that 
   ## RH for now I am merging the response with the initial soil data 

   d <- merge(m, d5, all.x=TRUE) 

 
   d$country <- "Tanzania"
   d$adm1 <- "Lindi"
   d$treatment <- c("farmers practice", "CvT-dibble stick ","CvT-dibble stick-glyphosate intercrop", "CA-dibble stick-glyphosate sorghum-legume rotation", "CA-glyphosate legume-sorghum rotation")[d$treatment]

	d$residue_prevcrop_used <- ifelse(grepl("CvT", d$treatment), TRUE, FALSE)
	    
	d$planting_method <- ifelse(grepl("CA", d$treatment), "direct seeding", 
	                            ifelse(grepl("CvT", d$treatment), "dibbling", "manual"))
	d$land_prep_method <- ifelse(grepl("farmers practice", d$treatment), "conventional", 
	                       ifelse(grepl("CvT", d$treatment), "minimum tillage", "reduced tillage"))
	                        

   d$fertilizer_type= "NPS"
   d$N_fertilizer <- 23
   d$P_fertilizer <- 21
   d$K_fertilizer <- 0
   d$S_fertilizer <- 4

   d$on_farm= TRUE
   d$is_survey= FALSE
   d$irrigated= NA
   
   geo  <- data.frame(
      location= c("hamasa", "hiari", "mkombozi"),
      latitude= c(-6.775, -6.330, -6.674),
      longitude= c(39.243, 39.532, 39.0445),
      geo_from_source= c(rep(FALSE, 3))
   ) 
   d <- merge(d, geo, by="location", all.x = TRUE)   
   
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}


