# R script for "carob"


carob_script <- function(path) {
   
"Biochar can significantly change soil properties and improve soil quality. However, the effects of long-term combined application of biochar (B) and nitrogen (N) fertilizer on relationships between soil enzyme activity, microbial community structure and crop yield are still obscure. We characterized these relationships in a long-term (8 years) field experiment with rice, two biochar rates of 0 and 13.5 t ha-1 year-1 (B0 and B) and two N fertilizer rates of 0 and 300 kg N ha-1 year-1 (N0 and N). The repeated, long-term combined applications of biochar and N fertilizer significantly increased microbial biomass carbon and nitrogen (MBC and MBN), but biochar decreased the abundance of total bacteria, fungi, actinomycetes, Gram-positive and Gram-negative bacteria as well as the amount of total phospholipid fatty acids. The activity of leucine aminopeptidase (LAP) decreased significantly in the biochar-amended and N fertilized treatment, but the LAP activity either remained unchanged or increased with biochar amendment at N0. The relative abundance of bacterial phylum Chloroflexi was increased in the combined biochar and N fertilizer treatment. The changes in soil organic matter and the activity of α-1,4-xylosidase were the major properties influencing soil bacterial community composition, whereas the structure of fungal community was governed by MBC, MBN and LAP activity. In addition, long-term biochar and N fertilizer applied together significantly increased rice yield (more than biochar and nitrogen fertilizer applied alone). Yield was significantly positively correlated with LAP activity, but significantly negatively correlated with the relative abundance of Chloroflexi. In conclusion, long-term biochar and nitrogen fertilizer applications increased rice yield, which was associated with altered soil microbial community and enhanced activity of some enzymes."
   
   uri <- "doi:10.5061/dryad.rbnzs7hf0"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA, 
      data_organization = "CAAS", #Chinese Academy of Agricultural Sciences
      publication="doi:10.1111/gcbb.12995", 
      project=NA, 
      data_type= "experiment", 
      treatment_vars= "N_fertilizer; biochar", 
      response_vars = "yield", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-06-03",
	  completion=50, # % files f2-f5 were not porcessed
      notes=NA
   )
   
   f1 <- ff[basename(ff)=="SUN_a_2022_properties.xlsx"]
   
   #f2 <- ff[basename(ff)=="SUN_b_2022_enzyme_activities.xlsx"]
   #f3 <- ff[basename(ff)=="SUN_c_2022_PLFAs.xlsx"]
   #f4 <- ff[basename(ff)=="SUN_d_2022_bacterial_.xlsx"]
   #f5 <- ff[basename(ff)=="SUN_e_2022_fungal.xlsx"]
   
   r <- carobiner::read.excel(f1) 
   
   d <- data.frame(
      country= "China",
      adm1="Ningxia",
      adm2= "Yesheng Town", 
      latitude=38.123 , # 38°07′26″N) # from paper
      longitude=106.193 , #106°11′35″E,
      treatment= r$Treatment,
      N_fertilizer= ifelse(grepl("B0N0|BN0", r$Treatment), 0, 300),
      P_fertilizer= ifelse(grepl("BN|BN0", r$Treatment), 39.3*0.2/2.29, 0), #DSP 20% P205
      K_fertilizer= ifelse(grepl("BN|BN0", r$Treatment), 74.5*0.5/1.2051, 0),#KCl 50% K20
      biochar= ifelse(grepl("B0N0|B0N", r$Treatment), 0, 13.5*1000),
      fertilizer_type= "urea;DSP;KCl", #DSP: Double Superphosphate
      OM_type= "biochar",
      plot_area= 13*5/10000, # ha
      crop= "rice",
      variety= "Ningjing43", #From paper
      planting_date="2012-04",
      harvest_date= "2012-09",
      soil_SOC= r$SOC,
      soil_N= r$TN,
      soil_P_total= r$AP,
      soil_K= r$AK,
      soil_pH= r$pH,
      soil_sample_top= 0,
      soil_sample_bottom= 20,
      yield= r$Yield*1000,
      on_farm = TRUE,
      is_survey= FALSE,
      irrigated = NA,
      geo_from_source= TRUE, 
      trial_id= ifelse(grepl("B0N0", r$Treatment), "1", 
                ifelse(grepl("BN0", r$Treatment), "2", "3")), 
      yield_part= "grain",
      land_prep_method= "ploughing",
      planting_method= "broadcasting"
      #microbial_biomass_C= r$MBC,
      #microbial_biomass_N= r$MBN
   )
   
   ##  From the paper 90 kg/ha and 60 kg/ha of N was added later
## RH no. They apply 150 + 90 + 60 = 300
##   i <- d$N_fertilizer != 0
##   d$N_fertilizer[i] <- d$N_fertilizer[i] + 150
  
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}


