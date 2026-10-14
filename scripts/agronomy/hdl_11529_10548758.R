# R script for "carob"
# license: GPL (>=3)

## ISSUES

## treatment: 
 ## 1- Transplanted puddled rice * conventional tillage broadcasted wheat (TPR-CTW)
 ## 2- Transplanted puddled rice * zero tillage wheat * zero tillage mung bean (TPR-ZTWMb)
## 3 - Zero tillage Direct seeded rice * zero tillage wheat * zero tillage mungbean (ZTDSR-ZTWMb)
## 4- Zero tillage maize * zero tillage wheat * zero tillage mung bean (ZTM-ZTWMb)
## 5- Zero tillage direct seeded rice * zero tillage wheat * zero tillage mung bean +Sub-surface drip irrigation (ZTDSR-ZTWMb +SDI)
## 6 - Zero tillage maize * zero tillage wheat * zero tillage mung bean (ZTM-ZTWMb +SDI)

## convert kg/ha to ppm = kg/ha *100/ soil_bd* depth

carob_script <- function(path) {

"
Impact of long term conservation agriculture on soil quality under cereal based systems of North West India

Indo Gangetic plains of India. Hence, we evaluated long-term (10 years) effect of conservation agriculture (CA) practices on soil quality improvement under six different cropping scenarios (Sc), i.e. Sc1-represented by transplanted puddled rice (TPR) followed by conventional tilled broadcasted wheat (CT-wheat) with residue removal, Sc2-TPR rice followed by zero tillage (ZT) wheat and ZT-mung bean with partial residue retention, Sc3- direct seeded rice (DSR) followed by ZT-wheat and ZT-mung bean with full residue retention, Sc4-DSR is replaced by ZT-maize followed by ZT-wheat and ZT-mung bean, Sc5 and Sc6 were – Sc3 integrated with sub surface drip irrigation (SDI) and Sc4 + SDI, respectively. Soil samples were collected from 0 to 5, 5–15 and 15–30 cm soil depth from each scenario after harvesting of wheat in 2019. Results showed that, reduction in bulk density (BD), soil penetration resistance (SPR) and enhancement of water holding capacity and infiltration were associated with CA based scenarios (Sc3–Sc6). Scenario 3 recorded lowest BD of 1.39 and 1.58 g cm􀀀 3 at 0–5 and 5–15 cm soil depth, respectively. CA based Sc6 recorded highest infiltration rate (1.48 cm hr􀀀 1) and lowest was associated with Sc1 (0.5 cm hr􀀀 1). The enrichment of soil organic carbon (SOC) content, stock, available nitrogen and potassium was mainly confined to upper surface soil layer (0–5 cm). The SOC content and stock in CA based scenarios (average of Sc3 to Sc6) was 41–57 and 69–94% higher than Sc1 at 0–5 cm soil layer. Available nitrogen was increased by 23–50 and 64–98% and available potassium increased by 13–28 and 42–71% in 0–5 and 5–15 cm soil depth, respectively in CA based scenarios over Sc1. Similarly, microbial biomass carbon (MBC) and dehydrogenase (DHA) activity in top soil layer under CA based scenarios was increased by 177–195 and 67–107% over Sc1, respectively. The maximum SQI was registered with Sc6 (0.91) followed by Sc4 (0.89) and least was recorded in Sc1 (0.65) at 0–5 cm soil depth. Maize-wheat based cropping system recorded higher SQI over rice–wheat based cropping system. Sustainable yield index was strongly related with key soil quality indicators and also positively correlated with SQI. Thus our study suggests that CA based maize-wheat-mung bean cropping system should be recommended for better soil quality and yield sustainability in North West India.
"

	uri <- "hdl:11529/10548758"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICAR;CIMMYT",
		publication = "doi:10.1016/j.geoderma.2021.115391",
		project = NA,
		carob_date = "2026-10-12",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "land_prep_method;planting_method;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "soil_MBC;soil_SOC", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "Dibakar et al 2021_Geoderma.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Sheet1")
	
mgmnt <- data.frame(
	 treatment_id = 1:6,
	 treatment = c("transplanted puddled rice -> conventional tillage broadcasted wheat (TPR-CTW)", "transplanted puddled rice -> zero-tillage wheat -> zero-tillage mung bean (TPR-ZTWMb)", "zero-tillage direct-seeded rice -> zero-tillage wheat -> zero-tillage mungbean (ZTDSR-ZTWMb)", "zero-tillage maize -> zero-tillage wheat -> zero-tillage mung bean (ZTM-ZTWMb)", "zero-tillage direct seeded rice -> zero-tillage wheat -> zero-tillage mung bean + sub-surface drip irrigation (ZTDSR-ZTWMb+SDI)", "zero-tillage maize -> zero-tillage wheat -> zero-tillage mung bean + sub-surface drip irrigation (ZTM-ZTWMb+SDI)"),
	 crop = c("rice_wheat", "rice_wheat_mung bean", "rice_wheat_mung bean", "maize_wheat_mung bean", "rice_wheat_mung bean", "maize;wheat;mungbean"),
	 planting_method = c("transplanting;broadcasting", "transplanting;drilling;drilling", "drilling", "drilling", "drilling", "drilling"),
	 land_prep_method = c("puddling;conventional", "puddling;none", rep("none", 4)),
	 irrigation_method = c(rep("flood", 4), "subsurface drip", "subsurface drip")
 )
	
  d1 <- data.frame(
    rep = as.integer(r1$Rep),
    treatment_id = r1$Sc,
    depth_bottom = as.numeric(gsub("-", "", substr(r1$Depth,3, 5))) ,
    depth_top = as.numeric(gsub("-", "", substr(r1$Depth,1, 2))),
    #soil_moisture = r1$VWC, ## in %
    soil_pH = r1$pH,
    soil_EC = r1$Ec,
    soil_bd = r1$BD,
    infiltration_rate = r1$Infiltration,# cm/hr
    soil_SOC = r1$`SOC (g kg)`/10, ## in %
    soil_N = r1$`Av N`*100/(as.numeric(gsub("-", "", substr(r1$Depth,3, 5)))*r1$BD), # from kg/ha to ppm
    soil_P = r1$`Av P`*100/(as.numeric(gsub("-", "", substr(r1$Depth,3, 5)))*r1$BD),
    soil_K = r1$`Av K`*100/(as.numeric(gsub("-", "", substr(r1$Depth,3, 5)))*r1$BD),
    soil_Fe = r1$`Av Fe`*100/(as.numeric(gsub("-", "", substr(r1$Depth,3, 5)))*r1$BD),
    soil_Zn = r1$`AV Zn`*100/(as.numeric(gsub("-", "", substr(r1$Depth,3, 5)))*r1$BD),
    soil_Cu = r1$`Av Cu`*100/(as.numeric(gsub("-", "", substr(r1$Depth,3, 5)))*r1$BD),
    soil_Mn = r1$`Av Mn`*100/(as.numeric(gsub("-", "", substr(r1$Depth,3, 5)))*r1$BD),
    soil_MBC = r1$MBC,
    soil_ALP = r1$ALKP,
    soil_ACP = r1$ACDP,
    soil_DHA = r1$DHA, # dehydrogenase activity
    country = "India",
    location = "CSSRI, Karnal" ,
    longitude = 76.9555 ,
    latitude = 29.70575,
    elevation = 243,
    geo_from_source = TRUE,
    CA_years = 10
  )
  
#  d <- reshape(d1, varying = c("treat_rice", "treat_maize", "treat_wheat", "treat_bean"), v.names = "land_prep", timevar = "crop", time = #c("rice", "maize", "wheat", "mung bean"), direction = "long")
#  d <- d[!is.na(d$land_prep),]
#  row.names(d) <- d$id <- NULL

d <- merge(d1, mgmnt, by="treatment_id")	
  
  ## from publication
  fert <- data.frame(
    treatment_id = c(rep(1, 2), rep(2, 3), rep(3, 3), rep(4, 3), rep(5, 3), rep(6, 3)),
    crop = c("rice", "wheat", "rice", "wheat", "mung bean", "rice", "wheat", "mung bean", "maize", "wheat", "mung bean", "rice", "wheat", "mung bean", "maize", "wheat","mung bean"),
    N_fertilizer = c(175, 150, 150, 150, 0, 160, 150, 0, 175, 150, 0, 160, 150,0, 175, 150, 0),
    P_fertilizer = c(58, 58, 50, 64, 0, 64, 64, 0, 64, 64, 0, 64, 64, 0, 64, 64, 0),
    K_fertilizer = c(0, 0, 60, 32, 0, 62, 32, 0, 62, 32, 0, 62, 32, 0, 62, 32, 0)
  )
fert <- aggregate(fert[, c("N_fertilizer", "P_fertilizer", "K_fertilizer")], fert["treatment_id"], sum)
		   
  d <- merge(d, fert, by = c"treatment_id", all.x = TRUE)
  d$treament_id <- NULL
	
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  d$yield  <- NA
  d$yield_moisture <- NA
  d$yield_isfresh <- NA
  d$yield_part <- "none"
  d$irrigated <- NA
  d$planting_date <- NA
  d$harvest_date <- NA
  d$trial_id = "1"
  
  carobiner::write_files(path, meta, d)
}


