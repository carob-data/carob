# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1 - Rate of NPK applied is missing 
# 2- The survey date is ambiguous (the survey is for two seasons (2012–2013) and (2013–2014) but the data is not clear on which season yields were registered).


carob_script <- function(path) {

"
Replication Data for: Trade-Offs and Synergies Between Yield, Labor, Profit, and Risk in Malawian Maize-Based Cropping Systems

As part of an IFPRI-led study, raw and partially processed secondary data have been harmonized and analyzed to examine the ex-ante effects of different legume and fertilizer practices on cropping systems indicators for maize-based smallholder farmers in the Golomoti Extension Planning Area of central Malawi. The current dataset contains the data files and code needed to replicate the results presented in the publication 'Trade-Offs and Synergies Between Yield, Labor, Profit, and Risk in Malawian Maize-Based Cropping Systems.'  Household survey data collected as part of the Africa Research In Sustainable Intensification for the Next Generation (Africa RISING) program, along with secondary data from journal articles and the Malawian Agricultural Market Information System, as well as other biophysical and economic data have been harmonized and analyzed using Stata. Stata do files have been created for data processing and analysis as noted in the 'Documentation' file included in this study. 

This work was undertaken as part of, and funded by, the CGIAR Research Program on Policies, Institutions, and Markets (PIM) led by the International Food Policy Research Institute (IFPRI). PIM is in turn supported by the CGIAR Fund donors. The HarvestChoice Project, funded by the Bill and Melinda Gates Foundation, also funded this study. The United States Agency for International Development funded the collection of the household survey data, as part of the Africa RISING program.
"

	uri <- "doi:10.7910/DVN/ILVSXJ"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2025-10-05",
		design = "unitOfAnalysis", 
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 50,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "amisPrices.xls"]
	f2 <- ff[basename(ff) == "cropMod.xlsx"]
	f3 <- ff[basename(ff) == "arHhData.dta"]
	#f4 <- ff[basename(ff) == "arHhIncom.dta"]
	#f5 <- ff[basename(ff) == "cropPrices.dta"]
	#f6 <- ff[basename(ff) == "fert.dta"]
	#f7 <- ff[basename(ff) == "seed.dta"]
	#f8 <- ff[basename(ff) == "wage.dta"]
	#f9 <- ff[basename(ff) == "aglab.dta"]
	
	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2, sheet="soil")
	r3 <- haven::read_dta(f3)
	#r4 <- read_dta(f4)
	#r5 <- read_dta(f5)
	#r6 <- read_dta(f6)
	#r7 <- read_dta(f7)
	#r8 <- read_dta(f8)
	
	
	### process useful data
	
	
	### process crop price 
	 
	## comment out because it's not related to the crop yield data 
	
	# col <- names(r1)[grepl("y", names(r1))]
	# dcp <- reshape(r1, varying = c(col), v.names = "crop_price", 
	#                times = c(1989:2013),
	#                timevar = "date",
	#                direction = "long")
	# 
	# dcp <- dcp[!is.na(dcp$crop_price),]
	
	##process soil data 
	
	names(r2) <- r2[1,]
	r2 <- r2[-c(grep("Data collected", r2[,1]): nrow(r2)), ]
	r2 <- r2[-1,]
	d1 <- data.frame(
	   adm3 = r2$Lintipe,
	   #deph = r2$Depth,
	   depth_bottom = as.numeric(gsub("–", "", substr(r2$Depth, 3, 6))) ,
	   depth_top = as.numeric(gsub("–", "", substr(r2$Depth, 1, 2))) ,
	   soil_pH = as.numeric(r2$pH),
	   soil_P = as.numeric(r2$`Bray P (ppm)`),
	   #soil_GWC = r2$`PAWC (mm)`,
	   soil_sand = as.numeric(r2$`% sand`),
	   soil_silt = as.numeric(r2$`% silt`),
	   soil_clay = as.numeric(r2$`% Clay`),
	   soil_C_total = as.numeric(r2$`% C`),
	   soil_N = as.numeric(r2$`% N`)* 10000, ## from % to mg/kg
	   record = as.integer(1:nrow(r2))
	)
	
	d1$adm3 <- ifelse( d1$record <= 7, "Linthipe",
	               ifelse( d1$record>7 & d1$record<=11 , "Kandeu", "Golomoti"))
	d1$record <- NULL
	
	
	### process maize (main crop and soybean , groundnut, bean and cowpea as intercrops)
	
	rm <-  r3[grepl("1",  r3$maize), ]
	
  d2 <- data.frame(
     adm2 = rm$a1,
     adm3 = rm$a2,
     location = rm$a3,
     site = rm$a4,
     rain = rm$rain_epa,
     temp = rm$temp_epa/10,
     OM_amount_2012 = ifelse(rm$manure_tot1_ha==0, rm$otherorganic_tot1_ha, rm$manure_tot1_ha) ,
     mulch_2012 = rm$mulch_1,
     OM_amount_2013  = ifelse(rm$manure_tot2_ha==0, rm$otherorganic_tot2, rm$manure_tot2_ha) ,
     mulch_2013 = rm$mulch_2,
     fertilizer_amount = rm$fertilizer_tot_ha,
     fertilizer_price = as.character(rm$fert_value_ha),
     plot_area_maize = rm$maize_area_ha,
     yield_maize = rm$maize_yield,
     residue_prevcrop_maize = rm$maize_res_left_ha,
     urea_maize = rm$maize_urea_ha,
     residue_prevcrop_burnt_maize = rm$maize_res_burn_ha,
     npk_maize = rm$maize_npk_ha,
     
     #groundnut = rm$groundnut,
     plot_area_groundnut = rm$groundnut_area_ha,
     yield_groundnut = rm$groundnut_yield,
     residue_prevcrop_groundnut = rm$groundnut_res_left_ha,
     residue_prevcrop_burnt_groundnut = rm$groundnut_res_burn_ha,
     urea_groundnut = rm$groundnut_urea_ha,
     npk_groundnut = rm$groundnut_npk_ha,
     #rm$groundnut_other_ha
     
     #soybean = rm$soya,
     plot_area_soybean = rm$soya_area_ha,
     yield_soybean = rm$soya_yield,
     residue_prevcrop_soybean = rm$maize_res_left_ha,
     residue_prevcrop_burnt_soybean = rm$soya_res_burn_ha,
     urea_soybean = rm$soya_urea_ha,
     npk_soybean = rm$soya_npk_ha,
     #rm$soya_other_ha,
     
     #cowpea = rm$cowpea,
     plot_area_cowpea = rm$cowpea_area_ha,
     yield_cowpea = rm$cowpea_yield,
     residue_prevcrop_cowpea = rm$cowpea_res_left_ha,
     residue_prevcrop_burnt_cowpea = rm$cowpea_res_burn_ha,
     urea_cowpea = rm$cowpea_urea_ha,
     npk_cowpea = rm$cowpea_npk_ha,
     #rm$cowpea_other_ha,
     
     #bean = rm$bean,
     urea_bean = rm$bean_area_ha,
     plot_area_bean = rm$bean_area_ha,
     yield_bean = rm$bean_yield,
     residue_prevcrop_bean = rm$bean_res_left_ha,
     residue_prevcrop_burnt_bean = rm$bean_res_burn_ha,
     npk_bean = rm$bean_npk_ha,
     intercrops = gsub("^;|;$", "", paste(ifelse(!is.na(rm$bean_yield), "common bean", ""), ifelse(!is.na(rm$cowpea_yield), "cowpea", ""), ifelse(!is.na(rm$groundnut_yield), "groundnut", ""), sep = ";")),
     country = "Malawi",
     trial_id = as.character(rm$hhid),
     planting_date = "2013" ## not sure 
     #rm$bean_other_h
     
  )

dd <- reshape(d2, varying = list(c("yield_maize", "yield_groundnut", "yield_cowpea", "yield_bean", "yield_soybean"),
                                 c("residue_prevcrop_maize", "residue_prevcrop_groundnut", "residue_prevcrop_cowpea", "residue_prevcrop_bean", "residue_prevcrop_soybean"),
                                 c("residue_prevcrop_burnt_maize", "residue_prevcrop_burnt_groundnut", "residue_prevcrop_burnt_cowpea", "residue_prevcrop_burnt_bean", "residue_prevcrop_burnt_soybean"),
                                 c("urea_maize", "urea_groundnut", "urea_cowpea", "urea_bean", "urea_soybean"),
                                 c("npk_maize", "npk_groundnut", "npk_cowpea", "npk_bean", "npk_soybean"),
                                 c("plot_area_maize", "plot_area_groundnut", "plot_area_cowpea", "plot_area_bean", "plot_area_soybean")),
         v.names = c("yield", "residue_prevcrop", "residue_prevcrop_burnt", "urea", "NPK_amount", "plot_area"),
         times = c(1, 2, 3, 4, 5),
         timevar = "crop",
         direction = "long") 

  dd$crop <- c("maize", "groundnut", "cowpea", "common bean", "soybean")[dd$crop]
  
  dd <- dd[!is.na(dd$yield),]
  dd$residue_prevcrop <- as.numeric(dd$residue_prevcrop) 
  dd$residue_prevcrop_used <- ifelse(dd$residue_prevcrop!=0, TRUE, FALSE)
  
  i <- which(dd$residue_prevcrop==0)
  dd$residue_prevcrop[i] <- dd$residue_prevcrop_burnt[i]
  dd$residue_prevcrop_burnt <- NULL
  
  dd$plot_area <- dd$plot_area*10000 # m2
   ## Fixing intercrops 
  dd$intercrops <- ifelse(grepl("groundnut", dd$crop), gsub("groundnut", "maize", dd$intercrops), 
                   ifelse(grepl("cowpea", dd$crop), gsub("cowpea", "maize", dd$intercrops), 
                   ifelse(grepl("common bean", dd$intercrops), gsub("common bean", "maize", dd$intercrops),  dd$intercrops)))
  
  P <- carobiner::fix_name(dd$intercrops)
  P <- gsub("maize;;groundnut", "maize;groundnut", P)
  P <- gsub("common bean;;maize", "common bean;maize", P)
  P <- gsub("^;|;$", "", P)
  P[P == ""] <- NA
  dd$intercrops <- P
  
  dd$intercrops[is.na(dd$intercrops)] <- "none"
  dd$OM_amount <- rowMeans(dd[, c("OM_amount_2012", "OM_amount_2013")])
  dd$mulch <- rowMeans(dd[, c("mulch_2013", "mulch_2012")]) 
  dd$mulch_2012 <- dd$mulch_2013 <-  dd$OM_amount_2012 <- dd$OM_amount_2013 <- dd$id  <- NULL
  
  #### fixing location 
  dd$adm2 <- as.numeric(dd$adm2)-207
  dd$adm2 <- c("Dedza", "Ntcheu")[dd$adm2]
  dd$adm3 <- c("Golomoti", "Linthipe", "Kandeu", "Nsipe", "Mtakataka", "Lobi")[as.numeric(dd$adm3)]
  dd$location <- c("Golomoti Centre", "Mposa", "Kampanje", "Mpamadzi", "Mwalaoyera", "Thete", "Mtakataka Center", "Sitolo")[as.numeric(dd$location)]
  dd$site <- as.numeric(dd$site)-10
  
  dd$site <- c( "Kalumo", "Msamala", "Pitala", "Wilson", "Chibwana", "Mbidze", "Mkuwazi", "Ng'anjo", "Phwere","Katsese", "Kampanje I", "Kampanje II", NA, NA,  "Kazputa", "Dauka", "Gonthi", "Khomba", "Selemani",
                    "Mitchi", "Amosi", "Champiti", "Gwauye", "Hiwa", "Malinda","Njolomole", "Mtambalika", "Ngaipite","Nzililongwe", "Fwalikire", "Chidzondo","Kakhome", "Kautsile", "Kudoole",
                    "Chikawola", "Manyika", "Tseka", "Kambadya",  "Majawa", "Sitolo", "Zaunda", "Chilumo","Chimwala", "Sanjani", "Jingo", "Hauya", "Kahowela", "Mnkhwani", "Mnkhwani II","Pendanyama", "Chizuzu I", "Kabinda II","Gogo", "Maphiri", "Mafuko", "Chimbwala", "Mambewe")[dd$site]
      
  
  geo <- data.frame(
     site = c("Msamala", "Pitala", "Wilson", "Chibwana", "Mbidze", "Mkuwazi", "Ng'anjo", "Kampanje I", "Kampanje II", "Gonthi", "Khomba", "Selemani","Champiti", "Malinda", "Njolomole", "Mtambalika", "Kakhome", "Tseka", "Majawa", "Sitolo", "Chimwala", "Sanjani", "Hauya", "Gogo", "Mafuko", "Chimbwala"),
     longitude = c(34.324, 34.754, 34.9919, 35.458, 34.866, 34.250, 34.0625, 34.589, 34.589, 33.479, 33.609, 34.635, 34.646, 35.0944, 34.928,  34.637, 33.339, 34.868, 35.0548,  35.460, 35.182, 34.822, 34.6101, 35.158, 33.8003, 33.826 ),
     
     latitude = c(-13.240, -14.744, -15.7978, -16.0194, -16.515, -11.680, -11.326, -14.604, -14.604, -13.0409, -13.941, -14.8217, -14.822, -15.845, -15.681, -14.816, -13.245, -14.965, -15.817, -16.0353, -14.6367, -14.687, -15.2197, -15.604, -11.498, -14.0135)
  )
 
  d <- merge(dd, geo, by= "site", all.x = TRUE)
  
  ### Many sites are unknown in Google Maps, so we used d$locations to fill the longitude and latitude of those sites  
  geo_add <- data.frame(
     location = c("Golomoti Centre", "Kampanje", "Sitolo", "Mposa", "Thete", "Mtakataka Center", "Mpamadzi", "Mwalaoyera"),
     lon = c(34.5916, 34.589, 35.4609, 35.5429, 34.078, 34.5126, 35.07274, 34.868),
     lat = c(-14.428, -14.6046, -16.035, -15.139, -14.313, -14.216, -14.2184, -14.045)
  )
  
  d <- merge(d, geo_add, by= "location", all.x = TRUE)
  d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
  d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
  d$lon <- d$lat <- NULL 
  
  
  ### Adding soil information 
  
  ### aggregate soil data by location 
  d1 <- aggregate(cbind(soil_pH, soil_P, soil_sand, soil_silt, soil_clay, soil_C_total, soil_N ) ~ adm3, data= d1, FUN=mean)
  d1$soil_P_method <- "Bray"
  
  d <- merge(d, d1, by= "adm3", all.x = TRUE)
  
  ## Adding fertilizer 
  
  ## the rate of NPK apply is missing
  
  d$N_fertilizer <- ifelse(d$NPK_amount==0, d$urea*0.46, NA)
  d$P_fertilizer <- ifelse(d$NPK_amount==0, 0, NA)
  d$K_fertilizer <- ifelse(d$NPK_amount==0, 0, NA) 
  d$urea <- d$NPK_amount <- NULL
  d$currency <- "MWK"
  
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$geo_from_source <- FALSE
  d$yield_part <- "grain"
  d$yield_moisture <- as.numeric(NA)
  d$irrigated <-  NA


carobiner::write_files(path, meta, d)

}


