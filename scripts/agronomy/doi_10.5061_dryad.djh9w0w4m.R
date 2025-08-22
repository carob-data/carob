# R script for "carob"


carob_script <- function(path) {
   
"Estimates of cropland nutrient budgets and nutrient use efficiencies at national to global scales have generally relied on average nutrient concentrations for quantifying nutrients removed in crops and by-products. Calculation of crop nutrient removal requires estimates of the yield of crop products (e.g. grain or beans) and crop residues (e.g. straw or stover) and the nutrient concentration of these components. Data for yields of crop products are often readily available, but those of crop residues are not. The harvest index can be used to estimate the quantity of crop residue from available data on crop product yields. Nutrients removed in harvested crops are the major source of nutrient losses. The availability of more locally relevant crop coefficients can improve the granularity of estimates for nutrient budgets and use efficiencies.  To aid this, field experimental data were collated for major crops from August 2020 to improve estimates of crop harvest index and nutrient concentrations of crop products and crop residues. This dataset contains available data from published scientific articles or theses. Articles were manually checked for relevant data (generally as summary statistics) and transferred into a standardised format. Initially, these articles were selected based on relevance to maize, rice, soybeans and wheat and for nitrogen, phosphorus, potassium and sulphur. It is envisioned this dataset will increase in scope over time, in terms of included nutrients and crops to further improve crop nutrient budgets and use efficiency estimates across the world"
  
   uri <- "doi:10.5061/dryad.djh9w0w4m"
   group <- "agronomy"
   ff  <- carobiner::get_data(uri, path, group)

  
   meta <- carobiner::get_metadata(uri, path, group, major=12, minor=NA, 
      data_organization = "WUR;APNI;IFA",
      publication=NA, 
      project=NA, 
      data_type= "compilation", 
      response_vars= "yield", 
      treatment_vars = "N_fertilizer; P_fertilizer; K_fertilizer", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-05-31",
      notes=NA
   )
   
   
   f <- ff[ basename(ff)=="Summary_statistics_data_from_articles.csv"]
   
   ## Processing data 
   r <- unique(read.csv(f))
   
   d <- data.frame(
      trial_id = paste0(r$Article_ID, "-", r$Experiment_Number),
	  treatment= r$Treatment_Name,
      rep= r$Replicate_n,
      country= r$Country,
      adm1= r$State_province,
	  location = r$Nearest_town_city,
      elevation= r$GPS_coordinates_altitude,
      latitude= r$GPS_lat_DD,
      longitude= r$GPS_long_DD,
      #r$Annual_rainfall_mean_mm_year,
      rain= r$Actual_rainfall_during.trial_mm, 
      irrigation_method= 
	     ifelse(grepl("Sprinkler|spinkler|sprinklers|sprinkler", r$Irrigation_method_Name), "sprinkler", 
         ifelse(grepl("Pre-watering humidity|Irrigated at booting|Irrigated at silking|Drought-stressed|flexible gated|soil|average", r$Irrigation_method_Name), "unknwon", 
         ifelse(grepl("Furrow|furrow", r$Irrigation_method_Name), "furrow", 
         ifelse(grepl("pivot|Pivot", r$Irrigation_method_Name), "center pivot sprinkler",
         ifelse(grepl("border", r$Irrigation_method_Name), "border",
         ifelse(grepl("surface", r$Irrigation_method_Name), "surface", r$Irrigation_method_Name)))))),

      soil_WHC_sat = r$Soil_moisture_._WHC,
      irrigation_amount= r$Actual_irrigation_mm,
   
   ### Process plot management data
   
      plot_area= r$Plot_area,
      land_prep_method= 
	       ifelse(grepl("conventional tillage|Conventional|convential|cultural practices were|conventional", r$Cultivation_method_Name), "conventional",
           ifelse(grepl("Manual|hand|manual|Hand", r$Cultivation_method_Name), "manual", 
           ifelse(grepl("Conservation", r$Cultivation_method_Name), "minimum tillage", 
           ifelse(grepl("Ploughing|Ploughed|ploughing|plough", r$Cultivation_method_Name), "Ploughing",
           ifelse(grepl("rotavator|rotation", r$Cultivation_method_Name),"rotovating",
           ifelse(grepl("Continuous spring barley|Spring barley|Planted with interrow|Plots were seeded|low-input|Organic|plowed with cultivar|rain-proof shelter|vibrocultivator", r$Cultivation_method_Name), "unknown", 
           ifelse(grepl("Reduced tillage", r$Cultivation_method_Name), "reduced tillage" ,
           ifelse(grepl("seedling transplanting|Transplantation|transplanted", r$Cultivation_method_Name), "transplanting" , 
           ifelse(grepl("Seeds dribbled ", r$Cultivation_method_Name), "dibbling", r$Cultivation_method_Name))))))))),
      
      previous_crop= 
	       ifelse(grepl("barley", r$Previous_crop_Name ),"barley", 
           ifelse(grepl("lupin", r$Previous_crop_Name), "white lupin",
           ifelse(grepl("Wheat|wheat", r$Previous_crop_Name), "wheat", 
           ifelse(grepl("soybean", r$Previous_crop_Name), "soybean",
           ifelse(grepl("Potato", r$Previous_crop_Name), "Potato", 
           ifelse(grepl("maize|Zea mays L.", r$Previous_crop_Name), "maize", r$Previous_crop_Name)))))),
   
      planting_date= as.character(as.Date(as.numeric(gsub("0", NA, r$Sowing_date_DD_MM_YY)), origin="1899-12-31")),
      harvest_date= r$Date_harvest_YYYYMMDD, 
      #r$Latin_name,
      #crop= r$Crop_original,
      crop= tolower(r$Crop_standardised),
      variety= r$Crop_variety,
      #r$Crop_variety_release_Year,
      #r$Crop_variety_other,
     # intercrops= r$Intercrop_name_Name,
      seed_rate= r$Sow_density_Seeds_m2*10000,
      plant_density= r$Harvest_density_Plants_m2*10000,
      fertilizer_type= 
	     ifelse(grepl("zospirilum brasiliense|extra calcium added|Government |recommendations|mineral|planting time", r$Fertiliser_type_Name), "unknown", 
         ifelse(grepl("Combination| Conventional", r$Fertiliser_type_Name), "NPK",
         ifelse(grepl("organic", r$Fertiliser_type_Name), "unknown", 
         ifelse(grepl("N: Urea, P: Triple super phosphate, K:", r$Fertiliser_type_Name),"NPK;urea", 
         ifelse(grepl("N: Urea; P: Di-ammonium phosphate ", r$Fertiliser_type_Name), "ura;DAP", 
         ifelse(grepl("N: Urea; P: Bio-activated", r$Fertiliser_type_Name), "ura;unknown;MOP", r$Fertiliser_type_Name)))))),
      N_fertilizer= r$Fertiliser_N_kg_N_ha,
      P_fertilizer= r$Fertiliser_P_kg_P_ha,
      K_fertilizer= r$Fertiliser_K_kg_K_ha,
      S_fertilizer= r$Fertiliser_S_kg_S_ha,
      Ca_fertilizer= r$Fertiliser_Ca_kg_Ca_ha,
      Mg_fertilizer= r$Fertiliser_Mg_kg_Mg_ha,
      B_fertilizer= r$Fertiliser_B_kg_B_ha,
      Cl_fertilizer= r$Fertiliser_Cl_kg_Cl_ha,
      Cu_fertilizer= r$Fertiliser_Cu_kg_Cu_ha,
      Fe_fertilizer= r$Fertiliser_Fe_kg_Fe_ha,
      Mn_fertilizer= r$Fertiliser_Mn_kg_Mn_ha,
      Mo_fertilizer= r$Fertiliser_Mo_kg_Mo_ha,
      Zn_fertilizer= r$Fertiliser_Zn_kg_Zn_ha,
      N_fixation= r$N_fixation_kg_N_ha,
      #r$pc_Ndfa

   ## Process yield data
      #fwy_cobs= r$CobY_mean_kg_fresh_ha,
      yield= r$CPY_mean_kg_fresh_ha,
      yield_moisture= r$CPCon_DM_mean_kg_DM_kg_fresh*100,
      fwy_residue= r$CRY_mean_kg_fresh_ha,
      fwy_total= r$AGY_mean_kg_fresh_ha,
      dmy_storage= r$CPY_mean_kg_DM_ha,
      #dmy_cobs= r$CobY_mean_kg_DM_ha,
      dmy_residue= r$CRY_mean_kg_DM_ha,
      dmy_total= r$AGY_mean_kg_DM_ha,
      harvest_index= r$HI_mean_Unitless,
      geo_from_source= TRUE,
      on_farm= NA, 
      is_survey= FALSE,
      yield_part= "none", 
      
   

   ## Process nutrient content  data
  
      grain_protein= r$CPCon_Prot_mean_kg_kg_DM*1000,  # to mg/g
      grain_N= r$CPCon_N_mean_kg_kg_DM*1000,
      grain_P= r$CPCon_P_mean_kg_kg_DM*1000,
      grain_K= r$CPCon_K_mean_kg_kg_DM*1000,
      grain_S= r$CPCon_S_mean_kg_kg_DM*1000,
      grain_Ca= r$CPCon_Ca_mean_kg_kg_DM*1000,
      grain_Mg= r$CPCon_Mg_mean_kg_kg_DM*1000,
      grain_B= r$CPCon_B_mean_kg_kg_DM*1000,
      grain_Cl= r$CPCon_Cl_mean_kg_kg_DM*1000,
      grain_Cu= r$CPCon_Cu_mean_kg_kg_DM*1000,
      grain_Fe= r$CPCon_Fe_mean_kg_kg_DM*1000,
      grain_Mn= r$CPCon_Mn_mean_kg_kg_DM*1000,
      grain_Mo= r$CPCon_Mo_mean_kg_kg_DM*1000,
      grain_Zn= r$CPCon_Zn_mean_kg_kg_DM*1000,
	  
	  # this should be a fraction between 0 and 1, but some are too low, some are too high. None seem good
      #residue_protein= r$CRCon_Prot_mean_kg_kg_DM, 

      residue_N= r$CRCon_N_mean_kg_kg_DM*1000,
      residue_P= r$CRCon_P_mean_kg_kg_DM*1000,
      residue_K= r$CRCon_K_mean_kg_kg_DM*1000,
      residue_S= r$CRCon_S_mean_kg_kg_DM*1000,
      residue_Ca= r$CRCon_Ca_mean_kg_kg_DM*1000,
      residue_Mg= r$CRCon_Mg_mean_kg_kg_DM*1000,
      residue_B= r$CRCon_B_mean_kg_kg_DM*1000,
      residue_Cl= r$CRCon_Cl_mean_kg_kg_DM*1000,
      residue_Cu= r$CRCon_Cu_mean_kg_kg_DM*1000,
      residue_Fe= r$CRCon_Fe_mean_kg_kg_DM*1000,
      residue_Mn= r$CRCon_Mn_mean_kg_kg_DM*1000,
      residue_Mo= r$CRCon_Mo_mean_kg_kg_DM*1000,
      residue_Zn= r$CRCon_Zn_mean_kg_kg_DM*1000, ## mg/g

   ## process Soil data 

      soil_clay= r$Soil_clay_.,
      soil_silt= r$Soil_silt_.,
      soil_sand= r$Soil_sand_.,
      soil_type= r$Soil_texture_Name,
      #soil_texture= r$Soil_class_Name,
      soil_pH= ifelse(is.na(r$Soil_pH_H2O_1.1_Number) & !is.na(r$Soil_pH_H2O_1.2.5_Number) , r$Soil_pH_H2O_1.2.5_Number ,
               ifelse(is.na(r$Soil_pH_H2O_1.1_Number) & !is.na(r$Soil_pH_H2O_1.5_Number), r$Soil_pH_H2O_1.5_Number , r$Soil_pH_H2O_1.1_Number)),
     
      soil_pH_CaCl2= r$Soil_pH_CaCl2_Number,
      soil_bd= r$Soil_Bulk_density_mg_cm3,
      soil_SOC= r$Soil_SOC_K2Cr2O7_g_C_kg*10, # in %
      soil_N= r$Soil_NC_Kjeldahl_g_N_kg*1000,
      soil_C= r$Soil_Carbon_Wakley_Black_g_C_kg*10,
      #r$Soil_Carbon_test_1_g_C_kg,
      #r$Soil_N_.Parco2020._kg_N_ha,
      #r$Soil_N_.De.Silva.2008._kg_N_ha,
      soil_K= r$Soil_K_.Black._kg_K_ha,
      soil_S= r$Soil_S_available_mg_S_kg,
      soil_S_Mehlich= r$Soil_S_Mehlich.3_mg_S_kg,
      #r$Soil_S_.Method.2._mg_S_kg,
      soil_P_available= r$Soil_P_Olsen_mg_kg,
      soil_P_total= r$Soil_P_Bray_2_mg_P_kg,
      #r$Soil_P_Bray_1_mg_P_kg,
      soil_P_Mehlich= r$Soil_P_Mehlich.3_mg_P_kg,
      #r$Soil_K_dry_mg_K_kg,
      #r$Soil_K_wet_mg_K_kg,
      soil_ex_K= r$Soil_K_exchangeable_mg_K_kg,
      soil_K_Mehlich= r$Soil_K_.Mehlich.3._mg_K_kg,
      #r$Soil_K_.Method.1._mg_K_kg,
      soil_Fe= r$Soil_FE_DETPA_extractable_mg_kg,
      soil_Zn= r$Soil_Zn_DETPA_extractable_mg_kg,
      soil_Ca_Mehlich= r$Soil_Ca_Mehlich.3_mg_Ca_kg,
      soil_pH_KCl= r$Soil_pH_KCl_Number,
      soil_SOM= r$Soil_OM_.,
      #r$Soil_N_Alkaline_permanganate_kg_N_ha,
      soil_Cu= r$Soil_Cu_DETPA_extractable_mg_kg,
      soil_Mn= r$Soil_Mn_DETPA_extractable_mg_kg
      #soil_NO3= r$Soil_NO3._.Dumas_method._mg_kg,
      #soil_NH4= r$Soil_NH4._.Dumas_method._mg_kg
      #r$Soil_Ca_.ammonium_acetate._mg_kg,
      #r$Soil_Mg_.ammonium_acetate._mg_kg,
      #r$Soil_K_.ammonium_acetate._mg_kg
   )
	refs = paste0(r$First_author_name, " & ", r$Second_author_name, ", ", r$Year_published, ". ", r$Title_of_article, ". ",
			r$Journal_title, " ", r$Journal_vol, ": ", r$Page_numbers, ". ", r$DOI)
	refs <- gsub("& et al", "et al.", refs)
	refs <- gsub("\\.\\.", ".", refs)
	refs <- gsub("\n| NA", "", refs)
	refs <- gsub(" NA:", ":", refs)
	d$reference <- gsub("‐", "-", refs)
	

   ## Fixing country names Ethopia
   d$country[grepl("United States of America|USA", d$country)] <-"United States"
   d$country[grepl("Republic of Côte d'Ivoire", d$country)] <-"Côte d'Ivoire"
   d$country[grepl("Ethopia", d$country)] <-"Ethiopia"
   d$country[grepl("Irak", d$country)] <-"Iraq"
   
   i <- grepl("0", d$country)
	# from paper ETH Eschikon
	# Eschikon 33, 8315 Lindau, Schweiz
	d$country[i] <- "Switzerland"
	d$latitude[i] <- 47.45065
	d$longitude[i] <- 8.68200
   d$geo_from_source[i]= FALSE
	   
   ## RH do not remove records! You can remove set bad lon/lat to NA if you must
   ## d <- d[!grepl("06°13'N,16°24'E", d$elevation),] ## coord not in land
   ## RH article says: experiments were conducted at Agroscope-Changins (16° 24′ E, 06° 13′ N; altitude: 445 m)
   ## 16 shoudl be 46; but the station is even better identified on Gmaps.
   i <- grepl("06°13'N,16°24'E", d$elevation)
   d$longitude[i] <- 6.2334
   d$latitude[i] <- 46.3979 
   d$geo_from_source[i]= FALSE

## RH  d$elevation <- NULL
	elv <- strsplit(d$elevation, "altitude")
	elv <- gsub("of|above sea level|above sealevel|metres|m", "", sapply(elv, \(i) ifelse(length(i) == 2, i[2], NA)))
	d$elevation <- as.numeric(elv)

	d$elevation[i] <- 445 
   
### solving conflict Error 
   i <- grepl("Cordoba", d$adm1) & grepl("Argentina", d$country)
   d$longitude[i] <- -d$longitude[i]
   d$latitude[i] <- -d$latitude[i]
   i <- grepl("Cordoba", d$adm1) & grepl("Spain", d$country)
   d$longitude[i] <- -4.8
   d$geo_from_source[i]= FALSE
   i <- grepl("Normandy", d$adm1) & grepl("France", d$country)
   d$longitude[i] <- -0.87750000 ## took it from original data
   d$latitude[i] <- 49.03167
   i <- grepl("Sicily", d$location) & grepl("Italy", d$country)
   d$longitude[i] <- 14.6561
   d$latitude[i] <- 37.4004
   d$geo_from_source[i]= FALSE
   
   i <- grepl("Indiana", d$adm1)
   d$longitude[i] <- -abs(d$longitude[i])
   d$geo_from_source[i]= FALSE

   
   geo <- data.frame(
      adm1= c("Punjab","Vysočina", "Khuzestan", "Mymensingh", "Baghdad", "Macedonia", "Central Java", "South Sulawesi Province","Sao Paulo", "Apulia", "New South Wales"),
      country= c("India", "Croatia", "Iran", "Bangladesh","Iraq", "Greece", "Indonesia", "Indonesia", "Brazil", "Italy", "Australia"),
      lon= c(75.05825, 15.63795, 48.206189,  90.4148, 44.3593, 22.98505, 110.1360, 119.88941, -50.15194, 15.9650, 146.6184),
      lat= c(30.30610, 49.49499, 30.597726, 24.747, 33.3247, 40.53592, -7.1466, -3.667005, -25.01333, 41.193183, -32.41697 )
 
   ) 
   d <- merge(d, geo, by= c("country", "adm1"), all.x = TRUE)
   d$longitude[!is.na(d$lon)] <- d$lon[!is.na(d$lon)]
   d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
   d$geo_from_source[!is.na(d$lat)]= FALSE
   d$lat <- d$lon <- NULL
   d$country[grepl("Vysočina", d$adm1)] <-"Czech Republic"
   

	# match planting date
	d$harvest_date[d$harvest_date == "2029-09-25"] <- "2020-09-25"
	d$harvest_date[d$harvest_date == "1899-12-31"] <- NA
  
	i <- d$planting_date == "2017-05-01" & d$harvest_date == "2018-09-25"
	d$harvest_date[i] <- "2017-09-25" ## Liu et al has same dates in both years. Here taking first year.

	i <- d$planting_date == "2017-05-11" & d$harvest_date == "2018-10-02"
	d$planting_date[i] <- "2017-05-07" ## Zhang et al have two years
	d$harvest_date[i] <- "2017-08-28" ## These are for 2017

	i <- d$planting_date == "2007-06-13" & d$harvest_date == "2008-09-10"
	d$planting_date[i] <- "2008-06-12" ## Sharma et al 
	d$harvest_date[i] <- "2008-09-15"
	i <- d$planting_date == "2008-06-16" & d$harvest_date == "2009-09-13"
	d$planting_date[i] <- "2009-06-15" ## Sharma et al 
	d$harvest_date[i] <- "2009-09-13" 

	i <- is.na(d$planting_date) & (!is.na(r$Year_experiment)) & (nchar(r$Year_experiment) == 4)
	d$planting_date[i] <- r$Year_experiment[i]
	d$harvest_date[i] <- r$Year_experiment[i]

	d$irrigated <- FALSE
   d$irrigated[!is.na(d$irrigation_amount)] <- TRUE
  
   ## Fixing method
   P <- carobiner::fix_name(tolower(d$land_prep_method))
   P <- gsub("inoculation|0", "none",  P)
   P <- gsub("strip till", "strip tillage",  P)
   P <- gsub("direct seeding rice", "direct seeding",  P)
   P <- gsub("machine sowed and harvested", "mechanized line sowing", P)
   P <- gsub("spot-planting|dibbling method", "dibbling", P)
   P <- gsub("drill seeding", "mechanized", P)
   P <- gsub("direct seed|no till|no-tillage|no-till|no tillage", "direct seeding", P)
   P <- gsub("paddy|liming pre sowing" , "unknown", P)
   P <- gsub("flooded|herbicide isoproturon|biodynamic|bioorganic", "unknown", P)
   P <- gsub("vibrocultivator (5cm depth)|organic|intensive", "unknown", P)
   P <- gsub("autmoated rolling rainout shelter", "mechanized", P)
   P <- gsub("rain shelter", "unknown",  P)
   P <- gsub("non-fertilized", "unknown",  P)
   P <- gsub("direct seeding rice" , "direct seeding", P)
   P <- gsub("strip tillageage" , "strip tillage", P) 
   P <- gsub("direct sowing" , "direct seeding", P)
   P <- gsub("\\(nt)" , "", P)
   d$land_prep_method <- P
   d$land_prep_method[grepl("plowing", d$land_prep_method)] <- "reduced tillage"
  
    ### Fixing fertilizer type 
   fer <- carobiner::fix_name(tolower(d$fertilizer_type))
   fer <- gsub("monoammonium phosphate", "MAP", fer)
   fer <- gsub("single super phosphate|superphosphate|single superphosephate|singe super phosphate|suerphosphate", "SSP", fer)
   fer <- gsub("triple superphosphate|triple super phosphate|triple super phospate", "TSP", fer)
   fer <- gsub("potassium chloride", "KCl", fer)
   fer <- gsub("diammonium phosphate|di-ammonium phosphate|ammonium phosphate|dap", "DAP", fer)
   fer <- gsub("P2O5 and K20","NPK", fer)
   fer <- gsub("ammonium nitrate", "AN", fer)
   fer <- gsub("calcium superphosphate", "SPCa", fer)
   fer <- gsub("sulfur coated urea", "SCU", fer)
   fer <- gsub("potassium dihydrogen phosphate|potassium dihyrogen phosphate", "MKP", fer)
   fer <- gsub("no fertilizer", "control", fer)
   fer <- gsub("calcium-magnesium phosphate", "CMP", fer)
   fer <- gsub("muriate of potash", "MOP", fer)
   fer <- gsub("potassium sulphate|sulphate of potash", "SOP", fer)
   fer <- gsub("organic fertilizer soybean meal and maize straw", "unknown", fer)
   fer <- gsub("polymer and sulfur-coated urea|poly-coated urea|polymer coated urea", "SCU", fer)
   fer <- gsub("potassium sulphate|potassium sulfate", "SOP", fer)
   fer <- gsub("microelements foliar application|new yara legume|old yara legume", "unknown", fer)
   fer <- gsub("inorganic fertiliser| solution", "", fer)
   fer <- gsub("mustard cake|cropking600s|humic acid|seaweed extract", "unknown", fer)
   fer <- gsub("ammonium sulfate|ammonium sulphate", "DAS", fer)
   fer <- gsub("ammonium thiosulfate", "ATS", fer)
   fer <- gsub("amminium bicarbonate", "(NH₄)HCO3", fer)
   fer <- gsub("amminium bicarbonate", "CaHPO4", fer)
   fer <- gsub("calcium sulfate", "gypsum", fer)
   fer <- gsub("zinc sulphate", "ZnSO4", fer)
   fer <- gsub("boric acid", "H3BO3", fer)
   fer <- gsub("npk|np|n + p + kcl|n, p2o5, k2o", "NPK", fer)
   d$fertilizer_type <- fer
   
   
   # List of Normalized names of fertilizer 
   normalized_names <- c("urea", "SSP", "SOP", "H3BO3", "ZnSO4", "CaHPO4", 
                "\\(NH₄\\)HCO3", "ATS", "DAS", "CMP", "SCU", "KCl", 
                "TSP", "AN", "MAP", "gypsum", "NPK", "unknown")
   
   # Create the regex pattern
   pattern <- paste0("\\b(", paste( normalized_names, collapse = "|"), ")\\b")
   
   # Extract matches 
   d$fertilizer_type <- sapply(
      regmatches(d$fertilizer_type, gregexpr(pattern, d$fertilizer_type, perl = TRUE)),
      function(x) if (length(x) > 0) paste(x, collapse = "; ") else "unknown"
   )
   
   ### Fixing irrigation method 
   P <- carobiner::fix_name(tolower(d$irrigation_method))
   
   P <-  gsub("Overhead |irrigation|-irrigation|ene|", "", P)
   P <-  gsub("rainfed", "none", P)
   P <-  gsub("irrigated\\; None|irrigation bar|irrigated weekly", "unknown", P)
   P <-  gsub("non-irrigated|no|0|none ", "none", P)
   P <-  gsub("flooding\\?|flooded", "continuous flooding", P)
   P <-  gsub("boom|irrigated & not irrigated|ssc| bar|ci|tape", "unknown", P)
   P <-  gsub("irrigarted|pre-sowing|channel|siphon|paddy", "unknown", P)
   P <-  gsub("3 day interval flooding|11 day interval flooding", "continuous flooding", P) ## not sure
   P <-  gsub("drip unknown|drip ", "drip", P)
   P <-  gsub("nonene", "none", P)
   P <-  gsub("flood ", "continuous flooding", P)
   P <-  gsub("irrigated; none|irrigated & nonet irrigated|irrigated|unknonewn|unknown ", "unknown", P)
   P <-  gsub("basin ", "basin", P)
   P <-  gsub("surface ", "surface", P)
   P <-  gsub("unknwon", "unknown", P)
   P[P==""] <- NA
   d$irrigation_method <- P
   
### Fixing crop names
   P <- carobiner::fix_name(tolower(d$previous_crop))
   P <- gsub("\\(solanum lycopersicum l.)|\\(cucumis melo l.))|\\(helianthus annuus l.)|spring|winter", "", P)
   P <- gsub("sugar beets \\(beta vulgaris)", "sugarbeet", P)
   P <- gsub("rye \\(secale cereale)", "rye", P)
   P <- gsub("tef \\(eragrostis tef \\(zucc.)trotter)", "teff", P)
   P <- gsub("berseem \\(trifolium alexandrinum)", "berseem clover", P)
   P <- gsub("\\(sesamum indicum l.)", "sesame", P)
   P <- gsub("\\(sorghum bicolor \\(l)moench)|\\(glycine max \\(l.) merr.)|\\(oryza sativa l.)", "", P)
   P <- gsub("\\(brassica napus l., cultivar eminem-baer)|\\(phaseolus vulgaris l., cultivar torcaza-inia)|\\(brassica napus l. \\(eminem-baer))", "", P)
   P <- gsub("\\(phaseolus vulgaris l. \\(torcaza-inia))", "", P)
   P <- gsub("\\(beta vulgaris l.)|\\(raphanus sativus l.)|\\(avena sativa)|\\(secale cereale l.)|\\(beta vulgaris l.)", "", P)
   P <- gsub("simsim sesame", "sesame", P)
   P <- gsub("tomato and spinach", "tomato; spinach", P)
   P <- gsub("maize \\(zea mays)", "maize", P)
   P <- gsub("no crop for previous 3 years.", "none", P)
   P <- gsub("black-oats|oats|green oat|oat| oat ", "oats", P)
   P <- gsub("0", "unknown", P)
   P <- gsub("linon usitatissimum", "lemon", P)
   P <- gsub("^triticum aestivum l.", "wheat", P)
   P <- gsub("carrots", "carrot", P)
   P <- gsub("soybeans|soyeansoy beans\\?|soyean", "soybean", P)
   P <- gsub("soybeans|soy common beans\\?", "soybean", P)
   P <- gsub("peas", "pea", P)
   P <- gsub("alfalfa|medicago sativa|medic", "lucerne", P)
   P <- gsub("common vetch", "vetch", P)
   P <- gsub("soy beans\\?", "soybean", P)
   P <- gsub("beta vulgaris", "beetroot", P)
   P <- gsub("cajanus cajan", "pigeon pea", P)
   P <- gsub("canola|oilseed rape|rape", "rapeseed", P)
   P <- gsub("cereals", "cereal", P)
   P <- gsub("fodder radish", "radish", P)
   P <- gsub("spelt", "wheat", P)
   P <- gsub("sudangrass", "sorghum", P)
   P <- gsub("sugarbeet", "sugar beet", P)
   P <- gsub("corn", "maize", P)
   P <- gsub("fallow", "none", P)
   P <- gsub("bean ", "bean", P)
   P[P=="bean"] <- "common bean"
   d$previous_crop <- P
   #d$previous_crop[d$previous_crop=="bean"]  <- "common bean"
   
   ### Fixing crop names
   P <- carobiner::fix_name(d$crop)
   P <- gsub("alfalfa", "lucerne", P)
   P <- gsub("indian mustard", "brown mustard", P)
   P <- gsub("soybeans", "soybean", P)
   P <- gsub("linseed flax", "flax", P)
   d$crop <- P
   
# removing one duplicate
	d <- unique(d)
	carobiner::write_files(path, meta, d)
}


