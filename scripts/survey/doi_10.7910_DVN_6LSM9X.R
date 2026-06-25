# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. added other important tools in d$land_prep_implement
#2. invalid terms errors are as a result of aggregation, which if not done, could have exploded the observations
#3. NA values in country and longitude originates from the raw data

carob_script <- function(path) {
  
"Datasets from the Area-Based Farm Household Survey in 11 provinces from the Mekong Delta River in Vietnam. The Metrics and Indicators for Tracking in GRiSP (MISTIG) main objective is to collect quantitative data from rice farm households in South and South East Asia for the purpose of monitoring progress towards Global Rice Science Partnership (GRiSP) intermediate development outcomes (IDOs). Other objectives • To provide insight into constraints to adoption, adoption, technology targeting, evaluation, and impact assessment. • To make data available for scientists at social sciences and other departments of IRRI for research and publication purposes (2024-07-02)"
  
  uri <- "doi:10.7910/DVN/6LSM9X"
  group <- "survey"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
   data_organization = "IRRI;IITA",
   publication = NA,
   project = NA,
   data_type = "survey",
   treatment_vars = "none",
   response_vars = "none", 
   carob_completion = 0,
   carob_contributor = "Blessing Dzuda",
   carob_date = "2026-05-22",
   carob_effort = NA,
   notes = NA, 
   design = NA
  )
  
  f <- ff[basename(ff) == "A1toA3-Household_information_VN_final-non sensitive data.dta"]
  f2<- ff[basename(ff) == "A4-GPS_VN_final.dta"]
  f3<- ff[basename(ff) == "B-Household_characteristics_VN_final-non sensitive data.dta"]
  f5<- ff[basename(ff) == "D1-Landholding_information_VN_final.dta"]
  f6<- ff[basename(ff) == "D2&D3-Plot_list_&_Rice_varieties_and_seeds_VN_final.dta"]
  f9<- ff[basename(ff) == "F1-Seedbed_establishment_VN_final.dta"]
  f10<- ff[basename(ff) == "F2-Land_preparation_VN_final.dta"]
  f14<- ff[basename(ff) == "G1.2-Fertilizer_use_VN_final.dta"]
  f16<- ff[basename(ff) == "G2.2-Insecticide_use_VN_final.dta"]
  f18<- ff[basename(ff) == "G3.2-Herbicide_use_VN_final.dta"]

  r <- haven::read_dta(f)
  r2 <- haven::read_dta(f2)
  r3<- haven::read_dta(f3)
  r5 <- haven::read_dta(f5)
  r6 <- haven::read_dta(f6)
  r9 <- haven::read_dta(f9)
  r10 <- haven::read_dta(f10)
  r14 <- haven::read_dta(f14)
  r16 <- haven::read_dta(f16)
  r18 <- haven::read_dta(f18)

 d <- data.frame(
   country=r$country_,
   hhid=r$HHID,
   adm1=r$vietnam_state,
   adm2=r$vietnam_District,
   adm3=r$vietnam_commune,
   adm4=r$vietnam_village,
   longitude=r2$Longitude,
   latitude=r2$Latitude)
 
 #country and adm1 fixes
 country_labels <- c(
   "1" = "Philippines",
   "2" = "Vietnam",
   "3" = "India",
   "4" = "Bangladesh",
   "5" = "Myanmar"
 )
 
 adm1_labels <- c(
   "1"  = "Kien Giang",
   "2"  = "An Giang",
   "3"  = "Long An",
   "4"  = "Dong Thap",
   "5"  = "Soc Trang",
   "6"  = "Tien Giang",
   "7"  = "Can Tho",
   "8"  = "Tra Vinh",
   "9"  = "Hau Giang",
   "10" = "Vinh Long",
   "11" = "Bac Lieu",
   "12" = "Ca Mau",
   "13" = "Ben Tre"
 )
 
 d$country <- country_labels[as.character(d$country)]
 d$adm1 <- adm1_labels[as.character(d$adm1)]
 d$adm2 <- tools::toTitleCase(tolower(d$adm2))
 d$adm3 <- tools::toTitleCase(tolower(d$adm3))
 d$adm4 <- tools::toTitleCase(tolower(d$adm4))
 
 ##education
 edu <- data.frame(
  hhid=r3$HHID,
  farmer_education=r3$Literacy)
 
 #fixing farmer education
 edu_labels <- c(
   "1" = "Cannot read & write",
   "2" = "Can sign only",
   "3" = "Can read only",
   "4" = "Can read and write")
 
 edu$farmer_education <- edu_labels[as.character(edu$farmer_education)]
 
 #aggregate
 edu <- aggregate(
   farmer_education ~ hhid,
   data = edu,
   FUN = function(x) {
     x <- unique(na.omit(x))
     paste(x, collapse = ";")
   }
 )
  
 d <- merge(d,edu,by="hhid", all.x = TRUE)

 ## LAND OWNERSHIP
  land <- data.frame(
   hhid      = r5$HHID,
   area      = r5$Area_,
   unit      = r5$Area_unit,
   ownership = r5$Ownership_status,
   soil_type = r5$Soil_type
 )
 
 land$farmland_owned     <- NA
 land$farmland_rentedin  <- NA
 land$farmland_rentedout <- NA
 
 land$farmland_owned[land$ownership == 1] <- land$area[land$ownership == 1]
 land$farmland_rentedin[land$ownership %in% c(2,3,6)] <- land$area[land$ownership %in% c(2,3,6)]
 land$farmland_rentedout[land$ownership %in% c(4,5,7)] <- land$area[land$ownership %in% c(4,5,7)]
 
 #convert area values to hectares
 unit_to_ha <- c(
   "1" = 1,         # hectare
   "2" = 0.404686,  # acre
   "3" = 0.0001,    # sqm
   "4" = 0.250838,  # bigha
   "5" = 0.031354,  # katha
   "6" = 0.004047,  # decimal
   "7" = 0.012641,  # biswa
   "8" = 0.050586   # nali
 )
 
 land$unit      <- as.numeric(as.character(land$unit))
 land$ha_factor <- unit_to_ha[as.character(land$unit)]
 
 land$farmland_owned <- land$farmland_owned*land$ha_factor
 land$farmland_rentedin  <- land$farmland_rentedin*land$ha_factor
 land$farmland_rentedout <- land$farmland_rentedout*land$ha_factor
 
 #soil type
 soil_labels <- c(
   "1" = "sand", "2" = "clay",  "3" = "loam",
   "4" = "clay loam", "5" = "sandy loam", "6" = "silt")
 
 land$soil_texture <- soil_labels[as.character(land$soil_type)]
 
 #aggregate numeric columns 
land_num <- aggregate(land[, c("farmland_owned","farmland_rentedin","farmland_rentedout")],
                      by = list(hhid = land$hhid),
                      FUN = function(x) if(all(is.na(x))) NA else sum(x, na.rm = TRUE))
 
 #aggregate soil texture 
 land_soil <- aggregate(
   soil_texture ~ hhid,
   data = land,
   FUN  = function(x) {
     x <- unique(na.omit(x))
     if (length(x) == 0) NA else paste(x, collapse = ";")})
 
 #merge
 land <- merge(land_num, land_soil, by = "hhid", all = TRUE)
 land <- land[, c("hhid", "farmland_owned", "farmland_rentedin", "farmland_rentedout", "soil_texture")]
 d <- merge(d,land,by="hhid",all.x = TRUE)
 
 ##production variables
 crop <- data.frame(
   hhid=r6$HHID,
   crop=r6$Name_Crop,
   variety=r6$Variety_Name,
   planting_method=r6$method_crop_est,
   seed_rate=r6$Seed_rate
   )
 
 #fixing planting method 
   est_labels <- c(
     "1" = "transplanted",
     "2" = "broadcasting",
     "3" = "broadcasting",
     "4" = "line sowing",
     "5" = "line sowing",
     "6" = "unknown")
 
 crop$planting_method <- est_labels[as.character(crop$planting_method)]
 
 #crop type
 crop_labels <- c(
   "1" = "rice",
   "2" = NA,
   "3" = "wheat",
   "4" = "maize",
   "5" = "legume",
   "6" = "vegetable",
   "7" = NA,
   "8" = NA)
 
 crop$crop <- crop_labels[as.character(crop$crop)]
 
 #AGGREGATE
 # aggregate text columns (collapse unique values with ;)
 crop_text <- aggregate(crop[, c("crop", "variety", "planting_method"),
                             drop = FALSE], by = list(hhid = crop$hhid),
                        FUN = function(x) {x <- unique(na.omit(x))
                          if(length(x) == 0) NA else paste(x, collapse = ";")})
 
 # aggregate seed_rate (sum per household)
 crop_num <- aggregate(crop[, "seed_rate", drop = FALSE],
      by = list(hhid = crop$hhid),FUN = function(x) 
        if(all(is.na(x))) NA else sum(x, na.rm = TRUE))
 
 #merge
 crop <- merge(crop_text, crop_num, by = "hhid", all = TRUE)
 d <- merge(d,crop,by="hhid", all.x = TRUE)
 
 ##labour
 lab <- data.frame(
   hhid=r9$hhid,
   labour_days=r9$hired_day
 )

 #aggregate
 lab <- aggregate(lab[, "labour_days", drop = FALSE],
                       by = list(hhid = lab$hhid),FUN = function(x) 
                         if(all(is.na(x))) NA else sum(x, na.rm = TRUE))
 #merge
 d <- merge(d,lab,by="hhid", all.x=TRUE)
 
 ##implement used
 
 tools <- data.frame(
   hhid=r10$HHID,
   land_prep_implement=r10$land_prep_power_used
 )
 
 #fixing implement
 tool_labels <- c(
   "1"  = "manual",
   "2"  = "animal",
   "3"  = "2 wheel tractor",
   "4"  = "4 wheel tractor",
   "5"  = "floating tiller",
   "6"  = "transplanter",
   "7"  = "drum seeder",
   "8"  = "reaper",
   "9"  = "stripper",
   "10" = "thresher",
   "11" = "combine",
   "12" = "dryer",
   "13" = "irrigation pump",
   "14" = NA,
   "15" = NA,
   "16" = NA,
   "90" = NA
 )
 
 tools$land_prep_implement <- tool_labels[as.character(tools$land_prep_implement)]
 
 #aggregate
 tools <- aggregate(tools[, "land_prep_implement", drop = FALSE],
                    by = list(hhid = tools$hhid),
                    FUN = function(x) {
                      x <- unique(na.omit(x))
                      if(length(x) == 0) NA else paste(x, collapse = ";")
                    })
 #merge
 d <- merge(d,tools,by="hhid", all.x = TRUE)
 
 ##fertilizer
 fert <- data.frame(
   hhid=r14$HHID,
   N_fertilizer=as.numeric(r14$N),
   P_fertilizer=as.numeric(r14$P),
   K_fertilizer=as.numeric(r14$K))
 
 fert <- unique(fert)
 
 #aggregate the amounts
 fert <- aggregate(
   cbind(N_fertilizer, P_fertilizer, K_fertilizer) ~ hhid,
   data = fert,
   FUN = function(x) sum(x, na.rm = TRUE)
 )
 
 #merge
 d <- merge(d,fert,by="hhid", all.x = TRUE)
 
 ##insecticides fixing
 insect <- data.frame(
   hhid=r16$HHID,
   insecticide_product=r16$insecticides_use_applied
 )
 
 insect <- unique(insect)
 
 #insecticide active-ingredient
 insctcde_labels <- c(
   "REGENT" = "fipronil",
   "GERENT" = "fipronil",
   "CAGENT" = "fipronil",
   "CAGENT 3G" = "fipronil",
   "ACTARA" = "thiamethoxam",
   "KARATE" = "lambda-cyhalothrin",
   "SHERPA" = "cypermethrin",
   "PREVATHON" = "chlorantraniliprole",
   " PREVATHON" = "chlorantraniliprole",
   "REVATHON" = "chlorantraniliprole",
   "REVARTHON" = "chlorantraniliprole",
   "PADAN" = "cartab",
   "BADAN" = "cartab",
   "BADANG" = "cartab",
   "BASUDIN" = "diazinon",
   "BUSUDIN" = "diazinon",
   "DIAZAN" = "diazinon",
   "ABAMECTIN" = "abamectin",
   "AMAMECTIN" = "emamectin benzoate",
   "EMAMECTIN" = "emamectin benzoate",
   "TIK ABAMEC" = "abamectin",
   "AGBAMEC" = "abamectin",
   "VIBAMEC" = "abamectin",
   "ABA SUPER" = "abamectin",
   "ABACOL" = "abamectin",
   "AMECTINAIC" = "abamectin",
   "EMAPLANT" = "emamectin benzoate",
   "EMAK" = "emamectin benzoate",
   "TUNGMECTIN" = "abamectin",
   "CHESS 50WG" = "pymetrozine",
   "NEWCHESS" = "pymetrozine",
   "CHEES" = "pymetrozine",
   "SIEU CHESS" = "pymetrozine",
   "INDO SUPER" = "indoxacarb",
   " INDOXACARB" = "indoxacarb",
   "OSHIN" = "dinotefuran",
   "OSIN" = "dinotefuran",
   "APPLAUD" = "buprofezin",
   "DIOPHOS" = "chlorpyrifos",
   "DIAPHOS" = "chlorpyrifos",
   "CYPER ALPHA" = "cypermethrin;alpha-cypermethrin",
   "CYPERAN" = "cypermethrin",
   "SITEXALPHA" = "alpha-cypermethrin",
   "MOCAP" = "ethoprop",
   "DANITOL" = "fenpropathrin",
   "VIRTAKO" = "chlorantraniliprole;thiamethoxam",
   "VIRTAKO 40WG" = "chlorantraniliprole;thiamethoxam",
   "VOLIAM" = "chlorantraniliprole;thiamethoxam",
   "ADMIRE O-TED" = "imidacloprid"
 )
 
 insect$insecticide_product <- insctcde_labels[trimws(insect$insecticide_product)]
 insect <- unique(insect)
 
 #aggregate
 insect <- aggregate(
   insecticide_product ~ hhid,
   data = insect,
   FUN = function(x) {
     x <- unique(na.omit(x))
     paste(x, collapse = ";")
   }
 )
 
 #merge
 d <- merge(d,insect, by="hhid", all.x = TRUE)
 
 ##herbicede
 herb <- data.frame(
   hhid=r18$HHID,
   herbicide_product=r18$herbicide_use_applied
 )
 
 #mapping
 herb_labels <- c(
   "GLYPHOSAN" = "glyphosate",
   "KAPHOSAT" = "glyphosate",
   "CLINCHER" = "cyhalofop-butyl",
   "CLIPER" = "cyhalofop-butyl",
   "LINCHOR" = "cyhalofop-butyl",
   "SOFIT" = "pretilachlor",
   "sofit" = "pretilachlor",
   "SÃ’IT" = "pretilachlor",
   "2,4 D AMINE" = "2,4-D",
   "CO 2.4 D" = "2,4-D",
   "FACET" = "quinclorac",
   "PHACET" = "quinclorac",
   "TOPSHOT" = "oxyfluorfen",
   "STAMP" = "propanil",
   "DUAL" = "metolachlor",
   "CANTACHLOR" = "pretilachlor",
   "SUNRICE" = "penoxsulam",
   "WHIP'S" = "fenoxaprop-p-ethyl",
   "ROCKET" = "oxadiazon",
   "GRAMOXONE" = "paraquat dichloride",
   "GFAXONE" = "paraquat dichloride",
   "CYBU" = "cyhalofop-butyl",
   "SONATO" = "pretilachlor"
 )
 
 herb$herbicide_product <- herb_labels[as.character(herb$herbicide_product)]
 
 #aggregate
 herb <- aggregate(
   herbicide_product ~ hhid,
   data = herb,
   FUN = function(x) {
     x <- unique(na.omit(x))
     paste(x, collapse = ";")
   }
 )
 
 #merge
 d <- merge(d,herb, by="hhid", all.x = TRUE)
 
 d$planting_date <- as.character(NA)
 d$trial_id <- paste(d$hhid, as.character(d$adm4), sep = "_")
 d$on_farm <- TRUE
 d$is_survey <- TRUE
 d$irrigated <- TRUE
 d$geo_from_source <- TRUE
 d$yield <- as.numeric(NA)
 d$yield_part <- "grain"
 d$hhid <- as.character(d$hhid)
 d$yield_moisture <- is.numeric(NA)
 
 carobiner::write_files(path, meta, d)
}
