# R script for "carob"

### fertilizer computations need to be fixed. 

carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
  
  uri <- "doi:10.25502/Y18Z-6T60"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
    publication= NA, 
    data_organization = "IITA",
    carob_contributor="Cedric Ngakou",
    carob_effort = NA,
    carob_date="2023-08-16",
    data_type="experiment",
    project=NA,
	response_vars = "yield",
	treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;inoculated"	
  )
  
  f <- ff[basename(ff) == "a_general_1.csv"] 
  f1 <- ff[basename(ff) == "c_land_holding_management_2.csv"]
  f2 <- ff[basename(ff) == "d_crop_production_use.csv"]
  f4 <- ff[basename(ff) == "e_changes_production_use_2.csv"]
  f5 <- ff[basename(ff) == "c_land_holding_management.csv"]
  # read the dataset
  r <- read.csv(f)
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r4 <- read.csv(f4)
  r5 <- read.csv(f5)
  
  ## process file(s)
  d <- r[, c("farm_id","country","sector_state","action_site","village","gps_latitude","gps_longitude","gps_latitude_dec","gps_longitude_dec")] 
  
  colnames(d) <- c("trial_id", "country","location","adm2","adm3","latitude1","longitude1","latitude2","longitude2")
  
  # fix long and lat
  i <- is.na(d$latitude1)
  d$latitude1[i] <- d$latitude2[i]
  
  i <- is.na(d$longitude1)
  d$longitude1[i] <- d$longitude2[i]
  d$longitude <- d$longitude1
  d$latitude <- d$latitude1
   # process management file
  # use stringr:: if need be do not call library(stringr) or any other library
  # library("stringr")
#  library("stringr")
  d1 <- r1[, c("farm_id","size_ha","crops_grown","varieties","min_fert_type","harvest_amount","inoculant_applied","weight_unit","min_fert_amount")] 
         colnames(d1) <- c("trial_id","farm_size","crop","variety","fertilizer_type","yield1","inoculation_type","yield_unit","fertilizer_amount")
  d1[c('crop', 'crop1','crop2','crop3')] <- stringr::str_split_fixed(d1$crop, "/", 4)
  d1[c('yield1', 'yield2','yield3','yield4')] <- stringr::str_split_fixed(d1$yield1, "/", 4)
  
  d1[c('variety', 'variety1','variety2','variety3')] <- stringr::str_split_fixed(d1$variety, "/", 4)
  
  ## extraction of crop1,crop2,crop3 in d1 with the respective yield
  
  d1e1 <- d1[,c("trial_id","farm_size","crop1","variety1","fertilizer_type","yield2","inoculation_type","yield_unit","fertilizer_amount")]
  
  d1e2 <- d1[,c("trial_id","farm_size","crop2","variety2","fertilizer_type","yield3","inoculation_type","yield_unit","fertilizer_amount")]
  d1e3 <- d1[,c("trial_id","farm_size","crop3","variety3","fertilizer_type","yield4","inoculation_type","yield_unit","fertilizer_amount")]
  
  i <- c(50,40,60,90,70,30,20,35,80,10,5,45,15,55,6,65,100,1,2,3,4,7,8,43,13,88,0)
  p <- gsub("%","",d1$crop)
  p1 <- gsub("%","",d1e1$crop1)
  p2 <- gsub("%","",d1e2$crop2)
  p3 <- gsub("%","",d1e3$crop3)
  for(j in i){
    p <- gsub(j,"",p)
    p1 <- gsub(j,"",p1)
    p2 <- gsub(j,"",p2)
    p3 <- gsub(j,"",p3)
  }
  d1$crop <- p
  d1e1$crop1 <- p1
  d1e2$crop2 <- p2
  d1e3$crop3 <- p3
  
  d1 <- d1[,c("trial_id","farm_size","crop","variety","fertilizer_type","yield1","inoculation_type","yield_unit","fertilizer_amount")]
 
 d1e1 <- d1e1[d1e1$crop1!="",]
 colnames(d1e1) <- c("trial_id","farm_size","crop","variety","fertilizer_type","yield1","inoculation_type","yield_unit","fertilizer_amount")
  
 d1e2 <- d1e2[d1e2$crop2!="",]
 colnames(d1e2) <- c("trial_id","farm_size","crop","variety","fertilizer_type","yield1","inoculation_type","yield_unit","fertilizer_amount")
 # d1e3 still have multiple crop in cells 
 # we split again d1e3
 d1e3[c('crop3', 'crop1','crop2')] <- stringr::str_split_fixed(d1e3$crop3, "/", 3)
 d1e3[c('yield4', 'yield2','yield3')] <- stringr::str_split_fixed(d1e3$yield4, "/", 3)
 d1e3[c('variety3', 'variety1','variety2')] <- stringr::str_split_fixed(d1e3$variety3, "/", 3)
 
 d1e3 <- d1e3[,c("trial_id","farm_size","crop3","variety3","fertilizer_type","yield4","inoculation_type","yield_unit","fertilizer_amount")]
 colnames(d1e3) <- c("trial_id","farm_size","crop","variety","fertilizer_type","yield1","inoculation_type","yield_unit","fertilizer_amount")

 # append all the data from d1
 d1 <- rbind(d1,d1e1,d1e2,d1e3)
 
 # merge d and d1
 d1 <- merge(d,d1,by="trial_id",All.X=T)
 
 ## fix crop name in d1
 p <- carobiner::fix_name(d1$crop,"lower")
 p <- gsub("sweet potato","sweetpotato",p)
 p <- gsub("soybeans","soybean",p)
 p <- gsub("banana trees" ,"banana",p)
 p <- gsub("sugar canne"  ,"sugarcane" ,p)
 p <- gsub("sweet-potato" ,"sweetpotato",p)
 p <- gsub("groundnuts"  ,"groundnut" ,p)
 p <- gsub("corn"  ,"maize",p)
 p <- gsub("bean , mais"   ,"common bean",p)
 p <- gsub("sugar cane"   ,"sugarcane",p)
 p <- gsub("cabbages"    ,"cabbage" ,p)
 p <- gsub("sweet patato"    ,"sweetpotato" ,p)
 p <- gsub("cinchona"    ,"quina" ,p)
 p <- gsub("bens"    ,"common bean" ,p)
 p <- gsub("soyabeans"    ,"soybean" ,p)
 p <- gsub("patotoes"    ,"potato" ,p)
 p <- gsub("soya beans"    ,"soybean" ,p)
 p <- gsub("soya" ,"soybean",p)
 p <- gsub("maise"    ,"maize" ,p)
 p <- gsub("9maize"    ,"maize" ,p)
 p <- gsub("soja beans" ,"soybean" ,p)
 p <- gsub("patatoes" ,"potato"  ,p)
 p <- gsub("spud"  ,"potato"  ,p)
 p <- gsub("sobean" ,"soybean"  ,p)
 p <- gsub("soja" ,"soybean"  ,p)
 p <- gsub("haricot"  ,"common bean",p)
 p <- gsub("colocasia" ,"taro",p)
 p <- gsub("beant"  ,"common bean",p)
 p <- gsub("cassa ava"  ,"cassava",p)
 p <- gsub("cassava bread"  ,"cassava",p)
 p <- gsub("soyben"  ,"soybean",p)
 p <- gsub("s weet patato"   ,"sweetpotato",p)
 p <- gsub("seet potato"   ,"sweetpotato",p)
 p <- gsub("groundnuta"   ,"groundnut",p)
 p <- gsub("goundnut"    ,"groundnut",p)
 p <- gsub("peanut"    ,"groundnut",p)
 p <- gsub("groungnut" ,"groundnut",p)
 p <- gsub("soya:"   ,"soybean",p)
 p <- gsub("soybean:"   ,"soybean",p)
 p <- gsub(", bean"   ,"common bean",p)
 p <- gsub("manioc"   ,"cassava",p)
 p <- gsub("manioc:"   ,"cassava",p)
 p <- gsub("cassava:"   ,"cassava",p)
 p <- gsub("cobbage"   ,"cabbage",p)
 p <- gsub("maize:"   ,"maize",p)
 p <- gsub("aubergine"    ,"eggplant",p)
 p <- gsub("aubergine"    ,"eggplant",p)
 p <- gsub("cucumber"    ,"eggplant",p)
 p <- gsub("green pea"     ,"pea" ,p)
 p <- gsub("banana treee"     ,"banana" ,p)
 p <- gsub("banana tree"      ,"banana" ,p)
 p <- gsub("trees"     ,"banana" ,p)
 p <- gsub("legumes"     ,"legume" ,p)
 d1$crop <- p
 d1$crop[d1$crop=="beans:"] <- "common bean"
 d1$crop[d1$crop=="beans"] <- "common bean"
 d1$crop[d1$crop=="bean"] <- "common bean"
 d1$crop[d1$crop=="assava"] <- "cassava"
 d1$crop[d1$crop=="soy"] <- "soybean"
 d1$crop[d1$crop==", maize"| d1$crop==",maize"] <- "maize"
 d1$crop[d1$crop==", cassava"| d1$crop==",cassava"] <- "cassava"
 d1$crop[d1$crop==",common bean"] <- "common bean"
 
# remove cells with two crops and unknown crops
 d1 <- d1[d1$crop!="bean ,cassava and soybean" & d1$crop!="eggplant ,colocase and tomato" & d1$crop!="beans , cassava" & d1$crop!="and cassava"& d1$crop!="eggplant and bean" & d1$crop!="sweetpotato and bean" 
         & d1$crop!="soybean and sweetpotato" & d1$crop!="groundnut and bean" & d1$crop!="maize and bean" & d1$crop!="reforestation" & d1$crop!="maize, cassava" & d1$crop!="maize, cassava" & d1$crop!="maize common beans" 
         & d1$crop!="garden of vegetables" & d1$crop!="forest" & d1$crop!="eucalpytus" & d1$crop!="eucalyptus" & d1$crop!="fallow" & d1$crop!="quinquina" & d1$crop!="quina" & d1$crop!="soybean and bean" & d1$crop!="colcase",]
 #e1 <- d1e3[,c("trial_id","farm_size","crop1","variety1","fertilizer_type","yield2","inoculation_type","yield_unit")]
 #e1 <- e1[e1$crop1!="",]
 #colnames(e1) <- c("trial_id","farm_size","crop","variety","fertilizer_type","yield1","inoculation_type","yield_unit")
 
 #e2 <- d1e3[,c("trial_id","farm_size","crop2","variety3","fertilizer_type","yield3","inoculation_type","yield_unit")]
 #e2 <- e2[e2$crop2!="",]
 #colnames(e2) <- c("trial_id","farm_size","crop","variety","fertilizer_type","yield1","inoculation_type","yield_unit")
 # 
 ##############################################
 # Process production data and  land management
 
  d2 <- r2[, c("farm_id", "crop","total_production_farm","weight_unit")] 
         colnames(d2) <- c("trial_id", "crop","yield1","yield_unit")
  d22 <- r5[,c("farm_id","farm_size_ha")]
      colnames(d22) <- c("trial_id","farm_size")
  #merge d2 and d22
  d2 <- merge(d2,d22,by="trial_id") # add farm-size in the data
  # merge d2 and d (location data)
  d2 <- merge(d,d2,by="trial_id")
  
  d2$yield1[d2$yield1=="12O"] <- "120"
  d2$yield1[grepl("UNKNOWN",d2$yield1) | d2$yield1==""] <- NA
  d2$yield1 <- as.numeric(d2$yield1)
  d2$inoculation_type <- NA
  d2$fertilizer_type <- "unknown"
  d2$variety <- NA
  d2$fertilizer_amount <- NA
  
  
  
################################################################
  # process second production_use_2
  d3 <- r4[,c("farm_id","legume_area_now_ha","crop","yield_amount_now","yield_unit_now")]
  colnames(d3) <- c("trial_id","farm_size","crop","yield1","yield_unit")
  d3$inoculation_type <- NA
  d3$fertilizer_type <- "unknown"
  d3$fertilizer_amount <- NA
  d3$variety <- NA
# merge d3 and d
  d3 <- merge(d,d3,by="trial_id")
  
######################################################################  
  # Append All the data we process 
################################################
  d <- rbind(d1,d2,d3) 
  # remove bad value in the yield
  ## RH should the comma be replaced by a decimal point?
  d <- d[d$yield1!="Unknow" & d$yield1!="unknown" & d$yield1!="4 and 6"& d$yield1!="30 et 50" & d$yield1!="-88"& d$yield1!="7,5"& d$yield1!=" 144,5",]
  d$yield1 <- as.numeric(d$yield1)
  
  ## How is this correct? Are there no other units?
  d$yield <- ifelse((trimws(tolower(d$yield_unit)) %in% c("kg", "kgs")) & (d$farm_size > 0), d$yield1 / d$farm_size, d$yield1)
  
  d$inoculated <- FALSE
  d$inoculated[!is.na(d$inoculation_type)| d$inoculation_type !=""] <- TRUE
  d <- d[, c("country", "trial_id", "location","adm2","adm3","longitude", "latitude","crop", "yield","fertilizer_type","inoculated")]
  
  # Add columns
  
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  
  ## Fix crop name in d2 and d3
  p <- carobiner::fix_name(d$crop,"lower")
  p <- gsub("soyabean","soybean",p)
  p <- gsub("sweet potato","sweetpotato",p)
  p <- gsub("banana trees","banana",p)
  p <- gsub("seed patato","potato",p)
  p <- gsub("sweet-potato","sweetpotato",p)
  p <- gsub("trees","no crop",p)
  p <- gsub("sugar canne","sugarcane",p)
  p <- gsub("soya","soybean",p)
  p <- gsub("soybeans","soybean",p)
  p <- gsub("banana tree","banana",p)
  p <- gsub("groundnuts","groundnut",p)
  p <- gsub("aubergine","eggplant",p)
  p <- gsub("soybeanbean","soybean",p)
  p <- gsub("sugar cane","sugarcane",p)
  p <- gsub("cabbages","eggplant",p)
  p <- gsub("soy","soybean",p)
  p <- gsub("sweet patato","sweetpotato",p)
  p <- gsub("legumes","legume",p)
  p <- gsub("soya beans","soybean",p)
  p <- gsub("soja","soybean",p)
  p <- gsub("oignon","onion",p)
  p <- gsub("soyabeans","soybean",p)
  p <- gsub("sweet potatoes","sweetpotato",p)
  p <- gsub("potatoes","potato",p)
  p <- gsub("patate douce","sweetpotato",p)
  p <- gsub("patate","sweetpotato",p)
  p <- gsub("colocasia","taro",p)
  p <- gsub("cassava bean","cassava",p)
  p <- gsub("peanut","groundnut",p)
  p <- gsub("manioc","cassava",p)
  p <- gsub("mais","maize",p)
  p <- gsub("green pea","pea",p)
  p <- gsub("goundnut","groundnut",p)
  p <- gsub("sweet poato","sweetpotato",p)
  p <- gsub("colocase","taro",p)
  p <- gsub("groungnut","groundnut",p)
  p <- gsub("banania","banana",p)
  p <- gsub("sobean","soybean",p)
  p <- gsub("soybeanbean beans","soybean",p)
  p <- gsub("sean","common bean",p)
  p <- gsub("ben","common bean",p)
  p <- gsub("haricot","common bean",p)
  p <- gsub("beans","common bean",p)
  p <- gsub("soybeanbean","soybean",p)
  
  d$crop <- p
  d$crop[d$crop=="bean"] <- "common bean"
  
  # fix fertilizer_type 
  d$fertilizer_type[d$fertilizer_type=="NPK 17-17-17"|d$fertilizer_type=="NPK 17- 17- 17"|d$fertilizer_type=="NPK 17-17 -17" |d$fertilizer_type=="NPK 17-171-17"
                    |d$fertilizer_type=="NPK 17 -17 -17" |d$fertilizer_type=="NPK 17 -17-17" |d$fertilizer_type=="NPK17-17-17" |d$fertilizer_type=="NPK 17 17 17" |d$fertilizer_type=="NPK 17 -17- 17"] <- "NPK"
  
  d$fertilizer_type[d$fertilizer_type=="DAP/  NPK17-17-17"|d$fertilizer_type=="DAP/ NPK17-17-17"|d$fertilizer_type=="NPK 17-17-17, DAP"|d$fertilizer_type=="NPK 17- 17-17   DAP" |d$fertilizer_type=="NPK 17-17-17    DAP"] <- "NPK; DAP"
  d$fertilizer_type[d$fertilizer_type=="local" |d$fertilizer_type=="-88"|d$fertilizer_type=="RWRK10, local"] <- "unknown"
  
  #add fertilizer
  
  ## WRONG fertlizer amounts must be computed by multiplying quantity applied with content
  ## should probably assing the others to NA, not zero
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- 0
  d$N_fertilizer[d$fertilizer_type=="NPK"|d$fertilizer_type=="NPK 17 17 18" |d$fertilizer_type=="NPK 17-17-18"] <- 17
  d$P_fertilizer[d$fertilizer_type=="NPK"|d$fertilizer_type=="NPK 17 17 18" |d$fertilizer_type=="NPK 17-17-18"] <- 17/2.29

  d$K_fertilizer[d$fertilizer_type=="NPK"] <- 17/1.095
  d$K_fertilizer[d$fertilizer_type=="NPK 17 17 18" |d$fertilizer_type=="NPK 17-17-18"] <- 18/1.095
  
  # fix fertilizer_type 
  d$fertilizer_type[d$fertilizer_type=="NPK 17 17 18" |d$fertilizer_type=="NPK 17-17-18"] <- "NPK"
  
  #fix country name
  dd <- carobiner::fix_name(d$country,"title")
  d$country <- dd
  d$country[d$country=="D.R. Congo"] <- "Democratic Republic of the Congo"
  d$adm3[d$adm2=="MUMOSHO"] <- "MUMOSHO"
  # fix long and lat coordinate
	geo <- data.frame(
		adm3=c("Lulanga","lukunga","kalirine","kasheke","bishwira","bushwira","nfuzi","ikoma","ishungu","buderhe","Buyenga","mufa","IGOBEGOBE","CHIMBI 2","MULAMBI","BURHEMBO","MUMOSHO","CHANIA","BWIREMBE","BUREMBO","Shebeye","KALIRINA","BURHWAGA","CAGOMBE","Chirheja","CIRHOGOLE","cirimba","ibamba","Irambi","IRAMBO","KABIBANGA", "Kabiganda","kalunga","Kananda","karhwa","Karhwa","KARWA","Kashisiraboba","LUGANDA","mazinzi","Mazinzi","MAZINZI","MULAMBA","mulambi","mumosho","Ndola","Nshebeyi","NYANGEZI"),
		lat=c(-0.5220252, -4.371491,-2.7046167,-2.1518846,-2.44139,-2.4413,-2.7708003,-2.5557499,-2.300289,-2.34904,-2.6279646,-2.6279646,-3.2968958,-2.6279646,-2.7708003,-2.6279646,-2.6279646,-3.2968958,-3.2968958,-2.6560833,-2.3883942,-2.259,-2.7708003,-3.2968958,-2.2509531,-2.3883942,-2.6560833,-1.4715551,-2.1065462,-2.1065462,-2.6560833,-2.6560833,-5.5390095,-4.23383,-2.7008167,-2.7008167,-2.7008167,-2.6560833,-2.4991556,-2.6560833,-2.6560833,-2.6560833,-2.259,-2.8146579,-2.6279646,-2.7065333,-2.3883942,-2.6560833),
		long=c(24.5609263,15.2279106,28.5622667,28.8560076,28.73278,28.73278,28.6000504,28.7399014,28.9415696,28.89343,28.8819501,28.8819501,28.1674008,28.8819501,28.6000504,28.8819501,28.8819501,28.1674008,28.1674008,28.86975,28.7938066,29.0447778,28.6000504,28.1674008,28.8140382,28.7938066,28.86975,18.7718785,28.9186227,28.9186227,28.86975,28.86975,19.0386855,28.91839,28.5701333,28.5701333,28.5701333,28.86975,28.8343339,28.86975,28.86975,28.86975,29.0447778,28.6950944,28.8819501,28.5760167,28.7938066,28.86975)
	)
   
  d <- merge(d,geo,by="adm3",all.x = T)
  i <- !is.na(d$lat)
  d$geo_from_source <- !i
  d$latitude[i] <- d$lat[i]
  d$longitude[i] <- d$long[i]
  d$lat <- d$long <- NULL
  
  d$country[d$location=="south kivu"] <- "Democratic Republic of the Congo" # correct country issue
  d$longitude[d$location=="sud kivu" ] <- 27.8705717
  d$latitude[d$location=="sud kivu"] <- -3.2653155
  
  #fix crop yield limit by crop
  d$k <- d$yield
  d$yield[d$crop=="common bean" & d$k>9000] <- NA
  d$yield[d$crop=="groundnut" & d$k>8500] <- NA
  d$yield[d$crop=="soybean" & d$k>15000] <- NA
  d$k <- NULL
 # remove crop with very low yield value after divided by the plot area and it's not realistic
  d <- d[d$crop!="eggplant" & d$crop!="legume" & d$crop!="onion"& d$crop!="pea"& d$crop!="quinquina" & d$crop!="taro" & d$crop!="tomato" & d$crop!="yam" & d$crop!="cabbage" & d$crop!="amaranth" & d$crop!="no crop" & d$crop!="legume",]
  d <- d[!is.na(d$crop),]
  
  # fix whitespace in variable
  d$fertilizer_type[d$fertilizer_type==""] <- NA
  d$location[d$location==""] <- NA
  d$adm2[d$adm2==""] <- NA
  d$adm3[d$adm3==""] <- NA
  d$adm2 <- carobiner::fix_name(d$adm2, "title")
  d$adm3 <- carobiner::fix_name(d$adm3, "title")
	d$planting_date <- as.character(NA)
  
  d$yield_part <- "seed"
  
  d <- unique(d)
  carobiner::write_files(meta, d, path=path)
}
