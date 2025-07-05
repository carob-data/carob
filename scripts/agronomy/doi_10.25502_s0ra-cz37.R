# R script for "carob"

carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."

  
  uri <- "doi:10.25502/s0ra-cz37"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
    publication= NA, 
    data_organization = "IITA;WUR",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-08-20",
    data_type="experiment",
    project=NA,
	response_vars = "yield",
	treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;inoculated"
  )
  
  f <- ff[basename(ff) == "a_general_1.csv"] 
  f6 <- ff[basename(ff) == "c_land_holding_management.csv"]
  f7 <- ff[basename(ff) == "d_crop_production_use.csv"]
  f10 <- ff[basename(ff) == "e_changes_legume_haulm_process.csv"]
  f12 <- ff[basename(ff) == "e_changes_production_use_2.csv"]
  # read the dataset
  r <- read.csv(f)
  r6 <- read.csv(f6)
  r7 <- read.csv(f7)
  r10 <- read.csv(f10)
  r12 <- read.csv(f12)
  
  
  ## process file(s)
  d <- r[, c("farm_id","country","sector_state","action_site","village","gps_latitude","gps_longitude","gps_latitude_dec","gps_longitude_dec")] 
  
  colnames(d) <- c("trial_id", "country","location","adm1","adm2","latitude1","longitude1","latitude2","longitude2")
  
  # fix long and lat
  i <- is.na(d$latitude1)
  d$latitude1[i] <- d$latitude2[i]
  
  i <- is.na(d$longitude1)
  d$longitude1[i] <- d$longitude2[i]
  d$longitude <- d$longitude1
  d$latitude <- d$latitude1
  
  d1 <- r6[, c("farm_id","farm_size_ha","inoculant_type","other_min_fert_type")] 
  
  colnames(d1) <- c("trial_id","farm_size","inoculation_type","fertilizer_type")
  
  d2 <- r7[, c("farm_id", "crop","total_production_farm","weight_unit")] 
  
  colnames(d2) <- c("trial_id", "crop","yield1","yield_unit")
  
  #merge d1 and d
  d2 <- merge(d1,d2,by="trial_id")
  
  # process 
  d3 <- r12[,c("farm_id","legume_area_now_ha","crop","yield_amount_now","yield_unit_now")]
  colnames(d3) <- c("trial_id","farm_size","crop","yield1","yield_unit")
  d3$inoculation_type <- NA
  d3$fertilizer_type <- NA
  # append d3 and d2
  d3 <- rbind(d3,d2)
  
  d3$yield <- ifelse((d3$yield_unit=="Kg"| d3$yield_unit=="kg" ) & d3$farm_size >0, d3$yield1/d3$farm_size,
                     ifelse(d3$yield_unit=="tonnes" & d3$farm_size >0, d3$yield1*1000/d3$farm_size,
                           ifelse(d3$yield_unit=="Kg/acre" & d3$farm_size >0, d3$yield1/0.4046, d3$yield1)))
  d3 <- d3[d3$yield_unit!="Bunches" & d3$yield_unit!="Bags" & d3$yield_unit!="MT" & d3$yield_unit!="",]
  # merge d and d3
  d <- merge(d,d3,bx="trial_id", all.y = T)
  d$inoculated <- FALSE
  d$inoculated[!is.na(d$inoculation_type)| d$inoculation_type !=""] <- TRUE
  d <- d[, c("country", "trial_id", "location","adm1","adm2","longitude", "latitude","crop", "yield","fertilizer_type","inoculated")]
  # # EGB: These are not OM applications, but more like the residue management.
  # # In any case none of the values indicate it was integrated in the field. Therefore, removing.
  # #process
  # d4 <- r10[,c("farm_id","haulm_use_now")]
  # colnames(d4) <- c("trial_id","OM_type")
  # #merge d and d4
  # d <- merge(d,d4,by="trial_id",all.x = T)
  # Add columns
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  # fertilizer apply is 10-23-23 in 125kg that mean 12.5-28.5-28.5 IN 100kg 
  # Sympal contain: 0-23-26 from protocol
  # DAP content: 18% of N  and 46% P205 
  # Fertilizer rates: SSP and DAP will be applied using a uniform rate of 50 kg per hectare;
  #Urea was applied at a rate of 60 kg N/ha in Kenya and Rwanda trials and we assume it was the same in others Trials
  # Amount of CAN fertilizer apply is unknown 
  #add fertilizer
  d$N_fertilizer <- 0
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  d$N_fertilizer[d$fertilizer_type=="NPK"] <- 12.5
  d$P_fertilizer[d$fertilizer_type=="NPK"|d$fertilizer_type=="NPK, CAN"] <- 28.5
  d$K_fertilizer[d$fertilizer_type=="NPK"|d$fertilizer_type=="NPK, CAN"] <- 28.5
  
  d$N_fertilizer[d$fertilizer_type=="Sympal(NPK)" |d$fertilizer_type=="Sympal"] <- 0
  d$P_fertilizer[d$fertilizer_type=="Sympal(NPK)" |d$fertilizer_type=="Sympal"] <- 23
  d$K_fertilizer[d$fertilizer_type=="Sympal(NPK)" |d$fertilizer_type=="Sympal"] <- 26
  
  d$N_fertilizer[d$fertilizer_type=="Urea"] <- 60 * 0.46
  d$N_fertilizer[d$fertilizer_type=="DAP"] <- 50 * 0.18
  d$P_fertilizer[d$fertilizer_type=="DAP"|d$fertilizer_type=="DAP, Urea" |d$fertilizer_type=="DAP, CAN"] <- 50*0.46/2.29
  d$P_fertilizer[d$fertilizer_type=="SSP"] <- 50
  d$N_fertilizer[d$fertilizer_type=="DAP, Urea"] <- 60*0.46 + 50*0.18 
  d$N_fertilizer[d$fertilizer_type=="NPK, CAN"] <- 12.5 
  d$N_fertilizer[d$fertilizer_type=="DAP, CAN" ] <- 50*0.18 
  d$N_fertilizer[d$fertilizer_type=="Urea, CAN" ] <- 60*0.46 
  #fix country name
  dd <- carobiner::fix_name(d$country,"title")
  d$country <- dd
  # fix crop name 
  p <- carobiner::fix_name(d$crop,"lower")
  p <- gsub("soybeans","soybean",p)
  p <- gsub("sweet potatoes","sweetpotato",p)
  p <- gsub("upland rice","rice",p)
  p <- gsub("soya","soyben",p)
  p <- gsub("soyebean","soybean",p)
  p <- gsub("soybeanbean","soybean",p)
  p <- gsub("fingermillet","finger millet",p)
  p <- gsub("soy","soybean",p)
  p <- gsub("potatoes","potato",p)
  p <- gsub("cow pea","cowpea",p)
  p <- gsub("beams","common bean",p)
  p <- gsub("vegetable","vegetables",p)
  p <- gsub("vegetabless","vegetables",p)
  p <- gsub("simsim","sesame",p)
  p <- gsub("green grams","mung bean",p)
  p <- gsub("greengrams","mung bean",p)
  p <- gsub("soybean beens","soybean",p)
  p <- gsub("soybeanbean","soybean",p)
  p <- gsub("groundnuts","groundnut",p)
  p <- gsub("bambaranut","bambara groundnut",p)
  p <- gsub("bananas","banana",p)
  p <- gsub("bush beans","common bean",p)
  p <- gsub("bush bean","common bean",p)
  p <- gsub("climbing beans","common bean",p)
  p <- gsub("climbing bean","common bean",p)
  p <- gsub("beans","common bean",p)
  p <- gsub("kales","kale",p)
  p <- gsub("roscoco","common bean",p)
  p <- gsub("soybeans","soybean",p)
  p <- gsub("soybeanben","soybean",p)
  d$crop <- p
  d$crop[d$crop=="bean"] <- "common bean"
  # Fix fertilizer_type
  p <- carobiner::fix_name(d$fertilizer_type)
  p <- gsub("DAP, CAN","DAP; CAN",p)
  p <- gsub("NPK, CAN","NPK; CAN",p)
  p <- gsub("DAP, Urea","DAP; urea" ,p)
  p <- gsub("Urea, CAN","urea; CAN" ,p)
  p <- gsub("Urea","urea",p)
  p <- gsub("Compost manure","unknown",p)
  d$fertilizer_type <- p
  d$fertilizer_type[d$fertilizer_type=="Sympal(NPK)"] <- "NPK"
  d$fertilizer_type[d$fertilizer_type=="Sympal"] <- "NPK"
  # fix long and lat coordinate
	geo <- data.frame(
		adm2=c("Ageti","Kadianga","Ojamii","Okuleu","Oyamu","Waanda","Bumagunda","Jemugongu","Kitumba","Manyatta","Mbale","Sio-Port", "Stella", "Abur-Rwatana","Apokor","Apokori","Katakua B'","Katakwa","Katelepai"),
        lat=c(0.3712048,-0.6627352,0.3712048,0.3712048,0.3712048,-0.1029109,0.3712048,-0.1029109,0.0819283,-0.374523,0.083501,0.2240966,-1.0211616,0.3712048,0.5299792,0.5299792,0.3712048,0.3712048,0.73333),
        lon=c(34.2647952,34.6114179,34.2647952,34.2647952,34.2647952,34.7541761,34.2647952,34.7541761,34.7084197,37.4491755,34.7203265,34.0216329,34.3096432,34.2647952,34.28206,34.28206,34.3096432,34.3096432,34.36667)
	)
  
  d <- merge(d, geo, by="adm2", all.x = TRUE)
  d$geo_from_source <- TRUE
  i <- !is.na(d$lat)
  d$latitude[i] <- d$lat[i]
  d$longitude[i] <- d$lon[i]
  d$geo_from_source[i] <- FALSE
  d$lat <- d$lon <- NULL
  #Fix error in lon and lat
  i <- d$adm1=="Kakamega"
  d$longitude[i] <- 34.75
  d$latitude[i] <- 0.2833
  d$geo_from_source[i] <- FALSE
  i <- d$adm1=="Kisumu"
  d$longitude[i] <- 34.7542
  d$latitude[i] <- -0.1029
  d$geo_from_source[i] <- FALSE
  i <- d$adm1=="Busia"
  d$longitude[i] <- 34.36667
  d$latitude[i] <- 0.73333
  d$geo_from_source[i] <- FALSE
  
  #fix crop yield limit by crop
  d$yield[d$crop=="common bean" & d$yield > 9000] <- NA
  d$yield[d$crop=="groundnut" & d$yield > 8500] <- NA
  d$yield[d$crop=="soybean" & d$yield > 15000] <- NA
  # remove crop with very low yield value after divided by the plot area and it's not realistic
  d <- d[d$crop!="mung bean"&d$crop!="rice"&d$crop!="sesame"&d$crop!="bambara groundnut"&d$crop!="banana",]
  
  # fix whitespace in variable
  d$fertilizer_type[d$fertilizer_type==""] <- NA
  d$location[d$location==""] <- NA
  d$adm1[d$adm1==""] <- NA
  d$adm2[d$adm2==""] <- NA
  # # EGB:
  # # Removing OM_type and OM_used since there is no info on such
  # d$OM_type[d$OM_type==""] <- NA
  # # add column
  # d$OM_used <- FALSE
  # d$OM_used[!is.na(d$OM_type)] <- TRUE
  # data type
  d$location <- as.character(d$location)
  d$yield_part <- "seed"
  d$planting_date <- as.character(NA)
  carobiner::write_files(meta, d, path=path)
}
