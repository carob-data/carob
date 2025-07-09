# R script for "carob"


carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
  
  uri <- "doi:10.25502/an1w-pt92"
  group <- "survey"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
    publication= NA, 
    data_organization = "IITA;ICRAF;WUR",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-08-20",
    data_type="survey",
	response_vars = "none",
	treatment_vars = "none",
    project=NA 
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
  d3$yield <- ifelse((d3$yield_unit=="kg") & d3$farm_size >0, d3$yield1/d3$farm_size,
              ifelse(d3$yield_unit=="kg " & d3$farm_size >0, d3$yield1/d3$farm_size,
              ifelse(d3$yield_unit=="g" & d3$farm_size >0, d3$yield1*0.01/d3$farm_size,
              ifelse(d3$yield_unit=="tonnes" & d3$farm_size >0, d3$yield1*1000/d3$farm_size,
              ifelse(d3$yield_unit=="tonnes" & d3$farm_size >0, d3$yield1*1000/d3$farm_size,
              ifelse(d3$yield_unit=="KG" & d3$farm_size >0, d3$yield1*1000/d3$farm_size, d3$yield1))))))
  # remove rows without yield_unit or unknown yield_unit
  d3 <- d3[d3$yield_unit!="" & !is.na(d3$yield_unit) & d3$yield_unit!="bales " & d3$yield_unit!="bales",]
  
  # merge d and d3
  d <- merge(d,d3,bx="trial_id", all.y = T)
  d$inoculated <- FALSE
  d$inoculated[!is.na(d$inoculation_type)| d$inoculation_type !=""] <- TRUE
  d <- d[, c("country", "trial_id", "location","adm1","adm2","longitude", "latitude","crop", "yield","fertilizer_type","inoculated")]
  # # EGB: These are not OM applications, but more like the residue management.
  # # In any case none of the values indicate it was integrated in the field. Therefore, removing.
  #process
  # d4 <- r10[,c("farm_id","haulm_use_now")]
  # colnames(d4) <- c("trial_id","OM_type")
  # #merge d and d4
  # d <- merge(d,d4,by="trial_id",all.x = T)
  # Add columns
  
  d$on_farm <- FALSE
  d$is_survey <- TRUE
  d$irrigated <- FALSE
  # Fix fertilizer_type
  p <- carobiner::fix_name(d$fertilizer_type)
  p <- gsub("AN, Compound D" ,"D-compound; AN" ,p)
  p <- gsub("Comound D, AN"   ,"D-compound; AN" ,p)
  p <- gsub("Compound D, AN, tobaccofert" ,"D-compound; AN; unknown" ,p)
  p <- gsub("Compound C, Compound D, AN"    ,"D-compound; AN; C-compound" ,p)
  p <- gsub("Compound D, Compound C"    ,"D-compound; C-compound" ,p)
  p <- gsub("Compound D,AN","D-compound; AN" ,p)
  p <- gsub("compound D, AN","D-compound; AN" ,p)
  p <- gsub("D-compound; C-compound, AN","D-compound; AN; C-compound" ,p)
  p <- gsub("Compound C, AN","C-compound; AN" ,p)
  p <- gsub("Compound D, AN " ,"D-compound; AN" ,p)
  p <- gsub("Compound d, AN" ,"D-compound; AN" ,p)
  p <- gsub("Compound D, AN" ,"D-compound; AN" ,p)
  p <- gsub("Ammonium Nitrate" ,"AN" ,p)
  p <- gsub("Compound D" ,"D-compound",p)
  p <- gsub("compound D"   ,"D-compound",p)
  d$fertilizer_type <- p
  d$fertilizer_type[d$fertilizer_type=="Gypsum, D-compound"] <- "gypsum; D-compound"
  # contain of D compound : 10 N:20 P2O5:10 K2O  #  
  # contain of C compound : 5 N:15 P2O5:12 K2O+ 11 S 0.1 B # I found this online 
  # AN contain is unknown 
  #add fertilizer
  d$N_fertilizer <- 0
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  
  d$N_fertilizer[d$fertilizer_type=="D-compound; AN"|d$fertilizer_type=="D-compound; AN; unknown"] <- 10 +0
  d$N_fertilizer[d$fertilizer_type=="gypsum; D-compound" |d$fertilizer_type=="D-compound"] <- 10
  d$N_fertilizer[d$fertilizer_type=="C-compound; AN"] <- 12+0
  d$N_fertilizer[d$fertilizer_type=="D-compound; AN; C-compound"] <- 10+0 + 12
  d$N_fertilizer[d$fertilizer_type=="D-compound; C-compound"] <- 10+12
  
  d$P_fertilizer[d$fertilizer_type=="D-compound; AN" |d$fertilizer_type=="D-compound" |d$fertilizer_type=="gypsum; D-compound" |d$fertilizer_type=="D-compound; AN; unknown"] <- 20/2.29
  d$P_fertilizer[d$fertilizer_type=="D-compound; AN; C-compound" |d$fertilizer_type=="D-compound; C-compound"] <- (20+15)/2.29
  d$P_fertilizer[d$fertilizer_type== "C-compound; AN"] <- 15/2.29
  
  d$K_fertilizer[d$fertilizer_type=="D-compound; AN" |d$fertilizer_type=="D-compound" |d$fertilizer_type=="gypsum; D-compound" |d$fertilizer_type=="D-compound; AN; unknown"] <- 10/1.2051
  d$K_fertilizer[d$fertilizer_type=="D-compound; AN; C-compound" |d$fertilizer_type=="D-compound; C-compound"] <- (20+12)/1.2051
  d$K_fertilizer[d$fertilizer_type== "C-compound; AN"] <- 12/1.2051
  #fix country name
  dd <- carobiner::fix_name(d$country,"title")
  d$country <- dd
  
  # fix crop name 
  p <- carobiner::fix_name(d$crop, "lower")
  p <- gsub("soya beans", "soybean", p)
  p <- gsub("soyabeans", "soybean", p)
  p <- gsub("soyabean", "soybean", p)
  p <- gsub("soy beans", "soybean", p)
  p <- gsub("sweet potatoes", "sweetpotato", p)
  p <- gsub("sweet potato", "sweetpotato", p)
  p <- gsub("sweet potatoe", "sweetpotato", p)
  p <- gsub("sweetpotatoe", "sweetpotato", p)
  p <- gsub("sweetpotatos", "sweetpotato", p)
  p <- gsub("cowpeas", "cowpea", p)
  p <- gsub("potatoes", "potato", p)
  p <- gsub("rapoko", "finger millet", p)
  p <- gsub("common beans", "common bean", p)
  p <- gsub("sugra beans", "common bean", p)
  p <- gsub("sugarbeans", "common bean", p)
  p <- gsub("sugar beans", "common bean", p)
  p <- gsub("groundnuts", "groundnut", p)
  p <- gsub("roundnuts", "groundnut", p)
  p <- gsub("roundnuta", "groundnut", p)
  p <- gsub("grounduts", "groundnut", p)
  p <- gsub("velvet bean, lablab", "velvet bean; lablab", p)
  p <- gsub("velvet beans", "velvet bean", p)
  p <- gsub("velvet beans", "velvet bean", p)
  p <- gsub("tobbaco", "tobacco", p)
  p <- gsub("beans", "common bean", p)
  d$crop <- p
  d$crop[d$crop=="roudnuts"] <- "groundnut"
  
  d$crop[d$crop=="velvet bean; lablab"] <- "velvet bean" ## I'm not sure it's a good idea to just keep one crop. should we remove them in the dataset? 
  #d <- d[d$crop!="velvet bean; lablab",] # remove rows with two crop in one cell but just one value of yield
  
  #fix crop yield limit by crop
  d$yield[d$crop=="common bean" & d$yield>9000] <- NA
  d$yield[d$crop=="groundnut" & d$yield>8500] <- NA
  d$yield[d$crop=="soybean" & d$yield>15000] <- NA

  #remove crop with very low yield value after divided by the plot area
  d <- d[d$crop!="millet" &d$crop!="pearl millet"&d$crop!="rice",]
  # Fix long and lat coordinate
  d$geo_from_source <- TRUE
  
  d$latitude <- -d$latitude
  i <- d$adm2=="Zano"
  d$latitude[i] <- -20.2044
  d$longitude[i] <- 31.0952
  d$geo_from_source[i] <- FALSE
  i <- d$adm1=="Mudzi "
  d$latitude[i] <- -17.7927
  d$longitude[] <- 31.76780
  d$geo_from_source[i] <- FALSE
  i <- d$adm2=="Gumbodete "
  d$latitude[i] <- -17.8183
  d$longitude[i] <- 31.3724
  d$geo_from_source[i] <- FALSE
  i <- d$adm2=="Chirikure "
  d$latitude[i] <- -16.3432
  d$longitude[i] <- 30.6290
  d$geo_from_source[i] <- FALSE
  i <- d$adm2=="Muzvuwe"
  d$latitude[i] <- -18.3849
  d$longitude[i] <- 32.1372
  d$geo_from_source[i] <- FALSE
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
  
  d$yield_part <- "seed"
  
  # EGB:
  # Adding dates. Since it is a survey conducted in 2013,
  # it is assumed that the responses correspond to the previous cropping season
  # Assuming that the crop was harvested 120 days after planting. Only indicating %Y-%m
  d$planting_date <- as.character(format(as.Date("2012-12-01"), "%Y-%m"))
  d$harvest_date <- as.character(format(as.Date("2012-12-01") + 120, "%Y-%m"))
  
  d$adm1 <- trimws(d$adm1)
  d$adm2 <- trimws(d$adm2)
  
  carobiner::write_files(meta, d, path=path)
}
