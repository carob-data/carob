# R script for "carob"


carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
  
  uri <- "doi:10.25502/ysjw-cw72"
  group <- "survey"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
    publication= NA, 
    data_organization = "IITA;ICRAF;WUR",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-09-18",
    data_type="survey",
	response_vars= "none",
	treatment_vars="none",
    project=NA 
  )
  
  bn <- basename(ff)

  r1 <- read.csv(ff[bn == "a_general.csv"])
  r2 <- read.csv(ff[bn == "a_inputs.csv"])
  r3 <- read.csv(ff[bn == "c_use_of_package_1.csv"])
  r4 <- read.csv(ff[bn == "c_use_of_package_2.csv"])
  r5 <- read.csv(ff[bn == "c_use_of_package_4.csv"])
  r6 <- read.csv(ff[bn == "d_cropping_calendar.csv"])
  r7 <- read.csv(ff[bn == "e_harvest.csv"])
  ## process file(s)
  ### Process General file containing locations information
  # all NA: "gps_longitude", "gps_latitude"
  d <- r1[, c( "farm_id","country","season","district","vilage", "farm_size")] 
  colnames(d) <- c("trial_id", "country","season", "adm2", "adm3", "farm_size")
  
  ############
### process file with crop and management information
  
  d1 <- r4[, c("farm_id","plot_no", "crop_1","variety_1","inoculant_used","organic_fert_type","organic_fert_amount","mineral_fert_type","mineral_fert_amount")] 
  colnames(d1) <- c("trial_id","plot_no", "crop","variety","inoculated", "OM_type", "OM_amount","fertilizer_type","fertilizer_amount")
  
### merge d and d1
   d <- merge(d, d1, by="trial_id",all.x = T)    
        
 ###### process file with management information
  d2 <- r5[, c("farm_id","plot_no","crop_1_spacing_row_to_row","crop_1_spacing_plant_to_plant")] 
  colnames(d2) <- c("trial_id","plot_no", "row_spacing","plant_spacing")
 
  ### merge d and d2
  d <- merge(d,d2,by=c("trial_id","plot_no"),all.x = T)
  
  ######### process file with date  
  
  d3 <- r6[, c("farm_id","date_planting_dd","date_planting_mm","date_planting_yyyy")] 
  d3$planting_date <- paste(d3$date_planting_yyyy, d3$date_planting_mm, d3$date_planting_dd, sep = "-")
  d3 <- d3[,c("farm_id", "planting_date")]
  colnames(d3) <- c("trial_id", "planting_date")
  ## merge d and d3
  d <- merge(d,d3,by=("trial_id"), all.x = T)
  
  ### process file with yield data
  
  d4 <- r7[,c("farm_id","crop_1_area_harvested","crop_1_weight_grain","plot_no")]
  colnames(d4) <- c("trial_id","area_harvested","weight_grain","plot_no")
  
  ### merge d and d4
  d <- merge(d,d4,by=c("trial_id","plot_no"),all.x = T)
  
  d$yield <- (d$weight_grain/d$area_harvested)*10000 ## convert yield to kg/ha
  
  ## Extract Relevant columns 
  
  d <- d[,c("trial_id", "country","season","adm2","adm3", "crop","variety","yield","inoculated","OM_amount","fertilizer_type","fertilizer_amount","row_spacing","plant_spacing","planting_date")]
  
  # Add columns
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$adm2 <- carobiner::fix_name(d$adm2, "title")
  d$adm3 <- carobiner::fix_name(d$adm2, "title")

  # fix lon and Lat 
 #RH: not correct. georeferencing should be done with adm3, not with adm2! 
#  geo <- data.frame(adm2=c("Mudzi","Hwedza","Guruve","Goromonzi","Chegutu","Murehwa","Makoni","MAKONI","Murewa"),
#        latitude=c(-17.0516554,-18.6257778,-16.3431777,-17.8182737,-18.1867654,-17.8044363,-18.3848893,-18.3848893,-17.646074),
#        longitude=c(32.5494492,31.5769196,30.6290426,31.3723592,30.39756,31.8388002,32.1371586,32.1371586,31.777541))
 
#  d <- merge(d, geo, by="adm2")
  
  # Fix fertilizer_type
  p <- carobiner::fix_name(d$fertilizer_type) |> tolower()
  p[grep("harve", p)] <- NA
  p[grep("5|2", p)] <- NA
  p <- gsub(" and | \\+ |, ", "; ", p)
  p <- gsub(",", "; ", p)
  p <- gsub("ssp", "SSP", p)
  p <- gsub("SSP ", "SSP; ", p)
  p <- gsub("gypum", "gypsum", p)
  d$fertilizer_type <- p
  
  #add fertilizer
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- ifelse(is.na(d$fertilizer_type), NA, 0)
  i <- grepl("SSP", d$fertilizer_type)
  d$P_fertilizer[i] <- d$fertilizer_amount[i]
  d$lime <- as.numeric(NA)
  d$gypsum <- as.numeric(NA)
    
  d$fertilizer_amount <- NULL
  #fix country name
  dd <- carobiner::fix_name(d$country,"title")
  d$country <- dd
  
  # fix crop name 
  p <- carobiner::fix_name(d$crop,"lower")
  p <- gsub("climbing bean","common bean",p)
  p <- gsub( "bush bean" ,"common bean",p)
  d$crop <- p
  # remove NA in Crop 
  d <- d[!is.na(d$crop),]
  ## data type
  d$inoculated[d$inoculated==""] <- NA
  d$inoculated <- d$inoculated=="Y" 

  d$planting_date <- as.character(as.Date(d$planting_date, format = "%Y-%m-%d"))
  
  # fix whitespace in variable
  d$fertilizer_type[d$fertilizer_type==""] <- NA
  d$adm2[d$adm2==""] <- NA
  d$adm3[d$adm3==""] <- NA
  d$variety[d$variety==""] <- NA
  
  d$yield_part <- "seed"
  d$yield_part[d$crop=="groundnut"] <- "grain"
  
  # EGB:
  # Capturing latitude and longitud of the ADM3 centroids
  # ll <- carobiner::geocode(country = unique(d$country), location = unique(d$adm3))
  ll <- data.frame(country = c("Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe"), 
                   location = c("Mudzi", "Hwedza", "Guruve", "Goromonzi", "Chegutu", "Murehwa", "Makoni", "Murewa"),
                   longitude = c(32.5494, 31.5708, 30.629, 31.3724, 30.3976, 31.8388, 32.1372, 31.7775), 
                   latitude = c(-17.0517, -18.6225, -16.3432, -17.8183, -18.1868, -17.8044, -18.3849, -17.6461),
				   geo_from_source = FALSE
				)
  d <- merge(d, ll, by.x = c("country", "adm3"), by.y = c("country", "location"))

  ### fix crop yield
#  d$yield[d$crop=="common bean" & d$yield > 9000] <- NA
#  d$yield[d$crop=="groundnut" & d$yield > 8500] <- NA
#  d$yield[d$crop=="cowpea" & d$yield > 5000] <- NA

  carobiner::write_files(meta, d, path=path)
}
