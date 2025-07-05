# R script for "carob"

carob_script <- function(path) {
  
"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
  
  uri <- "doi:10.25502/gzyt-ks92"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
    publication= NA, 
    data_organization = "IITA;WUR",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-07-29",
    data_type="experiment",
    project="N2Africa" ,
	response_vars= "yield",
	treatment_vars="N_fertilizer;P_fertilizer"
  )
  
  f <- ff[basename(ff) == "general.csv"] 
  f1 <- ff[basename(ff) == "experiment.csv"] 
  f2 <- ff[basename(ff) == "land_crops_and_livestock.csv"] 
  f3 <- ff[basename(ff) == "nutrition.csv"] 
  f4 <- ff[basename(ff) == "production.csv"] 
  f5 <- ff[basename(ff) == "labour_income_and_assets.csv"] 
  f6 <- ff[basename(ff) == "production_constraints.csv"]
  # read the dataset
  r <- read.csv(f)
  r1 <- read.csv(f1)
  r2 <- read.csv(f2)
  r3 <- read.csv(f3)
  r4 <- read.csv(f4)
  r5 <- read.csv(f5)
  r6 <- read.csv(f6)
  
  ## process file(s)
  d <- r[, c("farm_id","country","lga_district_woreda","gps_latitude_hh.decimal_degrees","gps_longitude_hh.decimal_degrees")] 
  colnames(d) <- c("trial_id", "country","location","latitude","longitude")
  d1 <- r1[, c("farm_id", "legume_planted_on_the_n2a_plot")]   
  colnames(d1) <- c("trial_id", "crop")
  
  # # EGB: Add area info
  d2 <- r4[, c("farm_id", "area_field_1", "area_field_1_unit.ha", "area_harvested_most_important_crop_field_1")]
  colnames(d2) <- c("trial_id", "area", "area_units", "area_harvest")
  d2$area_ha <- NA
  d2$area_ha[grep("acre", d2$area_units)] <- d2$area_harvest[grep("acre", d2$area_units)] * 0.405 # 1 acre = 0.40486 Ha
  d2$area_ha[grep("meter_squared", d2$area_units)] <- d2$area_harvest[grep("meter_squared", d2$area_units)] * 10000
  d2$area_ha[grep("hectare", d2$area_units)] <- d2$area_harvest[grep("hectare", d2$area_units)]
  d2 <- d2[,c("trial_id", "area_ha")]
  
  #merge d1 and d
  d <- merge(d,d1,by="trial_id")
  
  # # EGB: Merge d and d2
  d <- merge(d,d2,by="trial_id")
  
  oldnms <- c("farm_id", "grain_weight_kg_shelled_grain_crop_1_plot_X.kg","pod_weight_kg_unshelled_grain_groundnut_crop_1_plot_X.kg","date_of_planting_X","date_of_final_harvest_X","type_of_fertilizer_used_n2africa_plot")  
  
  newnms <- c("trial_id", "yield1", "yield2","pdate","hdate","fertilizer_type")
  
  lst <- list()
  i <- c(1,3)  
  for (j in i) {
    inms <- gsub("X", j, oldnms)
    ri <- r1[, inms] 
    colnames(ri) <- newnms
    lst[[j]] <- ri
  }	
  
  # append all the treatment data
  d2 <- do.call(rbind, lst)
  
  # fix	yield column 
  i <- is.na(d2$yield1)
  d2$yield1[i] <- d2$yield2[i]
  
  #EGB: Calculate yield in kg/ha
  d2 <- merge(d2, d[,c("trial_id", "area_ha")], by = "trial_id")
  d2$yield <- d2$yield1/d2$area_ha
  
  # merge d and d2
  d <- merge(d,d2,bx="trial_id", all.y = T)
  
  d <- d[, c("country", "trial_id", "location", "longitude", "latitude", "pdate","hdate"
             , "crop", "yield","fertilizer_type")]
  # Add columns
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  
  # Fix long and lat coordinate 
  
  # drop NA in location or site
  #d <- d[!(is.na(d$location)| is.na(d$site)),]
  
  #add fertilizer
  # Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare;
  # DAP content: 18% of N  and 46% P205 
  #Urea was applied at a rate of 60 kg N/ha in Kenya and Rwanda trials and we assume it was the same in others Trials
  # EGB: Assuming that 
  d$N_fertilizer <- 0
  d$P_fertilizer <- 0
  d$K_fertilizer <- 0
  d$P_fertilizer[grepl("NPK",d$fertilizer_type)] <- 30
  d$P_fertilizer[grepl("TSP",d$fertilizer_type)] <- 30
  d$P_fertilizer[grepl("DAP",d$fertilizer_type)] <- 30*0.46/2.29 
  d$K_fertilizer[grepl("NPK",d$fertilizer_type)] <- 20
  d$N_fertilizer[grepl("DAP",d$fertilizer_type)] <- 30*0.18
  d$N_fertilizer[grepl("Urea",d$fertilizer_type)] <- 60
  #fix country name
  dd <- carobiner::fix_name(d$country,"title")
  d$country <- dd
  # fix crop name 
  P <- carobiner::fix_name(d$crop,"lower")
  d$crop <- gsub("climbing_bean","common bean",P)
  
  # fix fertilize type
  d$fertilizer_type[d$fertilizer_type==""] <- "none"
  d$fertilizer_type[d$fertilizer_type=="Urea"] <- "urea"
  #fix crop yield limit by crop
  d$k <- d$yield
  d$yield[d$crop=="common bean" & d$k>9000] <- NA
  d <- subset(d,select = -k)
  
  d$pdate <- as.character(as.Date(d$pdate, format='%d/%m/%Y'))
  d$pdate <- gsub("1999", "2015", d$pdate)

  d$hdate <- as.character(as.Date(d$hdate, format='%d/%m/%Y'))
  d$hdate <- gsub("1999", "2015", d$hdate)
 
  # fix whitespace in variable: location, harvest_date
  d$location[d$location==""] <- NA
  # data type
  
  d$yield_part <- "seed"
  d$yield_part[grepl("groundnut",d$crop)] <- "pod"
  
  # EGB:
  # Trying to capture and fix most of the planting date missing values.
  # I am assuming that the same trial_id were planting on the same date.
  ds <- d[, c("trial_id", "pdate", "hdate")]
  pdates <- ds[complete.cases(ds[,c("trial_id", "pdate")]), c("trial_id", "pdate")]
  hdates <- ds[complete.cases(ds[,c("trial_id", "hdate")]), c("trial_id", "hdate")]
  tdates <- merge(pdates, hdates, by = "trial_id", all = TRUE)
  tdates <- unique(tdates)
  del <- merge(d, tdates, by = "trial_id", all.x = TRUE)
  dd <- del[,c("trial_id","on_farm","is_survey",
              "country","location","longitude","latitude",
              "crop","pdate.y","hdate.y","yield","fertilizer_type",
              "N_fertilizer","P_fertilizer","K_fertilizer","yield_part","irrigated")]
  dd <- carobiner::change_names(dd, c("pdate.y","hdate.y"), c("planting_date", "harvest_date"))

  dd$geo_from_source <- TRUE
  
  ## the intervals are all over the place; retain the year
  dd$harvest_date <- dd$planting_date <- substr(dd$planting_date, 1, 4)
   
   dd <- dd[!is.na(dd$yield), ]
  carobiner::write_files(meta, dd, path=path)
}
