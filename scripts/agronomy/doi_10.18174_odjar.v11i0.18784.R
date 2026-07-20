# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. P and K fertilizer data not available.
#2. higher values in Prec;ETP;fwy_storage
#3. 4 records with harvest_date within 45 days of planting_date
carob_script <- function(path) {
  
  
  "This paper presents data from a two-year study with sugar beet in Germany (2000 and 2001). A total of 27 field trials were conducted in a wide range of environmental conditions including trials with and without irrigation. Sequential harvests were made every 14-28 days between May and October. Root yield and quality, leaf yield, leaf area index and soil water content were determined in four replicates at each harvest date. Soil characteristics were assessed in the field and daily weather data were collected for each trial site. The dataset is suitable for validating sugar beet growth models."
  
  uri <- "doi:10.18174/odjar.v11i0.18784"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group,
                         files="https://odjar.org/article/view/18784/18279/Dataset.zip")
  
  meta <- data.frame(
    uri            = uri,
    dataset_id     = "doi_10.18174_odjar.v11i0.18784",
    license        = "CC BY 4.0",          
    title          = "Sugar beet (Beta vulgaris L.) yield and quality data at different harvest dates from a multi-environmental study in Germany",
    authors        = "Christine Kenter;Christa M. Hoffmann",
    date_published = "2025-04-04",        
    publisher = "ODjAR",              
    version        = NA,
    description    = "This paper presents data from a two-year study with sugar beet in Germany (2000 and 2001). A total of 27 field trials were conducted in a wide range of environmental conditions including trials with and without irrigation. Sequential harvests were made every 14-28 days between May and October. Root yield and quality, leaf yield, leaf area index and soil water content were determined in four replicates at each harvest date. Soil characteristics were assessed in the field and daily weather data were collected for each trial site. The dataset is suitable for validating sugar beet growth models.",
    design         = NA,
    data_citation  = "Christine Kenter.Christa M. Hoffmann.2025. Sugar beet (Beta vulgaris L.) yield and quality data at different harvest dates from a multi-environmental study in Germany. ODjAR. doi:10.18174/odjar.v11i0.18784",
	carob_group = group,
	data_organization = "IFZ", #nstitute of Sugar Beet Research",
	publication = "doi:10.18174/odjar.v11i0.18784",
	project = NA,
	data_type = "experiment",
	treatment_vars = "irrigated",
	response_vars = "yield",
	carob_completion = 90,
	carob_effort = 7,
	carob_contributor = "Blessing Dzuda",
	carob_date = "2026-07-14",
	notes = NA
  )

  ## read data 
  
  f1 <- ff[basename(ff) == "1_sheets_variables.csv"]
  f2 <- ff[basename(ff) == "2_site_information.csv"]
  f3 <- ff[basename(ff) == "3_tillage.csv"]
  f4 <- ff[basename(ff) == "4_site_field_information.csv"]
  f5 <- ff[basename(ff) == "5_sowing_field_emergence.csv"]
  f6 <- ff[basename(ff) == "6_N_fertilisation.csv"]
  f7 <- ff[basename(ff) == "7_irrigation.csv"]
  f8 <- ff[basename(ff) == "8_field_data.csv"]
  f9 <- ff[basename(ff) == "9_weather_data.csv"]
  
  #r1 <- read.table(f1) varoable definition sheet
  r2 <- read.table(f2,sep = ";",header=TRUE,fileEncoding = "latin1", stringsAsFactors = FALSE)
  r3 <- read.table(f3,sep = ";",header=TRUE,fileEncoding = "latin1", stringsAsFactors = FALSE)
  r4 <- read.table(f4,sep = ";",header=TRUE,fileEncoding = "latin1", stringsAsFactors = FALSE)
  r5 <- read.table(f5,sep = ";",header=TRUE,fileEncoding = "latin1", stringsAsFactors = FALSE)
  r6 <- read.table(f6,sep = ";",header=TRUE,fileEncoding = "latin1", stringsAsFactors = FALSE)
  r7 <- read.table(f7,sep = ";",header=TRUE,fileEncoding = "latin1", stringsAsFactors = FALSE)
  r8 <- read.table(f8,sep = ";",header=TRUE,fileEncoding = "latin1", stringsAsFactors = FALSE)
  r9 <- read.table(f9,sep = ";",header=TRUE,fileEncoding = "latin1", stringsAsFactors = FALSE)
  
  #field level
  field <- data.frame(
    year=r8$year,
    site=r8$site_no,
    country="Germany",
    location=r8$site,
    crop="sugar beet",
    irrigated=r8$irrigation,
    plot_id=as.character(r8$plot),
    rep=r8$rep,
    harvest_date=r8$date,
    DAP=r8$dps,
    LAI=r8$LAI,
    yield=r8$root_yield*1000,
    yield_moisture=r8$root_dry_matter,
    yield_part="roots",
    yield_isfresh=TRUE,
    root_K=r8$K,
    root_Na=r8$Na,
    root_N=r8$soluble_Nt,
    sugar_content=r8$invert_sugar)

  ##soil attributes
  site <- data.frame(
    site=r2$site_no,
    location=r2$site,
    soil_type=r2$soil_type,
    soil_texture=r2$soil_texture,
    soil_FC=r2$field_capacity)
  
  d <- merge(field, site, by=c("site","location"), all.x=TRUE)
  d <- unique(d)
  
  #there are multiple tillage operations per site+year, later combining to one
  tillage <- data.frame(
    site=r3$site_no,
    location=r3$site,
    year=r3$year,
    land_prep_implement=r3$tillage)
  
  tillage_agg <- aggregate(land_prep_implement ~ site + location + year, data=tillage,
                           FUN=function(x) paste(unique(x), collapse="; "))
  
  d <- merge(d, tillage_agg, by=c("site","location","year"), all.x=TRUE)
  d <- unique(d)
  
  ##prev crop and soil chem
  crops <- data.frame(
    site=r4$site_no,
    location=r4$site,
    year=r4$year,
    previous_crop=r4$precrop,
    intercrops=r4$intercrop,
    soil_N=r4$soil_mineral_N,
    depth_top=0,
    depth_bottom=r4$depth,
    soil_pH=r4$pH)
  
  crops <- unique(crops)  
  
  d <- merge(d, crops, by=c("site","location","year"), all.x=TRUE)
  d <- unique(d)
  
  #splitting sowing event from germination-count observations
  sowing <- r5[r5$measure == "sowing", ]
  planting <- data.frame(
    site=sowing$site_no,
    location=sowing$site,
    year=sowing$year,
    variety=tolower(sowing$variety),
    planting_date=sowing$date,
    plant_density=sowing$sowing_density)
  
  counts <- r5[r5$measure != "sowing", ]
  germ_agg <- aggregate(rate_emergence ~ site_no + site + year, data=counts, FUN=max, na.rm=TRUE)
  names(germ_agg) <- c("site","location","year","germination_rate")
  
  planting <- merge(planting, germ_agg, by=c("site","location","year"), all.x=TRUE)
  
  d <- merge(d, planting, by=c("site","location","year"), all.x=TRUE)
  d <- unique(d)
  
  ##summing split N applications, and combining fertilizer types
  fertilizer <- data.frame(
    site=r6$site_no,
    location=r6$site,
    year=r6$year,
    fertilizer_date=r6$date,
    fertilizer_type=r6$N.fertiliser,
    N_fertilizer=r6$amount_N,
    P_fertilizer=NA_real_,
    K_fertilizer=NA_real_)
  
  fert_N <- aggregate(N_fertilizer ~ site + location + year, data=fertilizer, FUN=sum, na.rm=TRUE)
  fert_type <- aggregate(fertilizer_type ~ site + location + year, data=fertilizer,
                         FUN=function(x) paste(unique(x), collapse="; "))
  fert_agg <- merge(fert_N, fert_type, by=c("site","location","year"))
  
  d <- merge(d, fert_agg, by=c("site","location","year"), all.x=TRUE)
  d <- unique(d)
  
  ##multiple irrigations summing applied amounts per site+year
  irrigation <- data.frame(
    site=r7$site_no,
    location=r7$site,
    year=as.integer(format(as.Date(r7$date), "%Y")),
    irrigation_dates=r7$date,
    irrigation_amount=r7$irrigation)
  
  irrig_agg <- aggregate(irrigation_amount ~ site + location + year, data=irrigation,
                         FUN=sum, na.rm=TRUE)
  
  d <- merge(d, irrig_agg, by=c("site","location","year"), all.x=TRUE)
  d <- unique(d)
  
  #weather data
  weather <- data.frame(
    location=r9$site,
    date=as.character(r9$date),
    tmin=r9$min_temp,
    tmax=r9$max_temp,
    temp=(r9$min_temp + r9$max_temp) / 2,
    prec=r9$precipitation,
    srad=r9$glob_radiation*1000,
    ETP=r9$ET_grass)
  

  ##added administrative levels basing on the locations where the experiment was done
 ##adding coordinates using the point radius method
  ##using GADM adm names because GADM strips the German administrative-type prefixes, leaving only the place names
  loc <- data.frame(
    location=c("Kannemoor","Reutershof","Suderburg","Hamersleben","Wörbzig","Harste","Friemar","Kelz","Heuchelheim","Euerhausen","Plattling"),
    adm1 = c("Bayern", "Bayern", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Niedersachsen", "Nordrhein-Westfalen", "Sachsen-Anhalt", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"),
    adm2 = c("Deggendorf", "Würzburg", "Gießen", "Mecklenburgische Seenplatte", "Göttingen", "Uelzen", "Düren", "Anhalt-Bitterfeld", "Börde", "Dithmarschen", "Gotha"),
    adm3 = c("Plattling", "Giebelstadt", "Heuchelheim", "Stavenhagen", "Bovenden", "Suderburg", "Vettweiß", "Südliches Anhalt", "Westliche Börde", "Marne-Nordsee", "Nesseaue"),
    adm4 = c("Plattling", "Giebelstadt", "Heuchelheim", "Stavenhagen", "Bovenden", "Suderburg", "Vettweiß", "Südliches Anhalt", "Am Großen Bruch", "Volsemenhusen", "Friemar"),
    longitude = c(12.901, 9.936, 8.6181, 12.9084, 9.9374, 10.4245, 6.6052, 12.0383, 11.0949, 9.071, 10.7762),
    latitude = c(48.7833, 49.6486, 50.5879, 53.7055, 51.593, 52.8767, 50.7424, 51.7048, 52.0404, 53.9721, 50.9762),
    geo_uncertainty = c(5066, 5965, 3171, 5581, 9350, 8392, 6836, 14035, 6343, 3381, 2555),
    geo_source = c("GADM 4.1, adm4", "GADM 4.1, adm4", "GADM 4.1, adm4", "GADM 4.1, adm4", "GADM 4.1, adm4", "GADM 4.1, adm4", "GADM 4.1, adm4", "GADM 4.1, adm4", "GADM 4.1, adm4", "GADM 4.1, adm4", "GADM 4.1, adm4")
  )
  
  d <- merge(d,loc,by="location",all.x = T)
  d$trial_id <- paste(d$location,d$plot_id,sep = "_")
  
  d$on_farm <- TRUE #from the report
  d$is_survey <- FALSE
  d$irrigated <- tolower(trimws(d$irrigated)) == "yes"    
  d$geo_from_source <- FALSE
  d$K_fertilizer <- as.numeric(NA)
  d$P_fertilizer <- as.numeric(NA)
  d$yield_part <- "roots"
  d$yield_moisture <- (100-r8$root_dry_matter)
  d$fwy_storage <- r8$root_yield*1000

  ##quality fixes
  
  #fert and OM
  d$OM_type[grepl("cattle manure", d$fertilizer_type, ignore.case = TRUE)] <- "cattle dung"
  d$OM_amount[d$OM_type=="cattle dung"] <- 40000
  
  fert_values <- c(
    "calcium ammonium nitrate" = "CAN",
    "urea ammonium nitrate" = "UAN",
    "urea ammonium nitrate; diammonium phosphate" = "UAN;DAP",
    "NPK 15-15-15 compound fertiliser; calcium ammonium nitrate" = "NPK;CAN",
    "cattle manure, 40 t/ha; calcium ammonium nitrate" = "CAN",
    "monoammonium phosphate; urea" = "MAP;urea",
    "urea" = "urea",
    "urea ammonium nitrate; monoammonium phosphate" = "UAN;MAP",
    "diammonium phosphate; urea ammonium nitrate" = "DAP;UAN"
  )
  
  d$fertilizer_type <- fert_values[as.character(d$fertilizer_type)]  
  
  #land_prep_implements
  landprep_values <- c(
    "autumn ploughing; seedbed combination"="mouldboard plough;disc harrow",
    "seedbed combination"="disc harrow",
    "grubber"="cultivator",
    "autumn ploughing"="mouldboard plough",
    "autumn ploughing; rotary harrow"="mouldboard plough;disc harrow",
    "autumn ploughing; rotary harrow; seedbed combination"="mouldboard plough;disc harrow",
    "autumn ploughing; seedbed combination (2x)"="mouldboard plough;disc harrow",
    "spring ploughing; grubber; rotary harrow/seedbed combination"="mouldboard plough;cultivator;disc harrow",
    "autumn ploughing; grubber; rotary harrow/seedbed combination"="mouldboard plough;cultivator;disc harrow",
    "mulch seeding"="direct seeder",
    "spring ploughing"="mouldboard plough",
    "grubber; Cambridge roller"="cultivator;roller")
  
  d$land_prep_implement <- landprep_values[as.character(d$land_prep_implement)]
  
  #previous crop fix
  crop_values <- c(
    "winter wheat" = "wheat",
    "silage maize" = "maize",
    "spring barley" = "barley",
    "winter barley" = "barley"
  )
  
  d$previous_crop <- crop_values[as.character(d$previous_crop)]
  
  #intercrops
  d$intercrops <- ifelse(d$intercrops == "phacelia","phacelia",
    ifelse(d$intercrops == "oil radish","radish",NA))
  
  #soil_texture
  soil_values <- c(
    "silty loam" = "silty loam",
    "clayey loam" = "clay loam",
    "sandy loam" = "sandy loam",
    "loamy fine sand" = "fine loamy sand"
  )
  
  d$soil_texture <- soil_values[as.character(d$soil_texture)]
  
  d$year <- NULL
  d$site <- NULL
  
  carobiner::write_files(path, meta, d,wth = weather)
}

