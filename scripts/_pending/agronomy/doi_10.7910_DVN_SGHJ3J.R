# R script for "carob"

#ISSUES
# need to unpack the treatmens
# process weather data (use carobiner::write_files(path, dset, d, wth=w))


carob_script <- function(path) {

"In the Sudanian region of Mali where sorghum is an important crop, inorganic fertilizer use is limited due to the high cost and limited availability, and limited soil moisture availability. However, fertilizers from livestock and poultry manure are largely available. This study evaluated different fertilizer response scenarios which will combine both organic and inorganic sources. Our target is to increase productivity (grain and biomass). Biomass so produced could further be used as a source of feed for the livestock and vice-versa. Furthermore, data collected from different agro-ecologies will be used to set up crop simulation models with climate model output of the Coupled Model Inter-comparison Project Phase 5 (CMIP5) to assess climate change impacts on sorghum yields and to evaluate the marginal cost-benefit of different fertility scenarios and varieties as adaptation options to climate change"
  
  uri <- "doi:10.7910/DVN/SGHJ3J"
  group <- "agronomy"
  
  ff  <- carobiner::get_data(uri, path, group)
  
  dset <- data.frame(
    carobiner::get_metadata(uri, path, group, major=2, minor=0,
    data_organization = "ICRISAT",
    publication = NA,
    project = NA,
    data_type = "experiment",
    treatment_vars = "N_fertilizer;P_fertlizer;OM_applied",
	response_vars = "yield",
    carob_contributor = "Hope Mazungunye",
    carob_effort = NA,
    carob_date = "2024-07-18"
  )
  
  f <- ff[basename(ff) == "Boug_17_18.csv"]
  f1 <- ff[basename(ff) == "Kout_17_18.csv"]
  f2 <- ff[basename(ff) == "Madina_WX.csv"]
  f3 <- ff[basename(ff) == "M'pessoba_WX.csv"]
  f4 <- ff[basename(ff) == "SMK_17_18.csv"]
  #f5 <- ff[basename(ff) == "weather_agronomic_soil_data_2017_2018.csv"]
  r <- read.csv(f)
  r2 <- read.csv(f1)
  r3 <- read.csv(f2)
  r4 <- read.csv(f3)
  r5 <- read.csv(f4)
  #r6 <- read.csv(f5)
  #having a difficulty downloading and reading weather_agronomic_soil_data_2017_2018 sheet.
  #r6 <- readxl::read_excel(f5, sheet = Madina_WX)
  #r6 <- carobiner::read.excel(f5)
  d1 <- data.frame(
    variety=r$Sorghum,
    planting_date=r$Year,
    treatment=r$Treatment,
    maturity_days=r$'Maturity.days.',
    rep=r$Rep,
    flowering_days=r$'X50..flowering..days.',
    dmy_total=r$'Total.biomass..kg.ha.',
    dmy_stems=r$'Stalk_Yield..kg.ha.',
    yield = r$'Grain_yield..kg.ha.'
  )
  d1$crop[d1$variety %in% c("Fadda", "Tieble","Soumba")]<- "sorghum"
  d1$site <- "Bougouni"
  d1$trial_id[d1$site=="Bougouni"] <-"1"
  d1$longitude[d1$site=="Bougouni"] <- -7.48323
  d1$latitude[d1$site=="Bougouni"] <- 11.41768
  d1$elevation[d1$site=="Bougouni"] <- 344
  d2 <- data.frame(
    variety=r2$Sorghum,
    planting_date=r2$Year,
    treatment=r2$Treatment,
    maturity_days=r2$Maturity.days.,
    rep=r2$Rep,
    flowering_days=r2$X50..flowering..days.,
    dmy_total=r2$Total.biomass..kg.ha.,
    dmy_stems=r2$Stalk_Yield..kg.ha.,
    yield = r2$Grain_yield..kg.ha. 
    # etc
  )
  d2$crop[d2$variety %in% c("Fadda", "Tieble","Soumba")]<- "sorghum"
  d2$site <- " Koutiala"
  d2$trial_id[d2$site=="Koutiala"] <- "2"
  d2$longitude[d2$site=="Koutiala"] <- -5.7168
  d2$latitude[d2$site=="Koutiala"]<- 12.6666
  d2$elevation[d2$site=="Koutiala"] <- 351
  ## geolocation data for this site "Koutiala" is is coming off as NA's. Assistance required. 
  ## I have so far ommitted the weather data sheets d3 and d4 as i cannot merge with the agronomic datasheets. 
  #d3 <- data.frame(
   # season=r3$Year,
    #rain=r3$Rain,
    #tmin=r3$Tmin,
    #tmax=r3$Tmax,
    #srad=r3$Srad,
    #date=r3$Date
  #)
  #d3$site <- "Madina"
  
  #d4 <- data.frame(
   # $season=r4$Year,
    #rain=r4$rain,
    #tmin=r4$Tmin,
    #tmax=r4$Tmax,
    #srad=r4$Srad,
    #date=r4$Date
  #)
  #d4$site <- "M’pessoba"
  
  d5 <-  data.frame(
    variety=r5$Sorghum,
    planting_date=r5$Year,
    treatment=r5$Treatment,
    maturity_days=r5$Maturity.days.,
    rep=r5$Rep,
    flowering_days=r5$X50..flowering..days.,
    dmy_total=r5$Total.biomass..kg.ha.,
    dmy_stems=r5$Stalk_Yield..kg.ha.,
    yield = r5$Grain_yield..kg.ha. 
    # etc
  )
  d5$crop[d5$variety %in% c("Fadda", "Tieble","Soumba")]<- "sorghum"
  d5$site <- "Samako"
  d5$trial_id[d5$site=="Samako"]<- "3"
  d5$longitude[d5$site=="Samako"]<- -5.7168
  d5$latitude[d5$site=="Samako"]<- 12.6666
  d5$elevation[d5$site=="Samako"]<- 351
  
  d <-carobiner::bindr(d1,d2,d5)
  d$on_farm <- TRUE 
  d$is_survey <- FALSE
  d$irrigated <- FALSE

    d$planting_date <- as.character(as.Date(d$planting_date))  
  d$country <- "Mali"
  d$yield_part <- "grain"
    carobiner::write_files(path, dset, d)
}

