# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1.yield is a simulated future parameter, not a field observation, dropping the year as it is up-to 2099 
#2. out of bounds yield originates from the dataset

carob_script <- function(path) {

"
Yield  and simulated yield data for sunflower, peanut and soybean from three sites at Punda Maria, Ofcolaco and Syferkuil, Limpopo Province, South Africa

The data contains yield data from farm experiments and DSSAT simulated future yield for sunflower, peanut and soybean. The field experiments were conducted during the summer growing seasons of 2016/2017 and 2017/2018, across three sites (Punda Maria, Ofcolaco and Syferkuil) in the Limpopo province, South Africa. The province has three distinct climatic regions that can be classified as (i) lowveld (arid and semi-arid) regions, (ii) middle veld and highveld (semi-arid) region, and (iii) the escarpment region which has a sub-humid climate receiving 700 mm rainfall per annum. Crop production highly depends on the summer rainfalls received mostly from October to March. Most smallholder farming is rainfed. In the first season, experiments were established at the Syferkuil experimental farm (23°50'38' S and 29°41'13' E) and at a farmer's field at Ofcolaco (24°06'41' S and 30°23'26' E). In addition to these field trials, a farmer's field at Punda Maria (22°49'18' S and 30° 54 '37' E) was included in the second season following the granting of an ethical clearance from the University of Limpopo ethics committee and by the signing of a consent form by the farmers.
"
  
	uri <- "doi:10.6084/m9.figshare.24999194"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessin Dzuda",
		carob_date = "2026-08-31",
		carob_completion = 100,	
		carob_effort = 6
	)
	
	f1 <- ff[basename(ff) == "Peanut_yield_3 sites.xlsx"]
	f2 <- ff[basename(ff) == "Soybean_yield_3 sites.xlsx"]
	f3 <- ff[basename(ff) == "Sunflower_yield_3 sites.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Ofcoloco_Pnut")
	r1b <- carobiner::read.excel(f1, sheet="Pmaria_Pnut")
	r1c <- carobiner::read.excel(f1, sheet="Syferkuil_Pnut")
	r2b <- carobiner::read.excel(f2, sheet="Ofcoloco_Soy")
	r2c <- carobiner::read.excel(f2, sheet="Pmaria_Soy")
	r2d <- carobiner::read.excel(f2, sheet="Syferkuil_Soy")
	r3a <- carobiner::read.excel(f3, sheet="Ofcoloco_Sunf")
	r3b <- carobiner::read.excel(f3, sheet="Pmaria_Sunf")
	r3c <- carobiner::read.excel(f3, sheet="Syferkuil_Sunf")

  #fixing 
	# creating a function to normalize csv data
	standardize_yield_df <- function(df, crop, site) {
	  df <- df[, 1:7]
	  names(df) <- c("TRNO", "Fertilizer", "Year", "RCP2.6", "RCP4.5", "RCP6.0", "RCP8.5")
	  df$Crop <- crop
	  df$Site <- site
	  
	  # reshaping yield from wide to long
	  long <- reshape(df,varying = c("RCP2.6","RCP4.5","RCP6.0","RCP8.5"),v.names = "yield",
	             timevar = "RCP",times = c("RCP2.6","RCP4.5","RCP6.0","RCP8.5"),direction = "long")
	  
	  #creating treatment identification
	  long$treatment <- paste(long$Fertilizer, long$RCP, sep = "_")
	  long$id <- NULL
	  long$RCP <- NULL
	  rownames(long) <- NULL
	  
	  long}
	
	#applying the function
	r1a <- standardize_yield_df(r1a, "Pnut", "Ofcoloco")
	r1b <- standardize_yield_df(r1b, "Pnut", "Pmaria")
	r1c <- standardize_yield_df(r1c, "Pnut", "Syferkuil")
	r2b <- standardize_yield_df(r2b, "Sbean", "Ofcoloco")
	r2c <- standardize_yield_df(r2c, "Sbean", "Pmaria")
	r2d <- standardize_yield_df(r2d, "Sbean", "Syferkuil")
	r3a <- standardize_yield_df(r3a, "Sflower", "Ofcoloco")
	r3b <- standardize_yield_df(r3b, "Sflower", "Pmaria")
	r3c <- standardize_yield_df(r3c, "Sflower", "Syferkuil")
	
	#binding the dfs
	r <- do.call(rbind, list(r1a, r1b, r1c, r2b, r2c, r2d, r3a, r3b, r3c))
	r <- unique(r)

	d <- data.frame(
	  country="South Africa",
	  adm1="Limpopo",
	  location=r$Site,
	  crop=r$Crop,
	  treatment=r$Fertilizer,
	  yield=r$yield,
	  yield_part="seed",
	  yield_moisture=NA,
	  yield_isfresh=NA,
	  harvest_date=NA,
	  planting_date=NA)

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE#coordinates sourced from the abstract
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
  iN <- grepl("N/ha", d$treatment)
  iP <- grepl("P/ha", d$treatment)
  d$N_fertilizer[iN] <- as.numeric(sub(" .*", "", d$treatment[iN]))
  d$P_fertilizer[iP] <- as.numeric(sub(" .*", "", d$treatment[iP]))
  
  d$crop[d$crop=="Pnut"] <- "groundnut"
  d$crop[d$crop=="Sbean"] <- "soybean"
  d$crop[d$crop=="Sflower"] <- "sunflower"
  d$location[d$location=="Ofcoloco"] <- "Ofcolaco"
  d$location[d$location=="Pmaria"] <- "Punda Maria"
  
  #coordinates obtained from the abstract
  geo <- data.frame(
    location = c("Syferkuil", "Ofcolaco", "Punda Maria"),
    latitude = c(-23.843889, -24.111389, -22.821667),
    longitude = c(29.686944, 30.390556, 30.910278))
  
  d <- merge(d,geo,by="location", all.x = TRUE)
  
  d$trial_id <- paste(d$location,r$Year,sep = "_")
  
  
  d <- unique(d)
   
	carobiner::write_files(path, meta, d)
}


