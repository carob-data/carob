# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. data was reshaped twice to correctly capture how carob treats animal and crop
#2. after conversion of t/ha to kg/ha, yice yield is above normal (25000)
#3. new organisation added

carob_script <- function(path) {

"
Analysis of Household-Level Survey Data: Farm Characteristics and Resource Allocation in Laos PDR (2024)

Processed dataset from a household-level survey describing the main farm characteristics, production, and resource allocation in two municipalities.  The survey covers 300 farms across three districts (Kham, Moke, and Nonghet) in Xieng Khouang Province, collected between December 2023 and May 2024.
"

	uri <- "doi:10.71682/10549336"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;LFN",
		publication = "hdl:10883/35403",
		project = NA,
		design = "unitOfAnalysis: Household level; collectionMode: Online survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-06-26",
		carob_completion = 90,	
		carob_effort = 6
	)
	

	f1 <- ff[basename(ff) == "Laos 2024 farm characterization for typologies MFS.xlsx"]

	r <- carobiner::read.excel(f1, sheet="data")

	d <- data.frame(
		country="Laos",
		adm2=r$district,
		adm3=r$village,
		age=r$f_age_i,
		education=r$school_i,
		hh_size=r$hh_members,
		hh_adult_women=r$hh_memwom_s,
		farmland=r$surface_ha,
		cropland_owned=r$ownland_ha,
		land_irrigated=r$irri_ha,
		cattle=r$livcow23_n,
		pig=r$livpig23_n,
		chicken=r$livpouktry23_n,
		TLU=r$tlu_2023, #capturing previously recorded TLU at the time of the survey
		hh_income=r$inco_mon,
		currency="LAK",
		farm_labour=r$crop_lab,
		maize=r$yield_maizem,#estimated yield
		rice_up=r$yield_uplandm,#estimated yield
		rice_pad=r$yield_paddym#estimated yield
		)
	
	d$rice_up <- as.numeric(d$rice_up)#missing values stored as character "NA", so converting character NA to true NA
	d$rice_pad <- as.numeric(d$rice_pad)#missing values stored as character "NA", so converting character NA to true NA
	d$rice <- rowSums(cbind(d$rice_up, d$rice_pad), na.rm = TRUE)
	d$rice[is.na(d$rice_up) & is.na(d$rice_pad)] <- NA
	d[c("rice_pad", "rice_up")] <- NULL

	d$trial_id <- paste(d$adm3,d$age,sep = "_")
	
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- TRUE
	d$geo_from_source <- FALSE

  loc <- data.frame(
    adm2 = c("Kham", "Kham", 
             "Moke", "Kham", "Moke", "Moke", "Moke", "Nonghet", "Kham", "Moke", 
             "Nonghet", "Nonghet", "Nonghet", "Nonghet", "Kham", "Kham"),
    adm3 = c("Tha", "Boa", "Phadaeng", "Souanmone", "Namone", 
             "Phoumone", "Namueng", "Pakhom", "Pakhom", "Mor", "Yort", 
             "Palan", "Pounsaeng", "Houay", "Namhome", "Sankham"),
    latitude = c(19.741, 19.721, 19.45, 19.791, 19.078, 
                 19.959, 19.503, 19.57, 19.57, 19.163, 19.489, 19.499, 19.461, 
                 20.263, 19.545, 19.741),
    longitude = c(103.587, 103.58, 103.712, 
                  103.641, 102.142, 103.854, 102.061, 103.98, 103.98, 103.692, 
                  103.953, 104.032, 103.189, 100.433, 103.695, 103.587)
  )
	
  d <- merge(d,loc,by=c("adm2","adm3"), all.x = T)
	d$planting_date <- as.character(NA)
	d$harvest_date  <- as.character(NA)
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- TRUE
  
  #improving quality
  d$hh_income <- gsub("4e+05",400000,d$hh_income)
  d$hh_income <- gsub("1e+05",100000,d$hh_income)
  d$hh_income <- as.numeric(d$hh_income)
  d$farm_labour <- as.numeric(d$farm_labour)#coersion warning is converting character NA into true NA
  
  #since carob doesnt accept range of values, a midpoint has been extracted
  age_values<- c(
    "0"=21,#18-24
    "0.3"=30,#"25-34"
    "0.6"=40,#"35-44"
    "0.9"=50,#"45-54"
    "1.2"=60,#"55-64"
    "1.5"=65#>65
  )
  
  d$age <- as.numeric(age_values[as.character(d$age)])
  
  #cleaning education
  edu_values<- c(
    "0"="none",
    "0.5"="primary",
    "1"="secondary",
    "1.5"="high school",
    "2"="postgrad"
  )
  
  d$education <- edu_values[d$education]
  
#converting % of women to number of women in a household
  d$hh_adult_women <- (d$hh_adult_women*d$hh_size)/100
  
  #re-shaping livestock data from wide to long
  d$row_id <- seq_len(nrow(d))#creating unique row id for reshaping
  
  d <- reshape(
    d,
    varying   = c("cattle", "pig", "chicken"),
    v.names   = "heads",
    timevar   = "animal",
    times     = c("cattle", "pig", "chicken"),
    idvar     = "row_id",
    direction = "long"
  )
  rownames(d) <- NULL
  d$heads <- as.numeric(d$heads)#coersion warning is converting character NA into true NA
  
  #reshaping crop data to long format
  d <- reshape(
    d,
    varying   = c("maize", "rice"),
    v.names   = "yield",
    timevar   = "crop",
    times     = c("maize", "rice"),
    idvar     = c("row_id", "animal"),
    direction = "long"
  )
  d$yield <- as.numeric(d$yield)#coersion warning is converting character NA into true NA
  d$yield <- d$yield*1000#converting to kg/ha
  
  
  rownames(d) <- NULL
  d$row_id <- NULL
  
  d <- unique(d)#i assume duplicates have been created as a result of the reshape
  
	carobiner::write_files(path, meta, d)
}
