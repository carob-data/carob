# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. added "butachlor";"pyrazosulfuron";"quizalofop-ethyl" under herbicide_product
#2. harvest date in its correct format(as.Date), but carobiner cant capture it
carob_script <- function(path) {

"Developing crop and resource management practices for sustainable future cereal based cropping systems
"
	uri <- "hdl:11529/11082"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT;IRRI",
		publication = NA,
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method;planting_method",
		response_vars = "yield", 
		completion = 99,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-04-01",
		notes = NA,
		design = NA
	)
	
	f <- ff[basename(ff) == "RawData_RP1.csv"]
	r <- read.csv(f, na.strings = c("", "NA"))

	d <- data.frame(
	  country="India",
	  adm1="Bihar",
	  adm2="Patna",
	  adm3="Khagaul",
	  adm4="Sabajpura",
	  location="Research Complex for Eastern India Research Farm",
	  latitude=25.593,
	  longitude=85.083,
	  treatment=r$SCENARIO,
	  planting_method=tolower(r$CEST),
		land_prep_method=r$TILL,
		rep=r$REP,
		season=tolower(r$SEASON),
		crop=tolower(r$CROP),
		seed_rate=r$SRATE,
		N_fertilizer=r$N_BASAL+(r$N_SPLIT_1+r$N_SPLIT_2),
		P_fertilizer=r$P_BASAL,
		K_fertilizer=r$K_BASAL+r$K_SPLIT_1,
		harvest_date=(r$DHARV),
		irrigation_amount=r$IRRIG_VOL*0.1,#converting KL/ha to mm
		seed_treatment=r$SEED_CHEM,
		yield_moisture=as.numeric(NA),
		yield=r$YIELD*1000)
  
  
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- FALSE
	d$yield_part<- ifelse(d$crop=="potato","tubers","grain")
	d$land_prep_method <- ifelse(d$land_prep_method=="CT","conventional",
	                             ifelse(d$land_prep_method=="ZT","none",NA))
	d$harvest_date <-as.Date(r$DSOW, format = "%d-%b-%y")
	
	#fixing dates so they become uniform
	d$planting_date <-as.Date(r$DSOW, format = "%d-%b-%y") 
	fix <- is.na(d$planting_date) & !is.na(r$DSOW)
	d$planting_date[fix] <- as.Date(r$DSOW[fix], format = "%d/%m/%y")
	fix1 <-is.na(d$planting_date) & !is.na(r$DSOW)
	d$planting_date[fix1] <- as.Date(r$DSOW[fix1], format = "%d/%m/%Y")
	d$planting_date <- as.numeric(substr(d$planting_date, 1, 4))
	d$planting_date <- as.character(d$planting_date)
	
	#cleaning herbicide column
	d$herbicide_product <- apply(cbind(
	  tolower(r$PRE_PLNT_CHEM),
	  tolower(r$PRE_EMRG_CHEM_1),
	  tolower(r$POST_EMRG_CHEM_2),
	  tolower(r$POST_EMRG_CHEM_1)
	),
	1,
	function(x) {
	  x <- x[!is.na(x) & x != ""]
	  if (length(x) == 0) NA_character_ else paste(x, collapse = ";")})
	

	#cleaning insecticide column
	d$insecticide_product <- apply(
	  cbind(
	    tolower(r$INSTCDE_1),
	    tolower(r$INSTCDE_2)
	  ),
	  1,
	  function(x) {
	    x <- x[!is.na(x) & x != ""]
	    if (length(x) == 0) NA_character_ else paste(x, collapse = ";")})
	
	d <- subset(d, crop != "fallow") #dropping these values since there is no relevant info except NA's
	
	##standardizing herbicide values
	map <- c(
	  "glyphosat" = "glyphosate",
	  "pendimethaline" = "pendimethalin",
	  "bispyribac" = "bispyribac-sodium",
	  "clodinofop" = "clodinafop",
	  "sulfosulfuron+metsulfuron" = "sulfosulfuron;metsulfuron",
	  "bispyribac+pyrazosulfuron" = "bispyribac-sodium;pyrazosulfuron",
	  "turgasuper" = "quizalofop-ethyl"
	)
	
	d$herbicide_product <- sapply(d$herbicide_product, function(x) {
	  if (is.na(x)) return(NA_character_)
	  
	  # split initial values
	  parts <- unlist(strsplit(x, ";"))
	  parts <- trimws(parts)
	  
	  # mapping each part
	  parts <- ifelse(parts %in% names(map), map[parts], parts)
	  
	  # handling multiple values
	  parts <- unlist(strsplit(parts, ";"))
	  
	 #trimming
	  parts <- trimws(parts)
	  parts <- parts[parts != ""]
	  
	  # remove duplicates
	  parts <- unique(parts)
	  
	  paste(parts, collapse = ";")
	})
	####fixing treatment values
	#creating lookup map
	# 2009–2012 practices
	p1 <- c(
	  S1 = "Farmer practice: Conventional broadcast wheat (Rabi)- Fallow (Summer)- Puddled random manual transplanted rice (Kharif)",
	  S2 = "Zero tillage wheat- Conventional broadcast mungbean- Conventional till unpuddled manual transplanted rice",
	  S3 = "Zero tillage wheat- Zero tillage vegetable cowpea- Zero tillage direct seeded rice",
	  S4 = "Conventional tillage bed planted potato+maize (manual dibbling in ridges)- Conventional till vegetable cowpea- Conventional tillage direct seeded rice"
	)
	#2013-2016 practices
	p2_random <- c(
	  S1 = "1 (Random)- Farmer practice: Conventional broadcast wheat (Rabi) - Fallow (Summer)- Puddled random manual transplanted rice (Kharif)"
	  )
	p2_line <- c(
	  S1 = "1 (Line)- Conventional till seed-cum-fertiliser drill wheat- Fallow- Farmer best practice (Puddled line transplanted rice)"
	)	
	p2_rest <- c(
	  S2 = "Zero tillage wheat- Zero tillage mungbean- Conventional dry till unpuddled mechanical transplanted rice",
	  S3 = "Zero tillage wheat- Zero tillage mungbean- Zero tillage direct seeded rice",
	  S4 = "Zero Tillage mustard - Zero tillage maize- Conventional tillage direct seeded rice"
	)
	
	#mapping values to actual treatments
	idx1 <- d$planting_date >= 2009 & d$planting_date <= 2012
	d$treatment[idx1] <- p1[d$treatment[idx1]]
	
	idx2 <- d$planting_date >= 2013 & d$planting_date <= 2016 &
	  d$planting_method == "random tpr" & d$treatment == "S1"
	d$treatment[idx2] <- p2_random["S1"]
	
	idx3 <- d$planting_date >= 2013 & d$planting_date <= 2016 &
	  d$planting_method == "line tpr" & d$treatment == "S1"
	d$treatment[idx3] <- p2_line["S1"]
	
	idx4 <- d$planting_date >= 2013 & d$planting_date <= 2016 &
	  d$treatment %in% c("S2","S3","S4")
	d$treatment[idx4] <- p2_rest[d$treatment[idx4]]
	
	d$treatment[d$treatment == "S1" &
	              d$planting_method == "broadcast" &
	              d$planting_date >= 2013 & d$planting_date <= 2016] <- 
	  "1 (Random)- Farmer practice: Conventional broadcast wheat (Rabi) - Fallow (Summer)- Puddled random manual transplanted rice (Kharif)"
	
	d$treatment[d$treatment == "S1" &
	              d$planting_method == "drill" &
	              d$planting_date >= 2013 & d$planting_date <= 2016] <- 
	  "1 (Line)- Conventional till seed-cum-fertiliser drill wheat- Fallow- Farmer best practice (Puddled line transplanted rice"
	
	#fixing planting methods
	mapping <- c(
	  "broadcast"  = "broadcasting",
	  "drill"      = "mechanized",
	  "ct-bed"     = "line sowing",
	  "dibbling"   = "dibbling",
	  "random tpr" = "transplanted",
	  "dsr"        = "direct seeding",
	  "line tpr"   = "mechanized",
	  "mtr"        = "mechanized"
	)
	
	d$crop <- gsub("mung|mungbean","mung bean", d$crop)
	d$crop <- gsub("rabi","wheat",d$crop)
	d$insecticide_product <- gsub("acetamipride","acetamiprid", d$insecticide_product)
	d$insecticide_product <- gsub("cartap hydrochlorid","cartab",d$insecticide_product)
	d$insecticide_product <- gsub("rogor","dimethoate", d$insecticide_product)
	d$planting_method <- mapping[d$planting_method]
	d$season<- gsub("summer","dry",d$season)
	d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")
  
	carobiner::write_files(path, meta, d)
}
