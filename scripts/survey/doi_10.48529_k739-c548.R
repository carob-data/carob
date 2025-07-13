# R script for "carob"


carob_script <- function(path) {

"Ethiopia: Socioeconomic Survey 2018-2019"

	up <- carobiner::usr_pwd(path, "LSMS")

	uri <- "doi:10.48529/k739-c548"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group, protocol="LSMS", username=up$username, password=up$password)
	if (is.null(ff)) return(TRUE)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
		publication = NA,
		carob_contributor = "Robert Hijmans",
		data_organization = "ESS;WB",
		data_citation = "Central Statistics Agency of Ethiopia . (2020). Socioeconomic Survey 2018-2019 [Data set]. World Bank, Development Data Group. https://doi.org/10.48529/K739-C548",
		carob_date = "2025-05-14",
		completion = 0
	)

	#fpgeo <- ff[basename(ff) == "ETH_PlotGeovariables_Y4.csv"]
	#pgeo <- read.csv(fpgeo)

	fhhgeo <- ff[basename(ff) == "ETH_HouseholdGeovariables_Y4.csv"]
	fscpp <- ff[basename(ff) == "sect_cover_pp_w4.csv"]
	#fs1pp <- ff[basename(ff) == "sect1_pp_w4.csv"]
	#fs2pp <- ff[basename(ff) == "sect2_pp_w4.csv"]
	fs3pp <- ff[basename(ff) == "sect3_pp_w4.csv"]
	fs4pp <- ff[basename(ff) == "sect4_pp_w4.csv"]
	fs9app <- ff[basename(ff) == "sect9a_pp_w4.csv"]

	hhgeo <- read.csv(fhhgeo, colClasses="character")
	scpp <- read.csv(fscpp, colClasses="character")
	#s1pp <- read.csv(fs1pp, colClasses="character")
	#s2pp <- read.csv(fs2pp, colClasses="character")
	s3pp <- read.csv(fs3pp, colClasses="character")
	s4pp <- read.csv(fs4pp, colClasses="character")
	s9app <- read.csv(fs9app, colClasses="character")
	
	geo <- data.frame(
		hhid = hhgeo$household_id,
		ea_id = hhgeo$ea_id,
		longitude = as.numeric(hhgeo$lon_mod),
		latitude = as.numeric(hhgeo$lat_mod)  
	)


	adm1 <- c("Tigray", "Afar", "Amhara", "Oromia", "Somali", "Benshangul-Gumaz", "Southern Nations, Nationalities", NA, NA, NA, NA, "Gambela Peoples", "Harari People", "Addis Ababa", "Dire Dawa")

	dcpp <- data.frame(
		holder_id = scpp$holder_id,
		hhid = scpp$household_id,
		ea_id = scpp$ea_id,
		adm1 = as.integer(scpp$saq01)
	)
	dcpp$adm1 <- adm1[dcpp$adm1]

	d3pp <- data.frame(
		holder_id = s3pp$holder_id,
		hhid = s3pp$household_id,
		parcel_id = s3pp$parcel_id,
		field_id = s3pp$field_id,
		ea_id = s3pp$ea_id,
		irrigated = s3pp$s3q17 == "1. YES",
		urea = as.numeric(s3pp$s3q21a),
		dap = as.numeric(s3pp$s3q22a),
		nps = as.numeric(s3pp$s3q23a),
		npk = as.numeric(s3pp$s3q24a),
		OM_used = s3pp$s3q25 == 1
		#crop1 = gsub(".\\. |..\\. ", "", tolower(s3pp$s3q33b)),
		#crop2 = gsub(".\\. |..\\. ", "", tolower(s3pp$s3q33d))
	)
	
	famnt <- d3pp[, c("urea", "dap", "nps", "npk")]
	famnt[is.na(famnt)] <- 0
	d3pp$N_fertilizer <- colSums(t(famnt)  * c(.46, .18, .2, .2))
	d3pp$P_fertilizer <- colSums(t(famnt) * c(0, .201, .2, .2))
	d3pp$K_fertilizer <- colSums(t(famnt) * c(0, 0, 0, .2))
	d3pp$S_fertilizer <- colSums(t(famnt) * c(0, 0, 0.04, 0))
	d3pp$urea <- d3pp$dap <- d3pp$nps <- d3pp$npk <- NULL
		
	d4pp <- data.frame(
		holder_id = s4pp$holder_id,
		hhid = s4pp$household_id,
		parcel_id = s4pp$parcel_id,
		field_id = s4pp$field_id,
		crop_id = s4pp$field_id,
		ea_id = s4pp$ea_id,	
		crop = gsub(".\\. |..\\. |...\\. ", "", tolower(s4pp$s4q01b)),
		has_intercrop = s4pp$s4q02 == "2. Mixed",
		variety_type = s4pp$s4q11,
		stress = tolower(gsub(".\\. |..\\. ", "", s4pp$s4q09)),
		insecticide_used = s4pp$s4q05 == "1. YES",
		herbicide_used = s4pp$s4q06 == "1. YES",
		fungicide_used = s4pp$s4q07 == "1. YES",
		seed_rate = as.numeric(s4pp$s4q11a) * 1000, # kg, divide by area
		plant_month = carobiner::eng_months_to_nr(gsub(".\\. |..\\. ", "", s4pp$s4q13a)),
		plant_year = s4pp$s4q13b
	)

	d4pp$variety_type[grep("Traditional", d4pp$variety_type)] <- "traditional"
	d4pp$variety_type[grep("IMPROVED", d4pp$variety_type)] <- "improved"
    d4pp$plant_month <- gsub("pwagume", "9", d4pp$plant_month)
	d4pp$planting_date <- paste0(d4pp$plant_year, "-", formatC(as.numeric(d4pp$plant_month), width=2, flag="0"))
	d4pp$planting_date[d4pp$plant_month == ""] <- NA
	d4pp$plant_month <- d4pp$plant_year <- NULL

	d4pp$stress[d4pp$stress == "too much rain"] <- "excess water"
	d4pp$stress[d4pp$stress == "too little rain"] <- "drought"
	d4pp$stress[d4pp$stress == "crop disease"] <- "disease"
	d4pp$stress[d4pp$stress == "depletion of soil"] <- "nutrient deficiency"
	d4pp$stress[d4pp$stress == "floods"] <- "flood"
	d4pp$stress[d4pp$stress == "insects"] <- "pests"
	d4pp$stress[d4pp$stress == "security problems"] <- "theft"
	d4pp$stress[grep("shortage of seeds|bad seeds", d4pp$stress)] <- "none"
	d4pp$stress[d4pp$stress == ""] <- "none"
	d4pp$stress[d4pp$stress == "other specify"] <- "other"


#amboshika = Opuntia stricta?
#beer root = beetroot?
#red pepper / green pepper = bell pepper?

	d4pp$crop <- carobiner::replace_values(d4pp$crop,
		c("amboshika", "apples", "avocados", "bananas",  "beer root", "cardamon", "chat", "chick peas", "chilies", "field peas", "gibto", "ground nuts", "haricot beans", "horse beans", "lentils", "lineseed", "mung bean/ masho", "nueg", "pinapples", "potatoes", "pumpkins", "green pepper", "red pepper", "rape seed", "red kideny beans", "soya beans", "sugar cane", "sweet potato", "tomatoes", "lemons", "mandarins", "mangos", "oranges", "other cereal", "other fruits", "other pulses", "other root c", "other vegetable", "other oil seed", "gishita", "godere", "kazmir", "shiferaw", "white cumin", "sacred basil", "grazing land", "temporary gr", "other land", "other spices"),
		c("erect prickly pear", "apple", "avocado", "banana", "beetroot", "cardamom", "khat", "chickpea", "chili pepper", "pea", "white lupin", "groundnut", "green bean", "faba bean", "lentil", "flax", "mung bean", "noug", "pineapple", "potato", "pumpkin", "bell pepper", "bell pepper", "rapeseed", "kidney bean", "soybean", "sugarcane", "sweetpotato", "tomato", "lemon", "mandarin", "mango", "orange", "cereal", "fruit", "pulse", "root", "vegetable", "oilseed", "soursop", "taro", "white sapote", "moringa", "zira", "ethiopian basil", "pasture", "pasture", "other", "spices"))



	d9app <- data.frame(
		holder_id = s9app$holder_id,
		hhid = s9app$household_id,
		ea_id = s9app$ea_id,
		parcel_id = s9app$parcel_id,
		field_id = s9app$field_id,
		crop_id = s9app$crop_id,
		crop = gsub(".\\. |..\\. ", "", tolower(s9app$s4q01b)),
		harv_day = as.numeric(s9app$sccq02a),
		harv_month = as.numeric(s9app$sccq02b),
		fw_yield = as.numeric(s9app$sccq03) * 625,
		dm_yield = as.numeric(s9app$sccq04) * 625,
		crop_cut = TRUE
	)
	d9app <- d9app[!is.na(d9app$fw), ]
	d9app$harvest_date <- paste0("2019-", formatC(d9app$harv_month, width=2, flag="0"), "-", formatC(d9app$harv_day, width=2, flag="0"))
	d9app$harvest_date[is.na(d9app$harv_month)] <- "2019"
	d9app$harvest_date[d9app$harv_month < 1 | d9app$harv_month > 12] <- "2019"
	d9app$harvest_date[d9app$harvest_date %in% c("2019-02-29", "2019-02-30")] <- "2019-02"
	i <- which(d9app$harv_day < 1)
	d9app$harvest_date[i] <- substr(d9app$harvest_date[i], 1, 6)
	d9app$harv_day <- d9app$harv_month <- NULL
	
	d9app$cropcheck <- carobiner::replace_values(d9app$crop,
		c("chick peas", "field peas", "gibto", "ground nuts", "haricot beans", "horse beans", "lentils", "lineseed", "mung bean/ masho", "nueg", "rape seed", "red kideny beans", "soya beans"),
		c("chickpea", "pea", "white lupin", "groundnut", "green bean", "faba bean", "lentil", "flax", "mung bean", "noug", "rapeseed", "kidney bean", "soybean"))


	d <- merge(dcpp, geo, by=c("hhid", "ea_id"))
	d <- merge(d, d3pp, by=c("holder_id", "hhid", "ea_id"))
	d <- merge(d, d4pp, by=c("holder_id", "hhid", "ea_id", "parcel_id", "field_id"))
	d <- merge(d, d9app, by=c("holder_id", "hhid", "ea_id", "parcel_id", "field_id", "crop_id", "crop"), all.x=TRUE)



	d$cropcheck <- NULL
	d$ea_id <- d$holder_id <- d$parcel_id <- d$field_id <- d$crop_id <- NULL
	d$trial_id <- as.character(1:nrow(d))
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- as.character(NA)
	d$country <- "Ethiopia"
	d$geo_from_source <- TRUE
	d$geo_uncertainty <- 5000

#	d$has_intercrop <- d$crop_fraction <- NULL
#	d$N_splits <- as.integer(d$N_splits)
#	d$N_splits[d$N_splits>3] <- NA
	
	carobiner::write_files(path, meta, d)
}



