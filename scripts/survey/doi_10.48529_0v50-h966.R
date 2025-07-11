# R script for "carob"


carob_script <- function(path) {

"Mali: Enquête Agricole de Conjoncture Intégrée aux Conditions de Vie des Ménages 2017"

	up <- carobiner::usr_pwd(path, "LSMS")
	if (is.null(up)) return(TRUE)
	
	uri <- "doi:10.48529/0v50-h966"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group, protocol="LSMS", username=up$user, password=up$pwd)
	if (length(ff) == 0) return(TRUE)
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		carob_contributor = "Robert Hijmans",
		carob_date = "2025-05-14",
		data_organization = "MAM;WB",
		completion = 5
	)


	crop_codes <- data.frame(
		code = c(101, 102, 103, 104, 105, 106, 107, 110, 111, 112, 113, 120, 121, 122, 123, 124, 130, 131, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 401, 402, 403, 501, 502, 503, 504, 601, 602, 603, 701, 702, 703), 
		name = c("millet", "sorghum", "rice", "maize", "wheat", "barley", "white fonio", "sweetpotato", "yam", "cassava", "taro", "black eyed peas", "peanut", "voandzou", "soybean", "sesame", "sweet peas", "ginger", "tomato", "onion", "shallot", "garlic", "pepper", "anise-caraway", "carrot", "okra", "lettuce", "potato", "eggplant", "cucumber", "watermelon", "melon", "squash and zucchini", "green sorel", "red sorrel of guinea", "cabbages other food-crop productions", "mint", "beetroot", "green beas", "bell pepper", "orange", "soft banana", "plantain banana", "mango timber species", "papaya", "grapefruit", "mandarin", "guava", "lemon forage crops", "date", "cashew", "mexican lime", "custard apple non-classified crops", "graft lote", "oil palm", "tangerine", "cotton", "tobacco", "dah/fiber", "eucalyptus", "cacia", "gmelina tubers", "neem", "cowpea", "forage black eyed peas", "bourgou legumes", "sisal", "henna", "calabash")
	)
	crop_codes$name <- carobiner::replace_values(crop_codes$name,
		c("green sorel", "red sorrel of guinea", "dah/fiber", "sweet peas", "black eyed peas", "calabash", "peanut", "voandzou", "cabbages other food-crop productions", "squash and zucchini"),
		c("sorrel", "roselle", "roselle", "pea", "cowpea", "bottle gourd", "groundnut", "bambara groundnut", "cabbage", "squash")
	)
	


	fgeo <- ff[basename(ff) == "eaci_geovariables_2017.csv"]
	dgeo <- read.csv(fgeo)
	geo <- data.frame(
		cluster = dgeo$grappe,
		longitude = dgeo$lon_dd_mod,
		latitude = dgeo$lat_dd_mod
	)


	fs11bp1 <- ff[basename(ff) == "eaci17_s11bp1.csv"]
	fs11cp1 <- ff[basename(ff) == "eaci17_s11cp1.csv"]
	fs7cp2 <- ff[basename(ff) == "eaci17_s07cp2.csv"]
	fs7dp2 <- ff[basename(ff) == "eaci17_s07dp2.csv"]
	fs7fp2 <- ff[basename(ff) == "eaci17_s7fp2.csv"]

	s11bp1 <- read.csv(fs11bp1)
	s11cp1 <- read.csv(fs11cp1)
#	s7cp2 <- read.csv(fs7cp2)
	s7dp2 <- read.csv(fs7dp2)
	s7fp2 <- read.csv(fs7fp2)


	d11b1 <- data.frame(
		cluster = s11bp1$grappe,
		household = s11bp1$exploitation,
		#passage = s11bp1$passage,	
		block = s11bp1$s11bq01,
		parcel = s11bp1$s11bq02,
		crop_code = s11bp1$s11bq03,
		plot_size = s11bp1$s11bq07, #ha
		irrigated = s11bp1$s11bq36 > 4
	)


	d11c1 <- data.frame(
		cluster = s11cp1$grappe,
		household = s11cp1$exploitation,
		#passage = s11cp1$passage,	
		block = s11cp1$s11cq01,
		parcel = s11cp1$s11cq02,
		has_intercrop = s11cp1$s11cq06,
		crop_fraction = s11cp1$s11cq07 / 100,
		variety = s11cp1$s11cq09,
		variety_type = s11cp1$s11cq10,
		plant_day = s11cp1$s11cq14a,
		plant_month = s11cp1$s11cq14b,		
		prod_expected = s11cp1$s11cq16a * s11cp1$s11cq16c
	)
	d11c1$planting_date <- paste0("2017-", formatC(d11c1$plant_month, width=2, flag="0"), "-", formatC(d11c1$plant_day, width=2, flag="0"))
	d11c1$planting_date[!is.finite(d11c1$plant_month)] <- NA
	d11c1$plant_month <- d11c1$plant_day <- NULL
	d11c1 <- unique(d11c1)
	d11c1$variety <- c("BG 90-2", "Gambiaka", "Gambiaka Suruni", "Adny11", "Nerica", NA)[d11c1$variety]
	d11c1$variety_type <- c("Local", "Improved", "Improved", "Improved", "Improved")[d11c1$variety_type]

#	d7c2 <- data.frame(
#		cluster = s7cp2$grappe,
#		household = s7cp2$exploitation,
#		passage = s7cp2$passage,	
#		block = s7cp2$s7cq01,
#		parcel = s7cp2$s7cq02,
#		crop_code = s7cp2$s7cq03,
#	)


	d7d2 <- data.frame(
		cluster = s7dp2$grappe,
		household = s7dp2$exploitation,
		#passage = s7dp2$passage,	
		block = s7dp2$s7dq01,
		parcel = s7dp2$s7dq02,
		#crop_code = s7dp2$s7dq03,
		OM_used = s7dp2$s7dq05 == 1,
		fert_used = s7dp2$s7dq22 == 1,
		fert_day =s7dp2$s7dq23a,
		fert_month =s7dp2$s7dq23b,
		N_splits =s7dp2$s7dq24,
		fert_urea_amount =s7dp2$s7dq26a1,
		fert_urea_unit =s7dp2$s7dq26a2,
		fert_dap_amount =s7dp2$s7dq26b1,
		fert_dap_unit =s7dp2$s7dq26b2,
		fert_npk_amount =s7dp2$s7dq26c1,
		fert_npk_unit =s7dp2$s7dq26c2,
		fert_other_amount =s7dp2$s7dq26d1,
		fert_other_unit =s7dp2$s7dq26d2
	)

	fert_year <- c(2017,2018)[(d7d2$fert_month < 4)+1]
	d7d2$fertilizer_date <- paste0(fert_year, "-", formatC(d7d2$fert_month, width=2, flag="0"), "-", formatC(d7d2$fert_day, width=2, flag="0"))
	d7d2$fertilizer_date[!is.finite(d7d2$fert_month)] <- NA
	d7d2$fert_month <- d7d2$fert_day <- NULL
	
	famnt <- d7d2[, grep("fert_.*amount", names(d7d2))]
	funit <- d7d2[, grep("fert_.*unit", names(d7d2))]
	funit[!sapply(funit, is.finite)] <- 4
	# 1=Kilogram 2=Ton 3=Bag 4=Other
	funit <- sapply(funit, \(x) c(1,1000,50,NA)[x])
	famnt <- famnt * funit

	# amounts, not per ha
	d7d2$N_fertilizer <- colSums(t(famnt) * c(.46, .18, .2, .2))
	d7d2$P_fertilizer <- colSums(t(famnt) * c(0, .201, .2, .2))
	d7d2$K_fertilizer <- colSums(t(famnt) * c(0, 0, .2, .2))
	d7d2 <- d7d2[, -(grep("^fert_", names(d7d2)))]

		
	d7f2 <- data.frame(
		cluster = s7fp2$grappe,
		household = s7fp2$exploitation,
		#passage = s7fp2$passage,
		block = s7fp2$s7fq01,
		parcel = s7fp2$s7fq02,
		crop_code = s7fp2$s7fq03,
		stress = s7fp2$s7fq07,
		
		harvest_month1 = s7fp2$s7fq05b,
		harvest_day1 = s7fp2$s7fq05a,
		harvest_month2 = s7fp2$s7fq12b,
		harvest_day2 = s7fp2$s7fq12a,
		#ya = s7fp2$s7fq13a,
		#yb = s7fp2$s7fq13b,
		#yc = s7fp2$s7fq13c,
		prodkg = s7fp2$s7fq13a * s7fp2$s7fq13d
	)
	
	d7f2$stress <- c("drought", "excess water", "fire", "animals", "pest", "disease", "theft", NA, NA, NA)[d7f2$stress]
	
	harvest_year1 <- c(2017,2018)[(d7f2$harvest_month1 < 4)+1]
	harvest_date1 <- paste0(harvest_year1, "-", formatC(d7f2$harvest_month1, width=2, flag="0"), "-", formatC(d7f2$harvest_day1, width=2, flag="0"))
	harvest_date1[!is.finite(d7f2$harvest_month1)] <- NA
	harvest_year2 <- c(2017,2018)[(d7f2$harvest_month2 < 4)+1]
	harvest_date2 <- paste0(harvest_year2, "-", formatC(d7f2$harvest_month2, width=2, flag="0"), "-", formatC(d7f2$harvest_day2, width=2, flag="0"))
	harvest_date2[!is.finite(d7f2$harvest_month2)] <- NA
	
	harvest_date1 <- as.Date(harvest_date1)
	harvest_date2 <- as.Date(harvest_date2)
	
	d7f2$harvest_date <- as.Date(rowMeans(cbind(harvest_date1, harvest_date2), na.rm=TRUE))
	i <- which(abs(harvest_date2 - harvest_date1) > 60)
	d7f2$harvest_date[i] <- NA
	d7f2$harvest_date[is.na(d7f2$harvest_date)] <- NA # for NaN
	d7f2$harvest_date <- as.character(d7f2$harvest_date)

	d7f2$harvest_month1 <- d7f2$harvest_day1 <- d7f2$harvest_month2 <- d7f2$harvest_day2 <- NULL


	d <- merge(d11b1, d11c1, by=c("cluster", "household", "block", "parcel"), all.x=TRUE)
	d <- merge(d, d7d2, by=c("cluster", "household", "block", "parcel")) #, "crop_code" is wrong. 
	d <- merge(d, d7f2, by=c("cluster", "household", "block", "parcel", "crop_code"))
	d <- merge(d, geo, by="cluster")

	
	d$hhid <- paste0(d$cluster, formatC(d$household, width=2, flag="0"))
	d$cluster <- d$household <- d$block <- d$parcel <- NULL
	d$prod_expected	<- NULL

	d$crop <- crop_codes$name[match(d$crop_code, crop_codes$code)]
	d$crop_code <- NULL
	
	#d$yield_exp <- d$prod_expected / (d$plot_size * d$crop_fraction)
	d$yield <- d$prodkg / (d$plot_size * d$crop_fraction)
	d$prodkg <- NULL

	d$N_fertilizer <- d$N_fertilizer / d$plot_size
	d$P_fertilizer <- d$P_fertilizer / d$plot_size
	d$K_fertilizer <- d$K_fertilizer / d$plot_size

	i <- which(d$N_fertilizer > 200)
	d$N_fertilizer[i] <- d$P_fertilizer[i] <- d$K_fertilizer[i] <- NA
	d$yield[d$yield > 10000] <- NA

	i <- !sapply(d, \(i) is.finite(i) | is.character(i))
	d[i] <- NA
	
	d$trial_id <- as.character(1:nrow(d))
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- as.character(NA)
	d$country <- "Mali"
	d$geo_from_source <- TRUE
	d$geo_uncertainty <- 5000

	d$has_intercrop <- d$crop_fraction <- NULL
	d$N_splits <- as.integer(d$N_splits)
	d$N_splits[d$N_splits>3] <- NA
	
	carobiner::write_files(path, meta, d)
}



