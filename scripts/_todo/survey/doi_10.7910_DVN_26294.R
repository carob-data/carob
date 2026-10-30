# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI "Technology, Income Distribution and Poverty in the Philippines" (TIDP) benchmark survey, 1992 wet season - 1993 dry season, 4 villages: Tubuan (Pila, Laguna), Gabaldon (Muñoz, Nueva Ecija), Signe (Igbaras, Iloilo), Pandan (Dingle, Iloilo).
# Codes from "Coding Manual TIDP 92-96.DOC" (shared with the 1996 round, doi:10.7910/DVN/26295).
# One row per household x parcel x season x crop from LANDU92 (area in ha, crop, rice variety, production in kg, price).
#  Joined with CIR_92 (rice current inputs: fertilizer products in kg, organic fertilizer, seed, costs), LABOR_92 (man-days and hired labour cost per activity) by household, village, parcel and season,
#  and with Opar_92 (parcel topography, soil quality, irrigation source, tenure; wet season only) by household, village and parcel.
# Seasons: 1 = 92 WS (wet), 2 = 93 DS (dry), 3 = third crop (season NA; season_label_).
# N, P and K from fertilizer products: urea 46-0-0, ammonium sulfate 21-0-0, ammonium phosphate 16-20-0, complete 14-14-14, 18-46-0 (P = P2O5 x 0.4364, K = K2O x 0.8301). Products not in the file are counted as 0.
# Labour = all family, exchange and hired man-days (pre-land preparation, land preparation, crop establishment, crop care, harvest and post-harvest) per ha; marketing excluded.
# Negative values (-99, -999) are missing values.

## ISSUES
# No coordinates; villages, municipalities and provinces given. Need georeferencing.
# Production is only used when UNIT is kg; other units (pesos, pieces, cavans, kaing) give NA yield.
# The "2nd type of complete fertilizer" (COMP2), zinc fertilizers and liquid fertilizer have no stated grade: not included in N/P/K.
# CIR_92 has a separate NON-RICE sheet (Gabaldon, Tubuan) that is not used; non-rice inputs are not mapped (CINR_92 also not used).
# Duplicate keys in CIR/LABOR (same household, parcel and season) are reduced to the first record.
# Crop code 13 (milled rice), 21 (tomatoes & stringbeans) and 24 (blank in manual) have no crop. Rice variety codes with mixtures are kept as given (e.g. "IR36 & IR64").
# Production losses (LOSS1-3 with 60 reason codes), rent and by-product values are not mapped. YEAR kept as crop_year_ (meaning not documented).
# fertilizer_type "16-20-0" is not in the vocabulary. No yield_part for garlic and onion (bulb is not in the vocabulary).
# A few extreme yields on very small parcels (e.g. 0.002 ha); duplicate records (identical parcel rows in LANDU92).
# write_files: no coordinates; invalid fertilizer_type 16-20-0; low cowpea yield; duplicates; NA yield/yield_part.
# Unmapped: BASIC, HHINFO, HHCOMP, HHLOUT, ASSETS, RENTPAR, LABOR1-9 (detailed labour), CAPR, EMPINC, OTINC, BOR, INDEBT, LEND, MARKET, DRECORD, INC, INCOME93, OPNION, CINR, NRice1-5, ?LabNrice.

## NEW VARIABLES
# season_label_: original season (92 WS, 93 DS, 93 third crop)
# crop_label_: original crop label
# crop_year_: YEAR in LANDU
# topography_: parcel topography class


carob_script <- function(path) {

"
Technology, Income Distribution and Poverty in the Philippines (1992)

The study is based on village full enumeration field surveys.  The main purpose of the study is to examine the changes in the structure of rural household income and its distribution over the past decade with primary data generated through in-depth household surveys in four villages with contrasting production environments.  This study also shed light on the changes in the poverty situation of rural households over the 1985-97 period.
"

	uri <- "doi:10.7910/DVN/26294"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "IRRI",
		publication = NA,
		project = "TIDP",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 50,
		carob_effort = 0.8
	)

	# header row is the row starting with HHNO; strip the year suffix; negative values are missing
	rd <- function(fname, sheet, sfx) {
		x <- carobiner::read.excel(ff[basename(ff) == fname], sheet=sheet, col_names=FALSE, .name_repair="minimal")
		h <- which(grepl("^HHNO", x[[1]]))[1]
		nms <- sub(paste0(sfx, "$"), "", as.character(unlist(x[h, ])))
		x <- x[-(1:h), ]
		names(x) <- nms
		x <- x[, !is.na(names(x))]
		x <- x[grepl("^[0-9]+$", x$HHNO), ]
		x
	}
	num <- function(x) {
		if (is.null(x)) return(NULL)
		x <- trimws(as.character(x))
		x[!grepl("^-?[0-9]*\\.?[0-9]+$", x)] <- NA
		x <- as.numeric(x)
		x[x < 0] <- NA
		x
	}
	# fertilizer product quantity; 0 if not recorded in the file at all
	qty <- function(x, v) {
		q <- num(x[[v]])
		if (is.null(q) || all(is.na(q))) q <- rep(0, nrow(x))
		q
	}
	key <- function(x, season=TRUE) {
		k <- paste(num(x$HHNO), num(x$VIL), num(x$PNO))
		if (season) k <- paste(k, num(x$SEASON))
		k
	}

	sfx <- "92"
	lu <- rd("LANDU92.xls", 1, sfx)
	ci <- rd("CIR_92.xls", "CIR", sfx)
	lb <- rd("LABOR_92.xls", 1, sfx)
	op <- rd("Opar_92.xls", 1, sfx)
	ci <- ci[!duplicated(key(ci)), ]
	lb <- lb[!duplicated(key(lb)), ]
	op <- op[!duplicated(key(op, FALSE)), ]
	ic <- match(key(lu), key(ci))
	ci <- ci[ic, ]
	il <- match(key(lu), key(lb))
	lb <- lb[il, ]
	io <- match(key(lu, FALSE), key(op, FALSE))
	op <- op[io, ]

	ha <- num(lu$AREAP)
	ha[ha == 0] <- NA
	# CIR area is used for per ha inputs
	cha <- num(ci$AREAP)
	cha[cha == 0] <- NA

	crops <- c("rice", "watermelon", "asparagus bean", "mung bean", "garlic", "onion", "eggplant", "vegetable", "groundnut", "sweetpotato", "tomato", "okra", NA, "maize", "chili pepper", "cowpea", "bitter gourd", "squash", "jute mallow", "fruit", NA, "luffa", "cucumber", NA, "bottle gourd", "tobacco")
	clab <- c("rice", "watermelon", "string beans", "mungbean", "garlic", "onion", "eggplant", "vegetable (not specified)", "peanuts", "camote", "tomatoes", "okra", "milled rice", "corn", "pepper/chili", "cowpea", "ampalaya", "squash", "saluyot/kenaf", "fruits", "tomatoes & stringbeans", "patola", "cucumber", NA, "gourd (upo)", "tobacco")
	vars <- c("1"="IR64", "2"="IR66", "3"="IR70", "4"="IR72", "5"="IR74", "6"="R10", "7"="PSBRC2", "8"="PSBRC4", "9"="R4", "10"="C3", "11"="IR64 & 90 days", "12"="IR32 & IR64", "13"="IR36 & IR64", "14"="IR10 & IR64", "15"="R10 & IR64", "16"="IR42 & IR64", "17"="IR60 & IR64", "18"="IR36", "19"="IR42", "20"="CE3", "21"="IR32", "22"="Malagkit & IR64", "23"="C2 & IR64", "24"="California rice", "25"="IR60", "26"="IR64 & C3", "27"="C438", "28"="IR76", "29"="IR10", "30"="Milagrosa", "31"="San Isidro", "32"="Sinandomeng", "33"="841", "34"="1-3-8", "35"="IR68", "36"="IR38", "37"="Dinurado", "38"="Minindanao", "39"="IR58", "40"="206 variety", "41"="Kinarbao putot", "42"="IR29 & IR36", "43"="IR62", "44"="IR79", "45"="NOVA", "46"="IR5 & IR58", "47"="IR5", "48"="Mordagol/IR36/IR64", "49"="Kinarbao/IR64/IR10/K-5", "50"="Bordagol", "51"="Kinarbao/IR64", "52"="IR72 & IR74", "53"="IR79 & IR36", "54"="IR36 & IR74", "55"="C2 & C3", "57"="R10, Milagrosa, 841", "58"="Sangmaria", "59"="IR36 & Native", "60"="IR60 & RC10", "61"="C22", "62"="RC10", "63"="IR76/Malagkit", "64"="B9-2", "65"="C2 & C1", "66"="IR64 & Malagkit", "67"="IR69", "68"="747", "69"="IR36 & R10", "70"="RC12", "72"="IR94", "73"="Line 96", "74"="RC4", "75"="Fancy rice", "76"="IR64 & IR78", "77"="IR78", "78"="C10", "79"="RC10 & Menaria", "80"="RC2", "81"="RC6, Malagkit & IR64", "82"="RC8 & IR64", "83"="IR46", "84"="IR64 & IR8", "85"="R60", "86"="IR64, IR60 & Malagkit", "87"="R38", "88"="RI10", "89"="C14", "90"="C40", "91"="R40", "92"="Binicol", "93"="RC10 & Kinarbao putot", "94"="IR64 & RC12", "95"="RC20", "96"="RC8", "97"="IR64 & RC20", "98"="IR20", "99"="RC14", "100"="R14", "101"="IR29", "102"="Malagkit", "103"="RC8, RC20 & IR64", "104"="IR64 & RC4", "105"="92 varieties", "106"="C10 & RC20", "107"="IR60 & RC20", "108"="IR60 & RC14", "109"="IR64 & RC14", "110"="RC14 & RC12", "111"="RC10 & RC14", "112"="RC18 & RC14", "113"="IR42 & RC14", "114"="RC18", "115"="RC14 & Bordagol", "116"="RC16", "117"="RC30", "118"="RC6", "119"="IR14", "120"="RC28", "121"="IR28/R28", "122"="Super Intan", "125"="Ratoon", "126"="IR6", "127"="IR50", "128"="IR30", "129"="IR80")
	villages <- data.frame(location=c("Tubuan", "Gabaldon", "Signe", "Pandan"), adm2=c("Pila", "Mu\u00f1oz", "Igbaras", "Dingle"), adm1=c("Laguna", "Nueva Ecija", "Iloilo", "Iloilo"))
	vil <- num(lu$VIL)
	season <- num(lu$SEASON)
	code <- num(lu$CROP)
	unit <- tolower(trimws(lu$UNIT))
	prod <- num(lu$PRODN)
	prod[!(unit %in% c("kgs", "kg"))] <- NA

	urea <- qty(ci, "UREA")
	as <- qty(ci, "AMOSUL")
	ap <- qty(ci, "AMOPHOS")
	cp <- qty(ci, "COMP")
	dap <- qty(ci, "18460")
	ft <- cbind(ifelse(urea > 0, "urea", NA), ifelse(as > 0, "DAS", NA), ifelse(ap > 0, "16-20-0", NA), ifelse(cp > 0, "NPK", NA), ifelse(dap > 0, "DAP", NA))
	ftype <- apply(ft, 1, function(x) if (all(is.na(x))) NA else paste(x[!is.na(x)], collapse="; "))
	ftype[!is.na(urea + as + ap + cp + dap) & (urea + as + ap + cp + dap) == 0] <- "none"
	ftype[is.na(ic)] <- NA
	org <- num(ci$ORGANIC)
	orgtype <- c("cattle dung", "poultry manure", NA, "guano", NA)[match(num(ci$TYPORG), 1:5)]

	md <- grep("^[FH][MF](PLP|LP|CE|CC|HPH)MD$", names(lb), value=TRUE)
	mdays <- sapply(lb[, md], num)
	labour <- ifelse(rowSums(!is.na(mdays)) > 0, rowSums(mdays, na.rm=TRUE), NA)

	irrs <- c("no source", "government project", "community project", "private investment", "rainfed", "rainfed and pump", "river", "natural spring", "non-functional NIA")[match(num(op$IRRIGS), 1:9)]

	d <- data.frame(
		hhid = paste(vil, num(lu$HHNO), sep="_"),
		field_id = paste(vil, num(lu$HHNO), num(lu$PNO), sep="_"),
		season = c("wet", "dry", NA)[match(season, 1:3)],
		season_label_ = c("92 WS", "93 DS", "93 third crop")[match(season, 1:3)],
		crop_year_ = num(lu$YEAR),
		crop = crops[code],
		crop_label_ = clab[code],
		variety = ifelse(code %in% 1, vars[as.character(num(lu$VAR))], NA),
		field_size = ha,
		yield = prod / ha,
		crop_price = ifelse(unit %in% c("kgs", "kg"), num(lu$PRICE), NA),
		N_fertilizer = (urea * 0.46 + as * 0.21 + ap * 0.16 + cp * 0.14 + dap * 0.18) / cha,
		P_fertilizer = (ap * 0.2 + cp * 0.14 + dap * 0.46) * 0.4364 / cha,
		K_fertilizer = cp * 0.14 * 0.8301 / cha,
		fertilizer_type = ftype,
		OM_used = ifelse(is.na(org), NA, org > 0),
		OM_type = ifelse(org %in% 0, NA, orgtype),
		OM_amount = org / cha,
		seed_rate = num(ci$SEEDS) / cha,
		seed_cost = num(ci$VALSEED) / cha,
		fertilizer_cost = (qty(ci, "VALUREA") + qty(ci, "VAMSOL") + qty(ci, "VAMPOS") + qty(ci, "VCOMP") + qty(ci, "VCOMP2") + qty(ci, "V18460")) / cha,
		OM_cost = num(ci$VORGAN) / cha,
		insecticide_cost = num(ci$INSEC) / cha,
		herbicide_cost = num(ci$HERB) / cha,
		irrigation_cost = (num(ci$IRRIG) + num(ci$IRFUEL)) / cha,
		labour = labour / num(lb$AREAP),
		land_prep_cost = (num(lb$PLPCOST) + num(lb$LPCOST)) / num(lb$AREAP),
		planting_cost = num(lb$CECOST) / num(lb$AREAP),
		topography_ = c("upland", "lowland not flooded", "lowland flooded up to 30 cm", "lowland flooded 30 to 100 cm", "lowland flooded above 100 cm", "upland and lowland not flooded", "lowland not flooded and flooded up to 30 cm")[match(num(op$TOPOG), 1:7)],
		soil_quality = c("very good", "good", "average", "poor", "acidic")[match(num(op$SOILQ), 1:5)],
		irrigation_source = irrs,
		irrigated = ifelse(is.na(irrs), NA, !(irrs %in% c("no source", "rainfed", "non-functional NIA"))),
		land_tenure = c("share cropping", "leasehold (fixed rent after harvest)", "fixed rent before harvest", "CLT (owned)", "mortgaged", "owner", "borrowed", "communal", "homestead", "share tenant and leasehold", "owner and leasehold", "mortgaged and fixed rent", "owned and mortgaged", "owned and share tenant", "owned (bakod)", "landowner")[match(num(op$TENURE), 1:16)],
		currency = "PHP",
		location = villages$location[vil],
		adm2 = villages$adm2[vil],
		adm1 = villages$adm1[vil],
		country = "Philippines"
	)
	d <- d[!is.na(code), ]
	d$yield_part <- NA
	d$yield_part[d$crop %in% c("rice", "maize")] <- "grain"
	d$yield_part[d$crop %in% c("mung bean", "cowpea")] <- "seed"
	d$yield_part[d$crop %in% "groundnut"] <- "pod"
	d$yield_part[d$crop %in% "sweetpotato"] <- "roots"
	d$yield_part[d$crop %in% c("watermelon", "asparagus bean", "eggplant", "tomato", "okra", "chili pepper", "bitter gourd", "squash", "luffa", "cucumber", "bottle gourd", "fruit")] <- "fruit"
	d$yield_part[d$crop %in% c("jute mallow", "tobacco")] <- "leaves"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
