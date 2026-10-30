# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI SSD Laguna Loop Survey, 1981 wet season and 1982 dry season (Laguna province, Philippines). One record per household x season x parcel of the Production sheet (314 records).
# Sheets used: Production, Farmchar, Fertilizer, Crop Establishment, Hvt/Thr/Post harvest, Seeds, HH info (household head only). Coding manual: SEASON 1 = wet, 2 = dry (also coded WS/DS).
# Parcels are matched across sheets on year, season, HHCODE, PARNO and FSIZE (some households list sub-parcels under one PARNO with different FSIZE);
#   if that fails (FSIZE differs between sheets) they are matched without FSIZE when the parcel is unique in both sheets.
# FSIZE is area planted (ha); yield = PRODNKG / FSIZE. PRODNKG = PRODN (cavan) x KGCAV (kg/cavan at harvest), i.e. paddy weight at harvest (yield_isfresh).
# Fertilizer: grade (N-P2O5-K2O, %) taken from BRAND when it is a grade label (e.g. 14-14-14), else standard composition for urea, ammonium sulfate, muriate of potash, else the N, P, K columns.
#   kg nutrient = QTYKG x grade%, P = P2O5/2.29, K = K2O/1.2051, summed over the parcel's rows and divided by FSIZE.
#   TOT_N/TOT_P/TOT_K columns are not used (in this round TOT_P and TOT_K are oxides).
#   Parcels listed in the Fertilizer sheet without any product get 0; parcels absent from the sheet are NA.
# transplanting_date / harvest_date: earliest date of the Transplanting / Harvesting activity of the parcel. Excel serials are used if within 1 year of the survey YEAR;
#   text dates only when the year is certain (4-digit year in the text, or wet-season months Apr-Dec in the survey YEAR). 'Nth week of <month>' gives year-month.
# Seeds METHOD (dapog/wetbed/drybed are nursery types of transplanted rice) -> planting_method; QTYSEED / FSIZE -> seed_rate.
# Dry-season parcels with garlic, watermelon or tomato (in Production VARIETY) are given that crop; 'NO DRY SEASON RICE' gets crop NA. Production is not recorded for these.
# previous_crop: combinations ('Rice and Watermelon') written as ';'-separated crops. soil_texture from SOIL TYPE.

## ISSUES
# No coordinates, and no municipality/barangay per household. The sample covers six Laguna municipalities (Calauan, Bay, Calamba, Cabuyao, Sta. Rosa, Binan); human must georeference (only adm1 = Laguna set).
# QTYSEED unit is not given in the coding manual; treated as kg (values are often multiples of a cavan). Verify.
# The meaning of the dry-season YEAR label differs between rounds (planting year in 2007-08, harvest year in 1995), so dry-season text dates without a year are not parsed.
# Products without known composition (organics, foliars, brand names) contribute nothing to N/P/K, which may underestimate the totals.
# Fertilizer timing (DBTDAT, days before/after transplanting) and fertilizer prices are not processed; candidates for a long table.
# Unprocessed: Land Preparation (power source, labor), Crop Care (pesticides), labor and cost columns of all sheets, land rent, irrigation costs, production disposal (shares, sold, home consumption, seed kept).
# 'Malagkit' (glutinous rice) is a rice type rather than a variety; variety names and spellings are not standardized; some records list several varieties.
# No dry-season dates parsed (text without year, e.g. 'FEB 4TH WK'); Farmchar DATE PLNTED has 2011 Excel serials (artifact; dropped).
# HH 849 has tiny parcels (0.07-0.12 ha) with yields of 10-12.7 t/ha in both seasons and > 500 kg N/ha in the dry season. Check FSIZE.
# One wet-season 1981 harvest date in 1982-11. Check.
# Hhinfo covers 1981 WS only; head sex/age/size attached to both seasons by HHCODE.
# Fertilizer '12-00-00' kept as grade 12-0-0; 'ZINC' has no N/P/K.
# write_files: missing longitude/latitude/geo_from_source; new variables; invalid previous_crop 'vegetables'; invalid fertilizer_type 12-0-0, 16-20-0, ZINC; NA yield/yield_part for non-rice parcels.

## NEW VARIABLES
# survey_year_: YEAR label of the survey round (for the dry season this is not always the planting year)
# seedbed_method_: Seeds METHOD as recorded (dapog, wetbed, drybed, direct seeding)
# planting_pattern_: Farmchar PLANT SPACING (straight-row vs ordinary/random planting)
# share_output_: Farmchar SHARE OUTPUT (crop share arrangement, e.g. 50:50)


carob_script <- function(path) {

"Laguna Loop Survey (1981-1982)

The Laguna Loop survey is a farm household level data set collected by the IRRI - Agricultural Economics Department prior to the introduction of the high yielding varieties of rice.  This initial survey refers to 1965 dry season - data that provided excellent benchmark information from rice farming respondents.  The study was based upon a survey in which information was obtained regarding farm practices and costs and returns and other associated data related to rice production.   The subsequent surveys have been taken every two to four years to note the pattern of adoption of the high yielding varieties, paddy yield, prices, various farming inputs, labor use, land tenure arrangement and the changes that have occurred in farming practices and costs and returns that have occurred overtime.  These data are extremely useful historical records of the changes that have occurred during the four decades, the dynamic period of Philippine rice production. The sample farms are selected from the six municipalities in Laguna province - namely:  Calauan, Bay, Calamba, Cabuyao, Sta. Rosa and Binan .    In the later years - some of the farmers discontinue farming because their rice farms were converted into industrial and residential purpose.  Hence, there was a reduction in the number of respondents per year or per season in the later years. Generally, the focal objective of this study is to track the changes in the farme's rice technology, cultural practices, land tenure, mechanization, and labor practices that occurred during the survey period from 1965 to 2008.  A number of publications, research papers, reports,  and vast collections of   Masters and Dissertations theses have already been published that utilized this very exhaustive  panel data sets."

	uri <- "doi:10.7910/DVN/26357"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "Laguna Loop Survey",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Laguna Loop_1981-82.xlsx"]
	rd <- function(sheet, skip) {
		carobiner::read.excel(f, sheet=sheet, skip=skip, col_types="text")
	}
	rP <- rd("Production 1981-82", 3)
	rF <- rd("Farmchar 1981-82", 3)
	rN <- rd("Fertilizer 1981", 3)
	rE <- rd("Crop Establishment 1981-82", 3)
	rH <- rd("Hvt, thr, Post harvest 1981-82", 3)
	rS <- rd("Seeds 1981-82", 3)
	rI <- rd("HHinfo 1981-82", 3)

	# SEASON is "WS"/"DS" in most sheets and 1/2 in others (coding manual: 1 = wet, 2 = dry)
	season <- function(x) {
		x <- toupper(trimws(x))
		ifelse(x %in% c("1", "WS"), "wet", ifelse(x %in% c("2", "DS"), "dry", NA))
	}
	pkey <- function(r, fsize=TRUE) {
		k <- paste(r$YEAR, season(r$SEASON), r$HHCODE, r$PARNO, sep="_")
		if (fsize) k <- paste(k, round(as.numeric(r$FSIZE), 2), sep="_")
		k
	}
	key <- pkey(rP)
	key4 <- pkey(rP, FALSE)
	# x aggregated with fun for each Production record. Matched on the parcel key with FSIZE,
	# else on the key without FSIZE when that identifies a single parcel in both sheets
	agg <- function(x, r, fun) {
		if (length(x) == 0) return(rep(NA, length(key)))
		k5 <- pkey(r)
		k4 <- pkey(r, FALSE)
		a5 <- tapply(x, k5, fun)
		a4 <- tapply(x, k4, fun)
		out <- as.vector(a5[key])
		n4 <- tapply(k5, k4, function(z) length(unique(z)))
		alt <- !(key %in% k5) & !(key4 %in% key4[duplicated(key4)]) & key4 %in% names(n4)[n4 == 1]
		out[alt] <- as.vector(a4[key4[alt]])
		out
	}
	pick <- function(x, r) {
		agg(x, r, function(z) z[1])
	}

	# Excel serials; text dates only when the year is certain:
	# a 4-digit year in the text, or a wet season month (Apr-Dec) in the survey YEAR
	todate <- function(x, year, seas) {
		mons <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
		one <- function(s, y, w) {
			s <- tolower(trimws(s))
			if (is.na(s)) return(NA_character_)
			if (grepl("^[0-9]{5}$", s)) {
				dt <- as.Date(as.numeric(s), origin="1899-12-30")
				# serials far from the survey year (e.g. 2011 in the 1978 round) are data-entry artifacts
				if (is.na(y) || abs(as.numeric(format(dt, "%Y")) - as.numeric(y)) > 1) return(NA_character_)
				return(as.character(dt))
			}
			m <- regmatches(s, regexpr(paste(mons, collapse="|"), s))
			if (length(m) == 0) return(NA_character_)
			mon <- match(m, mons)
			y4 <- regmatches(s, regexpr("(19|20)[0-9]{2}", s))
			if (length(y4) == 0) {
				if (!(w %in% "wet" && mon >= 4)) return(NA_character_)
				y4 <- y
			}
			s <- sub("(19|20)[0-9]{2}", "", s)
			dd <- regmatches(s, regexpr("[0-9]{1,2}", s))
			if (length(dd) == 0 || grepl("week|wk", s)) return(sprintf("%s-%02d", y4, mon))
			as.character(as.Date(sprintf("%s-%02d-%02d", y4, mon, as.integer(dd)), format="%Y-%m-%d"))
		}
		mapply(one, x, year, seas, USE.NAMES=FALSE)
	}
	first_date <- function(r, pattern, exclude) {
		act <- tolower(r$ACTIVITY)
		r <- r[grepl(pattern, act) & !grepl(exclude, act), ]
		x <- unlist(todate(r$DATE, r$YEAR, season(r$SEASON)))
		agg(x, r, function(z) if (all(is.na(z))) NA else min(z, na.rm=TRUE))
	}

	## fertilizer
	b <- toupper(trimws(rN$BRAND))
	pN <- as.numeric(rN$N)
	pP <- as.numeric(rN$P)
	pK <- as.numeric(rN$K)
	# standard composition (N-P2O5-K2O) of urea, ammonium sulfate and muriate of potash
	std <- data.frame(
		b = c("UREA", "UREA - VIKINGS", "NH4 S04", "SULPHATE", "AMMONIUM SULPHATE", "AMONIUM SULFATE", "AS", "MURIATE"),
		N = c(46, 46, 21, 21, 21, 21, 21, 0),
		P = c(0, 0, 0, 0, 0, 0, 0, 0),
		K = c(0, 0, 0, 0, 0, 0, 0, 60)
	)
	i <- match(b, std$b)
	pN[!is.na(i)] <- std$N[i[!is.na(i)]]
	pP[!is.na(i)] <- std$P[i[!is.na(i)]]
	pK[!is.na(i)] <- std$K[i[!is.na(i)]]
	g <- grepl("^[0-9]+-[0-9]+(-[0-9]+)?$", b)
	gs <- t(sapply(strsplit(paste0(b[g], "-0-0"), "-"), function(z) z[1:3]))
	pN[g] <- as.numeric(gs[,1])
	pP[g] <- as.numeric(gs[,2])
	pK[g] <- as.numeric(gs[,3])
	qty <- as.numeric(rN$QTYKG)
	qty[is.na(qty) & is.na(b)] <- 0
	rN$kgN <- ifelse(qty == 0, 0, qty * pN / 100)
	rN$kgP <- ifelse(qty == 0, 0, qty * pP / 100 / 2.29)
	rN$kgK <- ifelse(qty == 0, 0, qty * pK / 100 / 1.2051)
	ftype <- b
	ftype[g] <- paste(pN[g], pP[g], pK[g], sep="-")
	ftype[g & pK > 0] <- "NPK"
	ftype[ftype %in% c("UREA", "UREA - VIKINGS", "46-0-0")] <- "urea"
	ftype[ftype %in% c(std$b[3:7], "21-0-0")] <- "DAS"
	ftype[ftype == "MURIATE"] <- "KCl"
	ftype[ftype == "18-46-0"] <- "DAP"
	ftype[ftype %in% c("NONE", "FALLOW", "NRICE")] <- "none"
	ftype[ftype == "-99"] <- NA
	ftype[is.na(b)] <- "none"
	sumna <- function(z) sum(z, na.rm=TRUE)
	ptype <- function(z) {
		z <- unique(z[!is.na(z)])
		if (length(z) > 1) z <- z[z != "none"]
		if (length(z) == 0) NA else paste(z, collapse="; ")
	}

	## crop and production
	v <- trimws(rP$VARIETY)
	v <- sub("^(.)", "\\U\\1", v, perl=TRUE)
	crop <- rep("rice", nrow(rP))
	crop[toupper(v) == "GARLIC"] <- "garlic"
	crop[toupper(v) == "WATERMELON"] <- "watermelon"
	crop[toupper(v) == "TOMATOES"] <- "tomato"
	# "NO DRY SEASON RICE": no rice on the parcel, other use not recorded
	crop[toupper(v) == "NO DRY SEASON RICE"] <- NA
	v[is.na(crop) | crop != "rice"] <- NA
	area <- as.numeric(rP$FSIZE)

	d <- data.frame(
		hhid = rP$HHCODE,
		field_id = rP$PARNO,
		survey_year_ = rP$YEAR,
		season = season(rP$SEASON),
		country = "Philippines",
		adm1 = "Laguna",
		crop = crop,
		variety = v,
		field_size = area,
		yield = ifelse(crop == "rice", as.numeric(rP$PRODNKG) / area, NA),
		crop_price = ifelse(crop == "rice", as.numeric(rP$PRICE), NA),
		currency = "PHP"
	)

	irr <- tolower(trimws(pick(rF$IRRTYPE, rF)))
	d$irrigation_source <- irr
	d$irrigated <- ifelse(is.na(irr), NA, irr != "rainfed")
	d$land_tenure <- tolower(trimws(pick(rF$TENURE, rF)))
	d$share_output_ <- pick(rF[["SHARE OUTPUT"]], rF)
	d$planting_pattern_ <- tolower(trimws(pick(rF[["PLANT SPACING"]], rF)))
	prev <- tolower(trimws(pick(rF[["CROP PLANTED LAST YEAR"]], rF)))
	prev <- gsub(" *(,| and ) *", ";", gsub("veg\\.", "vegetables", prev))
	d$previous_crop <- gsub(";+", ";", prev)
	soil <- tolower(trimws(pick(rF[["SOIL TYPE"]], rF)))
	soil[soil == "silt loam"] <- "silty loam"
	soil[soil == "sandy"] <- "sand"
	d$soil_texture <- soil
	pdate <- unlist(todate(rF[["DATE PLNTED"]], rF$YEAR, season(rF$SEASON)))
	d$planting_date <- pick(pdate, rF)

	# fertilizer of sub-parcels that share HHCODE, PARNO and FSIZE cannot be separated
	dup <- key %in% key[duplicated(key)]
	d$N_fertilizer <- ifelse(dup, NA, agg(rN$kgN, rN, sumna) / area)
	d$P_fertilizer <- ifelse(dup, NA, agg(rN$kgP, rN, sumna) / area)
	d$K_fertilizer <- ifelse(dup, NA, agg(rN$kgK, rN, sumna) / area)
	d$fertilizer_type <- agg(ftype, rN, ptype)

	d$transplanting_date <- first_date(rE, "transplant|direct seed", "seedbed|pulling")
	d$harvest_date <- first_date(rH, "harvest", "post")

	m <- tolower(trimws(pick(rS$METHOD, rS)))
	d$seedbed_method_ <- m
	d$planting_method <- ifelse(m %in% c("dapog", "wetbed", "drybed"), "transplanting",
		ifelse(m == "direct seeding", "direct seeding", NA))
	d$seed_rate <- as.numeric(pick(rS$QTYSEED, rS)) / area
	d$seed_source <- tolower(trimws(pick(rS$SEEDSRC, rS)))

	head <- rI[tolower(trimws(rI[["HOUSEHOLD MEMBER"]])) == "head" & !is.na(rI[["HOUSEHOLD MEMBER"]]), ]
	i <- match(d$hhid, head$HHCODE)
	sex <- toupper(trimws(head$SEX[i]))
	d$sex <- ifelse(sex == "M", "male", ifelse(sex == "F", "female", NA))
	d$age <- as.numeric(head$AGE[i])
	d$hh_size <- as.numeric(head[["HOUSEHOLD SIZE"]][i])

	d$trial_id <- paste(d$hhid, d$survey_year_, d$season, sep="_")
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- ifelse(d$crop == "rice", "grain", NA)
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d)
}
