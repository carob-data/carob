# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI survey of rural socio-economic conditions in 4 villages in China (Zhejiang, Hunan and Jiangxi provinces), 2004-05 round (same villages as doi:10.7910/DVN/26631). Single workbook CHINA05.xls with one sheet per module; codes from "CodingManual2005.doc".
# Three cropping seasons: late 2004, early 2005, middle 2005 (season_label_). Rice types: japonica, indica, hybrid (rice_type_).
# Two record types are combined in d (record_type_):
#  "parcel": LANDU, one row per parcel x season with a crop: parcel area, crop and production (kg), parcel type, ownership, soil quality, irrigation.
#  "household": AGPRAC/LABOR/PROD sheets, one row per household x season: area, crop, crop establishment, operations, fertilizer quantities, costs, labour, production (kg), normal production, price, losses.
#  The household sheets of a season have the same households in the same order and are joined by row.
# Area unit is not given in the manual; it is mu (1/15 ha), as in the 1994-95 round (rice yields ~6-7 t/ha).
# Crop entries mix crop codes and text: "3-2070" (code-variety), Xiushui (秀水) japonica varieties, "杂交2070" (hybrid 2070), "花育1号" (Huayu 1, groundnut), and Chinese crop names (pear, peach, asparagus, watermelon, vegetables, fruit); these are parsed. Bare "03", "09" and "110" are read as Xiushui 03/09/110 (they are not crop codes and occur in the same villages).
# Labour = sum of total man-days for plowing, crop establishment, weeding, irrigation, fertilizer, pesticide, harvest/transport and other operations, per ha.
# Costs are in Yuan (CNY) and converted to per ha.

## ISSUES
# No coordinates. 4 villages (Fangquan, Xieleqiao, Changjiang, Fuxia), but which village is in which province (Zhejiang, Hunan, Jiangxi) is not given. Need georeferencing.
# Area unit (mu) inferred from yields, not documented.
# Fertilizer quantities ("nitrogen/phosphate/potash fertilizer use (kg)") are product amounts of unknown type, so no N/P/K rates: kept as product kg/ha (new variables).
# Very high yields on very small parcels (e.g. vegetables up to 300 t/ha, groundnut 12.8 t/ha): area or production recording errors, not removed.
# Seed quantity has no unit in the manual: kept as seed_quantity_ (per ha).
# Planting and harvest times are a mix of month numbers and Excel date serials; not mapped. harvest_date is only the year of the season.
# Seasons (late, early, middle) not mapped to season vocabulary (season NA).
# Mixed entries ("9、5", "2、9、10"), "90", willow, "not planted" and "unclear" have no crop; codes 10 (cash crop) and 11 (others) have no crop name. Variety "2070"/"3070"/"2027" are given as numbers only.
# LANDU soil type code 3 (most common) and text entries are undocumented (soil_type_ NA); GEOG is empty.
# LABOR sheets have two columns named CE (cost and method of crop establishment); taken in that order. Harvest and transport cost (HT) not mapped (harvest_cost is per kg).
# Loss reasons R2 are partly Chinese free text; only R1 is mapped.
# Unmapped: BASIC (members), REMIT, ASSETS, INCC, INCLHG (livestock/homestead income), CFEEDS, TIMEH, LSPROB, MKTG, INCNF, BORROW, DAILYEX, MAJOREX, CHANGE, COMPANY, ATTITUDE, INCOME.

## NEW VARIABLES
# record_type_: "parcel" (LANDU) or "household" (AGPRAC/LABOR/PROD sheets)
# season_label_: late 2004, early 2005, middle 2005
# crop_label_: original crop entry
# rice_type_: japonica, indica or hybrid rice
# parcel_type_: paddy field, dry land, sloping field, water area
# topography_: lowland flat, upland flat, upland sloping, pond
# soil_type_: red soil or others
# fertilizer_product_: total fertilizer product applied (kg/ha)
# N_fertilizer_product_, P_fertilizer_product_, K_fertilizer_product_: nitrogen, phosphate and potash fertilizer product applied (kg/ha)
# fertilizer_times_: number of fertilizer applications
# pesticide_times_: number of pesticide applications
# seed_quantity_: quantity of seed used per ha (unit not given)
# production_status_: "on or above average yield" or "below average yield"


carob_script <- function(path) {

"
Survey for Assessment of Rural Socio-Economic Conditions in China (2004 - 2005)

To examine and analyze the growth of rice production, economic inefficiency, as well as yield gaps caused by inefficiently using existing resources and technology in China.
"

	uri <- "doi:10.7910/DVN/25899"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 40,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "CHINA05.xls"]
	rd <- function(sheet, sfx) {
		x <- carobiner::read.excel(f, sheet=sheet, skip=5, .name_repair="minimal")
		x <- x[, names(x) != "" & !is.na(names(x))]
		names(x) <- sub(paste0(sfx, "(\\.[0-9]+)?$"), "\\1", make.unique(names(x)))
		x
	}
	# numbers stored as text, with some free text entries
	num <- function(x) {
		x <- trimws(as.character(x))
		x[!grepl("^[0-9]*\\.?[0-9]+$", x)] <- NA
		as.numeric(x)
	}
	join <- function(x, y) {
		if (nrow(x) == nrow(y) && all(x$HHNO == y$HHNO)) {
			cbind(x, y[, !(names(y) %in% c("HHNO", "VLAGE"))])
		} else {
			y <- y[!duplicated(paste(y$VLAGE, y$HHNO)), ]
			merge(x, y, by=c("VLAGE", "HHNO"), all.x=TRUE)
		}
	}
	prefix <- function(x, p) {
		i <- !(names(x) %in% c("HHNO", "VLAGE"))
		names(x)[i] <- paste0(p, names(x)[i])
		x
	}
	sumNA <- function(...) {
		x <- cbind(...)
		ifelse(rowSums(!is.na(x)) > 0, rowSums(x, na.rm=TRUE), NA)
	}

	mu <- 1/15
	villages <- c("Fangquan", "Xieleqiao", "Changjiang", "Fuxia")
	crops <- data.frame(code = 1:11,
		crop = c("rice", "rice", "rice", "wheat", "barley", "soybean", "sweetpotato", "oilseed", "vegetable", NA, NA),
		rice_type = c("japonica", "indica", "hybrid", rep(NA, 8))
	)
	# crop entries: codes, "code-variety", variety names and Chinese crop names
	parse_crop <- function(x) {
		x <- trimws(as.character(x))
		code <- rep(NA_real_, length(x))
		crop <- rep(NA_character_, length(x))
		variety <- rep(NA_character_, length(x))
		i <- grepl("^[0-9]+$", x) & x %in% as.character(1:11)
		code[i] <- as.numeric(x[i])
		i <- grepl("^[0-9]+-[0-9]", x)
		code[i] <- as.numeric(sub("-.*", "", x[i]))
		variety[i] <- sub("^[0-9]+-", "", x[i])
		# Xiushui (japonica)
		i <- grepl("\u79c0\u6c34", x) | x %in% c("03", "09", "110")
		code[i] <- 1
		variety[i] <- paste("Xiushui", sub("\u79c0\u6c34", "", x[i]))
		# hybrid 2070
		i <- x %in% c("2070", "\u6742\u4ea42070")
		code[i] <- 3
		variety[i] <- "2070"
		code[code > 11] <- NA
		crop <- crops$crop[match(code, crops$code)]
		rtype <- crops$rice_type[match(code, crops$code)]
		# Huayu 1 (groundnut)
		i <- x == "\u82b1\u80b21\u53f7"
		crop[i] <- "groundnut"
		variety[i] <- "Huayu 1"
		crop[grepl("\u68a8", x)] <- "pear"
		crop[x == "\u6843\u6728"] <- "peach"
		crop[grepl("\u82a6\u7b0b", x)] <- "asparagus"
		crop[x == "\u83dc\u7c7b"] <- "vegetable"
		crop[x == "\u897f\u74dc"] <- "watermelon"
		crop[x %in% c("\u6c34\u679c", "11\uff08\u6c34\u679c\uff09", "\u6c34\u679c\uff08\u68a8\u3001\u6843\uff09")] <- "fruit"
		data.frame(crop=crop, variety=variety, rice_type_=rtype, variety_type=ifelse(code %in% 3, "hybrid", NA), crop_label_=x)
	}
	loss <- data.frame(code = 1:8,
		label = c("drought", "flood", "typhoon", "diseases", "insects", "weeds", "low input use", "others"),
		stress = c("drought", "flood", "wind", "disease", "pests", "weeds", NA, "other")
	)

	seasons <- data.frame(
		sfx = c("L04", "E05", "M05"),
		label = c("late 2004", "early 2005", "middle 2005"),
		year = c("2004", "2005", "2005"),
		agprac = c("AGPRAC1", "AGPRAC2", "AGPRAC3"),
		labA = c("LABOR1", "LABOR3", "LABOR5"),
		labB = c("LABOR2", "LABOR4", "LABOR6"),
		prod = c("PRODL04", "PRODE05", "PRODM05")
	)

	## household x season
	d2 <- lapply(1:3, function(i) {
		s <- seasons[i, ]
		x <- join(join(join(prefix(rd(s$agprac, s$sfx), "a_"), prefix(rd(s$labA, s$sfx), "l1_")), prefix(rd(s$labB, s$sfx), "l2_")), prefix(rd(s$prod, s$sfx), "p_"))
		ha <- num(x$a_AR) * mu
		data.frame(
			hhid = paste(x$VLAGE, x$HHNO, sep="_"),
			village = num(x$VLAGE),
			season_label_ = s$label,
			harvest_date = s$year,
			parse_crop(x$a_CR),
			field_size = ha,
			planting_method = c("direct seeding", "transplanting")[match(num(x$l1_CE.1), 1:2)],
			land_prep_times = as.integer(num(x$l1_NP)),
			weeding_times = as.integer(num(x$l1_NW)),
			irrigation_number = as.integer(num(x$l2_NI)),
			fertilizer_times_ = num(x$l2_NF),
			pesticide_times_ = num(x$l2_NP),
			seed_quantity_ = num(x$l1_QS) / ha,
			fertilizer_product_ = num(x$l2_QF) / ha,
			N_fertilizer_product_ = num(x$l2_QNF) / ha,
			P_fertilizer_product_ = num(x$l2_QPF) / ha,
			K_fertilizer_product_ = num(x$l2_QKF) / ha,
			land_prep_cost = num(x$l1_LP) / ha,
			planting_cost = num(x$l1_CE) / ha,
			seed_cost = num(x$l1_VS) / ha,
			weeding_cost = num(x$l1_WD) / ha,
			herbicide_cost = num(x$l1_VH) / ha,
			irrigation_cost = num(x$l2_IRR) / ha,
			fertilizer_cost = num(x$l2_FER) / ha,
			pesticide_cost = num(x$l2_PEST) / ha,
			labour = sumNA(num(x$l1_TMP), num(x$l1_TMCE), num(x$l1_TMH), num(x$l2_TMI), num(x$l2_TMF), num(x$l2_TMP), num(x$l2_TMHT), num(x$l2_TMO)) / ha,
			yield = num(x$p_CR) / ha,
			expected_yield = num(x$p_NP) / ha,
			crop_price = num(x$p_PRIC),
			production_status_ = c("on or above average yield", "below average yield")[match(num(x$p_PSTUS), 1:2)],
			loss_code = num(x$p_R1),
			currency = "CNY"
		)
	})
	d2 <- do.call(rbind, d2)
	d2 <- d2[!is.na(d2$crop_label_) & !is.na(d2$field_size) & d2$field_size > 0, ]
	d2$record_type_ <- "household"

	## parcel x season (LANDU)
	r1 <- carobiner::read.excel(f, sheet="LANDU", skip=5)
	d1 <- lapply(1:3, function(i) {
		s <- seasons[i, ]
		ha <- num(r1$PAREA) * mu
		data.frame(
			hhid = paste(r1$VLAGE, r1$HHNO, sep="_"),
			village = num(r1$VLAGE),
			field_id = as.character(r1$PNO),
			season_label_ = s$label,
			harvest_date = s$year,
			parse_crop(r1[[paste0("CR", s$sfx)]]),
			field_size = ha,
			yield = num(r1[[paste0("PR", s$sfx)]]) / ha,
			parcel_type_ = c("paddy field", "dry land", "sloping field", "water area")[match(num(r1$TYPEP), 1:4)],
			land_tenure = c("family plot", "contracted plot", "rented out", "rented in")[match(num(r1$TYPEO), 1:4)],
			topography_ = c("lowland flat", "upland flat", "upland sloping", "pond")[match(num(r1$TOPOG), 1:4)],
			soil_type_ = c("red soil", "others")[match(num(r1$TSOIL), 1:2)],
			soil_quality = c("good", "average", "poor")[match(num(r1$QSOIL), 1:3)],
			irrigated = c(TRUE, FALSE)[match(num(r1$IRIG), 1:2)]
		)
	})
	d1 <- do.call(rbind, d1)
	d1 <- d1[!is.na(d1$crop_label_), ]
	d1$record_type_ <- "parcel"

	d <- carobiner::bindr(d1, d2)
	d$yield_part <- NA
	d$yield_part[d$crop %in% c("rice", "wheat", "barley")] <- "grain"
	d$yield_part[d$crop %in% c("soybean", "oilseed")] <- "seed"
	d$yield_part[d$crop %in% "groundnut"] <- "pod"
	d$yield_part[d$crop %in% "sweetpotato"] <- "roots"
	d$yield_part[d$crop %in% c("pear", "peach", "watermelon", "fruit")] <- "fruit"
	d$yield_part[d$crop %in% "asparagus"] <- "stems"
	i <- match(d$loss_code, loss$code)
	d$season_constraint <- loss$label[i]
	d$stress <- loss$stress[i]
	d$loss_code <- NULL
	d$location <- villages[d$village]
	d$village <- NULL
	d$country <- "China"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
