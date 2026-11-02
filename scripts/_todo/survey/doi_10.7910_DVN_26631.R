# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI survey of rural socio-economic conditions in 4 villages in China (Zhejiang, Hunan and Jiangxi provinces), 1994-95. Single workbook CHINA95(060624).xls with one sheet per module; codes from "Coding Manual 1994-95.doc".
# Three cropping seasons: late 1994, early 1995, middle 1995 (season_label_). Rice types: japonica, indica, hybrid (rice_type_).
# Two record types are combined in d (record_type_):
#  "parcel": LANDU, one row per parcel x season with a crop: parcel area, crop and production (kg), soil quality, irrigation.
#  "household": AGPRAC/LABOR/PROD sheets, one row per household x season: area, crop, crop establishment, operations, fertilizer quantities, costs, production (kg), price, losses.
#  The household sheets of a season are joined by row when they have the same households in the same order, else by village and household number.
# Area unit is not given in the manual; it is mu (1/15 ha): parcel and household rice yields are then ~420 kg/mu (6.2 t/ha), in hectares they would be 0.4 t/ha.
# Costs are in Yuan (CNY) and converted to per ha.

## ISSUES
# No coordinates. 4 villages (Fangquan, Xieleqiao, Changjiang, Fuxia), but which village is in which province (Zhejiang, Hunan, Jiangxi) is not given. Need georeferencing.
# Area unit (mu) inferred from yields, not documented.
# Fertilizer quantities ("nitrogen/phosphate/potash fertilizer use (kg)") are product amounts of unknown type, so no N/P/K rates: kept as product kg/ha (new variables).
# Very high yields on very small parcels (e.g. vegetables up to 300 t/ha, groundnut 12.8 t/ha): area or production recording errors, not removed.
# Seed quantity has no unit in the manual: kept as seed_quantity_ (per ha).
# Planting and harvest times are Excel date serials with wrong years (1996, 1998) or mmdd numbers; not mapped. harvest_date is only the year of the season.
# Seasons (late, early, middle) not mapped to season vocabulary (season NA).
# Crop codes 10 (cash crop) and 11 (others) have no crop name (crop NA); a few non-numeric crop entries in LANDU (e.g. "11,9", "M", "rice", "single") set to NA.
# Labour (man-days) only given for plowing, crop establishment, weeding and irrigation: not mapped.
# Unmapped: BASIC (members), REMIT, ASSETS, INCLHG (livestock/homestead income), CFEEDS, TIMEH, LSPROB, MKTG, INCNF, BORROW, DAILYEX, MAJOREX, CHANGE, INCOME.

## NEW VARIABLES
# record_type_: "parcel" (LANDU) or "household" (AGPRAC/LABOR/PROD sheets)
# season_label_: late 1994, early 1995, middle 1995
# crop_label_: original crop label
# rice_type_: japonica, indica or hybrid rice
# topography_: lowland flat, upland flat, upland sloping, pond
# geography_: plain or hill
# soil_type_: red soil or others
# fertilizer_product_: total fertilizer product applied (kg/ha)
# N_fertilizer_product_, P_fertilizer_product_, K_fertilizer_product_: nitrogen, phosphate and potash fertilizer product applied (kg/ha)
# fertilizer_times_: number of fertilizer applications
# pesticide_times_: number of pesticide applications
# seed_quantity_: quantity of seed used per ha (unit not given)
# production_status_: "on or above average yield" or "below average yield"


carob_script <- function(path) {

"
Survey for Assessment of Rural Socio-Economic Conditions in China (1994 - 1995)

To examine and analyze the growth of rice production, economic inefficiency, as well as yield gaps caused by inefficiently using existing resources and technology in China
"

	uri <- "doi:10.7910/DVN/26631"
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
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "CHINA95(060624).xls"]
	rd <- function(sheet, sfx) {
		x <- carobiner::read.excel(f, sheet=sheet, skip=5, .name_repair="minimal")
		x <- x[, names(x) != "" & !is.na(names(x))]
		names(x) <- sub(paste0(sfx, "$"), "", names(x))
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

	mu <- 1/15
	villages <- c("Fangquan", "Xieleqiao", "Changjiang", "Fuxia")
	crops <- data.frame(code = 1:11,
		label = c("Japonica rice", "Indica rice", "Hybrid rice", "Wheat", "Barley", "Soybean", "Sweet potato", "Oilseeds", "Vegetables", "Cash crop", "Others"),
		crop = c("rice", "rice", "rice", "wheat", "barley", "soybean", "sweetpotato", "oilseed", "vegetable", NA, NA),
		rice_type = c("japonica", "indica", "hybrid", rep(NA, 8)),
		yield_part = c("grain", "grain", "grain", "grain", "grain", "seed", "roots", "seed", NA, NA, NA)
	)
	loss <- data.frame(code = 1:8,
		label = c("drought", "flood", "typhoon", "diseases", "insects", "weeds", "low input use", "others"),
		stress = c("drought", "flood", "wind", "disease", "pests", "weeds", NA, "other")
	)

	seasons <- data.frame(
		sfx = c("L94", "E95", "M95"),
		label = c("late 1994", "early 1995", "middle 1995"),
		year = c("1994", "1995", "1995"),
		agprac = c("AGPRAC1", "AGPRAC2", "AGPRAC3"),
		labA = c("LABOR1", "LABOR3", "LABOR5"),
		labB = c("LABOR2", "LABOR4", "LABOR6"),
		prod = c("PRODL94", "PRODE95", "PRODM95")
	)

	## household x season
	d2 <- lapply(1:3, function(i) {
		s <- seasons[i, ]
		x <- join(join(join(prefix(rd(s$agprac, s$sfx), "a_"), prefix(rd(s$labA, s$sfx), "l1_")), prefix(rd(s$labB, s$sfx), "l2_")), prefix(rd(s$prod, s$sfx), "p_"))
		ha <- num(x$a_AR) * mu
		code <- num(x$a_CR)
		data.frame(
			hhid = paste(x$VLAGE, x$HHNO, sep="_"),
			village = num(x$VLAGE),
			season_label_ = s$label,
			harvest_date = s$year,
			crop_code = code,
			field_size = ha,
			planting_method = c("direct seeding", "transplanting")[match(num(x$l1_CE), 1:2)],
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
			seed_cost = num(x$l1_VS) / ha,
			land_prep_cost = rowSums(cbind(num(x$l1_TRC), num(x$l1_ANC)), na.rm=TRUE) / ha,
			herbicide_cost = num(x$l1_VH) / ha,
			irrigation_cost = num(x$l2_VI) / ha,
			fertilizer_cost = num(x$l2_VF) / ha,
			pesticide_cost = num(x$l2_VP) / ha,
			yield = num(x$p_CR) / ha,
			crop_price = num(x$p_PRIC),
			production_status_ = c("on or above average yield", "below average yield")[match(num(x$p_PSTUS), 1:2)],
			loss_code = num(x$p_RLOS),
			currency = "CNY"
		)
	})
	d2 <- do.call(rbind, d2)
	d2 <- d2[!is.na(d2$crop_code) & !is.na(d2$field_size) & d2$field_size > 0, ]
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
			crop_code = num(r1[[paste0("CR", s$sfx)]]),
			field_size = ha,
			yield = num(r1[[paste0("PR", s$sfx)]]) / ha,
			topography_ = c("lowland flat", "upland flat", "upland sloping", "pond")[match(num(r1$TOPOG), 1:4)],
			geography_ = c("plain", "hill")[match(num(r1$GEOG), 1:2)],
			soil_type_ = c("red soil", "others")[match(num(r1$TSOIL), 1:2)],
			soil_quality = c("good", "average", "poor")[match(num(r1$QSOIL), 1:3)],
			irrigated = c(TRUE, FALSE)[match(num(r1$IRIG), 1:2)]
		)
	})
	d1 <- do.call(rbind, d1)
	d1 <- d1[!is.na(d1$crop_code), ]
	d1$record_type_ <- "parcel"

	d <- carobiner::bindr(d1, d2)
	d <- d[d$crop_code %in% crops$code, ]
	i <- match(d$crop_code, crops$code)
	d$crop <- crops$crop[i]
	d$crop_label_ <- crops$label[i]
	d$rice_type_ <- crops$rice_type[i]
	d$yield_part <- crops$yield_part[i]
	d$variety_type <- ifelse(d$crop_code == 3, "hybrid", NA)
	i <- match(d$loss_code, loss$code)
	d$season_constraint <- loss$label[i]
	d$stress <- loss$stress[i]
	d$crop_code <- d$loss_code <- NULL
	d$location <- villages[d$village]
	d$village <- NULL
	d$country <- "China"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
