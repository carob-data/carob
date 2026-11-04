# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI survey of agriculture and livelihood in 4 villages of Ayeyarwady division, Myanmar, 1996 round.
# Used Parcel96.xls and Cost&Return96.xls (ALL_DATA96.xls and "Working file.xls" contain the same tables with title rows). Codes from "Documentation of 1996 survey.xls".
# Two record types are combined in d (record_type_):
#  "parcel": Parcel96, one row per parcel x season (monsoon season 1, monsoon season 2, dry season) with a crop; all crops. Area in acres -> ha.
# HAR/NORM are labelled "yield baskets/acre" in the codebook but are total baskets per parcel: HAR/ACRE has median 50 baskets/acre for rice, matching Cost&Return96 (45), and HAR correlates 0.8 with ACRE. Yield = HAR / ACRE.
#  "cost_return": Cost&Return96, one row per household x rice season with inputs and production. Area in acres -> ha.
# Production is reported in baskets. For rice it is converted with the standard Myanmar paddy basket of 46 lb = 20.87 kg; 1 acre = 0.4046863 ha.
# Fertilizer is in bags. Bag = 50 kg: the household file HHData96 gives urea in kg/ha (urh), which equals Cost&Return96 urea bags x 50 / area for every household. Urea 46% N, TSP 19.23% P, MOP (KCl) 49.8% K.
# Season: monsoon 1 and 2 -> "wet", summer/dry -> "dry" (original label in season_label_).
# Rice variety names decoded from the Cost&Return "VARIETY" code list; Vrty = MV/TV rice -> variety_type modern/traditional.
# Labour = all family + hired man-days (carrying seedlings, transplanting, seeding, weeding, irrigation, fertilizer, pesticide, harvest, bundling, carrying, threshing, winnowing, drying, storage) per ha; animal and machine days not included.

## ISSUES
# No coordinates. The 4 villages (Natpay, Htonewa, Tazin Ye Kyaw, Tuchaung) are in Ayeyarwady division; township not given. Need georeferencing.
# Basket weight (20.87 kg paddy) is the standard but not stated in the codebook; non-rice production (pulses, oilseeds, vegetables, ...) is only kept in baskets (production_baskets_) because basket weights differ by crop.
# Parcel96 VAR codes are decoded for rice assuming the Cost&Return VARIETY list; VAR codes of other crops are not documented (kept in variety_code).
# Planting/harvest month and week are given without year (crop year 1995/96 or 1996/97?); kept as new variables, no planting_date.
# METHOD code 4 (7 rows) and parcel TYPE are undocumented. HIGH/LOW (expected production under best/worst conditions, "basket") not mapped: unclear if per acre.
# Cost&Return rows cannot be linked to a specific parcel (no parcel id).
# FYM in carts (cart weight unknown): kept as FYM_carts_ (per ha), not converted to OM_amount.
# Unmapped: HHData96 (household aggregates, income, capital), Member96 (members), straw/bran by-product values, land tax, wage rates.
# A few rice yields are very high (up to 17 t/ha on small parcels): probably recording errors, not removed.
# soil type "others" (STY=4, 1 parcel) set to NA. Crop "Thatch" (5 rows) has no terminag crop name (crop NA).
# write_files: no coordinates; NA yield for non-rice parcel rows; duplicate records (households reporting identical parcels); production_baskets_ up to 22000 for some dry-season non-rice crops (unit doubtful).

## NEW VARIABLES
# record_type_: "parcel" (Parcel96) or "cost_return" (Cost&Return96)
# season_label_: original season (monsoon 1, monsoon 2, dry/summer)
# crop_label_: original crop group label (e.g. "Pulses", "Oilseeds")
# production_baskets_: reported production of the parcel in baskets (all crops)
# flood_depth_: depth of flooding of the parcel at the height of the monsoon (class)
# planting_month_, planting_week_: month (1-12) and week (1-4) of planting, year not given
# harvest_month_, harvest_week_: month (1-12) and week (1-4) of harvest, year not given
# FYM_carts_: farmyard manure applied, carts per ha
# fertilizer_times_: number of fertilizer applications
# pesticide_times_: number of pesticide sprays


carob_script <- function(path) {

"
Survey for assessing changes in agriculture and livelihood in Ayeyarwardy division, Myanmar  (1996)

To assess the impact of adoption of new rice-based technologies on income and employment of rural households under varying agro-ecosystems.
"

	uri <- "doi:10.7910/DVN/26287"
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
		carob_completion = 45,
		carob_effort = 0.8
	)

	fdoc <- ff[basename(ff) == "Documentation of 1996 survey.xls"]
	# value labels from the codebook: variable name (filled down), code, label (last column)
	codes <- function(sheet, var) {
		x <- carobiner::read.excel(fdoc, sheet=sheet, col_names=FALSE)
		nm <- x[[1]]
		nm <- nm[!is.na(nm)][cumsum(!is.na(nm))]
		k <- nm %in% var & !is.na(x[[ncol(x)]])
		data.frame(code=as.numeric(x[[2]][k]), label=trimws(x[[ncol(x)]][k]))
	}
	lab <- function(x, cds) {
		cds$label[match(x, cds$code)]
	}

	acre <- 0.4046863
	# standard Myanmar paddy basket: 46 lb
	basket <- 20.87
	# bag weight, verified against urh (kg/ha) in HHData96
	bag <- 50

	villages <- codes("Genral_val", "Village")
	rvar <- codes("CostRet_val", "VARIETY")

	## Parcel96
	r1 <- carobiner::read.excel(ff[basename(ff) == "Parcel96.xls"])
	crops <- data.frame(code=1:8,
		label = c("Rice", "Maize", "Pulses", "Oilseeds", "Vegetables", "Spices", "Jute", "Thatch"),
		crop = c("rice", "maize", "pulse", "oilseed", "vegetable", "spices", "jute", NA)
	)
	seasons <- data.frame(i=1:3, season=c("wet", "wet", "dry"), label=c("monsoon 1", "monsoon 2", "dry"))
	d1 <- lapply(1:3, function(i) {
		data.frame(
			hhid = r1$HHNO,
			village = r1$Village,
			field_id = ifelse(i == 2, r1$PID2, r1$PID),
			season = seasons$season[i],
			season_label_ = seasons$label[i],
			crop_code = r1[[paste0("CROP", i)]],
			variety_code = as.character(r1[[paste0("VAR", i)]]),
			field_size = r1[[paste0("ACRE", i)]] * acre,
			production_baskets_ = r1[[paste0("HAR", i)]],
			acres = r1[[paste0("ACRE", i)]],
			normal = r1[[ifelse(i == 1, "NORMI", paste0("NORM", i))]],
			season_constraint = lab(r1[[paste0("WHY", i)]], codes("Parcel_val", c("WHY1", "WHY2", "WHY3"))),
			land_tenure = lab(r1$TEN, codes("Parcel_val", "TEN")),
			field_distance = r1$DPH * 1.609344,
			flood_depth_ = lab(r1$DEFL, codes("Parcel_val", "DEFL")),
			soil_texture = c("sandy loam", "clay loam", "clay")[match(r1$STY, 1:3)],
			soil_quality = lab(r1$SQL, codes("Parcel_val", "SQL")),
			irrigation_source = lab(r1$IRR, codes("Parcel_val", "IRR"))
		)
	})
	d1 <- do.call(rbind, d1)
	d1 <- d1[d1$crop_code > 0, ]
	i <- match(d1$crop_code, crops$code)
	d1$crop <- crops$crop[i]
	d1$crop_label_ <- crops$label[i]
	rice <- d1$crop %in% "rice"
	d1$variety <- ifelse(rice, lab(d1$variety_code, rvar), NA)
	d1$yield <- ifelse(rice, d1$yield_baskets_acre_ * basket / acre, NA)
	d1$expected_yield <- ifelse(rice, d1$normal * basket / acre, NA)
	d1$irrigated <- ifelse(d1$irrigation_source == "Not irrigated - Rainfed", FALSE, TRUE)
	d1$field_id[d1$field_id == "."] <- NA
	d1$crop_code <- d1$normal <- d1$acres <- NULL
	d1$record_type_ <- "parcel"

	## Cost&Return96 (rice only)
	r2 <- carobiner::read.excel(ff[basename(ff) == "Cost&Return96.xls"])
	ha <- r2$SOWNAREA * acre
	labour <- c("CARRFAM", "CARRHIRE", "TRANFAM", "TRANHIRE", "DIRFAM", "DIRHIRE", "BROFAM", "BROHIRE", "WEEDFAM", "WEEHIRE", "IRRFAM", "IRRHIRE", "FERTFAM", "FERTHIRE", "PESTFAM", "PESTHIRE", "HARFAM", "HARHIRE", "BUNDFAM", "BUNDHIRE", "CARFAM", "CARHIRE", "THRESFAM", "THRESHIRE", "WINFAM", "WINHIRE", "DRYFAM", "DRYHIRE", "STORFAM", "STORHIRE")
	d2 <- data.frame(
		hhid = r2$HHNO,
		village = r2$Village,
		season = c("wet", "wet", "dry")[match(r2$SEASON, 1:3)],
		season_label_ = c("monsoon 1", "monsoon 2", "dry")[match(r2$SEASON, 1:3)],
		crop = "rice",
		crop_label_ = "Rice",
		variety = lab(r2$VARIETY, rvar),
		variety_type = c("modern", "traditional")[match(r2$Vrty, 1:2)],
		field_size = ha,
		planting_month_ = r2$PLANTMO,
		planting_week_ = r2$PLANTWK,
		harvest_month_ = r2$HARVMO,
		harvest_week_ = r2$HARVWK,
		planting_method = c("transplanting", "direct seeding", "transplanting;direct seeding")[match(r2$METHOD, 1:3)],
		weeding_times = as.integer(r2$WEEDNO),
		irrigation_number = as.integer(r2$IRRNO),
		fertilizer_times_ = r2$FERTNO,
		N_fertilizer = r2$UREA * bag * 0.46 / ha,
		P_fertilizer = r2$TSP * bag * 0.1923 / ha,
		K_fertilizer = r2$MOP * bag * 0.498 / ha,
		gypsum = r2$GYPSUM * bag / ha,
		FYM_carts_ = r2$FYM / ha,
		pesticide_times_ = r2$PESTNO,
		seed_rate = (r2$SEEDOWN + r2$SEEDBUY) * basket / ha,
		labour = rowSums(r2[, labour]) / ha,
		fertilizer_cost = (r2$UREA * r2$UPRICE + r2$TSP * r2$TSPPRICE + r2$MOP * r2$MOPPRICE + r2$GYPSUM * r2$GYPPRICE) / ha,
		OM_cost = r2$FYM * r2$FYMPRICE / ha,
		pesticide_cost = r2$PESTCOST / ha,
		irrigation_cost = (r2$PUMPRENT + r2$DIESEL) / ha,
		yield = r2$PRODUCE * basket / ha,
		crop_price = r2$PRICE / basket,
		currency = "MMK"
	)
	ft <- cbind(ifelse(r2$UREA > 0, "urea", NA), ifelse(r2$TSP > 0, "TSP", NA), ifelse(r2$MOP > 0, "KCl", NA), ifelse(r2$GYPSUM > 0, "gypsum", NA))
	d2$fertilizer_type <- apply(ft, 1, function(x) paste(x[!is.na(x)], collapse="; "))
	d2$fertilizer_type[d2$fertilizer_type == ""] <- "none"
	d2$OM_used <- r2$FYM > 0
	d2$OM_type <- ifelse(r2$FYM > 0, "farmyard manure", NA)
	d2$crop_price[d2$crop_price == 0] <- NA
	d2$record_type_ <- "cost_return"

	d <- carobiner::bindr(d1, d2)
	d$yield_part <- ifelse(d$crop %in% c("rice", "maize"), "grain", NA)
	d$location <- lab(d$village, villages)
	d$village <- NULL
	d$adm1 <- "Ayeyarwady"
	d$country <- "Myanmar"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
