# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI survey of agriculture and livelihood in 4 villages of Ayeyarwady division, Myanmar, 2004 round (same villages as the 1996 round, doi:10.7910/DVN/26287).
# Used Parcel04.xls and Cost&Return04.xls (Excel 2.1 / BIFF2 files, read with a small reader below). Codes from "Documentation of 2004 survey.xls".
# Two record types are combined in d (record_type_):
#  "parcel": Parcel04, one row per parcel x season (wet, dry) with a crop; all crops. HAR is yield in baskets/acre (median 50 for rice, not correlated with area).
#  "cost_return": Cost&Return04, one row per household x rice season with inputs and production; rows with SEASON 0 (no rice) dropped.
# Rice production is converted with the standard Myanmar paddy basket of 46 lb = 20.87 kg; 1 acre = 0.4046863 ha.
# Fertilizer is in bags. Bag = 50 kg: the household file HHData04 gives urea in kg/ha (urh), which equals Cost&Return04 urea bags x 50 / area. Urea 46% N, TSP 19.23% P, MOP (KCl) 49.8% K.
# Rice variety names decoded from the Cost&Return "VARIETY" code list; Vrty = MV/TV rice -> variety_type modern/traditional.
# Labour = all family + hired man-days (carrying seedlings, transplanting, seeding, weeding, irrigation, fertilizer, pesticide, harvest, bundling, carrying, threshing, winnowing, drying, storage) per ha; animal and machine days not included.

## ISSUES
# No coordinates. The 4 villages (Natpay, Htonewa, Tazin Ye Kyaw, Tuchaung) are in Ayeyarwady division; township not given. Need georeferencing.
# Cost&Return04 SOWNAREA is 0 for all rows; LANDAREA ("land area owned", acre) is used as the rice area. PRODUCE/LANDAREA has median 50 baskets/acre, the same as the parcel yields, so this seems right.
# Basket weight (20.87 kg paddy) is the standard but not stated in the codebook. Non-rice yields are kept in baskets/acre only (yield_baskets_acre_), because basket weights differ by crop; for some crops (e.g. chilli, onion) the unit is probably not baskets (values up to 15000).
# Parcel04 VAR codes are decoded for rice assuming the Cost&Return VARIETY list; VAR codes of other crops, VARTYPE1/VARTYPE2 and parcel TYPE are undocumented. WHY codes > 24 and crop code 6 are undocumented.
# Parcel04 has no tenure, soil or irrigation variables (unlike 1996).
# PESTCOST is labelled "Cost of pesticide used (acre/Kyats)": unclear if per acre or total, kept as reported (pesticide_cost_kyat_).
# OTHERF ("other fertilizer", bags) has unknown composition: kept as other_fertilizer_bags_ (per ha), not included in N/P/K.
# Planting/harvest month and week are given without year; kept as new variables, no planting_date.
# FYM in carts (cart weight unknown): kept as FYM_carts_ (per ha).
# Cost&Return rows cannot be linked to a specific parcel (no parcel id).
# Unmapped: HHData04 (household aggregates, income, capital), Member04 (members), straw/bran by-product values, land tax, wage rates.

## NEW VARIABLES
# record_type_: "parcel" (Parcel04) or "cost_return" (Cost&Return04)
# crop_label_: original crop label
# yield_baskets_acre_: reported yield in baskets per acre (all crops)
# planting_month_, planting_week_: month (1-12) and week (1-4) of planting, year not given
# harvest_month_, harvest_week_: month (1-12) and week (1-4) of harvest, year not given
# FYM_carts_: farmyard manure applied, carts per ha
# other_fertilizer_bags_: other (unspecified) fertilizer, bags per ha
# fertilizer_times_: number of fertilizer applications
# pesticide_times_: number of pesticide sprays
# pesticide_cost_kyat_: PESTCOST as reported (Kyat; per acre or per parcel is unclear)


carob_script <- function(path) {

"
Survey for assessing changes in agriculture and livelihood in Ayeyarwardy division, Myanmar (2004)

To assess the changes that took place in agriculture and socio-economic conditions in the rural economy of the survey sites.
"

	uri <- "doi:10.7910/DVN/26286"
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

	# Excel 2.1 (BIFF2) worksheet reader: INTEGER, NUMBER, LABEL, BOOLERR and FORMULA cell records
	read_biff2 <- function(f) {
		b <- readBin(f, "raw", file.size(f))
		bi <- as.integer(b)
		n <- length(bi)
		pos <- integer(n %/% 8L)
		ids <- integer(n %/% 8L)
		p <- 1L
		k <- 0L
		while (p + 3L <= n) {
			id <- bi[p] + 256L * bi[p + 1L]
			k <- k + 1L
			pos[k] <- p + 4L
			ids[k] <- id
			if (id == 10L) break
			p <- p + 4L + bi[p + 2L] + 256L * bi[p + 3L]
		}
		cell <- ids[1:k] %in% 2:6
		pos <- pos[1:k][cell]
		ids <- ids[1:k][cell]
		u16 <- function(i) bi[i] + 256L * bi[i + 1L]
		rows <- u16(pos) + 1L
		cols <- u16(pos + 2L) + 1L
		num <- rep(NA_real_, length(pos))
		chr <- rep(NA_character_, length(pos))
		i <- ids == 2L
		num[i] <- u16(pos[i] + 7L)
		i <- which(ids %in% c(3L, 6L))
		if (length(i) > 0) {
			idx <- as.vector(outer(0:7, pos[i] + 7L, "+"))
			num[i] <- readBin(b[idx], "double", n=length(i), size=8, endian="little")
		}
		for (j in which(ids == 4L)) {
			nc <- bi[pos[j] + 7L]
			chr[j] <- if (nc > 0) rawToChar(b[(pos[j] + 8L):(pos[j] + 7L + nc)]) else ""
		}
		nr <- max(rows)
		h <- rows == 1L
		hdr <- paste0("V", 1:max(cols))
		hdr[cols[h]] <- chr[h]
		out <- lapply(seq_along(hdr), function(j) {
			i <- which(cols == j & rows > 1L)
			if (all(is.na(chr[i]))) {
				v <- rep(NA_real_, nr - 1L)
				v[rows[i] - 1L] <- num[i]
			} else {
				v <- rep(NA_character_, nr - 1L)
				v[rows[i] - 1L] <- ifelse(is.na(chr[i]), as.character(num[i]), chr[i])
			}
			v
		})
		names(out) <- hdr
		as.data.frame(out, check.names=FALSE)
	}

	fdoc <- ff[basename(ff) == "Documentation of 2004 survey.xls"]
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
	# bag weight, verified against urh (kg/ha) in HHData04
	bag <- 50

	villages <- codes("Genral_val", "Village")
	rvar <- codes("Cost&Retrn_vaL", "VARIETY")

	## Parcel04
	r1 <- read_biff2(ff[basename(ff) == "Parcel04.xls"])
	crops <- codes("Parcel_val", "CROP1/CROP2")
	crops$crop <- c("rice", "maize", "groundnut", "sesame", "mustard", NA, "cowpea", "mung bean", "black gram", "chili pepper", "onion", "garlic", "potato", "tobacco", "sugarcane", "cassava", "sweetpotato", "coconut", NA, "jute", "banana", "fruit", "betel", NA, "bitter gourd", "vegetable", "sunflower")[match(crops$code, 1:27)]
	why <- codes("Parcel_val", "WHY1/WHY2")
	d1 <- lapply(1:2, function(i) {
		data.frame(
			hhid = r1$HHNO,
			village = r1$Village,
			field_id = r1$PNO,
			season = c("wet", "dry")[i],
			crop_code = r1[[paste0("CROP", i)]],
			variety_code = as.character(r1[[paste0("VAR", i)]]),
			field_size = r1[[paste0("ACRE", i)]] * acre,
			yield_baskets_acre_ = r1[[paste0("HAR", i)]],
			normal = r1[[c("NORMWET", "NORMDRY")[i]]],
			season_constraint = lab(r1[[paste0("WHY", i)]], why)
		)
	})
	d1 <- do.call(rbind, d1)
	d1 <- d1[!is.na(d1$crop_code) & d1$crop_code > 0, ]
	i <- match(d1$crop_code, crops$code)
	d1$crop <- crops$crop[i]
	d1$crop_label_ <- crops$label[i]
	rice <- d1$crop %in% "rice"
	d1$variety <- ifelse(rice, lab(d1$variety_code, rvar), NA)
	d1$yield <- ifelse(rice, d1$yield_baskets_acre_ * basket / acre, NA)
	d1$expected_yield <- ifelse(rice, d1$normal * basket / acre, NA)
	d1$field_id[d1$field_id == "."] <- NA
	d1$crop_code <- d1$normal <- NULL
	d1$record_type_ <- "parcel"

	## Cost&Return04 (rice only)
	r2 <- read_biff2(ff[basename(ff) == "Cost&Return04.xls"])
	r2 <- r2[r2$SEASON > 0, ]
	ha <- r2$LANDAREA * acre
	labour <- c("CARRFAM", "CARRHIRE", "TRANFAM", "TRANHIRE", "DIRFAM", "DIRHIRE", "BROFAM", "BROHIRE", "WEEDFAM", "WEEHIRE", "IRRFAM", "IRRHIRE", "FERTFAM", "FERTHIRE", "PESTFAM", "PESTHIRE", "HARFAM", "HARHIRE", "BUNDFAM", "BUNDHIRE", "CARFAM", "CARHIRE", "THRESFAM", "THRESHIRE", "WINFAM", "WINHIRE", "DRYFAM", "DRYHIRE", "STORFAM", "STORHIRE")
	d2 <- data.frame(
		hhid = r2$HHNO,
		village = r2$Village,
		season = c("wet", "dry")[match(r2$SEASON, 1:2)],
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
		other_fertilizer_bags_ = r2$OTHERF / ha,
		FYM_carts_ = r2$FYM / ha,
		pesticide_times_ = r2$PESTNO,
		pesticide_cost_kyat_ = r2$PESTCOST,
		seed_rate = (r2$SEEDOWN + r2$SEEDBUY) * basket / ha,
		labour = rowSums(r2[, labour]) / ha,
		fertilizer_cost = (r2$UREA * r2$UPRICE + r2$TSP * r2$TSPPRICE + r2$MOP * r2$MOPPRICE + r2$OTHERF * r2$OTFPRICE) / ha,
		OM_cost = r2$FYM * r2$FYMPRICE / ha,
		irrigation_cost = (r2$PUMPRENT + r2$DIESEL) / ha,
		yield = r2$PRODUCE * basket / ha,
		crop_price = r2$PRICE / basket,
		currency = "MMK"
	)
	ft <- cbind(ifelse(r2$UREA > 0, "urea", NA), ifelse(r2$TSP > 0, "TSP", NA), ifelse(r2$MOP > 0, "KCl", NA), ifelse(r2$OTHERF > 0, "unknown", NA))
	d2$fertilizer_type <- apply(ft, 1, function(x) paste(x[!is.na(x)], collapse="; "))
	d2$fertilizer_type[d2$fertilizer_type == ""] <- "none"
	d2$OM_used <- r2$FYM > 0
	d2$OM_type <- ifelse(r2$FYM > 0, "farmyard manure", NA)
	d2$crop_price[d2$crop_price == 0] <- NA
	d2$record_type_ <- "cost_return"

	d <- carobiner::bindr(d1, d2)
	d$yield_part <- NA
	d$yield_part[d$crop %in% c("rice", "maize")] <- "grain"
	d$yield_part[d$crop %in% c("sesame", "mustard", "sunflower", "black gram", "mung bean", "cowpea", "groundnut")] <- "seed"
	d$location <- lab(d$village, villages)
	d$village <- NULL
	d$adm1 <- "Ayeyarwady"
	d$country <- "Myanmar"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
