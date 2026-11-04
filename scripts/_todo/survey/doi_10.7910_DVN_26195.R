# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey (IRRI Social Sciences Division) of 185 rice farmers in 5 municipalities of Laguna, Philippines
# (Famy, Mabitac, Majayjay, Siniloan, Santa Maria), interviewed January-March 2013, on the rice varieties
# grown (including Green Super Rice, GSR, varieties) in the dry (DS) and wet (WS) season.
# One record per farmer x season x parcel x plot x variety from tbl04aricevariety_x (area in ha, production in
# local units (cavan) with the farmer-reported kg per unit, planting/harvest month, price, maturity).
# Location (province, district, village) from tbl00surveyinfo.
# Fertilizer inputs (tbl06matinputs_x, kg per farmer x variety x season) are divided by the total area of that
# farmer x variety x season in tbl04aricevariety_x to get N, P, K in kg/ha.
# The same tables are also in "GSR_Phase2_Phils data sets_Orlee Velarde.xlsx" (plus sheet gsr_users, a subset
# of tbl04aricevariety_x for GSR users); the separate tbl*.xlsx files are used.

## ISSUES
# The survey year of the seasons is not explicit (data collection "2012", interviews Jan-Mar 2013).
#   Planting and harvest months are kept as planting_month_ and harvest_month_; planting_date and
#   harvest_date are not set. Confirm the season years with IRRI before building dates.
# yield = prodloc * kgperunit / area (kg/ha of fresh paddy, moisture unknown). Records with unit "999" or
#   kgperunit 0 or area 0 have NA yield; there are also 0 production records (kept as 0).
# Fertilizer: product grades taken as N-P2O5-K2O (Philippine labelling) and converted to elemental P and K.
#   N, P, K are NA for farmer x variety x season with a non-kg unit or a product with unknown nutrient content
#   ("crop giant", "foliar", "yara", "plant care"). 16-20-0 (ammonium phosphate sulfate) is not in the
#   fertilizer_type vocabulary and is not listed in fertilizer_type (its nutrients are included).
# Variety names are as written by enumerators (e.g. "RC-18", "rc-222", "222", "112A", "112a"); not harmonized.
# Pesticides, herbicides, labour, power use, crop disposal, credit, household and income tables are not mapped.
# GPS columns in tbl00surveyinfo are empty; no coordinates.
# Very small reported areas give extreme per-ha values (yield up to 25000 kg/ha, N up to 650 kg/ha, e.g.
#   farmer 3436 112A WS on 0.0173 ha); left as computed. Fertilizer for 191 of 687 records is NA (no match or
#   non-kg/unknown product).
# Farmer 3428 plot A1 variety 134 has an exact duplicate record (kept).
# write_files remaining: missing coordinates and yield_moisture; NA yield; out of bounds N_fertilizer;
#   high yield; duplicate record; unknown (new) variable.

## NEW VARIABLES
# land_type_: land type of the plot as reported (Lowland, Medium, Upland)
# planting_month_, harvest_month_: reported month (year not given)


carob_script <- function(path) {

"
Green Super Rice for the Resource Poor of Asia and Africa (GSR) - Phase 2

1. Assessing the performance of GSR varieties;   2. Initial tracking of seed distribution among farmers of their first harvest;   3. Identify farmers' response and reaction to the performance of GSR varieties;   4. Similar approach will be implemented in Bangladesh as soon as the seeds are available and  5. Monitoring of GSR varieties performance across seasons.
"

	uri <- "doi:10.7910/DVN/26195"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "Green Super Rice (GSR)",
		design = "household survey; 185 farmers in Laguna",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		notes = "Yield is farmer-reported production (fresh paddy) per reported area",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 50,
		carob_effort = 0.6
	)

	f0 <- ff[basename(ff) == "tbl00surveyinfo.xlsx"]
	f4 <- ff[basename(ff) == "tbl04aricevariety_x.xlsx"]
	f6 <- ff[basename(ff) == "tbl06matinputs_x.xlsx"]

	s <- carobiner::read.excel(f0)
	r <- carobiner::read.excel(f4)
	m <- carobiner::read.excel(f6)

	area <- as.numeric(r$area)
	area[area == 0] <- NA
	kgu <- as.numeric(r$kgperunit)
	kgu[r$unit == "kg"] <- 1
	kgu[kgu == 0 | !(r$unit %in% c("cav", "kg"))] <- NA

	d <- data.frame(
		hhid = as.character(r$farmerid),
		field_id = paste0(r$parcel, r$plot),
		season = ifelse(r$season == "DS", "dry", "wet"),
		land_type_ = r$landtype,
		variety = trimws(r$ricevar),
		variety_type = ifelse(r$mvtv == "MV", "improved", "landrace"),
		planting_method = ifelse(r$cem == "TPR", "transplanting", "direct seeding"),
		field_size = area,
		yield = as.numeric(r$prodloc) * kgu / area,
		planting_month_ = r$plantmonth,
		harvest_month_ = r$harvestmonth,
		maturity_days = as.numeric(r$maturity),
		crop_price = as.numeric(r$prodprice),
		currency = "PHP"
	)
	i <- match(d$hhid, as.character(s$farmerid))
	d$country <- "Philippines"
	d$adm1 <- "Laguna"
	d$adm2 <- s$district[i]
	d$location <- s$village[i]

	## fertilizer, kg per farmer x variety x season, divided by the area of that combination
	f <- m[m$matinputs == "Fertilizer", ]
	f$specify <- tolower(trimws(f$specify))
	npk <- data.frame(
		specify = c("urea", "46-0-0", "14-14-14", "ammonium sulfate", "21-0-0", "16-20-0"),
		ftype = c("urea", "urea", "NPK", "DAS", "DAS", NA),
		N = c(46, 46, 14, 21, 21, 16), P = c(0, 0, 14, 0, 0, 20) * 0.4364, K = c(0, 0, 14, 0, 0, 0) * 0.8301,
		S = c(0, 0, 0, 24, 24, 0)
	)
	f <- merge(f, npk, by="specify", all.x=TRUE)
	q <- as.numeric(f$quantity)
	q[f$unit != "kg"] <- NA
	f$key <- paste(f$farmerid, trimws(f$ricevar), f$season)
	agg <- do.call(rbind, lapply(split(f, f$key), function(x) {
		qx <- as.numeric(x$quantity)
		qx[x$unit != "kg" | is.na(x$N)] <- NA
		ft <- unique(na.omit(x$ftype))
		data.frame(key = x$key[1],
			N = sum(qx * x$N / 100), P = sum(qx * x$P / 100), K = sum(qx * x$K / 100), S = sum(qx * x$S / 100),
			fertilizer_type = if (length(ft) > 0) paste(sort(ft), collapse="; ") else NA)
	}))
	d$key <- paste(d$hhid, d$variety, r$season)
	a <- tapply(d$field_size, d$key, sum, na.rm=TRUE)
	agg$area <- as.vector(a[agg$key])
	agg$area[agg$area == 0] <- NA
	j <- match(d$key, agg$key)
	d$N_fertilizer <- agg$N[j] / agg$area[j]
	d$P_fertilizer <- agg$P[j] / agg$area[j]
	d$K_fertilizer <- agg$K[j] / agg$area[j]
	d$S_fertilizer <- agg$S[j] / agg$area[j]
	d$fertilizer_type <- agg$fertilizer_type[j]
	d$key <- NULL

	d$trial_id <- paste0(d$hhid, "_", d$field_id, "_", r$season)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	d$geo_from_source <- FALSE
	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$irrigated <- as.logical(NA)

	carobiner::write_files(path, meta, d)
}
