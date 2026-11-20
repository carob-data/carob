# R script for "carob"
# license: GPL (>=3)

## NOTES
# ILSSI Ghana baseline household survey (IFPRI, 2015): 810 households in 14 villages of Garu-Tempane, Kassena-Nankana East and Nabdam (Upper East) and Savelugu (Northern) districts. Crop data refer to the 2014 rainy season and the following dry season.
# One record per household x plot x season x crop, from 019_C_cropproduction_v2.dta. Rows without a crop were dropped. Plot numbers are unique within a household (they run across parcels).
# field_size is the plot size from the land module (018). Yield and all per-ha inputs, costs and labour use the area planted to the crop (019), which can be smaller than the plot.
# Inputs, seed and costs (020) and irrigation (021) are recorded per plot x season and were joined to the crop records. Soil type and land tenure (018) are per plot.
# Family labour (026) is person-days per plot x season x crop. Hired labour (027) is only recorded per household and season: farm_labour_hired is the household total over both seasons.
# Crop sales (024) are per household x season x crop. crop_price is the reported price per sale unit converted to currency/kg; crop_amount_sold is only given when the household grew the crop on one plot in that season.
# Most harvests (91%) are in kg. Tins, litres, bushels and bags without a stated weight were converted with the proposed factors in "conversion to kg" (see ISSUES).
# "Ammonia" (020, 1st fertilizer) is taken to be sulphate of ammonia (DAS, 21% N), as the name is used in northern Ghana. NPK grade is not reported, so N, P and K are NA when NPK was used. Nutrient contents used: urea 46% N; DAP 18% N and 20.1% P; DAS 21% N.
# "cowdung" written as a fertilizer type was recorded as OM_used.
# Fertilizer and manure amounts and costs are per plot in the source and were divided by the area planted.
# Irrigation: frequency per day, week or month was converted to the interval between irrigations in days.
# Crop names: "frafra potato" is Plectranthus rotundifolius; "late millet" is pearl millet, "guinea corn" is sorghum, "aleefu" is amaranth, "ayoyo" is jute mallow, "garden eggs" is african eggplant, "mellon seed" is melon (egusi).

# Village coordinates are not in the source; see ISSUES.

## ISSUES
# 1. Coordinates: Bihinaayili, Zanlerigu and Kugri were found with carobiner::geocode (Nominatim). The other villages were not found and have the coordinates of the district capital (Garu, Navrongo). Nambako has no district. These need to be checked.
# 2. Harvests and sales in tins, litres, bushels (36.37 L) and "bags" without a weight were converted with proposed factors by crop (bulk density and bag weight in the script). Basins, bowls, pans, crates, baskets, bunches and pieces were not converted; yield is NA for these.
# 3. Cotton yield is seed cotton; yield_part is set to "lint" for lack of a better term.
# 4. Some values are implausible and were not changed: very high yields for small areas, and fertilizer amounts above 1000 kg/ha.
# 5. Irrigation durations above 24 hours were set to NA.

## NEW VARIABLES
# Provisional water variables, proposed in controvoc/terminag PR #15 (without the trailing underscore):
# irrigation_interval_: days between irrigation events
# irrigation_duration_: average duration of one irrigation event (hours)
# waterlogging_: Was the plot waterlogged? (021)
# well_type_: type of well when the source is groundwater

## NOT PROCESSED
# Soil fertility, slope, erosion, distance, tractor use, land value and decision maker (018). Groundwater depth (021) is only a class (<7, 8-50, >50 m).
# Pumps (022, 023). Crop residue, feed, seed, gift and own-consumption amounts (024). Hired labour by activity, exchange labour and wages (027).
# Household-level modules that do not fit the plot x crop records: roster (017), livestock (029, 030), soil conservation (028), loans, income, social programmes and expenditure (031-037), shocks (038), food consumption, diet diversity and food security (039-042), water, sanitation and health (043-050), networks (051, 052), anthropometry (053, 054), WEAI (055-064), and the community survey (003-014).

carob_script <- function(path) {

"
Ghana Feed the Future Innovation Lab for Small-Scale Irrigation (ILSSI) Baseline Survey, 2015

The Feed the Future Innovation Lab on Small-Scale Irrigation (FTF-ILSSI) is a cooperative agreement funded by USAID under the Feed the Future program to undertake research aimed at increasing food production, improving nutrition, accelerating economic development, and contributing to the protection of the environment. The project pursues these objectives by identifying, testing, and demonstrating technological options in small-scale irrigation and irrigated fodder, supported by a continual dialogue approach with stakeholders and capacity development toward sustained use of research approaches and evidence.

Collaborators on this project include Texas A&M University, the International Water Management Institute (IWMI), the International Food Policy Research Institute (IFPRI), the International Livestock Research Institute (ILRI), North Carolina A&T State University (NCAT), and Texas A&M AgriLife Research (TAMUS). As part of this project, IFPRI is undertaking a study of irrigating and non-irrigating households in Ethiopia, Tanzania, and Ghana to investigate the connections between irrigation, gender, nutrition, and health. The survey explores these linkages through an in-depth household questionnaire with questions on agricultural production, nutrition and health, a WEAI module, and a community questionnaire. This work is part of the CGIAR Research Program on Water, Land, and Ecosystems (WLE).
"

	uri <- "doi:10.7910/DVN/7E7LG7"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		design = "Household survey of irrigating and non-irrigating households in 14 villages of 4 districts. Units of analysis: household, plot, crop",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Zolo Kiala",
		carob_date = "2026-11-11",
		carob_completion = 70,
		carob_effort = 4
	)

	rd <- function(f) haven::read_dta(ff[basename(ff) == f])
	r2 <- rd("018_B_land_v2.dta")
	r3 <- rd("019_C_cropproduction_v2.dta")
	r4 <- rd("020_D_inputs_v2.dta")
	r5 <- rd("021_E_irrig_v2.dta")
	r7 <- rd("024_G_cropsales_v2.dta")
	r8 <- rd("026_H_familylabor_v2.dta")
	r9 <- rd("027_I_hiredlabor_v2.dta")

	# Stata value labels to text
	txt <- function(x) {
		x <- haven::as_factor(x)
		trimws(as.character(x))
	}
	low <- function(x) {
		x <- as.character(x)
		x <- gsub("  +", " ", tolower(trimws(x)))
		x
	}
	num <- function(x) as.numeric(x)
	keys <- c("hhid", "plot", "ssn")

### crop names
	crop_raw <- function(x, other) {
		x <- low(x)
		other <- low(other)
		i <- which(x == "other crop (specify)")
		x[i] <- other[i]
		x
	}
	crops <- c(
		"millet (pearl millet)" = "pearl millet", "late millet" = "pearl millet", "latemillet" = "pearl millet",
		"millet" = "pearl millet", "guinea corn" = "sorghum", "guinecorn" = "sorghum",
		"common bean/yellow bean" = "common bean", "beans" = "common bean",
		"white beans" = "common bean", "soyabean" = "soybean", "soya bean" = "soybean", "soya beans" = "soybean",
		"onions" = "onion", "potatoes" = "potato", "bambara nuts" = "bambara groundnut",
		"cow pea (ater)" = "cowpea", "chick pea" = "chickpea", "peas" = "pea", "lentils" = "lentil",
		"red pepper" = "chili pepper", "green pepper" = "chili pepper", "pepper" = "chili pepper",
		"black pepper" = "pepper", "amaranthus" = "amaranth", "alefu" = "amaranth", "aleefu" = "amaranth",
		"ayoyo" = "jute mallow", "garden eggs" = "african eggplant", "mellon seed" = "melon",
		"sweet potato (orange)" = "sweetpotato", "sweet potato (white)" = "sweetpotato", "yams" = "yam",
		"other green leafy vegetables" = "vegetable"
	)
	map_crop <- function(x) {
		i <- which(x %in% names(crops))
		x[i] <- crops[x[i]]
		unname(x)
	}

### conversion to kg (proposed factors, see ISSUES 2)
	# bulk density (kg/L) of the harvested product, for tins, litres and bushels
	dens <- c(maize = 0.72, rice = 0.58, sorghum = 0.72, "pearl millet" = 0.75,
		sesame = 0.6, "common bean" = 0.78, cowpea = 0.78, soybean = 0.72, "bambara groundnut" = 0.7,
		groundnut = 0.35, tomato = 0.55, onion = 0.6)
	# kg in a "bag" when the bag weight is not given
	bag <- c(maize = 100, rice = 80, sorghum = 100, "pearl millet" = 100, soybean = 100,
		cowpea = 100, "bambara groundnut" = 80, groundnut = 40, sesame = 80, onion = 50)
	litres <- c("20 L tins" = 20, "10 L tins" = 10, "5 L tins" = 5, "2 L tins" = 2,
		"Liters" = 1, "ml" = 0.001, "Bushel" = 36.37)

	to_kg <- function(q, crop, unit, other) {
		other <- low(other)
		cd <- unname(dens[crop])
		f <- rep(NA_real_, length(q))
		f[which(unit == "Kilograms")] <- 1
		f[which(unit == "Grams")] <- 0.001
		i <- which(unit %in% names(litres))
		f[i] <- unname(litres[unit[i]]) * cd[i]
		o <- unit %in% c("Other (Specify)", "Other")
		i <- which(o & other %in% c("bag", "bags"))
		f[i] <- unname(bag[crop[i]])
		out <- q * f
		# nothing harvested or sold, whatever the unit
		out[which(q == 0)] <- 0
		out
	}

### crop records (one row per household x plot x season x crop)
	d <- data.frame(
		hhid = as.character(r3$hhid),
		plot = as.integer(r3$plotc),
		ssn = txt(r3$season),
		ccode = as.integer(r3$c3),
		crop = crop_raw(txt(r3$c3), r3$c3_o),
		area = num(r3$c4),
		area_unit = txt(r3$c4_unit),
		harvest = num(r3$c5),
		hunit = txt(r3$c5_unit),
		hunit_other = r3$c5_unit_o
	)
	d$crop <- map_crop(d$crop)

	# no crop grown, or a crop that cannot be identified
	nocrop <- c(".", "", "fallow", "left fallow", "not fallow", "left g", "did not grow", "nothing", "bra", "rented out")
	d <- d[!(is.na(d$crop) | d$crop %in% nocrop | is.na(d$plot)), ]

### area planted and yield
	d$crop_area <- ifelse(d$area_unit == "Acre", d$area * 0.404686, NA)
	i <- which(d$area_unit == "Hectare")
	d$crop_area[i] <- d$area[i]
	i <- which(d$area_unit == "Meters Squared")
	d$crop_area[i] <- d$area[i] / 10000
	d$crop_area[which(d$crop_area == 0)] <- NA

	d$harvest_kg <- to_kg(d$harvest, d$crop, d$hunit, d$hunit_other)
	d$yield <- d$harvest_kg / d$crop_area

	d$season <- ifelse(d$ssn == "Rainy", "wet", ifelse(d$ssn == "Dry", "dry", NA))
	d$planting_date <- "2014"

### plot size, soil type and tenure (per plot)
	pl <- data.frame(
		hhid = as.character(r2$hhid),
		plot = as.integer(r2$plotb),
		size = num(r2$b3),
		unit = txt(r2$b3_unit),
		soil_type = low(txt(r2$b7)),
		land_tenure = low(txt(r2$b9))
	)
	pl$field_size <- ifelse(pl$unit == "Acres", pl$size * 0.404686, NA)
	i <- which(pl$unit == "Hectares")
	pl$field_size[i] <- pl$size[i]
	i <- which(pl$unit == "Squared Meters")
	pl$field_size[i] <- pl$size[i] / 10000
	pl$field_size[which(pl$field_size == 0)] <- NA
	tenure <- c("inherited/parents? gift" = "inherited", "rented-in" = "rented in",
		"sharecropped in" = "sharecropped in", "borrowed free" = "borrowed")
	i <- which(pl$land_tenure %in% names(tenure))
	pl$land_tenure[i] <- tenure[pl$land_tenure[i]]
	pl <- pl[!is.na(pl$plot) & !duplicated(pl[, c("hhid", "plot")]), c("hhid", "plot", "field_size", "soil_type", "land_tenure")]
	d <- merge(d, pl, by = c("hhid", "plot"), all.x = TRUE)

### inputs, seed and costs (per plot x season)
	p <- data.frame(
		hhid = as.character(r4$hhid),
		plot = as.integer(r4$plot_number),
		ssn = txt(r4$season),
		f1 = txt(r4$d81a),
		f1o = low(r4$dd81a_o),
		q1 = num(r4$d81b),
		u1 = txt(r4$d81c_unit),
		u1o = low(r4$dd81c_unit_o),
		c1 = num(r4$d81d),
		f2 = txt(r4$d82a),
		f2o = low(r4$dd82a_o),
		q2 = num(r4$d82b),
		u2 = txt(r4$d82c_unit),
		u2o = low(r4$dd82c_unit_o),
		c2 = num(r4$d82d),
		manure = txt(r4$dmanure),
		mq = num(r4$dmanureb),
		mu = txt(r4$ddmanureb_unit),
		pest_cost = num(r4$d6)
	)

	manures <- c("cowdung", "cowdongh")
	ftype <- function(x, other) {
		other[is.na(other)] <- ""
		out <- rep(NA_character_, length(x))
		out[which(x == "Urea")] <- "urea"
		out[which(x == "DAP")] <- "DAP"
		out[which(x == "NPK")] <- "NPK"
		out[which(x == "Ammonia")] <- "DAS"
		i <- which(x == "Other (Specify)")
		out[i] <- other[i]
		out[which(x == "Other (Specify)" & other == "yara")] <- "Yara"
		out[which(x == "Other (Specify)" & (other == "" | grepl("know|fallow|for all", other)))] <- "unknown"
		out[which(x == "Other (Specify)" & grepl("^nil$|^no other", other))] <- "none"
		out[which(x == "Other (Specify)" & other %in% manures)] <- "none"
		out[grep("^Did not use", x)] <- "none"
		out
	}
	fkg <- function(q, unit, other) {
		kg <- ifelse(unit == "Kilograms", q, NA)
		i <- which(unit == "Grams")
		kg[i] <- q[i] / 1000
		kg
	}
	# nutrient (kg) in a fertilizer; NA if the type or its composition is unknown
	nut <- function(type, kg, content) {
		out <- kg * unname(content[type]) / 100
		out[which(type == "none")] <- 0
		out
	}

	t1 <- ftype(p$f1, p$f1o)
	t2 <- ftype(p$f2, p$f2o)
	# the second fertilizer was only asked if a first one was used
	t2[is.na(t2)] <- "none"
	k1 <- fkg(p$q1, p$u1, p$u1o)
	k2 <- fkg(p$q2, p$u2, p$u2o)

	Nc <- c(urea = 46, DAP = 18, DAS = 21)
	# DAP has 46% P2O5; P = P2O5 / 2.29
	Pc <- c(urea = 0, DAP = 20.1, DAS = 0)
	Kc <- c(urea = 0, DAP = 0, DAS = 0)

	p$fertilizer_used <- t1 != "none" | t2 != "none"
	p$fertilizer_type <- ifelse(t2 == "none" | t2 == t1, t1, paste(t1, t2, sep = ";"))
	# a few records have no first fertilizer but do have a second one
	i <- which(t1 == "none" & t2 != "none")
	p$fertilizer_type[i] <- t2[i]
	p$fert_kg <- ifelse(t1 == "none", 0, k1) + ifelse(t2 == "none", 0, k2)
	p$N <- nut(t1, k1, Nc) + nut(t2, k2, Nc)
	p$P <- nut(t1, k1, Pc) + nut(t2, k2, Pc)
	p$K <- nut(t1, k1, Kc) + nut(t2, k2, Kc)
	p$fert_cost <- ifelse(t1 == "none", 0, p$c1) + ifelse(t2 == "none", 0, p$c2)

	# manure written as a fertilizer type
	om_other <- p$f1o %in% manures | p$f2o %in% manures
	p$OM_used <- p$manure == "Yes" | om_other
	# manure amounts are mostly in motorking loads, buckets and pans; only kg and tons are used
	p$OM_kg <- ifelse(p$mu == "Kilograms", p$mq, NA)
	i <- which(p$mu == "Tons")
	p$OM_kg[i] <- p$mq[i] * 1000
	p$OM_kg[which(!p$OM_used)] <- 0
	p$OM_kg[which(om_other & p$manure != "Yes")] <- NA

	# seed used, by source
	src <- c(d1 = "saved", d2 = "gift or barter", d3 = "purchased local", d4 = "purchased improved")
	sq <- sapply(names(src), function(v) num(r4[[v]]))
	su <- sapply(names(src), function(v) txt(r4[[paste0(v, "_unit")]]))
	skg <- sq
	skg[] <- NA
	skg[which(su == "Kilograms")] <- sq[which(su == "Kilograms")]
	skg[which(su == "Grams")] <- sq[which(su == "Grams")] / 1000
	skg[which(sq == 0)] <- 0
	none <- rowSums(!is.na(sq)) == 0
	skg[is.na(sq) & !none] <- 0
	p$seed_kg <- rowSums(skg)
	used <- !is.na(sq) & sq > 0
	p$seed_source <- apply(used, 1, function(u) if (any(u)) paste(src[u], collapse = ";") else NA)

	p <- p[!is.na(p$plot) & !duplicated(p[, keys]), c(keys, "fertilizer_used", "fertilizer_type", "fert_kg", "N", "P", "K", "fert_cost",
		"OM_used", "OM_kg", "pest_cost", "seed_kg", "seed_source")]
	d <- merge(d, p, by = keys, all.x = TRUE)

	d$fertilizer_amount <- d$fert_kg / d$crop_area
	d$N_fertilizer <- d$N / d$crop_area
	d$P_fertilizer <- d$P / d$crop_area
	d$K_fertilizer <- d$K / d$crop_area
	d$OM_amount <- d$OM_kg / d$crop_area
	# the type of manure is not reported
	d$OM_type <- ifelse(d$OM_used, "animal dung", "none")
	d$seed_rate <- d$seed_kg / d$crop_area
	d$fertilizer_cost <- d$fert_cost / d$crop_area
	d$pesticide_cost <- d$pest_cost / d$crop_area
	d$currency <- "GHS"

### irrigation (per plot x season)
	g <- data.frame(
		hhid = as.character(r5$hhid),
		plot = as.integer(r5$plote),
		ssn = txt(r5$season),
		irrigated = txt(r5$e1) == "Yes",
		irrigation_source = low(txt(r5$e2)),
		well = txt(r5$e3),
		method = txt(r5$e5),
		irrigation_lifting = low(txt(r5$e6)),
		freq = num(r5$e7a),
		per = txt(r5$e7b),
		irrigation_fulfullment = low(txt(r5$e8)),
		dur = num(r5$e9a),
		dunit = txt(r5$e9b),
		waterlogging_ = txt(r5$e10) == "Yes"
	)
	g$irrigation_source[which(g$irrigation_source %in% c("other (specify)", "na"))] <- NA
	g$irrigation_lifting[which(g$irrigation_lifting %in% c("other (specify)", "na"))] <- NA

	methods <- c(
		"Bucket/hose/watering can" = "manual", "Surface/flooding" = "surface",
		"Bay/border strip" = "border", "Furrow" = "furrow", "Drip" = "drip",
		"Level basin" = "basin", "Sprinkler" = "sprinkler"
	)
	g$irrigation_method <- unname(methods[g$method])
	g$irrigation_method[which(!g$irrigated)] <- "none"

	wells <- c("Drilled" = "borehole", "Hand-dug" = "hand-dug well")
	g$well_type_ <- unname(wells[g$well])
	g$well_type_[which(g$irrigation_source != "groundwater")] <- NA

	# times per period to days between irrigations
	days <- c(Day = 1, Week = 7, "Two weeks" = 14, Month = 30)
	g$irrigation_interval_ <- unname(days[g$per]) / g$freq
	g$irrigation_interval_[which(g$freq <= 0)] <- NA

	g$irrigation_duration_ <- ifelse(g$dunit == "Hours", g$dur, NA)
	i <- which(g$dunit == "Minutes")
	g$irrigation_duration_[i] <- g$dur[i] / 60
	g$irrigation_duration_[which(g$irrigation_duration_ <= 0 | g$irrigation_duration_ > 24)] <- NA

	g$irrigation_fulfullment[which(!g$irrigated | g$irrigation_fulfullment %in% c("", "na"))] <- NA
	g$waterlogging_[which(!g$irrigated)] <- NA

	g <- g[!is.na(g$plot) & !duplicated(g[, keys]), c(keys, "irrigated", "irrigation_source", "irrigation_method", "irrigation_lifting",
		"irrigation_interval_", "irrigation_duration_", "irrigation_fulfullment", "waterlogging_", "well_type_")]
	d <- merge(d, g, by = keys, all.x = TRUE)

### family labour (person-days per plot x season x crop)
	sumc <- function(r, v) rowSums(sapply(r[v], num), na.rm = TRUE)
	fam <- data.frame(
		hhid = as.character(r8$hhid),
		plot = as.integer(r8$plotccopy),
		ssn = txt(r8$season),
		ccode = as.integer(r8$c3copy),
		hh = sumc(r8, paste0("h", 2:25)),
		hh_weed = sumc(r8, c("h11", "h12", "h13")),
		hh_fert = sumc(r8, c("h8", "h9", "h10"))
	)
	fam <- aggregate(cbind(hh, hh_weed, hh_fert) ~ hhid + plot + ssn + ccode, data = fam, FUN = sum)
	d <- merge(d, fam, by = c(keys, "ccode"), all.x = TRUE)
	d$plot_labour_hh <- d$hh
	d$weeding_labour <- d$hh_weed / d$crop_area
	d$fertilizer_labour <- d$hh_fert / d$crop_area

### hired labour (person-days per household, rainy and dry season)
	hir <- data.frame(
		hhid = as.character(r9$hhid),
		farm_labour_hired = rowSums(cbind(num(r9$i2), num(r9$i4)), na.rm = TRUE)
	)
	hir$farm_labour_hired[which(txt(r9$i1) == "No")] <- 0
	hir <- hir[!duplicated(hir$hhid), ]
	d <- merge(d, hir, by = "hhid", all.x = TRUE)

### crop sales (per household x season x crop)
	cs <- data.frame(
		hhid = as.character(r7$hhid),
		ssn = txt(r7$season),
		ccode = as.integer(r7$gid),
		crop = map_crop(low(txt(r7$gid))),
		sold = num(r7$g9),
		su = txt(r7$g9_unit),
		suo = r7$g9_unit_o,
		price = num(r7$g10)
	)
	cs$sold_kg <- to_kg(cs$sold, cs$crop, cs$su, cs$suo)
	cs$crop_price <- cs$price / to_kg(rep(1, nrow(cs)), cs$crop, cs$su, cs$suo)
	cs$crop_price[which(!(cs$sold > 0) | cs$price <= 0)] <- NA
	cs <- cs[!duplicated(cs[, c("hhid", "ssn", "ccode")]), c("hhid", "ssn", "ccode", "sold_kg", "crop_price")]
	d <- merge(d, cs, by = c("hhid", "ssn", "ccode"), all.x = TRUE)
	# sales are for all plots with the crop; only assign the amount if there is one plot
	n <- table(paste(d$hhid, d$ssn, d$ccode))
	one <- as.vector(n[paste(d$hhid, d$ssn, d$ccode)]) == 1
	d$crop_amount_sold <- ifelse(one, d$sold_kg, NA)
	# amounts sold larger than the harvest look like money, not quantities; the price is then also wrong
	bad <- which(one & d$sold_kg > 1.1 * d$harvest_kg)
	d$crop_amount_sold[bad] <- NA
	d$crop_price[bad] <- NA

### location
	vill <- data.frame(
		location = c("Moognori", "Gbanterago Alimgbek", "Akara", "Asikiri Zasbugu", "Yidigu", "Zanlerigu", "Denugu",
			"Dimbasinia/Nyangua", "Bugri Natinga", "Zuli", "Bihinaayili", "Binpiala", "Kugri", "Nambako"),
		adm2 = c("Kassena-Nankana East", "Garu-Tempane", "Garu-Tempane", "Garu-Tempane", "Garu-Tempane", "Nabdam", "Garu-Tempane",
			"Kassena-Nankana East", "Garu-Tempane", "Garu-Tempane", "Savelugu", "Garu-Tempane", NA, NA),
		# Bihinaayili, Zanlerigu and Kugri: carobiner::geocode; others: district capital (see ISSUES 1)
		longitude = c(-1.0938, -0.1801, -0.1801, -0.1801, -0.1801, -0.7362, -0.1801,
			-1.0938, -0.1801, -0.1801, -0.8628, -0.1801, -0.2910, NA),
		latitude = c(10.8956, 10.8526, 10.8526, 10.8526, 10.8526, 10.8044, 10.8526,
			10.8956, 10.8526, 10.8526, 9.6017, 10.8526, 10.7859, NA)
	)
	vill$adm1 <- ifelse(vill$adm2 == "Savelugu", "Northern", "Upper East")
	h <- data.frame(hhid = as.character(r3$hhid), location = txt(r3$vill))
	h <- h[!duplicated(h$hhid), ]
	d <- merge(d, h, by = "hhid", all.x = TRUE)
	d <- merge(d, vill, by = "location", all.x = TRUE)
	d$country <- "Ghana"
	d$geo_from_source <- FALSE

### yield part
	parts <- c(
		maize = "grain", rice = "grain", sorghum = "grain", "pearl millet" = "grain",
		sesame = "seed", "common bean" = "seed", soybean = "seed", cowpea = "seed", chickpea = "seed",
		pea = "seed", lentil = "seed", "bambara groundnut" = "seed", melon = "seed", pepper = "seed",
		"pigeon pea" = "seed", "faba bean" = "seed", oats = "grain",
		groundnut = "pod", cotton = "lint", kenaf = "stems",
		tomato = "fruit", okra = "fruit", "chili pepper" = "fruit", "african eggplant" = "fruit",
		watermelon = "fruit", orange = "fruit", avocado = "fruit", eggplant = "fruit",
		onion = "bulbs", sweetpotato = "roots", yam = "tubers", potato = "tubers", "frafra potato" = "tubers",
		carrot = "roots", lettuce = "leaves",
		amaranth = "leaves", "jute mallow" = "leaves", vegetable = "leaves"
	)
	d$yield_part <- unname(parts[d$crop])
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	d$plot_id <- as.character(d$plot)
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	d <- d[order(d$hhid, d$plot, d$season, d$crop), ]
	# each survey record is its own "trial"
	d$trial_id <- as.character(seq_len(nrow(d)))

	drop <- c("plot", "ssn", "ccode", "area", "area_unit", "crop_area", "harvest", "hunit", "hunit_other",
		"harvest_kg", "fert_kg", "N", "P", "K", "fert_cost", "OM_kg", "pest_cost", "seed_kg",
		"hh", "hh_weed", "hh_fert", "sold_kg")
	d[drop] <- NULL

	carobiner::write_files(path, meta, d)
}
