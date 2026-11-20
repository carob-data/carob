# R script for "carob"
# license: GPL (>=3)

## NOTES
# ILSSI Tanzania baseline household survey (IFPRI, 2015) in Kilosa and Mvomero districts. Crop data refer to the Masika (long rains) and Vuli (short rains) seasons of 2014.
# One record per household x parcel x plot x season x crop, from 013_C_Crop_prodn_v2.dta. Rows without a crop were dropped.
# field_size is the plot size from the land module (012). Yield and all per-ha inputs, costs and labour use the area planted to the crop (013), which can be smaller than the plot.
# Inputs, seed and costs (014) and irrigation (015) are recorded per plot x season and were joined to the crop records. Soil type and land tenure (012) are per plot.
# Family labour (018) and hired and exchange labour (019) are person-days per plot x season x crop. plot_labour_* are totals for the plot; labour, weeding_labour and fertilizer_labour are per ha.
# Crop sales (017) are per household x season x crop. crop_price is the reported price per sale unit converted to TZS/kg. crop_amount_sold is only given when the household grew the crop on one plot in that season.
# Harvests and sales were converted to kg with the factors in "conversion to kg" below (see ISSUES 2).
# Fertilizer types written under "other" keep their original names (cleaned spelling). Nutrient contents used: urea 46% N; DAP 18% N and 46% P2O5 (20.1% P); CAN 26% N. Nutrient amounts are NA for other products.
# "manure", "organic manure" and "samadi" (Swahili for manure) written as a fertilizer type were recorded as OM_used.
# Fertilizer and manure amounts and costs are per plot in the source and were divided by the area planted.
# Irrigation: frequency per day, week, two weeks or month was converted to the interval between irrigations in days; frequency per season is irrigation_number.
# The region is not in the data files. Kilosa and Mvomero are districts of Morogoro Region.

# No coordinates in the source. Village coordinates were looked up with carobiner::geocode (Nominatim) and still need to be verified on a map. "Chanzulu" was found as "Chanzuru". "Zombolumbo" was not found and has the coordinates of Zombo ward.
# The irrigation module (015) covers 444 plot x season combinations. irrigated is NA for the other crop records.
# Seven plot x season combinations have more than one crop record. The plot-level inputs and costs are repeated for each crop.

## ISSUES
# 2. Harvests and sales in tins, litres and bushels (36.37 L) were converted with a bulk density by crop, and "bags" without a stated weight with a bag weight by crop. These factors are proposed values, not from the survey: please check. They are only given for grain, pulse and oilseed crops and for tomato, potato and onion. Other crops in these units, and bunches and pieces, have no yield.
# 6. Household altitude (alt) has implausible values (-613 to 2803 m; village medians are 349 to 774 m). Values below 200 m or above 1000 m were set to NA.
# 10. Some values are implausible and were not changed: very high yields for small areas, and a few fertilizer amounts above 1000 kg/ha. Some sale prices are very low or very high for the unit.
# 11. Irrigation durations above 24 hours were set to NA.
# 12. "chinese spinach" and "african eggplant" were added to the crop vocabulary by the reviewer; "bulbs" (onion) needs to be in the yield_part vocabulary.
# 13. Some amounts sold are larger than the harvest (up to 8,000,000 kg), probably money entered as quantity. crop_amount_sold and crop_price were set to NA for these (sold > 1.1 x harvest, or > 50 t when the harvest is unknown).

## NEW VARIABLES
# Provisional water variables, proposed in controvoc/terminag PR #15 (without the trailing underscore):
# irrigation_interval_: days between irrigation events
# irrigation_duration_: average duration of one irrigation event (hours)
# waterlogging_: Was the plot waterlogged? (015)
# well_type_: type of well when the source is groundwater
# irrigation_lifting_: method of obtaining the water

## NOT PROCESSED
# Soil fertility, slope, erosion, distance and decision maker (012); seed price is not asked (014); groundwater depth (015) is only a class (<7, 8-50, >50 m).
# Pumps (016): pumps are recorded per household and the plots they serve are coded as concatenated plot numbers ("010203"); flow rate is mostly 88 (don't know). The pump type is covered by irrigation_lifting_.
# Crop residue, feed, seed, gift and own-consumption amounts (017). Wages (019).
# Household-level modules that do not fit the plot x crop records: livestock (020, 021), soil conservation (022), diet diversity and food security (023-025), WASH (026, 027), disease and anthropometry (028, 029), income, social protection and expenditure (030-034), shocks (035), WEAI (038-040), and the community survey.

carob_script <- function(path) {

"
Tanzania Feed the Future Innovation Lab for Small-Scale Irrigation (ILSSI) Baseline Survey, 2015

The Feed the Future Innovation Lab on Small-Scale Irrigation (FTF-ILSSI) is a cooperative agreement funded by USAID under the Feed the Future program to undertake research aimed to increase food production, improve nutrition, accelerate economic development and contribute to the protection of the environment. The project seeks these objectives through identifying, testing and demonstrating technological options in small-scale irrigation and irrigated fodder, supported by a continual dialogue approach with stakeholders and capacity development toward sustained use of research approaches and evidence. Collaborators on this project include Texas A&M University, the International Water Management Institute (IWMI), the International Food Policy Research Institute (IFPRI), the International Livestock Research Institute (ILRI), North Carolina A&T State University (NCAT) and Texas A&M AgriLife Research (TAMUS). As part of this project, IFPRI is undertaking a study of irrigating and non-irrigating households in Ethiopia, Tanzania and Ghana to investigate the connections between irrigation, gender, nutrition and health. The survey explores these linkages through an in-depth household questionnaire with questions on agricultural production, nutrition and health, a WEAI module and a community questionnaire. This work forms part of the CGIAR Research Program on Water, Land and Ecosystems (WLE).
"


	uri <- "doi:10.7910/DVN/U9FG2N"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		design = "Household survey of irrigating and non-irrigating households in 14 villages of 2 districts. Units of analysis: household, plot, crop",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Zolo Kiala",
		carob_date = "2026-11-10",
		carob_completion = 75,
		carob_effort = 6
	)

	rd <- function(f) haven::read_dta(ff[basename(ff) == f])
	r1 <- rd("011_Section A.dta")
	r2 <- rd("012_B_land_v2.dta")
	r3 <- rd("013_C_Crop_prodn_v2.dta")
	r4 <- rd("014_D_Crop_inputs_v2.dta")
	r5 <- rd("015_E_Irrigation_v2.dta")
	r7 <- rd("017_G_Crop_Sales_v2.dta")
	r8 <- rd("018_H_Family_labor_v2.dta")
	r9 <- rd("019_I_Hired_Exchange_labor_v2.dta")

	# Stata value labels to text
	txt <- function(x) {
		x <- haven::as_factor(x)
		trimws(as.character(x))
	}
	low <- function(x) {
		x <- as.character(x)
		tolower(trimws(x))
	}
	num <- function(x) as.numeric(x)
	keys <- c("hhid", "parcel", "plot", "ssn")

### crop names
	# crops written under "other (specify)"
	crop_raw <- function(x, other) {
		x <- low(x)
		other <- low(other)
		i <- which(x == "other crop (specify)")
		x[i] <- other[i]
		x <- gsub("  +", " ", x)
		x[grep("cumber", x)] <- "cucumber"
		x[grep("^water ?mel", x)] <- "watermelon"
		x[grep("^banana", x)] <- "banana"
		x
	}
	# kunde = cowpea and mchicha = amaranth in Swahili
	crops <- c(
		"common bean/yellow bean" = "common bean", "beans" = "common bean",
		"soyabean" = "soybean", "peas" = "pea", "field peas" = "pea",
		"onions" = "onion", "potatoes" = "potato",
		"green pepper" = "chili pepper", "red pepper" = "chili pepper", "black pepper" = "pepper",
		"cow pea (ater)" = "cowpea", "kunde" = "cowpea",
		"amaranthus" = "amaranth", "mchicha" = "amaranth",
		"sweet potato (orange)" = "sweetpotato", "sweet potato leaves" = "sweetpotato",
		"sugar cane" = "sugarcane", "chick pea" = "chickpea",
		"cassava leaves" = "cassava", "pumpkin leaves" = "pumpkin",
		"green vegetables" = "vegetable"
	)
	map_crop <- function(x) {
		i <- which(x %in% names(crops))
		x[i] <- crops[x[i]]
		unname(x)
	}

### conversion to kg (proposed factors, see ISSUES 2)
	# bulk density (kg/L) of the harvested product, for tins, litres and bushels
	dens <- c(maize = 0.72, rice = 0.58, sorghum = 0.72, "finger millet" = 0.75, "pearl millet" = 0.75,
		sesame = 0.6, sunflower = 0.4, "common bean" = 0.78, cowpea = 0.78, "pigeon pea" = 0.78,
		soybean = 0.72, chickpea = 0.78, pea = 0.78, groundnut = 0.35,
		tomato = 0.55, potato = 0.65, onion = 0.6)
	# kg in a "bag" when the bag weight is not given
	bag <- c(maize = 100, rice = 80, sorghum = 100, sesame = 80, sunflower = 50, "common bean" = 100,
		cowpea = 100, "pigeon pea" = 100, soybean = 100, groundnut = 40)
	litres <- c("20 L tins" = 20, "10 L tins" = 10, "5 L tins" = 5, "2 L tins" = 2,
		"Liters" = 1, "mL" = 0.001, "Bushel" = 36.37)

	to_kg <- function(q, crop, unit, other) {
		other <- gsub("  +", " ", low(other))
		cd <- unname(dens[crop])
		f <- rep(NA_real_, length(q))
		f[which(unit == "Kilograms")] <- 1
		f[which(unit == "Grams")] <- 0.001
		i <- which(unit %in% names(litres))
		f[i] <- unname(litres[unit[i]]) * cd[i]
		o <- unit == "Other (Specify)"
		f[which(o & other %in% c("ton", "tons"))] <- 1000
		f[which(o & grepl("^120 ?kg ?bags?$", other))] <- 120
		i <- which(o & other %in% c("bag", "bags", "1 bag", "1bag"))
		f[i] <- unname(bag[crop[i]])
		i <- which(o & other %in% c("tin", "tins"))
		f[i] <- 20 * cd[i]
		i <- which(o & grepl("^bags with 7 \\(20 ?tin\\)", other))
		f[i] <- 7 * 20 * cd[i]
		i <- which(o & grepl("^bags \\(9 tins\\)", other))
		f[i] <- 9 * 20 * cd[i]
		out <- q * f
		# nothing harvested or sold, whatever the unit
		out[which(q == 0)] <- 0
		out
	}

### crop records (one row per household x parcel x plot x season x crop)
	d <- data.frame(
		hhid = as.character(r3$hhid),
		parcel = as.integer(r3$parcelb_),
		plot = as.integer(r3$plotb_),
		ssn = txt(r3$season),
		ccode = as.integer(r3$c3_),
		crop = crop_raw(txt(r3$c3_), r3$c3_o_),
		area = num(r3$c4_),
		area_unit = txt(r3$c4_unit_),
		harvest = num(r3$c5_),
		hunit = txt(r3$c5_unit_),
		hunit_other = r3$c5_unit_o_
	)

	# leaves were harvested, not roots or fruits
	d$leafy <- d$crop %in% c("cassava leaves", "sweet potato leaves", "pumpkin leaves")
	d$crop <- map_crop(d$crop)

	# no crop grown, or a crop that cannot be identified
	nocrop <- c(".", "", "did not plant", "no crop grown", "not cultivated", "other crops", "amalunch", "kaos", "flowers")
	d <- d[!(is.na(d$crop) | d$crop %in% nocrop), ]

### area planted and yield
	d$crop_area <- ifelse(d$area_unit == "Acre", d$area * 0.404686, NA)
	i <- which(d$area_unit == "Hectare")
	d$crop_area[i] <- d$area[i]
	d$crop_area[which(d$crop_area == 0)] <- NA

	d$harvest_kg <- to_kg(d$harvest, d$crop, d$hunit, d$hunit_other)
	d$yield <- d$harvest_kg / d$crop_area

	# Masika = long rains (March to May), Vuli = short rains (October to December)
	d$season <- ifelse(d$ssn == "Masika", "long rains", NA)
	d$season[which(d$ssn == "Vuli")] <- "short rains"
	d$planting_date <- "2014"

### plot size, soil type and tenure (per plot)
	pl <- data.frame(
		hhid = as.character(r2$hhid),
		parcel = as.integer(r2$parcelb_),
		plot = as.integer(r2$plotb_),
		size = num(r2$b3_),
		unit = txt(r2$b3_unit_),
		soil_type = low(txt(r2$b7_)),
		land_tenure = low(txt(r2$b9_))
	)
	pl$field_size <- ifelse(pl$unit == "Acres", pl$size * 0.404686, NA)
	i <- which(pl$unit == "Hectares")
	pl$field_size[i] <- pl$size[i]
	pl$field_size[which(pl$field_size == 0)] <- NA
	tenure <- c("inherited/parents? gift" = "inherited", "rented-in" = "rented in",
		"sharecropped in" = "sharecropped in", "borrowed free" = "borrowed")
	i <- which(pl$land_tenure %in% names(tenure))
	pl$land_tenure[i] <- tenure[pl$land_tenure[i]]
	pl <- pl[, c("hhid", "parcel", "plot", "field_size", "soil_type", "land_tenure")]
	d <- merge(d, pl, by = c("hhid", "parcel", "plot"), all.x = TRUE)

### inputs, seed and costs (per plot x season)
	p <- data.frame(
		hhid = as.character(r4$hhid),
		parcel = as.integer(r4$parcelb_),
		plot = as.integer(r4$plotb_),
		ssn = txt(r4$season),
		f1 = txt(r4$d81a_),
		f1o = low(r4$d81a_o_),
		q1 = num(r4$d81b_),
		u1 = txt(r4$d81c_unit_),
		u1o = low(r4$d81c_unit_o_),
		c1 = num(r4$d81d_),
		f2 = txt(r4$d82a_),
		f2o = low(r4$d82a_o_),
		q2 = num(r4$d82b_),
		u2 = txt(r4$d82c_unit_),
		u2o = low(r4$d82c_unit_o_),
		c2 = num(r4$d82d_),
		manure = txt(r4$dmanure_),
		mq = num(r4$dmanureb_),
		mu = txt(r4$dmanureb_unit_),
		pest_cost = num(r4$d6_)
	)

	# fertilizer types written under "other": original names, with spelling variants merged
	fnames <- c(can = "CAN", kan = "CAN", npk = "NPK",
		minjingu = "Minjingu", mijingu = "Minjingu",
		"minjingu mazao" = "Minjingu Mazao", "minjigu mazao" = "Minjingu Mazao",
		booster = "booster", boster = "booster", buster = "booster", busta = "booster",
		yaramila = "YaraMila", yalamila = "YaraMila", yala = "YaraMila",
		kani = "kani", keni = "kani", sa = "SA", "malti k" = "Multi-K", "99" = "unknown")
	manures <- c("manure", "organic manure", "samadi")
	ftype <- function(x, other) {
		other[is.na(other)] <- ""
		out <- rep(NA_character_, length(x))
		out[which(x == "Urea")] <- "urea"
		out[which(x == "DAP")] <- "DAP"
		i <- which(x == "Other (Specify)")
		out[i] <- ifelse(other[i] %in% names(fnames), unname(fnames[other[i]]), other[i])
		out[which(x == "Other (Specify)" & other == "")] <- "unknown"
		out[which(x == "Other (Specify)" & other %in% manures)] <- "none"
		out[grep("^Did not use", x)] <- "none"
		out
	}
	fkg <- function(q, unit, other) {
		kg <- ifelse(unit == "Kilograms", q, NA)
		i <- which(unit == "Grams")
		kg[i] <- q[i] / 1000
		i <- which(unit == "Other (Specify)" & grepl("50 ?kg", other))
		kg[i] <- q[i] * 50
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

	Nc <- c(urea = 46, DAP = 18, CAN = 26)
	# DAP has 46% P2O5; P = P2O5 / 2.29
	Pc <- c(urea = 0, DAP = 20.1, CAN = 0)
	Kc <- c(urea = 0, DAP = 0, CAN = 0)

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
	p$OM_kg <- ifelse(p$mu == "Kilograms", p$mq, NA)
	i <- which(p$mu == "Tons")
	p$OM_kg[i] <- p$mq[i] * 1000
	p$OM_kg[which(!p$OM_used)] <- 0
	p$OM_kg[which(om_other & p$manure != "Yes")] <- NA

	# seed used, by source
	src <- c(d1_ = "saved", d2_ = "gift or barter", d3_ = "purchased local", d4_ = "purchased improved")
	sq <- sapply(names(src), function(v) num(r4[[v]]))
	su <- sapply(names(src), function(v) txt(r4[[paste0(v, "unit_")]]))
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

	p <- p[, c(keys, "fertilizer_used", "fertilizer_type", "fert_kg", "N", "P", "K", "fert_cost",
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
	d$currency <- "TZS"


### irrigation (per plot x season)
	g <- data.frame(
		hhid = as.character(r5$hhid),
		parcel = as.integer(r5$parcelb_),
		plot = as.integer(r5$plotb_),
		ssn = txt(r5$season),
		irrigated = txt(r5$e1_) == "Yes",
		irrigation_source = low(txt(r5$e2_)),
		well = txt(r5$e3_),
		method = txt(r5$e5_),
		irrigation_lifting_ = low(txt(r5$e6_)),
		freq = num(r5$e7a_),
		per = txt(r5$e7b_),
		irrigation_fulfullment = low(txt(r5$e8_)),
		dur = num(r5$e9a_),
		dunit = txt(r5$e9b_),
		waterlogging_ = txt(r5$e10_) == "Yes"
	)
	g$irrigation_source[which(g$irrigation_source == "other (specify)")] <- NA
	g$irrigation_lifting_[which(g$irrigation_lifting_ == "other (specify)")] <- NA

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
	g$irrigation_number <- as.integer(ifelse(g$per == "Season" & g$freq > 0, g$freq, NA))

	g$irrigation_duration_ <- ifelse(g$dunit == "Hours", g$dur, NA)
	i <- which(g$dunit == "Minutes")
	g$irrigation_duration_[i] <- g$dur[i] / 60
	g$irrigation_duration_[which(g$irrigation_duration_ <= 0 | g$irrigation_duration_ > 24)] <- NA

	g$irrigation_fulfullment[which(!g$irrigated | g$irrigation_fulfullment %in% c("", "na"))] <- NA

	g <- g[, c(keys, "irrigated", "irrigation_source", "irrigation_method", "irrigation_lifting_",
		"irrigation_number", "irrigation_interval_", "irrigation_duration_", "irrigation_fulfullment",
		"waterlogging_", "well_type_")]
	d <- merge(d, g, by = keys, all.x = TRUE)

### labour (person-days per plot x season x crop)
	sumc <- function(r, v) rowSums(sapply(r[v], num), na.rm = TRUE)
	fam <- data.frame(
		hhid = as.character(r8$hhid),
		parcel = as.integer(r8$parcelb_),
		plot = as.integer(r8$plotb_),
		ssn = txt(r8$season),
		ccode = as.integer(r8$c3copy_),
		hh = sumc(r8, paste0("h", 2:25, "_")),
		hh_weed = sumc(r8, c("h11_", "h12_", "h13_")),
		hh_fert = sumc(r8, c("h8_", "h9_", "h10_"))
	)
	fam <- aggregate(cbind(hh, hh_weed, hh_fert) ~ hhid + parcel + plot + ssn + ccode, data = fam, FUN = sum)

	act <- c("i5", "i6", "i7", "i8", "i9", "i10", "i11")
	hir <- data.frame(
		hhid = as.character(r9$hhid),
		parcel = as.integer(r9$parcelb_),
		plot = as.integer(r9$plotb_),
		ssn = txt(r9$season),
		ccode = as.integer(r9$icropid_),
		hired = sumc(r9, paste0(act, "a_")),
		exch = sumc(r9, paste0(act, "b_")),
		ot_weed = sumc(r9, c("i8a_", "i8b_")),
		ot_fert = sumc(r9, c("i7a_", "i7b_"))
	)
	hir <- aggregate(cbind(hired, exch, ot_weed, ot_fert) ~ hhid + parcel + plot + ssn + ccode, data = hir, FUN = sum)

	d <- merge(d, fam, by = c(keys, "ccode"), all.x = TRUE)
	d <- merge(d, hir, by = c(keys, "ccode"), all.x = TRUE)
	d$plot_labour_hh <- d$hh
	d$plot_labour_hired <- d$hired
	d$plot_labour_exchange <- d$exch
	d$plot_labour <- d$hh + d$hired + d$exch
	d$labour <- d$plot_labour / d$crop_area
	d$weeding_labour <- (d$hh_weed + d$ot_weed) / d$crop_area
	d$fertilizer_labour <- (d$hh_fert + d$ot_fert) / d$crop_area

### crop sales (per household x season x crop)
	cs <- data.frame(
		hhid = as.character(r7$hhid),
		ssn = txt(r7$season),
		ccode = as.integer(r7$gid_),
		crop = map_crop(crop_raw(txt(r7$gid_), r7$gn_)),
		sold = num(r7$g9_),
		su = txt(r7$g9unit_),
		suo = r7$g9unito_,
		price = num(r7$g10_)
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
	bad <- which(one & (d$sold_kg > 1.1 * d$harvest_kg | (is.na(d$harvest_kg) & d$sold_kg > 50000)))
	d$crop_amount_sold[bad] <- NA
	d$crop_price[bad] <- NA

### household location
	h <- data.frame(
		hhid = as.character(r1$hhid),
		adm2 = txt(r1$disu),
		location = txt(r1$vill),
		elevation = num(r1$alt)
	)
	# "*" marks the two intervention villages
	h$location <- gsub("\\*|'", "", h$location)
	h$elevation[which(h$elevation < 200 | h$elevation > 1000)] <- NA
	d <- merge(d, h, by = "hhid", all.x = TRUE)

	d$country <- "Tanzania"
	d$adm1 <- "Morogoro"

	# coordinates from carobiner::geocode (Nominatim), not from the source; see NOTES
	geo <- data.frame(
		location = c("Chanzulu", "Ilonga", "Kibaoni", "Kimamba A", "Kondoa", "Madudumizi", "Rudewa Mbuyuni",
				"Zombolumbo", "Magali", "Mangae", "Mkindo", "Sangasanga", "Tangeni", "Vikenge"),
		longitude = c(37.0734, 37.0457, 37.0297, 37.1397, 37.0288, 36.8992, 37.1406,
				36.9222, 37.4401, 37.2409, 37.5532, 37.5616, 37.6099, 37.5718),
		latitude = c(-6.7991, -6.7624, -6.8310, -6.7862, -6.7976, -6.9828, -6.6868,
				-6.9601, -7.0300, -7.0631, -6.2401, -6.8923, -6.9319, -6.9111)
	)
	d <- merge(d, geo, by = "location", all.x = TRUE)
	d$geo_from_source <- FALSE

### yield part
	parts <- c(
		maize = "grain", rice = "grain", wheat = "grain", barley = "grain", oats = "grain",
		sorghum = "grain", millet = "grain", "pearl millet" = "grain", "finger millet" = "grain",
		sesame = "seed", sunflower = "seed", "pigeon pea" = "seed", "common bean" = "seed",
		soybean = "seed", pea = "seed", cowpea = "seed", chickpea = "seed", "faba bean" = "seed",
		pepper = "seed", groundnut = "pod",
		tomato = "fruit", watermelon = "fruit", okra = "fruit", eggplant = "fruit",
		"african eggplant" = "fruit", "chili pepper" = "fruit", banana = "fruit",
		mango = "fruit", orange = "fruit", coconut = "fruit", pumpkin = "fruit", cucumber = "fruit",
		cassava = "roots", sweetpotato = "roots", potato = "tubers", sugarcane = "stems", onion = "bulbs",
		cabbage = "leaves", amaranth = "leaves", "chinese spinach" = "leaves", vegetable = "leaves"
	)
	d$yield_part <- unname(parts[d$crop])
	d$yield_part[d$leafy] <- "leaves"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	d$plot_id <- paste(d$parcel, d$plot, sep = "-")
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	d <- d[order(d$hhid, d$plot_id, d$season, d$crop), ]
	# each survey record is its own "trial"
	d$trial_id <- as.character(seq_len(nrow(d)))

	drop <- c("parcel", "plot", "ssn", "ccode", "area", "area_unit", "crop_area", "harvest", "hunit", "hunit_other",
			"leafy", "fert_kg", "N", "P", "K", "fert_cost", "OM_kg", "pest_cost", "seed_kg",
			"hh", "hh_weed", "hh_fert", "hired", "exch", "ot_weed", "ot_fert", "sold_kg", "harvest_kg")
	d[drop] <- NULL

	carobiner::write_files(path, meta, d)
}

