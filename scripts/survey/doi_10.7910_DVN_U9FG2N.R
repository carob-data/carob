# R script for "carob"
# license: GPL (>=3)

## NOTES
# ILSSI Tanzania baseline household survey (IFPRI, 2015) in Kilosa and Mvomero districts. Crop data refer to the Masika (long rains) and Vuli (short rains) seasons of 2014.
# One record per household x parcel x plot x season x crop, from 013_C_Crop_prodn_v2.dta. Rows without a crop were dropped.
# Inputs (014) and irrigation (015) are recorded per plot x season and were joined to the crop records. Soil type (012) is per plot.
# field_size is the area planted to the crop (acres * 0.404686, or hectares).
# yield = harvest (kg) / area planted (ha). Only harvests reported in kilograms, in tons (* 1000) or in "120 kg bags" (* 120) were converted.
# Fertilizer and manure amounts are per plot in the source and were divided by the area planted.
# Nutrient contents used: urea 46% N; DAP 18% N and 46% P2O5 (20.1% P); CAN 26% N.
# The region is not in the data files. Kilosa and Mvomero are districts of Morogoro Region.

## ISSUES
# 1. No coordinates in the source. Village coordinates were looked up with carobiner::geocode (Nominatim) and still need to be verified on a map. "Chanzulu" was found as "Chanzuru". "Zombolumbo" was not found and has the coordinates of Zombo ward.
# 2. About half of the harvests are reported in volume or count units (20 L tins, bags, bushels, bunches, pieces) without a weight. yield is NA for these. The reported quantity and unit are kept in harvest_quantity_ and harvest_unit_.
# 3. Fertilizer types written under "other" (buster/booster, Minjingu, YaraMila, kani, ...) are set to "unknown" and their nutrient amounts are NA. Amounts in litres were not converted.
# 4. Seven plot x season combinations have more than one crop record. The plot-level inputs are repeated for each crop.
# 5. "chinese spinach", "african eggplant" and "black pepper" are not in the crop vocabulary. "green pepper" and "red pepper" were mapped to pepper. "beans" written under "other crop" was mapped to common bean.
# 6. Household altitude (alt) has implausible values (-613 to 2803 m; village medians are 349 to 774 m). Values below 200 m or above 1000 m were set to NA.
# 7. "Bucket/hose/watering can" has no term in the irrigation vocabulary; "manual" is used.
# 8. yield_part is not set for onion, and is assumed to be "pod" for groundnut.
# 9. The irrigation module (015) covers 444 plot x season combinations. irrigated is NA for the other crop records.
# 10. Some values are implausible and were not changed: yields up to 17 t/ha for maize and 24 t/ha for rice, and two fertilizer amounts above 1000 kg/ha (large amounts reported for very small areas).

## NEW VARIABLES
# harvest_quantity_: harvested amount as reported, in the unit given by harvest_unit_
# harvest_unit_: unit of the reported harvest
# irrigation_lifting_: how the irrigation water was obtained (gravity, pump, by hand)

## NOT PROCESSED
# Plot size, soil fertility, slope, erosion and tenure (012), seed quantities and input costs (014), irrigation frequency and duration (015), pumps (016), crop sales (017), labour (018, 019), the livestock, nutrition, health, WASH, income, expenditure, shocks and WEAI modules, and the community survey.


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
		carob_completion = 50,
		carob_effort = 3
	)

	f1 <- ff[basename(ff) == "011_Section A.dta"]
	f2 <- ff[basename(ff) == "012_B_land_v2.dta"]
	f3 <- ff[basename(ff) == "013_C_Crop_prodn_v2.dta"]
	f4 <- ff[basename(ff) == "014_D_Crop_inputs_v2.dta"]
	f5 <- ff[basename(ff) == "015_E_Irrigation_v2.dta"]

	r1 <- haven::read_dta(f1)
	r2 <- haven::read_dta(f2)
	r3 <- haven::read_dta(f3)
	r4 <- haven::read_dta(f4)
	r5 <- haven::read_dta(f5)

	# Stata value labels to text
	txt <- function(x) {
		x <- haven::as_factor(x)
		trimws(as.character(x))
	}
	low <- function(x) {
		x <- as.character(x)
		tolower(trimws(x))
	}
	keys <- c("hhid", "parcel", "plot", "ssn")

### crop records (one row per household x parcel x plot x season x crop)
	d <- data.frame(
		hhid = as.character(r3$hhid),
		parcel = as.integer(r3$parcelb_),
		plot = as.integer(r3$plotb_),
		ssn = txt(r3$season),
		crop = low(txt(r3$c3_)),
		crop_other = low(r3$c3_o_),
		area = as.numeric(r3$c4_),
		area_unit = txt(r3$c4_unit_),
		harvest = as.numeric(r3$c5_),
		hunit = txt(r3$c5_unit_),
		hunit_other = low(r3$c5_unit_o_)
	)

	# crops written under "other (specify)"
	i <- which(d$crop == "other crop (specify)")
	d$crop[i] <- d$crop_other[i]
	d$crop <- gsub("  +", " ", d$crop)
	d$crop[grep("cumber", d$crop)] <- "cucumber"
	d$crop[grep("^water ?mel", d$crop)] <- "watermelon"
	d$crop[grep("^banana", d$crop)] <- "banana"

	# leaves were harvested, not roots or fruits
	d$leafy <- d$crop %in% c("cassava leaves", "sweet potato leaves", "pumpkin leaves")

	# kunde = cowpea and mchicha = amaranth in Swahili
	crops <- c(
		"common bean/yellow bean" = "common bean", "beans" = "common bean",
		"soyabean" = "soybean", "peas" = "pea", "field peas" = "pea",
		"onions" = "onion", "potatoes" = "potato",
		"green pepper" = "pepper", "red pepper" = "pepper",
		"cow pea (ater)" = "cowpea", "kunde" = "cowpea",
		"amaranthus" = "amaranth", "mchicha" = "amaranth",
		"sweet potato (orange)" = "sweetpotato", "sweet potato leaves" = "sweetpotato",
		"sugar cane" = "sugarcane", "chick pea" = "chickpea",
		"cassava leaves" = "cassava", "pumpkin leaves" = "pumpkin",
		"green vegetables" = "vegetable"
	)
	i <- which(d$crop %in% names(crops))
	d$crop[i] <- crops[d$crop[i]]

	# no crop grown, or a crop that cannot be identified
	nocrop <- c(".", "", "did not plant", "no crop grown", "not cultivated", "other crops", "amalunch", "kaos", "flowers")
	d <- d[!(is.na(d$crop) | d$crop %in% nocrop), ]

### area and yield
	d$field_size <- ifelse(d$area_unit == "Acre", d$area * 0.404686, NA)
	i <- which(d$area_unit == "Hectare")
	d$field_size[i] <- d$area[i]
	d$field_size[which(d$field_size == 0)] <- NA

	kg <- ifelse(d$hunit == "Kilograms", d$harvest, NA)
	i <- which(d$hunit == "Other (Specify)" & d$hunit_other == "tons")
	kg[i] <- d$harvest[i] * 1000
	i <- which(d$hunit == "Other (Specify)" & grepl("^120 +kg +bags$", d$hunit_other))
	kg[i] <- d$harvest[i] * 120
	d$yield <- kg / d$field_size

	d$harvest_quantity_ <- d$harvest
	d$harvest_unit_ <- ifelse(d$hunit == "Other (Specify)", d$hunit_other, tolower(d$hunit))
	d$harvest_unit_[which(d$harvest_unit_ == "")] <- NA

	# Masika = long rains (March to May), Vuli = short rains (October to December)
	d$season <- ifelse(d$ssn == "Masika", "long rains", NA)
	d$season[which(d$ssn == "Vuli")] <- "short rains"
	d$planting_date <- "2014"

### inputs (per plot x season)
	p <- data.frame(
		hhid = as.character(r4$hhid),
		parcel = as.integer(r4$parcelb_),
		plot = as.integer(r4$plotb_),
		ssn = txt(r4$season),
		f1 = txt(r4$d81a_),
		f1o = low(r4$d81a_o_),
		q1 = as.numeric(r4$d81b_),
		u1 = txt(r4$d81c_unit_),
		u1o = low(r4$d81c_unit_o_),
		f2 = txt(r4$d82a_),
		f2o = low(r4$d82a_o_),
		q2 = as.numeric(r4$d82b_),
		u2 = txt(r4$d82c_unit_),
		u2o = low(r4$d82c_unit_o_),
		manure = txt(r4$dmanure_),
		mq = as.numeric(r4$dmanureb_),
		mu = txt(r4$dmanureb_unit_)
	)

	ftype <- function(x, other) {
		out <- rep(NA_character_, length(x))
		out[which(x == "Urea")] <- "urea"
		out[which(x == "DAP")] <- "DAP"
		out[which(x == "Other (Specify)")] <- "unknown"
		out[which(x == "Other (Specify)" & other == "can")] <- "CAN"
		out[which(x == "Other (Specify)" & other == "npk")] <- "NPK"
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

	p$OM_used <- p$manure == "Yes"
	p$OM_kg <- ifelse(p$mu == "Kilograms", p$mq, NA)
	i <- which(p$mu == "Tons")
	p$OM_kg[i] <- p$mq[i] * 1000
	p$OM_kg[which(!p$OM_used)] <- 0

	p <- p[, c(keys, "fertilizer_used", "fertilizer_type", "fert_kg", "N", "P", "K", "OM_used", "OM_kg")]
	d <- merge(d, p, by = keys, all.x = TRUE)

	d$fertilizer_amount <- d$fert_kg / d$field_size
	d$N_fertilizer <- d$N / d$field_size
	d$P_fertilizer <- d$P / d$field_size
	d$K_fertilizer <- d$K / d$field_size
	d$OM_amount <- d$OM_kg / d$field_size
	# the type of manure is not reported
	d$OM_type <- ifelse(d$OM_used, "animal dung", "none")

### irrigation (per plot x season)
	g <- data.frame(
		hhid = as.character(r5$hhid),
		parcel = as.integer(r5$parcelb_),
		plot = as.integer(r5$plotb_),
		ssn = txt(r5$season),
		irrigated = txt(r5$e1_) == "Yes",
		irrigation_source = low(txt(r5$e2_)),
		method = txt(r5$e5_),
		irrigation_lifting_ = low(txt(r5$e6_))
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
	g$method <- NULL
	d <- merge(d, g, by = keys, all.x = TRUE)

### soil type as reported by the farmer (per plot)
	s <- data.frame(
		hhid = as.character(r2$hhid),
		parcel = as.integer(r2$parcelb_),
		plot = as.integer(r2$plotb_),
		soil_type = low(txt(r2$b7_))
	)
	d <- merge(d, s, by = c("hhid", "parcel", "plot"), all.x = TRUE)

### household location
	h <- data.frame(
		hhid = as.character(r1$hhid),
		adm2 = txt(r1$disu),
		location = txt(r1$vill),
		elevation = as.numeric(r1$alt)
	)
	# "*" marks the two intervention villages
	h$location <- gsub("\\*|'", "", h$location)
	h$elevation[which(h$elevation < 200 | h$elevation > 1000)] <- NA
	d <- merge(d, h, by = "hhid", all.x = TRUE)

	d$country <- "Tanzania"
	d$adm1 <- "Morogoro"

	# coordinates from carobiner::geocode (Nominatim), not from the source; see ISSUES
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
		sorghum = "grain", millet = "grain", "finger millet" = "grain",
		sesame = "seed", sunflower = "seed", "pigeon pea" = "seed", "common bean" = "seed",
		soybean = "seed", pea = "seed", cowpea = "seed", chickpea = "seed", "faba bean" = "seed",
		groundnut = "pod",
		tomato = "fruit", watermelon = "fruit", okra = "fruit", eggplant = "fruit",
		"african eggplant" = "fruit", pepper = "fruit", "black pepper" = "fruit", banana = "fruit",
		mango = "fruit", orange = "fruit", coconut = "fruit", pumpkin = "fruit", cucumber = "fruit",
		cassava = "roots", sweetpotato = "roots", potato = "tubers", sugarcane = "stems",
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

	drop <- c("parcel", "plot", "ssn", "crop_other", "area", "area_unit", "harvest", "hunit", "hunit_other",
			"leafy", "fert_kg", "N", "P", "K", "OM_kg")
	d[drop] <- NULL

	carobiner::write_files(path, meta, d)
}

