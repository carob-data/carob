# R script for "carob"
# license: GPL (>=3)

## ISSUES
# h1 (multiple markets per household) is collapsed to one row per household (market_type only)
# b_income.csv, market_timeto/market_costs, h2_h3_access_to_markets.csv, and
# g3_nutrition_important_food_hh.csv are not processed
# livestock (one-to-many per household) collapsed to one row per household
# yield is NA for ~158/1279 rows
# percentage_sold sometimes holds a quantity ("1bag") instead of a percentage; non-numeric values coerced to NA.
# homestead_latitude/homestead_longitude are swapped and sign-flipped in the source corrected in the script, with
# geo_from_source = FALSE for the whole dataset. 
# 25 records fall outside Liberia
# one pepper record has a high yield (~5.9 t/ha) from a small (0.05 ha) plot.

carob_script <- function(path) {

"N2Africa Baseline Survey 1 - Liberia, 2012

N2Africa aims to contribute to increasing biological nitrogen fixation and
productivity of grain legumes among African smallholder farmers, thereby enhancing
soil fertility, improving household nutrition and increasing income levels. This
dataset covers the Liberia baseline survey, conducted 3 October-30 November 2012
across four action sites (Foya, Jorquelleh, Sannoyea, Zorzor) in the humid warm
tropics agroecological zone."

## Identifiers
	uri <- "doi:10.25502/ckxj-2t51/d"
	group <- "survey"

## Download data
	ff <- carobiner::get_data(uri, path, group)

## metadata
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "N2Africa",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_completion = 70,
		carob_effort = 5,
		carob_contributor = "Oscar Bautista",
		carob_date = "2026-07-12",
		notes = NA,
		design = NA
	)

## read data
	f1  <- ff[basename(ff) == "general.csv"]
	r1  <- read.csv(f1, na="")
	f2  <- ff[basename(ff) == "a_demographic.csv"]
	r2  <- read.csv(f2, na="")
	#f3  <- ff[basename(ff) == "b_income.csv"]
	#r3  <- read.csv(f3, na="")
	f4  <- ff[basename(ff) == "c_labour.csv"]
	r4  <- read.csv(f4, na="")
	f5  <- ff[basename(ff) == "d_livestock_ownership.csv"]
	r5  <- read.csv(f5, na="")
	f6  <- ff[basename(ff) == "e1_e2_land_use.csv"]
	r6  <- read.csv(f6, na="")
	f7  <- ff[basename(ff) == "e3_land_use.csv"]
	r7  <- read.csv(f7, na="")
	#f8  <- ff[basename(ff) == "e4_e6_land_use.csv"]
	#r8  <- read.csv(f8, na="")
	f9  <- ff[basename(ff) == "f_crop_production.csv"]
	r9  <- read.csv(f9, na="")
	f10 <- ff[basename(ff) == "g1_legume_utilisation.csv"]
	r10 <- read.csv(f10, na="")
	f11 <- ff[basename(ff) == "g2_legume_utilisation.csv"]
	r11 <- read.csv(f11, na="")
	f12 <- ff[basename(ff) == "h1_access_to_markets.csv"]
	r12 <- read.csv(f12, na="")
	#f13 <- ff[basename(ff) == "h2_h3_access_to_markets.csv"]
	#r13 <- read.csv(f13, na="")
	#f14 <- ff[basename(ff) == "g3_nutrition_important_food_hh.csv"]
	#r14 <- read.csv(f14, na="")

## crop names shared across crop production, e3 rotation, and legume utilisation
	standardise_crop <- \(x) {
		x <- trimws(tolower(x))
		x[x == "corn"] <- "maize"
		x[x == "cass"] <- "cassava"
		x[x %in% c("bean", "beans", "beas", "big beans", "big bean",
		           "country beans", "country bean", "long beans", "long bean",
		           "brown beans", "brown bean")] <- "common bean"
		x[x %in% c("mung bean", "mung beans")] <- "mung bean"
		x[grepl("^cowpean?$", x)] <- "cowpea"
		x[x %in% c("benneyseed", "benneyseeds")] <- "sesame"
		x[x %in% c("potato", "potatoes", "potatoe", "potatos",
		           "patatoes", "potatoes leaf")] <- "sweetpotato"
		x[x %in% c("garden egg", "garden eggs", "gardenegg", "ground eggs")] <- "eggplant"
		x[x %in% c("biter-ball", "bitter", "bitter-ball", "bitterball",
		           "bitterballs", "bitterboy", "bitterbal")] <- "bitterball"
		x[grepl("^eddoe?s?$", x)] <- "taro"
		x[x %in% c("ground", "ground nut", "ground nuts", "groundnu",
		           "groundnut", "groundnuts", "ground pea", "ground pean",
		           "ground peas", "peanut", "peanuts")] <- "groundnut"
		x[x %in% c("oil palm", "oilpalm", "palm")] <- "oil palm"
		x[x %in% c("sugar cane", "sugarcane")] <- "sugarcane"
		x[grepl("^oranges?$", x)] <- "orange"
		x[x %in% c("tomato", "tomatoe", "tomatoes", "tomota")] <- "tomato"
		x[x %in% c("peppet", "pepper", "peppers", "peppr")] <- "pepper"
		x[x %in% c("cucmber", "cucumbe", "cucumber")] <- "cucumber"
		x[x == "okro"] <- "okra"
		x[x %in% c("onion", "onions", "local onions")] <- "onion"
		x[x %in% c("pinapple", "pineapple", "poneapple")] <- "pineapple"
		x[x %in% c("pumkin", "pumpkin", "pumpkins", "pumppkin", "punkin")] <- "pumpkin"
		x[x == "rices"] <- "rice"
		x[x == "pawpaw"] <- "papaya"
		x[x %in% c("water-melon", "watermelon")] <- "watermelon"
		x[x %in% c("water-grain", "water-grains", "water-green",
		           "water-greens", "watergreen")] <- "waterleaf"
		x[grepl("other non-legume crop", x)] <- "unknown"
		# dish names, column-header leakage, single letters - not real crop names
		x[x %in% c("palava sauce", "palava sorce", "plava-suace", "price",
		           "principle crop", "butterfly", "greens")] <- NA
		x[x %in% c("na", "crop", "n", "none", "")] <- NA
		x
	}

## general.csv - one row per household, the only source of geo/admin data
## homestead_latitude/homestead_longitude are swapped and sign-flipped: as-is,
## 0/370 non-missing pairs fall inside Liberia; swapped+negated, 345/370 do,
## matching real site locations (e.g. Foya ~8.3/-10.3 vs actual 8.31/-10.22)
	lat <- -as.numeric(r1$homestead_longitude)
	lon <- -as.numeric(r1$homestead_latitude)

	d1 <- data.frame(
		field_id = as.character(r1$farm_id),
		country = r1$country,
		adm2 = carobiner::fix_name(r1$action_site, "title"),
		location = carobiner::fix_name(r1$location, "title"),
		longitude = lon,
		latitude = lat,
		stringsAsFactors = FALSE
	)

## a_demographic.csv - one row per household
	d2 <- data.frame(
		field_id = as.character(r2$farm_id),
		sex = trimws(tolower(r2$gender)),
		hh_adult_men = as.integer(r2$adults_male),
		hh_adult_women = as.integer(r2$adults_female),
		hh_child_18 = as.integer(r2$children_male) + as.integer(r2$children_female),
		age = as.numeric(r2$age),
		education = trimws(tolower(r2$highest_education_level)),
		stringsAsFactors = FALSE
	)
	d2$sex[d2$sex == "f"] <- "female"
	d2$sex[d2$sex == "m"] <- "male"
	d2$hh_size <- d2$hh_adult_men + d2$hh_adult_women + d2$hh_child_18

## b_income.csv - not processed here
	#d3 <- data.frame(
	#	field_id = as.character(r3$farm_id),
	#	hh_income_source = trimws(tolower(r3$main_source_income)),
	#	stringsAsFactors = FALSE
	#)

## c_labour.csv - one row per household
	d4 <- data.frame(
		field_id = as.character(r4$farm_id),
		# 0/1, not TRUE/FALSE: the source is a yes/no answer, not a day count,
		# but farm_labour_hired is expected to be numeric
		farm_labour_hired = as.numeric(trimws(tolower(r4$labour_hired)) == "y"),
		stringsAsFactors = FALSE
	)

## d_livestock_ownership.csv - one-to-many (farm x animal); collapsed to one row
## per household (";"-joined animal names, total heads summed across types)
	r5$field_id <- as.character(r5$farm_id)
	r5$animal   <- trimws(tolower(r5$livestock))
	r5$heads    <- as.numeric(r5$number_owned)
	r5$animal[grepl("^goats?$", r5$animal)] <- "goat"
	r5$animal[grepl("^ducks?$", r5$animal)] <- "duck"
	r5$animal[grepl("^pigs?$", r5$animal)] <- "pig"
	r5$animal[grepl("^rabbits?$", r5$animal)] <- "rabbit"
	r5$animal[grepl("guinea fowls?", r5$animal)] <- "guinea fowl"
	# dogs are pets, not production livestock, and are not an accepted animal term
	r5$animal[grepl("^dogs?$", r5$animal)] <- NA
	r5  <- r5[!is.na(r5$animal), ]
	d5a <- aggregate(animal ~ field_id, data = r5, FUN = \(x) paste(unique(x), collapse = ";"))
	d5b <- aggregate(heads ~ field_id, data = r5, FUN = sum, na.rm = TRUE)
	d5  <- merge(d5a, d5b, by = "field_id", all = TRUE)

## e1_e2_land_use.csv - one row per household
	d6 <- data.frame(
		field_id = as.character(r6$farm_id),
		cropland = as.numeric(r6$cropping_area_available_ha),
		land_fallow = trimws(tolower(r6$land_left_fallow_during_cropping_season)) == "y",
		years_fallow = as.numeric(r6$years_left_fallow),
		stringsAsFactors = FALSE
	)

## e3_land_use.csv - principle/second/third crop columns stacked into one long
## table, then collapsed to a ";"-separated crop_rotation string per household
	fid <- as.character(r7$farm_id)
	r7_long <- rbind(
		data.frame(field_id = fid, crop = standardise_crop(r7$principle_crop), stringsAsFactors = FALSE),
		data.frame(field_id = fid, crop = standardise_crop(r7$second_crop), stringsAsFactors = FALSE),
		data.frame(field_id = fid, crop = standardise_crop(r7$third_crop), stringsAsFactors = FALSE)
	)
	r7_long <- r7_long[!is.na(r7_long$crop), ]
	d7 <- aggregate(crop ~ field_id, data = r7_long, FUN = \(x) paste(unique(x), collapse = ";"))
	names(d7)[2] <- "crop_rotation"

## e4_e6_land_use.csv - not processed here
	#d8 <- data.frame(
	#	field_id = as.character(r8$farm_id),
	#	land_tree_ha = as.numeric(r8$land_owned_with_plantation_trees_ha),
	#	access_pasture = trimws(tolower(r8$access_to_pastures_for_grazing)) == "y",
	#	access_woodlot = trimws(tolower(r8$access_to_wood_lots_forest)) == "y",
	#	stringsAsFactors = FALSE
	#)

## h1_access_to_markets.csv - one-to-many (farm x market visited); aggregated to
## one row per household (";"-joined market names/types)
	r12$field_id <- as.character(r12$farm_id)
	r12$market_type <- trimws(toupper(r12$how_f_mc_c))
	r12$market_type[r12$market_type == "MC"] <- "market centre"
	r12$market_type[r12$market_type == "C"]  <- "cooperative"
	r12$market_type[r12$market_type == "F"]  <- "farmer group"
	r12$market_type[r12$market_type == "V"]  <- "village market"
	r12$market_type[!r12$market_type %in%
		c("market centre", "cooperative", "farmer group", "village market")] <- "unknown"
	d9 <- aggregate(market_type ~ field_id, data = r12, FUN = \(x) paste(unique(x), collapse = ";"))

## h2_h3_access_to_markets.csv - not processed here
	#d10 <- data.frame(
	#	field_id = as.character(r13$farm_id),
	#	sell_at_farm_gate = trimws(tolower(r13$sell_at_farm_gate)) == "y",
	#	sell_at_home = trimws(tolower(r13$sell_at_home)) == "y",
	#	stringsAsFactors = FALSE
	#)

## g3_nutrition_important_food_hh.csv - not processed here
	#clean_food <- \(x) {
	#	x <- trimws(tolower(x))
	#	x[x %in% c("cassave", "g-be (cassava)", "cassava/fufu", "cassava (dumboy)")] <- "cassava"
	#	x[x %in% c("eddo", "eddoe")] <- "eddoes"
	#	x[x == "plantains"] <- "plantain"
	#	x[x == "yams"] <- "yam"
	#	x[x %in% c("groundnuts", "peanut")] <- "groundnut"
	#	x[x == "dumbor"] <- "dumboy"
	#	x[x %in% c("palmoil", "red oil", "oil")] <- "palm oil"
	#	x[x == "banaa"] <- "banana"
	#	x[x == "bulgar wheat"] <- "bulgur wheat"
	#	x[x == "g.b."] <- NA
	#	x
	#}
	#r14$food <- clean_food(r14$food_household)
	#r14 <- r14[!is.na(r14$food), ]
	#d11 <- aggregate(food ~ farm_id, data = r14, FUN = \(x) paste(unique(x), collapse = ";"))
	#names(d11) <- c("field_id", "important_foods")

## g1_legume_utilisation.csv - one row per household x legume type
	dg1 <- data.frame(
		field_id = as.character(r10$farm_id),
		crop = standardise_crop(r10$legume_type),
		yield_marketable = as.numeric(r10$amount_used_for_sale), # kg
		amount_consumed = as.numeric(r10$hh_amount_used_for_consumption_seed_kg), # kg
		amount_produced = as.numeric(r10$total_prod_most_recent_season_kg), # kg
		stringsAsFactors = FALSE
	)

## g2_legume_utilisation.csv - one row per household x legume type
	dg2 <- data.frame(
		field_id = as.character(r11$farm_id),
		crop = standardise_crop(r11$legume_type),
		previous_crop_residue_management = trimws(tolower(r11$haulms_usage)),
		stringsAsFactors = FALSE
	)
	p <- dg2$previous_crop_residue_management
	p[grepl("incorporated in the soil", p)] <- "incorporated"
	p[grepl("composted", p)] <- "incorporated as compost"
	p[grepl("^burnt$", p)] <- "burned"
	p[grepl("feed for own livestock", p)] <- "fed to livestock"
	p[grepl("^give away$", p)] <- "given away"
	p[grepl("^left out on field$", p)] <- "left on field"
	p[grepl("sold to other people", p)] <- "sold"
	p[grepl("soda|soap", p)] <- "removed (other use)"
	p[grepl("throw away|thrown away|^dump$|wasted in dumpsite|^nothing$|^no use$|^unused$", p)] <- "discarded"
	dg2$previous_crop_residue_management <- p

## f_crop_production.csv - main table, one row per household x crop
	yield_txt <- trimws(tolower(r9$yield))
	# not-yet-harvested placeholders, not real values
	yield_txt[yield_txt %in% c("not ready", "not yet", "not ready yet",
	                            "not harvested yet", "not yet ready")] <- NA
	yield_num  <- as.numeric(gsub("[^0-9.]", "", yield_txt))
	yield_unit <- trimws(gsub("[0-9. ]", "", yield_txt))
	yield_unit[yield_unit %in% c("kf", "kh", "kgw")] <- "kg"
	yield_unit[yield_unit == "gallos"] <- "gallons"
	yield_unit[yield_unit %in% c("pcs", "pices")] <- "pieces"
	is_weight <- yield_unit %in% c("kg", "ton")
	yield_total <- ifelse(yield_unit == "ton", yield_num * 1000, yield_num) # kg
	yield_total[!is_weight] <- NA

	fert_type <- tolower(trimws(r9$mineral_fert_type))
	fert_type[fert_type == "no"] <- NA
	fert_type[grepl("^npk|^nkp", fert_type)] <- "NPK"

	om_type <- tolower(trimws(r9$other_organic_input_type))
	om_type[grepl("^grass$|rotten grass|bush trash", om_type)] <- "foliage"
	om_type[grepl("chiken manure", om_type)] <- "poultry manure"
	om_type[grepl("^composts?$", om_type)] <- "compost"
	om_type[om_type == "no"] <- NA
	# "grass" mistakenly entered as a fertilizer type is organic matter, not mineral
	om_type[is.na(om_type) & fert_type == "grass"] <- "foliage"
	fert_type[fert_type == "grass"] <- NA

	OM_used <- trimws(tolower(r9$animal_manure_applied)) == "y" |
	           trimws(tolower(r9$other_organic_input)) == "y"
	fertilizer_used <- trimws(tolower(r9$mineral_fert_applied)) == "y"
	# used but type unclear vs. not used at all
	om_type[OM_used %in% TRUE & is.na(om_type)] <- "unknown"
	fert_type[fertilizer_used %in% TRUE & is.na(fert_type)] <- "unknown"
	om_type[OM_used %in% FALSE & is.na(om_type)] <- "none"
	fert_type[fertilizer_used %in% FALSE & is.na(fert_type)] <- "none"
	# whether it was used at all is itself unanswered in the source
	om_type[is.na(om_type)] <- "unknown"
	fert_type[is.na(fert_type)] <- "unknown"

	variety <- tolower(trimws(r9$variety))
	variety[variety %in% c("no", "n0", "mung bean", "sweet potato")] <- "none"

	percentage_sold <- as.numeric(r9$percentage_sold)
	percentage_sold[!is.na(percentage_sold) & (percentage_sold < 0 | percentage_sold > 100)] <- NA

	crop_raw <- trimws(tolower(r9$crop))
	# rice upland/lowland is agronomically distinct; keep before collapsing to "rice"
	flooded <- rep(NA, length(crop_raw))
	flooded[crop_raw == "rice - lowland"] <- TRUE
	flooded[crop_raw == "rice - upland"]  <- FALSE

	d <- data.frame(
		field_id = as.character(r9$farm_id),
		crop = standardise_crop(ifelse(grepl("^rice", crop_raw), "rice", crop_raw)),
		flooded = flooded,
		intercropped = trimws(r9$sole_crop_or_intercrop) == "I",
		field_size = as.numeric(r9$area_ha),
		variety = variety,
		OM_used = OM_used,
		OM_type = om_type,
		fertilizer_used = fertilizer_used,
		fertilizer_type = fert_type,
		percentage_sold = percentage_sold,
		stringsAsFactors = FALSE
	)
	d$yield <- ifelse(!is.na(d$field_size) & d$field_size > 0, yield_total / d$field_size, NA)

## merge legume utilisation onto the matching household x crop rows
	d <- merge(d, dg1, by = c("field_id", "crop"), all.x = TRUE)
	d <- merge(d, dg2, by = c("field_id", "crop"), all.x = TRUE)

## fallback yield source for legumes: g1's total_prod_most_recent_season_kg is
## clean numeric (unlike the free-text yield above), used only where the main
## yield is missing/non-convertible
	i <- is.na(d$yield) & !is.na(d$amount_produced) & !is.na(d$field_size) & d$field_size > 0
	d$yield[i] <- d$amount_produced[i] / d$field_size[i]
	d$amount_produced <- NULL

## merge household-level (one row per household) tables
	d <- merge(d, d1,  by = "field_id", all.x = TRUE)
	d <- merge(d, d2,  by = "field_id", all.x = TRUE)
	d <- merge(d, d4,  by = "field_id", all.x = TRUE)
	d <- merge(d, d5,  by = "field_id", all.x = TRUE)
	d <- merge(d, d6,  by = "field_id", all.x = TRUE)
	d <- merge(d, d7,  by = "field_id", all.x = TRUE)
	d <- merge(d, d9,  by = "field_id", all.x = TRUE)

## drop rows with no identifiable crop (e.g. blank crop, "fallow"-style placeholder rows)
	d <- d[!is.na(d$crop), ]

	d$hhid     <- d$field_id
	d$trial_id <- as.character(as.integer(as.factor(1)))
	d$on_farm  <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- NA
	d$geo_from_source <- FALSE

	d$yield_part <- NA
	d$yield_part[d$crop %in% c("maize", "rice", "cowpea", "soybean", "groundnut",
	                            "mung bean", "common bean", "cocoa", "coffee")] <- "seed"
	d$yield_part[d$crop %in% c("cassava", "sweetpotato", "taro")] <- "roots"
	d$yield_part[d$crop %in% c("tomato", "pepper", "okra", "cucumber", "eggplant",
	                            "bitterball", "pumpkin", "banana", "plantain", "orange",
	                            "pineapple", "oil palm")] <- "fruit"
	d$yield_part[d$crop == "sugarcane"] <- "stems"
	# crop known but harvested part not confidently classified (e.g. rubber latex)
	d$yield_part[is.na(d$yield_part) & !is.na(d$crop)] <- "none"

	d$yield_isfresh  <- TRUE
	d$yield_moisture <- NA
	perennial <- c("cocoa", "coffee", "oil palm", "rubber", "banana", "plantain",
	               "orange", "pineapple", "sugarcane")
	d$planting_date <- ifelse(d$crop %in% perennial, NA, "2012")
	d$harvest_date   <- d$treatment <- NA
	d$N_fertilizer   <- d$P_fertilizer <- d$K_fertilizer <- NA

	char_cols <- sapply(d, is.character)
	d[char_cols] <- lapply(d[char_cols], trimws)

	d <- unique(d)

	carobiner::write_files(path, meta, d)
}
