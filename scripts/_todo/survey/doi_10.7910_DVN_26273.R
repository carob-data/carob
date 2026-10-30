# R script for "carob"
# license: GPL (>=3)

## NOTES
# STRASA baseline household survey, Bangladesh, 500 households (Project details.xlsx: year 2008; data collection 2008 in Dataverse metadata).
# Codebook: "STRASA_Baseline Var Lists.xls" (sheet "Variable labels"). Area in ha, production in kg, prices in Taka/kg.
# Two record types are combined in d, identified by survey_module_:
#   "land use" (tbl03landuse): household x parcel x plot x season x crop; area, production, yield (= prod/area), price. All crops.
#   "largest parcel" (tbl06largestparcel): the largest rice parcel per household and season, with variety, establishment, planting/harvest month and week, grain and straw production;
#     joined to tbl06matinputs and tbl06laborinput (same household x season) for fertilizer, manure, seed, pesticide and labour.
# There is no tbl04aricevariety (rice variety by plot) table in this country dataset.
# Fertilizer: N/P/K computed as product rate (qtyPerHa, kg/ha; only unit "kg") x nutrient fraction (terminag values_fertilizer_type: urea, DAP, TSP, KCl).
#   The source columns NperHa/PperHa/KperHa are NOT used: P and K appear computed with swapped oxide factors (e.g. TSP P = 0.398 x product = 48% P2O5 / 1.2051; MP K = 0.264 x product = 60% K2O / 2.29).
#   N/P/K set to 0 when the household-season has material input records but no fertilizer record; NA when it has a fertilizer of unknown composition.
# irrigation_source, soil, tenure for "land use" records from tbl02landholding (farmerid x parcel x plot).
# district -> adm2 ("Satkhira-Con" = salinity-control villages in Satkhira); village -> location. Stress domain of the village (tbl00surveyinfo) kept as village_stress_.

## ISSUES
# No coordinates in the data (the questionnaire lists GeogDegree/GeogMins but they are not in the files). Georeference villages (location) in districts Jamalpur, Kurigram, Habiganj, Rajshahi, Satkhira.
# planting_date/harvest_date not set: only month and week are given (kept as planting_month_, planting_week_, harvest_month_, harvest_week_). Crop year is ambiguous (Project details "2008"; interviews mostly Jan-Mar 2009; Boro planted Jan may be 2008 or 2009).
# Record types overlap: the largest-parcel rice record is also included in the land-use records. Do not sum production across survey_module_.
# "Mixed fertilizers" (23 rows) have unknown composition (source assumed 14-14-14 or 18-46-0): N/P/K NA for those household-seasons. DAP has two different source compositions (18-46 and 21-?); standard DAP used.
# Gypsum, zinc, sulfur, boron inputs (separate matinputs categories) not processed (S_fertilizer could be derived from gypsum).
# Seedbed costs, irrigation and land rent (Taka), tbl06poweruse, tbl06workhours and costs not processed.
# Unmapped modules: tbl01hhinfo (household members), tbl05cropsdisposal, tbl07-tbl13 (income, borrowing, consumption, adjustment, awareness, decisions, organizations, training).
# Crop names not matched to terminag kept as is ("arum", "bean", "gourd", "grampea", "patal" (pointed gourd), "rabi", "seedbed preparation"): check. "pepper" may be chili.
# yield_part set only for crops with an obvious harvested part; NA for others (write_files "NA detected: yield_part").
# Season "aus" (and "aman2") not in terminag season values.
# farmerid has two formats ("102" and "BDRJKRHD082001"); check they refer to unique households.
# Land-use yields up to 149123 kg/ha (tomato), straw up to 130769 kg/ha, and some 0 yields (crop failure, e.g. "washed away by flood" in comments) left as reported.
# write_files reports duplicate records (identical land-use rows); check.
# Remaining write_files messages: missing latitude/longitude; unknown (new) variables; fwy_residue out of bounds; invalid season/crop terms and fertilizer_type "Mixed fertilizers"; high yield (tomato, pepper); duplicates; NA yield_part.
# interviewdate in tbl00surveyinfo has implausible values (2008-02, 2020-09) and NAs; not processed.

## NEW VARIABLES
# survey_module_: source table of the record ("land use" or "largest parcel")
# village_stress_: stress domain of the sampled village (Submergence, Drought, Saline-affected, Salinity-controlled)
# variety_generation_: release period of the modern variety ("before 1990", "1990 and after")
# planting_month_, harvest_month_: month (3-letter abbreviation) of planting and harvest
# planting_week_, harvest_week_: week of the month (1st-4th) of planting and harvest
# residue_price_: price of rice straw (currency/kg)


carob_script <- function(path) {

"
STRASA = Stress Tolerant Rice for poor farmers in Africa and South Asia (Bangladesh)

1. Analyze patterns of varietal adoption and the factors that influence the adoption of improved varieties.   2. Analyze the economics of rice production and returns to varietal change.   3. Assess farmers' livelihood strategies in stress-prone rainfed environments.   4. Understand the gender roles in rice production and women's participation in decision making.   5. Derive implications for technology design, targeting, and policy reforms for enhancing the impact of improved technologies in stress-prone areas.
"

	uri <- "doi:10.7910/DVN/26273"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "STRASA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 1.5
	)

	rd <- function(name) carobiner::read.excel(ff[basename(ff) == name], guess_max=100000)
	rS <- rd("tbl00surveyinfo.xls")
	rH <- rd("tbl02landholding.xls")
	rU <- rd("tbl03landuse.xls")
	rP <- rd("tbl06largestparcel.xls")
	rM <- rd("tbl06matinputs.xls")
	rL <- rd("tbl06laborinput.xls")

	## material inputs on the largest parcel, by household x season
	rM$id <- paste(rM$farmerid, rM$season)
	rM$kg <- tolower(trimws(rM$unit)) %in% "kg"
	# nutrient content (%, elemental) from terminag values_fertilizer_type
	fert <- data.frame(
		specify = c("Urea", "DAP", "TSP", "MP"),
		fertilizer_type = c("urea", "DAP", "TSP", "KCl"),
		N = c(46, 18, 0, 0),
		P = c(0, 20.1, 19.23, 0),
		K = c(0, 0, 0, 49.8)
	)
	fm <- rM[rM$matinputs %in% "Fertilizer", ]
	i <- match(trimws(fm$specify), fert$specify)
	fm$type <- fert$fertilizer_type[i]
	fm$type[is.na(i)] <- trimws(fm$specify[is.na(i)])
	fm$qty <- ifelse(fm$kg, fm$qtyPerHa, NA)
	fm$N <- fm$qty * fert$N[i] / 100
	fm$P <- fm$qty * fert$P[i] / 100
	fm$K <- fm$qty * fert$K[i] / 100

	om <- rM[rM$matinputs %in% "Organic fertilizer", ]
	om$qty <- ifelse(om$kg, om$qtyPerHa, NA)
	sd <- rM[rM$matinputs %in% "Seed", ]
	sd$qty <- ifelse(sd$kg, sd$qtyPerHa, NA)

	ids <- unique(rM$id)
	inp <- data.frame(
		id = ids,
		N_fertilizer = sapply(ids, \(x) sum(fm$N[fm$id == x])),
		P_fertilizer = sapply(ids, \(x) sum(fm$P[fm$id == x])),
		K_fertilizer = sapply(ids, \(x) sum(fm$K[fm$id == x])),
		fertilizer_type = sapply(ids, \(x) paste(sort(unique(fm$type[fm$id == x])), collapse="; ")),
		OM_used = ids %in% om$id,
		OM_amount = sapply(ids, \(x) sum(om$qty[om$id == x])),
		seed_rate = sapply(ids, \(x) sum(sd$qty[sd$id == x])),
		herbicide_used = ids %in% rM$id[rM$matinputs %in% "Herbicide"],
		insecticide_used = ids %in% rM$id[rM$matinputs %in% "Insecticide"]
	)
	inp$fertilizer_type[inp$fertilizer_type == ""] <- "none"
	inp$OM_amount[!inp$OM_used] <- 0
	inp$seed_rate[inp$seed_rate == 0] <- NA

	# labour: family + hired person-days per ha, summed over activities
	rL$id <- paste(rL$farmerid, rL$season)
	lab <- tapply(rL$laborPerHa, rL$id, sum)

	## largest parcel
	dP <- data.frame(
		hhid = rP$farmerid,
		survey_module_ = "largest parcel",
		season = rP$season,
		crop = "rice",
		variety = trimws(rP$ricevar),
		variety_type = rP$mvtv,
		variety_generation_ = rP$mvgen,
		landscape_position = tolower(rP$landtype),
		planting_method = rP$cem,
		soil_type = rP$soiltype,
		land_tenure = rP$tenure,
		planting_month_ = rP$plantmonth,
		planting_week_ = rP$plantweek,
		harvest_month_ = rP$harvestmonth,
		harvest_week_ = rP$harvestweek,
		field_size = rP$area,
		yield = rP$yield,
		fwy_residue = rP$prodstraw / rP$area,
		crop_price = rP$prodprice,
		residue_price_ = rP$strawprice
	)
	i <- match(paste(rP$farmerid, rP$season), inp$id)
	dP <- cbind(dP, inp[i, -1])
	dP$labour <- as.vector(lab[paste(rP$farmerid, rP$season)])

	## land use, all crops and plots
	h <- match(paste(rU$farmerid, rU$parcel, rU$plot), paste(rH$farmerid, rH$parcel, rH$plot))
	dU <- data.frame(
		hhid = rU$farmerid,
		survey_module_ = "land use",
		field_id = paste(rU$parcel, rU$plot, sep="-"),
		season = rU$season,
		crop = tolower(trimws(rU$crop)),
		landscape_position = tolower(rU$landtype),
		field_size = rU$area,
		yield = rU$yield,
		crop_price = rU$price,
		soil_type = rH$soiltype[h],
		land_tenure = rH$tenure[h],
		irrigation_source = rH$irrigsource[h]
	)

	d <- carobiner::bindr(dP, dU)

	s <- match(d$hhid, rS$farmerid)
	d$country <- "Bangladesh"
	d$adm2 <- gsub("-Con$", "", rS$district[s])
	d$location <- trimws(rS$village[s])
	d$village_stress_ <- rS$stress[s]
	d$currency <- "BDT"

	d$season <- tolower(d$season)
	d$variety_type <- unname(c(MV="modern", TV="traditional", Hybrid="hybrid")[d$variety_type])
	d$planting_method <- unname(c(TPR="transplanting")[d$planting_method])
	d$landscape_position <- gsub("medium land", "medium", d$landscape_position)

	tex <- tolower(d$soil_type)
	tex[tex == "loamy"] <- "loam"
	tex[tex == "sandy"] <- "sand"
	d$soil_texture <- tex

	# mukhi = mukhi kachu (taro); till = til (sesame)
	crops <- c(bringle="eggplant", brinjal="eggplant", chili="chili pepper", chilli="chili pepper",
		raddish="radish", sesum="sesame", till="sesame", vegetables="vegetable", mukhi="taro")
	i <- match(d$crop, names(crops))
	d$crop[!is.na(i)] <- crops[i[!is.na(i)]]
	yp <- c(rice="grain", wheat="grain", maize="grain", lentil="seed", mustard="seed", sesame="seed",
		potato="tubers", tomato="fruit", eggplant="fruit", "chili pepper"="fruit", pumpkin="fruit",
		"bottle gourd"="fruit", guava="fruit", sugarcane="stems", ginger="rhizome", turmeric="rhizome",
		radish="roots", cabbage="leaves")
	d$yield_part <- unname(yp[d$crop])

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
