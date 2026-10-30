# R script for "carob"
# license: GPL (>=3)

## NOTES
# STRASA baseline household survey, Nepal, 300 households in drought-prone villages of Banke, Siraha and Tanahun districts (Project details.xlsx: year 2008).
# Same table layout as the other STRASA country datasets (codebook "STRASA_Baseline Var Lists.xls"). Area in ha, production in kg, prices in NPR/kg.
# Three record types are combined in d, identified by survey_module_:
#   "rice variety" (tbl04aricevariety): household x parcel x plot x subplot x season x rice variety; area, production, yield (= prod/area), establishment.
#   "land use" (tbl03landuse): household x parcel x plot x season x crop; all crops, including rice.
#   "largest parcel" (tbl06largestparcel): largest rice parcel per household and season, with variety, planting/harvest month and week, grain and straw production;
#     joined to tbl06matinputs and tbl06laborinput (household x season) for fertilizer, manure, seed, pesticide and labour.
# Fertilizer: N/P/K = product rate (qtyPerHa, kg/ha; unit "kg" only) x nutrient fraction (terminag values_fertilizer_type: urea, DAP, KCl for MOP).
#   The source NperHa/PperHa/KperHa are NOT used: P and K appear computed with swapped oxide factors.
#   N/P/K set to 0 when the household-season has material input records but no fertilizer record.
# Organic fertilizer is FYM (owned or purchased) -> OM_type "farmyard manure".
# district -> adm3 (GADM Nepal: adm1 development region, adm2 zone, adm3 district); village -> location.
# irrigation_source, soil, tenure for "rice variety" and "land use" records from tbl02landholding (farmerid x parcel x plot).

## ISSUES
# No coordinates. Georeference villages (location) Puraina, Parbatipur (Banke), Baluwa (Siraha), Bhanu, Purkot (Tanahun).
# planting_date/harvest_date not set: only month and week are given (planting_month_, planting_week_, harvest_month_, harvest_week_). Crop year ambiguous (Project details "2008"; interviews Dec 2008 - Feb 2009).
# Record types overlap: rice in "land use" is also in "rice variety", and the "largest parcel" is one of those plots. Do not sum production across survey_module_.
# Seasons "spring", "spring2", "perennial", "yearround", "summer" are not terminag values ("rainy" mapped to "wet").
# Crops not matched kept as is ("minor cereals", "mixed crops"); "lentil +mustard" -> "lentil_mustard" (intercrop). yield_part NA for crops without an obvious harvested part.
# Some land-use yields very high (up to 60060 kg/ha) or NA; left as reported. "Hybrid" is a variety name in the data (unnamed hybrid).
# interviewdate has implausible values (1931); not processed.
# Unmapped modules: tbl01hhinfo, tbl04breasonsgrowing, tbl05cropsdisposal, tbl06poweruse, tbl06workhours, tbl07-tbl13.
# Land rent, irrigation fees and input costs not processed.
# Remaining write_files messages: missing latitude/longitude; unknown (new) variables; invalid season and crop terms; NA yield and yield_part.

## NEW VARIABLES
# survey_module_: source table of the record ("rice variety", "land use" or "largest parcel")
# village_stress_: stress domain of the sampled village (Drought)
# planting_month_, harvest_month_: month (3-letter abbreviation) of planting and harvest
# planting_week_, harvest_week_: week of the month (1st-4th) of planting and harvest
# residue_price_: price of rice straw (currency/kg)


carob_script <- function(path) {

"
STRASA = Stress Tolerant Rice for poor farmers in Africa and South Asia (Nepal)

1. Analyze patterns of varietal adoption and the factors that influence the adoption of improved varieties.   2. Analyze the economics of rice production and returns to varietal change.  3. Assess farmers' livelihood strategies in stress-prone rainfed environments. 4. Understand the gender roles in rice production and women's participation in decision making.  5. Derive implications for technology design, targeting, and policy reforms for enhancing the impact of improved technologies in stress-prone areas.
"

	uri <- "doi:10.7910/DVN/26281"
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
		carob_effort = 0.4
	)

	rd <- function(name) carobiner::read.excel(ff[basename(ff) == name], guess_max=100000)
	rS <- rd("tbl00surveyinfo.xls")
	rH <- rd("tbl02landholding.xls")
	rU <- rd("tbl03landuse.xls")
	rV <- rd("tbl04aricevariety.xls")
	rP <- rd("tbl06largestparcel.xls")
	rM <- rd("tbl06matinputs.xls")
	rL <- rd("tbl06laborinput.xls")

	## material inputs on the largest parcel, by household x season
	rM$id <- paste(rM$farmerid, rM$season)
	rM$kg <- tolower(trimws(rM$unit)) %in% "kg"
	# nutrient content (%, elemental) from terminag values_fertilizer_type
	fert <- data.frame(
		specify = c("urea", "dap", "mop"),
		fertilizer_type = c("urea", "DAP", "KCl"),
		N = c(46, 18, 0),
		P = c(0, 20.1, 0),
		K = c(0, 0, 49.8)
	)
	fm <- rM[rM$matinputs %in% "Fertilizer", ]
	i <- match(tolower(trimws(fm$specify)), fert$specify)
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
	inp$OM_type <- ifelse(inp$OM_used, "farmyard manure", "none")
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

	## rice varieties by plot
	h <- match(paste(rV$farmerid, rV$parcel, rV$plot), paste(rH$farmerid, rH$parcel, rH$plot))
	dV <- data.frame(
		hhid = rV$farmerid,
		survey_module_ = "rice variety",
		field_id = paste(rV$parcel, rV$plot, rV$subplot, sep="-"),
		season = rV$season,
		crop = "rice",
		variety = trimws(rV$ricevar),
		variety_type = rV$mvtv,
		landscape_position = tolower(rV$landtype),
		planting_method = rV$cem,
		field_size = rV$area,
		yield = rV$yield,
		soil_type = rH$soiltype[h],
		land_tenure = rH$tenure[h],
		irrigation_source = rH$irrigsource[h]
	)

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

	d <- carobiner::bindr(dP, dV, dU)

	s <- match(d$hhid, rS$farmerid)
	d$country <- "Nepal"
	d$adm3 <- carobiner::fix_name(rS$district[s], "title")
	d$location <- carobiner::fix_name(trimws(rS$village[s]), "title")
	d$village_stress_ <- rS$stress[s]
	d$currency <- "NPR"

	d$season <- tolower(d$season)
	d$season[d$season == "rainy"] <- "wet"
	d$variety_type <- unname(c(MV="modern", TV="traditional")[d$variety_type])
	d$planting_method <- unname(c(TPR="transplanting", DSR="direct seeding")[d$planting_method])
	d$irrigation_source[tolower(d$irrigation_source) %in% "none"] <- "none"

	tex <- tolower(d$soil_type)
	tex[tex == "loamy"] <- "loam"
	tex[tex == "sandy"] <- "sand"
	tex[tex == "silty"] <- "silt"
	tex[tex == "alluvial soil"] <- NA
	d$soil_texture <- tex

	crops <- c(blackgram="black gram", forage="forage crop", fruits="fruit", "lentil +mustard"="lentil_mustard",
		oat="oats", "other legumes"="legume", "other oilseeds"="oilseed", peanut="groundnut",
		pigeonpea="pigeon pea", vegetables="vegetable")
	i <- match(d$crop, names(crops))
	d$crop[!is.na(i)] <- crops[i[!is.na(i)]]
	yp <- c(rice="grain", wheat="grain", maize="grain", oats="grain", lentil="seed", "black gram"="seed",
		chickpea="seed", "pigeon pea"="seed", mustard="seed", sesame="seed", groundnut="seed",
		potato="tubers", okra="fruit", pumpkin="fruit", sugarcane="stems", cauliflower="flowers")
	d$yield_part <- unname(yp[d$crop])

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
