# R script for "carob"
# license: GPL (>=3)

## NOTES
# STRASA baseline household survey, India (Orissa/Odisha), 508 households in Kendrapara, Bhadrak, Jajpur, Dhenkanal and Bolangir districts (Project details: year 2008).
# Same tables as the other STRASA country datasets, as sheets of "ORISSA_DATA 1.xlsx" (production, inputs) and "ORISSA_DATA 2.xlsx" (social modules).
# Codebook "STRASA_Baseline Var Lists.xls". Area in ha, production in kg, prices in Rs/kg.
# Three record types are combined in d, identified by survey_module_:
#   "rice variety" (sheet "Rice variety"): household x parcel x plot x subplot x season x rice variety; area, production, yield (= prod/area), establishment.
#   "land use" (sheet "Land use"): household x parcel x plot x season x crop; all crops, including rice.
#   "largest parcel" (sheet "Largest parcel"): largest rice parcel per household, season and variety type; some households report two parcels (one MV, one TV) in the same season.
#     Joined to sheets "Material inputs" and "Labor inputs" by household x season x area x variety type (MV/TV).
# Fertilizer: N/P/K = product rate (qtyPerHa, kg/ha; units kg/Kg/KG) x nutrient fraction (terminag values_fertilizer_type: urea, DAP, KCl for MOP/Potash).
#   Gromor taken as 28-28-0 (28% N, 28% P2O5 = 12.23% P), as the composition implied by the source NperHa/PperHa columns.
#   The source NperHa/PperHa/KperHa are NOT used otherwise: P and K appear computed with swapped oxide factors.
#   N/P/K set to 0 when the parcel has material input records but no fertilizer record; NA when a fertilizer of unknown composition (Navratna) was used.
# WSR (wet seeded rice) mapped to planting_method "direct seeding".
# irrigation_source, soil, tenure for "rice variety" and "land use" records from sheet "Landholding" (farmerid x parcel x plot).

## ISSUES
# No coordinates. Georeference villages (location) in the districts (Bolangir = GADM Balangir).
# planting_date/harvest_date not set: only month and week are given (planting_month_, planting_week_, harvest_month_, harvest_week_). Crop year ambiguous (Project details "2008"; interviews mostly Feb-Apr 2009).
# Record types overlap: rice in "land use" is also in "rice variety", and "largest parcel" records are among those plots. Do not sum production across survey_module_.
# Two largest-parcel records share household x season x area x variety type; their inputs cannot be separated (both get the same inputs).
# Gromor composition (28-28-0) taken from the source calculation, not from a label; check. Micronutrients (Zinc etc.) not processed.
# Organic fertilizer "specify" is mostly the source (own/purchased); OM_type set only for FYM and oilcakes.
# Soil "laterite", "grey", "red" kept in soil_type, soil_texture NA.
# write_files flags low yields for "fallow" and jute; check.
# Crops not matched kept as is ("horsegram", "pointedgourd", "poi", "fallow"); yield_part NA for crops without an obvious harvested part. Season "summer" not a terminag value.
# Some largest-parcel and land-use yields are 0 (crop failure); land-use yields up to 125000 kg/ha; left as reported.
# interviewdate has implausible values (1899) and NAs; not processed.
# Unmapped sheets: Household info, Non-farm labor, Non-rice costs, Work hours, Power use, Crop disposal, Reasons for growing, and all of DATA 2.
# Irrigation fees, land rent, input costs not processed.
# Remaining write_files messages: missing latitude/longitude; unknown (new) variables; invalid terms (season, crop, fertilizer_type Gromor/Navratna); NA yield_part; high yields.

## NEW VARIABLES
# survey_module_: source table of the record ("rice variety", "land use" or "largest parcel")
# village_stress_: stress domain of the sampled village (Submergence, Drought, Saline-affected, Salinity-controlled)
# planting_month_, harvest_month_: month (3-letter abbreviation) of planting and harvest
# planting_week_, harvest_week_: week of the month (1st-4th) of planting and harvest
# residue_price_: price of rice straw (currency/kg)


carob_script <- function(path) {

"
STRASA = Stress Tolerant Rice for poor farmers in Africa and South Asia (India, Orissa)

1. Analyze patterns of varietal adoption and the factors that influence the adoption of improved varieties.   2. Analyze the economics of rice production and returns to varietal change.  3. Assess farmers' livelihood strategies in stress-prone rainfed environments. 4. Understand the gender roles in rice production and women's participation in decision making.  5. Derive implications for technology design, targeting, and policy reforms for enhancing the impact of improved technologies in stress-prone areas.
"

	uri <- "doi:10.7910/DVN/26277"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
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
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "ORISSA_DATA 1.xlsx"]
	rd <- function(sheet) carobiner::read.excel(f, sheet=sheet, guess_max=100000)
	rS <- rd("Survey info")
	rH <- rd("Landholding")
	rU <- rd("Land use")
	rV <- rd("Rice variety")
	rP <- rd("Largest parcel")
	rM <- rd("Material inputs")
	rL <- rd("Labor inputs")

	key <- function(r) paste(r$farmerid, r$season, r$area, r$mvtv)

	## material inputs on the largest parcel(s)
	rM$id <- key(rM)
	rM$kg <- tolower(trimws(rM$unit)) %in% "kg"
	# nutrient content (%, elemental) from terminag values_fertilizer_type; Gromor 28-28-0
	fert <- data.frame(
		specify = c("Urea", "DAP", "MOP", "Potash", "Gromor"),
		fertilizer_type = c("urea", "DAP", "KCl", "KCl", "Gromor"),
		N = c(46, 18, 0, 0, 28),
		P = c(0, 20.1, 0, 0, 28 / 2.29),
		K = c(0, 0, 49.8, 49.8, 0)
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
	om$type <- c(FYM="farmyard manure", "Karanja oilcake"="oil cake", "Neem oilcake"="oil cake")[trimws(om$specify)]
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
		OM_type = sapply(ids, \(x) paste(sort(unique(om$type[om$id == x])), collapse="; ")),
		OM_amount = sapply(ids, \(x) sum(om$qty[om$id == x])),
		seed_rate = sapply(ids, \(x) sum(sd$qty[sd$id == x])),
		insecticide_used = ids %in% rM$id[rM$matinputs %in% "Insecticide"],
		fungicide_used = ids %in% rM$id[rM$matinputs %in% "Fungicide"]
	)
	inp$fertilizer_type[inp$fertilizer_type == ""] <- "none"
	inp$OM_type[inp$OM_type == ""] <- NA
	inp$OM_type[!inp$OM_used] <- "none"
	inp$OM_amount[!inp$OM_used] <- 0
	inp$seed_rate[inp$seed_rate == 0] <- NA

	# labour: family + hired person-days per ha, summed over activities
	lab <- tapply(rL$laborPerHa, key(rL), sum)

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
	i <- match(key(rP), inp$id)
	dP <- cbind(dP, inp[i, -1])
	dP$labour <- as.vector(lab[key(rP)])

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
	d$country <- "India"
	d$adm1 <- "Odisha"
	d$adm2 <- rS$district[s]
	d$location <- trimws(rS$village[s])
	d$village_stress_ <- rS$stress[s]
	d$currency <- "INR"

	d$season <- tolower(d$season)
	d$variety_type <- unname(c(MV="modern", TV="traditional")[d$variety_type])
	d$planting_method <- unname(c(TPR="transplanting", DSR="direct seeding", WSR="direct seeding")[d$planting_method])
	d$landscape_position <- gsub("medium land", "medium", d$landscape_position)
	d$irrigation_source[tolower(d$irrigation_source) %in% "none"] <- "none"

	tex <- tolower(d$soil_type)
	tex[tex == "sandy clayloam"] <- "sandy clay loam"
	tex[tex %in% c("laterite", "grey", "red")] <- NA
	d$soil_texture <- tex

	# arhar = pigeon pea; bengalgram = chickpea; greengram = mung bean; colocasia = taro; ridgegourd = luffa
	crops <- c(blackgram="black gram", greengram="mung bean", bengalgram="chickpea", arhar="pigeon pea",
		brinjal="eggplant", chili="chili pepper", sesamum="sesame", bittergourd="bitter gourd",
		"ladys finger"="okra", ridgegourd="luffa", colocasia="taro", "green vegetable"="vegetable",
		vegetables="vegetable", lathyrus="grass pea")
	i <- match(d$crop, names(crops))
	d$crop[!is.na(i)] <- crops[i[!is.na(i)]]
	yp <- c(rice="grain", wheat="grain", "black gram"="seed", "mung bean"="seed", chickpea="seed",
		"pigeon pea"="seed", groundnut="seed", sesame="seed", mustard="seed", "grass pea"="seed",
		potato="tubers", eggplant="fruit", "chili pepper"="fruit", cucumber="fruit", "bitter gourd"="fruit",
		pumpkin="fruit", okra="fruit", luffa="fruit", tomato="fruit", cabbage="leaves", cauliflower="flowers")
	d$yield_part <- unname(yp[d$crop])

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
