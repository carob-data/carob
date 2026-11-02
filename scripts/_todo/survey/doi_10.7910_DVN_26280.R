# R script for "carob"
# license: GPL (>=3)

## NOTES
# STRASA baseline household survey, India (West Bengal), 300 households in Nadia, Purulia and North 24 Parganas districts (Project details.xlsx: year 2008).
# Same table layout as the other STRASA country datasets (codebook "STRASA_Baseline Var Lists.xls"). Area in ha, production in kg, prices in Rs/kg.
# Three record types are combined in d, identified by survey_module_:
#   "rice variety" (tbl04aricevariety): household x parcel x plot x subplot x season x rice variety; area, production, yield (= prod/area), establishment.
#   "land use" (tbl03landuse): household x parcel x plot x season x crop; all crops, including rice.
#   "largest parcel" (tbl06largestparcel): largest rice parcel per household and season, with variety, planting/harvest month and week, grain and straw production;
#     joined to tbl06matinputs and tbl06laborinput (household x season) for fertilizer, manure, seed, pesticide and labour.
# Fertilizer: N/P/K = product rate (qtyPerHa, kg/ha; unit "kg" only) x nutrient fraction (terminag values_fertilizer_type: urea, DAP, SSP, KCl, DAS for ammonium sulphate; 10-26-26 from its grade).
#   Zinc/micronutrient products are counted as 0 N/P/K.
#   The source NperHa/PperHa/KperHa are NOT used: P and K appear computed with swapped oxide factors, and DAP was given 10% N and 26% P2O5.
#   N/P/K set to 0 when the household-season has material input records but no fertilizer record; NA when a fertilizer of unknown composition was used ("14-35", "NPK", "Grow more", "Others").
# irrigation_source, soil, tenure for "rice variety" and "land use" records from tbl02landholding (farmerid x parcel x plot).
# District "North 24_Con" (salinity-control villages) and "North 24paraganas" -> North 24 Parganas.

## ISSUES
# No coordinates. Georeference villages (location) in Nadia, Purulia and North 24 Parganas districts.
# planting_date/harvest_date not set: only month and week are given (planting_month_, planting_week_, harvest_month_, harvest_week_). Crop year ambiguous (Project details "2008"; interviews mostly Mar-Aug 2009; dry-season rice planted Jan).
# Record types overlap: rice in "land use" is also in "rice variety", and the "largest parcel" is one of those plots. Do not sum production across survey_module_.
# "14-35" may be 14-35-14 (source assumed no K); composition unverified, so N/P/K NA for those parcels.
# Land-use seasons "pre wet" and "wet and dry season" are not terminag values.
# Crops not matched kept as is ("mixed crops", "night queen", "parwal" (pointed gourd), "tube rose"); yield_part NA for crops without an obvious harvested part.
# Land-use yields up to 40000 kg/ha and some 0 yields left as reported; some NA yields.
# interviewdate has implausible values (1930, 2029); not processed.
# Unmapped modules: tbl01hhinfo, tbl04breasonsgrowing, tbl05cropsdisposal, tbl06poweruse, tbl06workhours, tbl07-tbl13.
# Irrigation fees (hours, Rs), land rent and input costs not processed.
# The labour table has no laborPerHa column here; labour = totallabor / area summed over activities.
# fertilizer_type "NPK" is the 10-26-26 grade.
# Remaining write_files messages: missing latitude/longitude; unknown (new) variables; invalid season, crop and fertilizer_type terms (14-35, Estozinc, Grow more, Micronutrient, Others, Zinc); NA yield and yield_part.

## NEW VARIABLES
# survey_module_: source table of the record ("rice variety", "land use" or "largest parcel")
# village_stress_: stress domain of the sampled village (Submergence, Drought, Saline-affected, Salinity-controlled)
# variety_generation_: release period of the modern variety ("before 1990", "1990 and after")
# planting_month_, harvest_month_: month (3-letter abbreviation) of planting and harvest
# planting_week_, harvest_week_: week of the month (1st-4th) of planting and harvest
# residue_price_: price of rice straw (currency/kg)


carob_script <- function(path) {

"
STRASA = Stress Tolerant Rice for poor farmers in Africa and South Asia (India, West Bengal)

1. Analyze patterns of varietal adoption and the factors that influence the adoption of improved varieties.   2. Analyze the economics of rice production and returns to varietal change.  3. Assess farmers' livelihood strategies in stress-prone rainfed environments. 4. Understand the gender roles in rice production and women's participation in decision making.  5. Derive implications for technology design, targeting, and policy reforms for enhancing the impact of improved technologies in stress-prone areas.
"

	uri <- "doi:10.7910/DVN/26280"
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
	# nutrient content (%, elemental) from terminag values_fertilizer_type; 10-26-26 from the grade (P2O5/2.29, K2O/1.2051)
	fert <- data.frame(
		specify = c("Urea", "DAP", "SSP", "MOP", "Amonsulphate", "10-26-26", "Micronutrient", "Zinc", "Estozinc", "Zinc sulphate"),
		fertilizer_type = c("urea", "DAP", "SSP", "KCl", "DAS", "NPK", "Micronutrient", "Zinc", "Estozinc", "ZnSO4"),
		N = c(46, 18, 0, 0, 21, 10, 0, 0, 0, 0),
		P = c(0, 20.1, 8.74, 0, 0, 26 / 2.29, 0, 0, 0, 0),
		K = c(0, 0, 0, 49.8, 0, 26 / 1.2051, 0, 0, 0, 0)
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
	om$type <- c(FYM="farmyard manure", "Oil cake"="oil cake")[trimws(om$specify)]
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
		herbicide_used = ids %in% rM$id[rM$matinputs %in% "Herbicide"],
		insecticide_used = ids %in% rM$id[rM$matinputs %in% "Insecticide"]
	)
	inp$fertilizer_type[inp$fertilizer_type == ""] <- "none"
	inp$OM_type[inp$OM_type == ""] <- "none"
	inp$OM_amount[!inp$OM_used] <- 0
	inp$seed_rate[inp$seed_rate == 0] <- NA

	# labour: family + hired person-days (totallabor) per ha of the parcel, summed over activities
	rL$id <- paste(rL$farmerid, rL$season)
	lab <- tapply(rL$totallabor / rL$area, rL$id, sum)

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
	d$adm1 <- "West Bengal"
	d$adm2 <- rS$district[s]
	d$adm2[grepl("^North 24", d$adm2)] <- "North 24 Parganas"
	d$location <- trimws(rS$village[s])
	d$village_stress_ <- rS$stress[s]
	d$currency <- "INR"

	d$season <- tolower(d$season)
	d$season[d$season == "wet season"] <- "wet"
	d$season[d$season == "dry season"] <- "dry"
	d$variety_type <- unname(c(MV="modern", TV="traditional")[d$variety_type])
	d$planting_method <- unname(c(TPR="transplanting")[d$planting_method])
	d$landscape_position <- gsub("medium land", "medium", d$landscape_position)
	d$irrigation_source[tolower(d$irrigation_source) %in% "none"] <- "none"

	tex <- tolower(d$soil_type)
	tex[tex == "sandy"] <- "sand"
	tex[tex == "carcarious"] <- NA
	d$soil_texture <- tex

	# gram = chickpea; ridge gourd = luffa
	crops <- c(chilli="chili pepper", gram="chickpea", "ground nut"="groundnut", pulses="pulse",
		"ridge gourd"="luffa", vegetables="vegetable")
	i <- match(d$crop, names(crops))
	d$crop[!is.na(i)] <- crops[i[!is.na(i)]]
	yp <- c(rice="grain", wheat="grain", mustard="seed", lentil="seed", sesame="seed", chickpea="seed",
		groundnut="seed", pea="seed", potato="tubers", banana="fruit", "bitter gourd"="fruit",
		"chili pepper"="fruit", cucumber="fruit", okra="fruit", luffa="fruit", cabbage="leaves",
		marigold="flowers")
	d$yield_part <- unname(yp[d$crop])

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
