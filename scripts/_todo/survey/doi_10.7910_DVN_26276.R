# R script for "carob"
# license: GPL (>=3)

## NOTES
# STRASA baseline household survey, India (Chhattisgarh), 100 households in 4 drought-prone villages of Raipur district (Project details.xlsx: year 2008).
# Same tables as the other STRASA country datasets, but as sheets of "CHATTISGARH_DATA 1.xls" (production, inputs) and "CHATTISGARH_DATA 2.xls" (social modules).
# Codebook "STRASA_Baseline Var Lists.xls". Area in ha, production in kg, prices in Rs/kg.
# Three record types are combined in d, identified by survey_module_:
#   "rice variety" (sheet "Rice variety"): household x parcel x plot x subplot x season x rice variety; area, production, yield (= prod/area), establishment.
#   "land use" (sheet "Land use"): household x parcel x plot x season x crop; all crops, including rice.
#   "largest parcel" (sheet "Largest parcel"): largest rice parcel per household and season, with variety, planting/harvest month and week, grain and straw production;
#     joined to sheets "Material inputs" and "Labor inputs" (household x season) for fertilizer, manure, seed, pesticide and labour.
# Fertilizer: N/P/K = product rate (qtyPerHa, kg/ha; unit "kg" only) x nutrient fraction (terminag values_fertilizer_type: urea, DAP, SSP, KCl for Potash/MOP).
#   The source NperHa/PperHa/KperHa are NOT used: P and K appear computed with swapped oxide factors, and SSP was given 17% N.
#   N/P/K set to 0 when the household-season has material input records but no fertilizer record.
# Soil names are local (Kanhar, Matasi, Dorsa, Bhata) and kept in soil_type; soil_texture not set.
# irrigation_source, soil, tenure for "rice variety" and "land use" records from sheet "Landholding" (farmerid x parcel x plot).

## ISSUES
# No coordinates. Georeference villages Khairkunt, Saguni, Sankara, Tarpongi (location), Raipur district.
# planting_date/harvest_date not set: only month and week are given (planting_month_, planting_week_, harvest_month_, harvest_week_). Crop year ambiguous (Project details "2008"; interviews Jan-Feb 2009; Rabi rice planted Jan, harvested Apr-May).
# Record types overlap: rice in "land use" is also in "rice variety", and the "largest parcel" is one of those plots. Do not sum production across survey_module_.
# Irrigation fees ("Monthly", "per hectare"), rental value and input costs not processed. Lathyrus is a relay crop sown before the rice harvest (comment in "Non-rice costs").
# Unmapped sheets: Household info, Non-farm labor, Non-rice costs, Work hours, Power use, Crop disposal, Reasons for growing, and all of DATA 2 (training, organizations, decisions, awareness, adjustment, consumption, borrowing, income, animal sales).
# Remaining write_files messages: missing latitude/longitude; unknown (new) variables.

## NEW VARIABLES
# survey_module_: source table of the record ("rice variety", "land use" or "largest parcel")
# village_stress_: stress domain of the sampled village (Drought)
# planting_month_, harvest_month_: month (3-letter abbreviation) of planting and harvest
# planting_week_, harvest_week_: week of the month (1st-4th) of planting and harvest
# residue_price_: price of rice straw (currency/kg)


carob_script <- function(path) {

"
STRASA = Stress Tolerant Rice for poor farmers in Africa and South Asia (India, Chhattisgarh)

1. Analyze patterns of varietal adoption and the factors that influence the adoption of improved varieties.   2. Analyze the economics of rice production and returns to varietal change.  3. Assess farmers' livelihood strategies in stress-prone rainfed environments. 4. Understand the gender roles in rice production and women's participation in decision making.  5. Derive implications for technology design, targeting, and policy reforms for enhancing the impact of improved technologies in stress-prone areas.
"

	uri <- "doi:10.7910/DVN/26276"
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

	f <- ff[basename(ff) == "CHATTISGARH_DATA 1.xls"]
	rd <- function(sheet) carobiner::read.excel(f, sheet=sheet, guess_max=100000)
	rS <- rd("Survey info")
	rH <- rd("Landholding")
	rU <- rd("Land use")
	rV <- rd("Rice variety")
	rP <- rd("Largest parcel")
	rM <- rd("Material inputs")
	rL <- rd("Labor inputs")

	## material inputs on the largest parcel, by household x season
	rM$id <- paste(rM$farmerid, rM$season)
	rM$kg <- tolower(trimws(rM$unit)) %in% "kg"
	# nutrient content (%, elemental) from terminag values_fertilizer_type
	fert <- data.frame(
		specify = c("Urea", "DAP", "SSP", "MOP", "Potash"),
		fertilizer_type = c("urea", "DAP", "SSP", "KCl", "KCl"),
		N = c(46, 18, 0, 0, 0),
		P = c(0, 20.1, 8.74, 0, 0),
		K = c(0, 0, 0, 49.8, 49.8)
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
	om$type <- c(FYM="farmyard manure", Compost="compost")[trimws(om$specify)]
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
		insecticide_used = ids %in% rM$id[rM$matinputs %in% "Insecticide"],
		fungicide_used = ids %in% rM$id[rM$matinputs %in% "fungicide"]
	)
	inp$fertilizer_type[inp$fertilizer_type == ""] <- "none"
	inp$OM_type[inp$OM_type == ""] <- "none"
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
	d$country <- "India"
	d$adm1 <- "Chhattisgarh"
	d$adm2 <- rS$district[s]
	d$location <- trimws(rS$village[s])
	d$village_stress_ <- rS$stress[s]
	d$currency <- "INR"

	d$season <- tolower(d$season)
	d$variety_type <- unname(c(MV="modern", TV="traditional")[d$variety_type])
	d$planting_method <- unname(c(TPR="transplanting", DSR="direct seeding")[d$planting_method])
	d$landscape_position <- gsub("medium land", "medium", d$landscape_position)
	d$irrigation_source[tolower(d$irrigation_source) %in% "none"] <- "none"

	# lathyrus = grass pea; gram = chickpea; masoor = lentil
	crops <- c(lathyrus="grass pea", gram="chickpea", masoor="lentil")
	i <- match(d$crop, names(crops))
	d$crop[!is.na(i)] <- crops[i[!is.na(i)]]
	yp <- c(rice="grain", wheat="grain", "grass pea"="seed", chickpea="seed", lentil="seed")
	d$yield_part <- unname(yp[d$crop])

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
