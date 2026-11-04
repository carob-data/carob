# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI farm survey on rice crop establishment (TPR, WSR, DSR) in the UPRIIS irrigated command area, Nueva Ecija, Philippines; wet season 1998 and dry season 1998-99, 114 farmers.
# Main records: "Form A.xls" sheet "LInvent-Landuse-Riceland": one row per farmer x parcel x plot x subplot x season (1 = wet, 2 = dry).
# yield = PRODKG / PlantArea (PRODKG = cavans x kg/cavan). Rice is paddy.
# Planting and harvest dates are week codes (code description "Dates": 1-24 = Jan-Jun, 25-48 = Jul-Dec, 49-72 = Jan-Jun of the next year, 4 weeks per month).
#   Converted to "yyyy-mm", taking codes 1-48 as 1998 (wet season 1998 per Form C and the documentation).
# Barangay from HHInfo (Brgy code) decoded with the code description; variety and other codes decoded from "Code description.doc".
# Fertilizer from "Form B.xls" sheet "Fert" (up to 5 products per subplot, kg), merged by farmer, season, parcel, plot, subplot; nutrient grades from the code description (N-P2O5-K2O), P = P2O5/2.29, K = K2O/1.2051.

## ISSUES
# No coordinates. Georeference barangays (Bantug, Sapang Kawayan, Maragol, ... in Munoz, Talavera, Aliaga, Jaen, Cabanatuan; Nueva Ecija).
# The base year of the week codes (1998) is inferred, not stated. Week codes > 72 (e.g. 73) are mapped to July 1999.
# District codes 1-3 are not described in the code description (not mapped).
# Fertilizer products with unknown nutrient content (code 11 "Crop giant", code 21 not in code list, foliar, organic) make N/P/K NA for that record.
# Form B Cropest "Variety" disagrees with Form A VARIETY for some plots (e.g. farmer 1 plot 2: 18 vs 1); Form A was used.
# Many variety codes are mixtures ("IR64 + IR68"); kept as given.
# Crop failure reason (Fail: 1 drought, 2 floods, 3 rats, 4 snail, 5 weeds) kept as season_constraint.
# Not processed: HHInfo (household members), CEHist (establishment history 1996-99 and reasons for switching), Income, Livestock, OthInc, Form B Seedbed/Plow/Harrow/OtherLP/Cropest (labor, seed rate)/Insect/Fungi/Snails/Rats/Weed/Irrig/Harvest, Form C (perceptions).
# Only 268 of 432 land-use records have a matching Fert record; others have NA N/P/K.
# write_files: missing coordinates; fertilizer_type grades 0-17-0, 0-18-0, 16-20-0, 17-0-17 and "code 11", "code 21" not in terminag; NA yield for fallow/failed subplots; "low yield" for asparagus bean (sitao), check units.
# The Documentation.docx file table describes another (Orissa risk) dataset; only the header is relevant.
# Publication: Velasco, Cenas and Pandey (2000) Dryseeding Rice: The Emerging Trend in Fully Irrigated Areas. IRRI monograph (no DOI).

## NEW VARIABLES
# establishment_method_: rice establishment method code (TPR, WSR = wet seeded, DSR = dry seeded)


carob_script <- function(path) {

"
Dry Seeding Rice: The Emerging Trend in Fully Irrigated Area

The objective of the study is to document the changes in rice establishment methods in the command area of UPRIIS. It also aims to examine the economics of the 3 methods of rice establishment; transplanting (TPR), wetseeding (WSR) and dryseeding (DSR) in the irrigated areas and to determine farmer's reasons for their choice of method of rice establishment.
"

	uri <- "doi:10.7910/DVN/26169"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 0.5
	)

	fA <- ff[basename(ff) == "Form A.xls"]
	fB <- ff[basename(ff) == "Form B.xls"]
	r1 <- carobiner::read.excel(fA, sheet="LInvent-Landuse-Riceland", skip=5)
	r2 <- carobiner::read.excel(fA, sheet="HHInfo", skip=4)
	r3 <- carobiner::read.excel(fB, sheet="Fert", skip=8)

	brgy <- c("Bantug", "Sapang Kawayan", "Maragol", "Tondod", "Homestead 2", "Homestead 1", "Minabuyok", "Bagong Sikat",
		"Pula Buliran", "Pula Cabanatuan", "Betes Aliaga", "Bantug Talavera", "Hilera / Pakul", "Calabasa", "Papaya", "Luwang Kupang")
	varieties <- c("IR64", "IR36", "RC10", "IR10", "IR60", "RC2", "RC4", "RC20", "glutinous/IR29", "RC8", "IR62", "IR72",
		"RC6/C4-6", "IR75", "RC14", "RC12", "CB8", "IR64/IR60", "IR36/C4", "IR66", "B6", "Azucena", "IR64 + IR68",
		"RC8/IR64", "IR36/IR60", "IR58", "IR36, Glutinous, RC10", "IR36 + IR60", "IR36, Glutinous, CB8", "PSBRC28",
		"PSBRC18", "IR32", "IR54+IR64", "IR54", "IR28", "C18", "C14", "IR42", "C10", "IR52", "IR30", "IR74", "R10+IR74",
		"C90", "Fancy Rice", "IR56", "RC30", "IR58+IR64", "RC6", "IR42+IR18", "RC28+IR64", "IR54+IR74", "C32",
		"C10+IR64+IR66+R10", "C10 + C18", "IR74 + PSBRC28", "IR26", "PSBRC10 + PSBRC5 + PSBRC28", "PSBRC10 + IR64",
		"IR64 + C10", "IR64 + IR56", "IR64 + PSBRC10 + PSBRC20 + R10", "C10 + IR64 + IR66", "Dinorado")
	crops <- c("0"="none", "1"="rice", "2"="maize", "3"="onion", "4"="mung bean", "5"="pigeon pea", "6"="asparagus bean",
		"7"="bitter gourd", "8"="eggplant", "9"="mango", "10"="melon")
	# week code -> "yyyy-mm"
	wdate <- function(w) {
		m <- ((w - 1) %/% 4) %% 12 + 1
		y <- 1998 + (w - 1) %/% 48
		ifelse(is.na(w), NA, paste0(y, "-", formatC(m, width=2, flag="0")))
	}

	d <- data.frame(
		farmer = r1$FARMER,
		field_id = paste(r1$PARCEL, r1$PLOT, r1$SUBPLOT, sep="-"),
		season = c("wet", "dry")[r1$SEASON],
		crop = crops[as.character(r1$CROP)],
		field_size = r1$AREA,
		land_irrigated = r1$IRAREA,
		land_tenure = c("owner", "share tenant", "leasehold", "CLT", "mortgaged-in", "parents", "CARP")[r1$TENURE],
		soil_type = c("Galas", "Lagkitin", "Mistezo", "Maganda", "Second class", "Lupain", "Sapang bato", "First class", "Kaingin")[r1$SOIL],
		irrigation_source = c("1"="NIA", "2"="motor pump", "3"="creek", "4"="rainfall", "12"="motor pump; NIA", "13"="creek; NIA")[as.character(r1$IRSOURCE)],
		planting_date = wdate(r1$CEMNOWK),
		harvest_date = wdate(r1$HARNOWK),
		yield = r1$PRODKG / r1$PlantArea,
		crop_price = r1$PRICE,
		season_constraint = c("0"="none", "1"="drought", "2"="floods", "3"="rats", "4"="snail", "5"="weeds")[as.character(r1$Fail)],
		variety = varieties[r1$VARIETY],
		land_prep_times = as.integer(r1$NOPLOW + r1$NOHARR),
		establishment_method_ = c("DSR", "WSR", "TPR")[r1$CEMETHOD]
	)
	d$irrigated <- d$land_irrigated > 0
	d$planting_method <- c(DSR="direct seeding", WSR="direct seeding", TPR="transplanting")[d$establishment_method_]
	d$currency <- "PHP"

	# fertilizer: 5 product slots, grades N-P2O5-K2O
	grade <- data.frame(
		code = c(1, 2, 3, 4, 6, 9, 12, 13, 18),
		fertilizer_type = c("urea", "DAS", "NPK", "16-20-0", "KCl", "DAP", "0-17-0", "17-0-17", "0-18-0"),
		N = c(46, 21, 14, 16, 0, 18, 0, 17, 0),
		P = c(0, 0, 14, 20, 0, 46, 17, 0, 18) / 2.29,
		K = c(0, 0, 14, 0, 60, 0, 0, 17, 0) / 1.2051
	)
	r3$key <- paste(r3$Farmer, r3$Season, r3$Parcel, r3$Plot, r3$Subplot)
	tcols <- c("FType1", "FType2", "FType3...19", "FType3...23", "FType3...27")
	acols <- c("FAmt1", "FAmt2", "FAmt3...20", "FAmt3...24", "FAmt3...28")
	fl <- lapply(1:5, function(i) data.frame(key=r3$key, area=r3$Area, code=r3[[tcols[i]]], amount=r3[[acols[i]]]))
	fl <- do.call(rbind, fl)
	fl <- fl[!is.na(fl$code), ]
	fl <- merge(fl, grade, by="code", all.x=TRUE)
	fl$N <- fl$amount * fl$N / 100 / fl$area
	fl$P <- fl$amount * fl$P / 100 / fl$area
	fl$K <- fl$amount * fl$K / 100 / fl$area
	fl$fertilizer_type[is.na(fl$fertilizer_type)] <- paste0("code ", fl$code[is.na(fl$fertilizer_type)])
	fert <- data.frame(
		key = unique(r3$key)
	)
	fert$N_fertilizer <- as.vector(tapply(fl$N, fl$key, sum)[fert$key])
	fert$P_fertilizer <- as.vector(tapply(fl$P, fl$key, sum)[fert$key])
	fert$K_fertilizer <- as.vector(tapply(fl$K, fl$key, sum)[fert$key])
	fert$fertilizer_type <- as.vector(tapply(fl$fertilizer_type, fl$key, function(x) paste(unique(x), collapse="; "))[fert$key])
	# subplots in the Fert sheet without any product
	i <- is.na(fert$fertilizer_type)
	fert$fertilizer_type[i] <- "none"
	fert$N_fertilizer[i] <- fert$P_fertilizer[i] <- fert$K_fertilizer[i] <- 0

	d$key <- paste(r1$FARMER, r1$SEASON, r1$PARCEL, r1$PLOT, r1$SUBPLOT)
	d <- merge(d, fert, by="key", all.x=TRUE)

	hh <- r2[!duplicated(r2$Farmer), ]
	d$location <- brgy[hh$Brgy[match(d$farmer, hh$Farmer)]]
	d$hhid <- as.character(d$farmer)
	d$farmer <- d$key <- NULL

	d$country <- "Philippines"
	d$adm1 <- "Nueva Ecija"
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("asparagus bean", "mung bean", "pigeon pea")] <- "seed"

	carobiner::write_files(path, meta, d)
}
