# R script for "carob"
# license: GPL (>=3)

## NOTES
# RICE-PRE station trials comparing three crop management packages ("treatment"): farmers' practice (FP / FPRAC),
#   "primitive yield" (PRI / PRIY; per "RICE-PRE Generic Protocol.docx" only land preparation as farmers' practice)
#   and the RICE-PRE prescription (RP / RPRE). 3-4 replicates per station.
# Mapped (consistent, non-duplicated subset = the ODK exports):
#   - RICEPRE_CropHealth_2014WS_2014_10_02...completedata.csv: IRRI Los Baños, 2014 wet season, visits 2-4.
#   - RICEPRE_CropHealth_2014WS_2015_04_21....csv: same ODK form name, but visits Feb-Apr 2015 = IRRI 2015 dry season.
#   - _home_..._RICEPRE_Crop_and_Injuries.csv: 2015 wet season, IRRI_LB, PhilRice_NE (Nueva Ecija), PhilRice_ADN (Agusan del Norte).
#   - _home_..._RICEPRE_Rat_Injuries.csv: 2015 wet season, IRRI_LB and PhilRice_ADN, 3 strata x 8 sampling points.
#   - _home_..._RICE_PRE_Crop_Cut.csv: IRRI 2015 wet season yields (mean of 3 crop cuts, t/ha at 14% moisture as computed in the file).
#   - "RICEPRE 2015DS- harvest.xlsx", sheet "per plot": IRRI 2015 dry season yields ("T/HA", adjusted to 14% moisture in the file).
# d (wide) = one record per station x season x treatment x replicate; dd (long) is linked with record_id and has these row types:
#   visit rows (visit_, DVS_, growth_stage, water_status_), hill rows (10 hills: tillers_hill_, panicles_hill_, leaves_tiller_),
#   hill injury rows (injury_, organ_, injury_count_, pest_species or disease, incidence), area injury rows (injured hills in
#   sub-areas A-E), weed rows by sub-area A-C, and rat-form rows (stratum_, point_).
# Injury codes and DVS stages decoded with "Crop Health Abbreviation Key.doc" (read with antiword) and the ODK field names.
#   Injured organ (tiller, leaf, panicle) follows the grouping of the 2015 form. Incidence (%) = 100 * injured organs /
#   (tillers, panicles, or tillers x leaves per tiller) of the same hill, as in the "Synthesis" sheets of the xls workbooks.
# Removed ODK submissions by "Training ..." (3, 2014 WS) and "test"/"Test1" (2, 2015 WS) researchers; they duplicate real plot visits.
# One 2015 date "10-Sep-04" corrected to "10-Sep-15" (all other visit 2 records at PhilRice NE).
# Coordinates: mean of the GPS readings in the forms per station and season.
# Variety (NSIC Rc222) is reported in the 2014WS/2015DS forms for all treatments; the generic protocol instead says
#   PSB Rc18 for FP/PRI and NSIC 222 for RICE-PRE (dry season). Data used.

## ISSUES
# 1. Not mapped: ~250 xls/xlsx crop-health workbooks (Form 1, Form 2 Visit 1-4, Summary Visit 1-4, Synthesis) covering
#    IRRI 2011 WS - 2015 DS, PhilRice NE 2012-2014, Negros (NEG) 2012 and ADN 2014-2015. They hold N/P/K rates and dates,
#    pesticides, crop establishment and harvest dates, field area and yield, plus hill counts per visit. Problems: 12 different
#    Summary layouts and two Form 2 templates; "Summary Visit 3/4" sheets are often labelled "Visit 2"; formula sheets show 0
#    for blanks; many duplicate names (_1, _2 suffixes) are different seasons/sites (identify them by the crop establishment
#    date "CED" and village in Form 1); yields are in t/ha in some and kg/ha in others. IRRI 2014 WS and 2015 DS workbooks
#    probably duplicate the ODK data mapped here. Parsing the raw "Form 1"/"Form 2" sheets per template is the way forward.
# 2. No fertilizer, planting/transplanting or management data in the ODK files (only in the xls Form 1); N/P/K omitted,
#    planting_date is year only. The treatment components per season are not given (only the generic protocol).
# 3. Yields are missing for IRRI 2014 WS (only in xls) and for PhilRice NE/ADN 2015 WS ("WS2015 harvest data.gsheet" is only a
#    Google Sheets link). Yield of IRRI 2015 WS primitive yield rep 4 is 674 kg/ha with three identical crop cuts: check.
#    2015 DS plot 402 harvest moisture is 14.7% vs ~25% for all other plots: check. The 2015 DS yield implies a 5 m2 sample
#    (weight x 2000); the crop cut area for 2015 WS is not stated.
# 4. Blank injury cells are kept as missing, but they probably mean 0 (zeros were only typed at the first 2015 DS visit).
# 5. Duplicate/missing submissions remain: 2015 DS farmers' practice rep 2 visit 2 submitted 3 times and primitive yield rep 2 /
#    RICE-PRE rep 2 missing at visit 2; several 2015 DS visit 3 records are dated 2015-04-13 (= visit 4 date); 2015 WS PhilRice NE
#    farmers' practice visit 1 has rep 3 twice and no rep 2; PhilRice ADN FP rep 2 visit 4 has DVS 70 and no researcher name.
# 6. Scales not documented: water status (1-9), weed rating above/below canopy (0-4 in 2014-2015 DS, 0/5/15/45/80 in 2015 WS,
#    possibly % cover class midpoints), weed type ranks (0-4), crop status variables (crop_status-* not mapped). Number of hills
#    per sub-area is not in the ODK files, so area injuries (bugburn, hopperburn, tungro etc.) are counts of injured hills only;
#    The 2014 WS values of 40 (hopperburn, bugburn) may be % rather than counts: check. "gas" assumed to be golden apple snail (area_pest_others text mentions
#    "Apple snail"/"Gas,20%"; free text not mapped). 2014 "pest_others"/"disease_others" counts without names not mapped.
#    Deadheart and whitehead in the 2014 forms do not separate stem borer from black bug, so pest_species is NA there.
# 7. Weed species are EPPO codes in the 2015 WS form and scientific names (presence list) in the 2014/2015 DS forms.
# 8. Rat form: one sampling point has 2025 total tillers (IRRI 2015 WS RICE-PRE rep 1, stratum 3, point 4), likely a typo.
# 9. Not mapped: "Crop_and_Injuries_Oct_23_2014_03_27.xlsx" (18 records of another ODK form with hill labels and contact
#    persons, location unclear), "DP18_rev_survey_portfolio.pdf", "RICE-PRE Prescriptions.doc" and "Snail management.doc"
#    (recommendations only). 24 restricted files (e.g. *_WS2014_raw.xlsx, 2015DS_RICEPRE_CropHealth...xlsx, concept note) were
#    not downloaded.
# 10. write_files: missing N/P/K_fertilizer; NA yield (see 3); many new variables in the long table; disease and pest names
#    are not yet in terminag (bacterial leaf blight, sheath blight, rice tungro, rice bug, leaffolder, ...).
# 11. adm2 not set: PhilRice CES is in Science City of Muñoz (Nueva Ecija), PhilRice Agusan in Remedios T. Romualdez
#    (Agusan del Norte), IRRI in Los Baños (Laguna); check GADM names.

## NEW VARIABLES
# visit_: visit number (1-4) of the crop health assessment
# DVS_: development stage code (10-100) from the abbreviation key; growth_stage has the decoded name
# water_status_: field water status score (scale not documented); per visit (2014/2015 DS) or per sub-area (2015 WS)
# hill_: hill number (1-10) within the plot
# tillers_hill_, panicles_hill_: number of tillers / panicles per hill
# leaves_tiller_: number of leaves per tiller
# injury_: injury name; organ_: injured unit (tiller, leaf, panicle, hill)
# injury_count_: number of injured organs per hill, injured hills per sub-area, or rat-injured tillers per sampling point
# pest_injury_incidence_: % of organs (tillers, panicles, leaves) injured by animal pests
# subarea_: sub-area of the plot (A-E) used for whole-area injuries and weeds
# weed_above_rating_, weed_below_rating_: weed rating above / below the rice canopy (scale see ISSUES 6)
# weed_rank_broadleaved_, weed_rank_grass_, weed_rank_sedge_, weed_rank_small_: rank of weed types
# stratum_, point_: stratum (1-3) and sampling point (1-8) of the rat injury form
# tillers_point_: number of tillers assessed at a rat-form sampling point

carob_script <- function(path) {

"
RICE-PRE - a concept for crop health syndrome model

RICE-PRE, a concept for crop health syndrome model  Adam Sparks and Serge Savary  Background  Plant disease management recommendations are a central premise of botanic epidemiology. Because many rice growers and their extension support systems are increasingly unable to accurately diagnose crop health issues it is important to find other ways to make recommen- dations which will be useful in preventing crop yield losses.  RICE-PRE was inspired by the EPIPRE model created by Zadoks [1981]. The premise of RICE-PRE is based upon agroecologies of rice growing areas as defined by Andy Nelson (IRRI, SSD GIS and Remote Sensing Lab). For each agroecology there is a combination of two agricultural objectives from these three: a - maximum yield; b - maximum quality of grain; c - minimal environmental impact, and the season: wet or dry. The combination of these factors allows for the construction of a crop health syndrome profile for the production system whereby a 'prescription' can be made. The prescription being field operations and crop protection strategies for what we predict to be the major causes of yield reductions.  Statistical analysis of survey data from 456 lowland rice farmers' fields in tropical and sub-tropical Asia indicate that despite a broad range of environments and possible yield- reducing factors, very strong statistical links were indicated between these syndromes and the production situations [Savary et al., 2011].  RICE-PRE is meant to be    strategic (before the season starts),  based on strong statistical bases making use of observational survey data collected in South and Southeast Asia in 450 rice fields,  make use of prophylactic and preventive tools, especially resistant varieties,  but can make use of preventive chemical protection as well, when the risks involved are too high to be accepted,  based on the recently developed typology of rice ecologies developed at IRRI,  which we combine with agricultural objectives and externalities (positive or negative).    

Bibliography  Serge Savary, Asimina Mila, Laetitia Willocquet, Paul Esker, Odile Carisse, and Neil McRoberts. Risk factors for crop health under global change and agricul- tural shifts: a framework of analyses using rice in tropical and subtropical asia as a model. Phytopathology, (ja), 2011. doi: 10.1094/PHYTO-07-10-0183. URL http://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-07-10-0183.  Jan C. Zadoks. EPIPRE: a disease and pest management system for winter wheat developed in the netherlands. EPPO Bulletin, 11(3):365–369, 1981. ISSN 1365-2338. doi: 10.1111/j.1365- 2338.1981.tb01945.x. URL http://dx.doi.org/10.1111/j.1365-2338.1981.tb01945.x.

Trials were conducted at IRRI starting in 2011 until current date of upload.

Trials were also conducted in conjunction with PhilRice at their stations,    Nueva Ecjia (NE),  Agusan del Norte (ADN), and  Negros (NEG).
"

	uri <- "doi:10.7910/DVN/VZP5CG"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = NA,
		project = "RICE-PRE",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield;disease_incidence",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 3
	)

	bn <- basename(ff)
	# ODK crop health form, IRRI Los Baños, 2014 wet season
	r1 <- read.csv(ff[bn == "RICEPRE_CropHealth_2014WS_2014_10_02_23_32_28_completedata.csv"], check.names=FALSE, na.strings=c("", "n/a"))
	# same form (still named "2014WS"), but visits Feb-Apr 2015: IRRI Los Baños, 2015 dry season
	r2 <- read.csv(ff[bn == "RICEPRE_CropHealth_2014WS_2015_04_21_23_40_32.csv"], check.names=FALSE, na.strings=c("", "n/a"))
	# ODK crop and injuries form, 2015 wet season, IRRI, PhilRice Nueva Ecija and PhilRice Agusan del Norte
	r3 <- read.csv(ff[bn == "_home_formcontroller_ODK_briefcase_RICEPRE_Crop_and_Injuries.csv"], check.names=FALSE, na.strings="")
	r4 <- read.csv(ff[bn == "_home_formcontroller_ODK_briefcase_RICEPRE_Rat_Injuries.csv"], check.names=FALSE, na.strings="")
	r5 <- read.csv(ff[bn == "_home_formcontroller_ODK_briefcase_RICE_PRE_Crop_Cut.csv"], check.names=FALSE)
	r6 <- carobiner::read.excel(ff[bn == "RICEPRE 2015DS- harvest.xlsx"], sheet="per plot")

	trt <- c("Farmers' Practice"="farmers' practice", "FPRAC"="farmers' practice", "Farmer's practice"="farmers' practice",
		"Primitive Yield"="primitive yield", "PRIY"="primitive yield", "Primitive yield"="primitive yield",
		"RICE-PRE"="RICE-PRE", "RPRE"="RICE-PRE", "RICE PRE"="RICE-PRE")
	visitno <- c("1st"=1L, "2nd"=2L, "3rd"=3L, "4th"=4L)
	# development stages (DVS) from "Crop Health Abbreviation Key.doc"
	dvs <- c("10"="seedling", "20"="tillering", "30"="stem elongation/panicle initiation", "40"="booting",
		"50"="heading", "60"="flowering", "70"="milk", "80"="dough", "90"="ripening", "100"="fully mature")

	## one row per form submission (plot x visit)
	visits14 <- function(r, season, year) {
		data.frame(
			form_id = r$`_uuid`,
			researcher = r$`group_contact/researcher_name`,
			station = r$`group_contact/station_name`,
			season = season,
			year = year,
			treatment = trt[r$`group_contact/treatment_name`],
			rep = as.integer(r$`group_contact/replicate_name`),
			visit = visitno[r$`group_crop_info/visitNo_label`],
			date = substr(r$datetime, 1, 10),
			DVS = as.integer(r$`group_crop_info/crop_stage`),
			water_status = as.integer(r$`group_crop_info/water_status`),
			variety = r$`group_crop_info/rice_variety`,
			latitude = as.numeric(r$`_gps1_latitude`),
			longitude = as.numeric(r$`_gps1_longitude`)
		)
	}
	v1 <- visits14(r1, "wet", 2014)
	v2 <- visits14(r2, "dry", 2015)

	# one 2015 assessment date was entered as "10-Sep-04"; all other visit 2 records at that station are "10-Sep-15"
	adate <- r3$`GenInfo-date_assessment`
	adate[adate == "10-Sep-04"] <- "10-Sep-15"
	v3 <- data.frame(
		form_id = r3$KEY,
		researcher = r3$`GenInfo-researcher_name`,
		station = r3$`GenInfo-station_name`,
		season = "wet",
		year = 2015,
		treatment = trt[r3$`GenInfo-treatment_name`],
		rep = as.integer(r3$`GenInfo-replicate_name`),
		visit = visitno[r3$`GenInfo-visitNo_label`],
		date = as.character(as.Date(adate, "%d-%b-%y")),
		DVS = as.integer(r3$`group_crop_info-crop_growth_stage`),
		latitude = r3$`barcode_number-gps1-Latitude`,
		longitude = r3$`barcode_number-gps1-Longitude`
	)

	v4 <- data.frame(
		form_id = r4$KEY,
		station = r4$`barcode_number-station_name`,
		season = "wet",
		year = 2015,
		treatment = trt[r4$`barcode_number-treatment_name`],
		rep = as.integer(r4$`barcode_number-replicate_name`),
		date = as.character(as.Date(r4$`barcode_number-date`, "%d-%b-%y")),
		DVS = as.integer(r4$`group_crop_info-crop_growth_stage`),
		latitude = r4$`gps1-Latitude`,
		longitude = r4$`gps1-Longitude`
	)

	## crop cuts, IRRI 2015 wet season; mean of 3 crop cuts, adjusted to 14% moisture by the data providers
	y5 <- data.frame(
		station = r5$`Geninfo-station_name`,
		season = "wet",
		year = 2015,
		treatment = trt[r5$`Geninfo-treatment_name`],
		rep = as.integer(r5$`Geninfo-replicate_name`),
		yield = r5$avecropcutyield * 1000,
		harvest_date = as.character(as.Date(r5$`Geninfo-datecropcut`, "%d-%b-%y"))
	)
	## plot harvest, IRRI 2015 dry season; mean of 3 samples per plot, "T/HA" adjusted to 14% moisture by the data providers
	y6 <- data.frame(
		station = "IRRI_LB",
		season = "dry",
		year = 2015,
		treatment = trt[r6$Treatment],
		rep = as.integer(r6$Replicate),
		plot_id = as.character(r6$`Plot ID`),
		yield = r6$`T/HA` * 1000,
		# weighing date of all samples in sheet "RICE PRE harvest"
		harvest_date = "2015-04-20"
	)

	## training and test submissions (duplicates of real records)
	vv <- carobiner::bindr(v1, v2, v3)
	vv <- vv[!grepl("^training|^test", vv$researcher, ignore.case=TRUE), ]
	vv$researcher <- NULL

	## plots: station x season x treatment x replicate
	keys <- c("station", "season", "year", "treatment", "rep")
	d <- unique(rbind(vv[, keys], v4[, keys], y5[, keys], y6[, keys]))
	d <- merge(d, rbind(y5[, c(keys, "yield", "harvest_date")], y6[, c(keys, "yield", "harvest_date")]), by=keys, all.x=TRUE)
	d <- merge(d, y6[, c(keys, "plot_id")], by=keys, all.x=TRUE)

	vv$variety <- ifelse(grepl("222", vv$variety), "NSIC Rc222", vv$variety)
	vr <- aggregate(variety ~ station + season + year + treatment + rep, data=vv, FUN=function(x) paste(unique(x), collapse=";"))
	d <- merge(d, vr, by=keys, all.x=TRUE)

	d$trial_id <- paste(d$station, d$year, d$season, sep="_")
	d$planting_date <- as.character(d$year)

	## GPS positions recorded with the forms, averaged by station and season
	gps <- rbind(vv[, c(keys, "latitude", "longitude")], v4[, c(keys, "latitude", "longitude")])
	gps <- aggregate(cbind(latitude, longitude) ~ station + season + year, data=gps, FUN=mean)
	d <- merge(d, gps, by=c("station", "season", "year"), all.x=TRUE)

	geo <- data.frame(
		station = c("IRRI_LB", "PhilRice_NE", "PhilRice_ADN"),
		adm1 = c("Laguna", "Nueva Ecija", "Agusan del Norte"),
		# PhilRice NE village "Maligaya" and PhilRice ADN village "Basilisa RTRomualdez" are from Form 1 in the xls workbooks
		location = c("IRRI, Los Baños", "PhilRice Central Experiment Station, Maligaya", "PhilRice Agusan, Basilisa, RT Romualdez")
	)
	d <- merge(d, geo, by="station", all.x=TRUE)

	d$country <- "Philippines"
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	# "RICE-PRE Generic Protocol.docx": agroecology 3, irrigated rice
	d$irrigated <- TRUE
	d$geo_from_source <- TRUE
	d$stress <- "pests;disease;weeds"
	d$station <- NULL
	d$year <- NULL
	d$record_id <- 1:nrow(d)

	## link each form submission to a record
	rid <- function(x) {
		x$trial_id <- paste(x$station, x$year, x$season, sep="_")
		d$record_id[match(paste(x$trial_id, x$treatment, x$rep), paste(d$trial_id, d$treatment, d$rep))]
	}
	vv$record_id <- rid(vv)
	v4$record_id <- rid(v4)

	## visit level observations
	lv <- data.frame(
		form_id = vv$form_id,
		record_id = vv$record_id,
		date = vv$date,
		visit_ = vv$visit,
		DVS_ = vv$DVS,
		growth_stage = dvs[as.character(vv$DVS)],
		water_status_ = vv$water_status
	)

	## hill level observations (10 hills per plot and visit)
	hills <- function(r, id) {
		hv <- grep("^group_[0-9]+[-/](group_number|group_pest|group_diseases|hill|group_others)", names(r), value=TRUE)
		data.frame(
			form_id = rep(id, length(hv)),
			hill_ = rep(as.integer(sub("^group_([0-9]+).*", "\\1", hv)), each=length(id)),
			var = rep(sub("^others[0-9]+_", "others_", sub("[0-9]+$", "", sub(".*[-/]", "", hv))), each=length(id)),
			value = trimws(as.character(unlist(r[hv], use.names=FALSE)))
		)
	}
	h <- rbind(hills(r1, r1$`_uuid`), hills(r2, r2$`_uuid`), hills(r3, r3$KEY))
	h <- h[!is.na(h$value) & h$form_id %in% vv$form_id, ]

	hc <- h[h$var %in% c("tiller_hill", "panicle_hill", "leaves_tiller"), ]
	hc$value <- as.numeric(hc$value)
	hc <- reshape(hc, idvar=c("form_id", "hill_"), timevar="var", direction="wide")
	names(hc) <- c("form_id", "hill_", "tillers_hill_", "panicles_hill_", "leaves_tiller_")[match(names(hc), c("form_id", "hill_", "value.tiller_hill", "value.panicle_hill", "value.leaves_tiller"))]

	# 2015 form: "other" injuries are written as a name and a number of injured tillers; all names are rat ("Rat", "Rt", "Ra")
	ho <- h[h$var %in% c("others_nametiller", "others_numbertiller"), ]
	ho <- reshape(ho, idvar=c("form_id", "hill_"), timevar="var", direction="wide")
	ho <- data.frame(
		form_id = ho$form_id,
		hill_ = ho$hill_,
		var = ifelse(tolower(ho$value.others_nametiller) %in% c("rat", "rt", "ra"), "pest_rat", ho$value.others_nametiller),
		value = ho$value.others_numbertiller
	)

	# injury names from "Crop Health Abbreviation Key.doc" and the ODK form field names.
	# The injured organ follows the grouping of the 2015 form (panicle, leaf, tiller injuries)
	ik <- data.frame(
		var = c("pest_bird", "pest_ricebug", "pest_ricegrainbug", "pest_whiteheadstemborer", "pest_whiteheadblackbug", "pest_whitehead",
			"disease_dirtypanicle", "disease_falsesmut", "disease_neckblast",
			"pest_leaffolder", "pest_leafminer", "pest_thrip", "pest_whorlmaggot", "pest_defoliators",
			"disease_bacterialleafblight", "disease_bacterialleafstreak", "disease_brownspot", "disease_leafblast",
			"disease_leafscald", "disease_narrowbrownspot", "disease_redstripe",
			"pest_deadheartstemborer", "pest_deadheartblackbug", "pest_deadheart", "pest_rat",
			"disease_bakanae", "disease_sheathblight", "disease_sheathrot", "disease_stemrot"),
		injury_ = c("bird injury", "rice bug injury", "rice grain bug injury", "whitehead", "whitehead", "whitehead",
			"dirty panicle", "false smut", "neck blast",
			"leaffolder injury", "leafminer injury", "thrips injury", "whorl maggot injury", "defoliator injury",
			"bacterial leaf blight", "bacterial leaf streak", "brown spot", "leaf blast",
			"leaf scald", "narrow brown spot", "red stripe",
			"deadheart", "deadheart", "deadheart", "rat injury",
			"bakanae", "sheath blight", "sheath rot", "stem rot"),
		organ_ = rep(c("panicle", "leaf", "tiller"), c(9, 12, 8)),
		pest_species = c("birds", "rice bug", "rice grain bug", "stem borer", "rice black bug", NA,
			NA, NA, NA,
			"rice leaffolder", "leafminer", "thrips", "rice whorl maggot", "defoliators",
			rep(NA, 7),
			"stem borer", "rice black bug", NA, "rats",
			NA, NA, NA, NA),
		disease = c(rep(NA, 6), "dirty panicle", "false smut", "neck blast",
			rep(NA, 5),
			"bacterial leaf blight", "bacterial leaf streak", "brown spot", "leaf blast",
			"leaf scald", "narrow brown spot", "red stripe",
			rep(NA, 4),
			"bakanae", "sheath blight", "sheath rot", "stem rot")
	)
	hi <- rbind(h[h$var %in% ik$var, ], ho)
	hi$injury_count_ <- as.numeric(hi$value)
	hi <- merge(hi[, c("form_id", "hill_", "var", "injury_count_")], ik, by="var")
	hi <- merge(hi, hc, by=c("form_id", "hill_"), all.x=TRUE)
	nunits <- ifelse(hi$organ_ == "tiller", hi$tillers_hill_,
		ifelse(hi$organ_ == "panicle", hi$panicles_hill_, hi$tillers_hill_ * hi$leaves_tiller_))
	nunits[nunits == 0] <- NA
	inc <- round(100 * hi$injury_count_ / nunits, 2)
	hi$disease_incidence <- ifelse(is.na(hi$disease), NA, as.character(inc))
	hi$pest_injury_incidence_ <- ifelse(is.na(hi$disease), inc, NA)
	hi$var <- hi$tillers_hill_ <- hi$panicles_hill_ <- hi$leaves_tiller_ <- NULL

	## whole-area injuries (number of injured hills in area A-E)
	areas <- function(r, id) {
		av <- grep("[-/]area_(bugburn|hopperburn|gas|tungro|grassy|ragged|orangeleaf|oramgeleaf|yellowdwarf)[A-E]$", names(r), value=TRUE)
		data.frame(
			form_id = rep(id, length(av)),
			subarea_ = rep(sub(".*([A-E])$", "\\1", av), each=length(id)),
			var = rep(sub(".*area_([a-z]+)[A-E]$", "\\1", av), each=length(id)),
			injury_count_ = as.numeric(unlist(r[av], use.names=FALSE))
		)
	}
	ha <- rbind(areas(r1, r1$`_uuid`), areas(r2, r2$`_uuid`), areas(r3, r3$KEY))
	ha <- ha[!is.na(ha$injury_count_) & ha$form_id %in% vv$form_id, ]
	ak <- data.frame(
		var = c("bugburn", "hopperburn", "gas", "tungro", "grassy", "ragged", "orangeleaf", "oramgeleaf", "yellowdwarf"),
		injury_ = c("bugburn", "hopperburn", "golden apple snail injury", "rice tungro", "rice grassy stunt", "rice ragged stunt", "rice orange leaf", "rice orange leaf", "rice yellow dwarf"),
		pest_species = c(NA, "planthoppers", "golden apple snail", NA, NA, NA, NA, NA, NA),
		disease = c(NA, NA, NA, "rice tungro", "rice grassy stunt", "rice ragged stunt", "rice orange leaf", "rice orange leaf", "rice yellow dwarf")
	)
	ha <- merge(ha, ak, by="var")
	ha$organ_ <- "hill"
	ha$var <- NULL

	## weeds by area (A-C)
	weeds <- function(r, id) {
		last <- sub(".*[-/]", "", names(r))
		wv <- grep("^(weedabove_area|weedbelow_area|weed_broadleaved|weed_grass|weed_sedge|weed_small|water_status|weeds_broadleaved|weeds_grass|weeds_sedge|weed_otherarea)[ABC]$", last)
		# 2014/2015 form: presence (TRUE/FALSE) of listed weed species
		sv <- grep("weed_listarea[ABC]/", names(r))
		x <- data.frame(
			form_id = rep(id, length(wv)),
			subarea_ = rep(sub(".*([ABC])$", "\\1", last[wv]), each=length(id)),
			var = rep(sub("_?area$", "", sub("[ABC]$", "", last[wv])), each=length(id)),
			value = trimws(as.character(unlist(r[wv], use.names=FALSE)))
		)
		if (length(sv) > 0) {
			s <- data.frame(
				form_id = rep(id, length(sv)),
				subarea_ = rep(sub(".*weed_listarea([ABC])/.*", "\\1", names(r)[sv]), each=length(id)),
				var = "weeds_list",
				value = rep(gsub("_", " ", sub(".*/", "", names(r)[sv])), each=length(id)),
				present = as.logical(unlist(r[sv], use.names=FALSE))
			)
			s <- s[s$present %in% TRUE, ]
			s$present <- NULL
			x <- rbind(x, s)
		}
		x[!is.na(x$value), ]
	}
	hw <- rbind(weeds(r1, r1$`_uuid`), weeds(r2, r2$`_uuid`), weeds(r3, r3$KEY))
	hw <- hw[hw$form_id %in% vv$form_id, ]
	spp <- hw[hw$var %in% c("weeds_broadleaved", "weeds_grass", "weeds_sedge", "weed_other", "weeds_list"), ]
	spp <- aggregate(value ~ form_id + subarea_, data=spp, FUN=function(x) paste(unlist(strsplit(x, " (?=[A-Z]{3})", perl=TRUE)), collapse="; "))
	names(spp)[3] <- "weed_species"
	wr <- hw[hw$var %in% c("weedabove", "weedbelow", "weed_broadleaved", "weed_grass", "weed_sedge", "weed_small", "water_status"), ]
	wr$value <- as.numeric(wr$value)
	wr <- reshape(wr, idvar=c("form_id", "subarea_"), timevar="var", direction="wide")
	wnames <- c(value.weedabove="weed_above_rating_", value.weedbelow="weed_below_rating_",
		value.weed_broadleaved="weed_rank_broadleaved_", value.weed_grass="weed_rank_grass_",
		value.weed_sedge="weed_rank_sedge_", value.weed_small="weed_rank_small_", value.water_status="water_status_")
	i <- names(wr) %in% names(wnames)
	names(wr)[i] <- wnames[names(wr)[i]]
	hw <- merge(wr, spp, by=c("form_id", "subarea_"), all=TRUE)

	## rat injury form (2015 wet season): 3 strata x 8 sampling points, total and rat-injured tillers
	rv <- grep("^stratum_[0-9]-sp[0-9]_[0-9]?(total|injured)$", names(r4), value=TRUE)
	rt <- data.frame(
		form_id = rep(r4$KEY, length(rv)),
		stratum_ = rep(as.integer(sub("^stratum_([0-9]).*", "\\1", rv)), each=nrow(r4)),
		point_ = rep(as.integer(sub("^stratum_[0-9]-sp([0-9]).*", "\\1", rv)), each=nrow(r4)),
		var = rep(sub(".*(total|injured)$", "\\1", rv), each=nrow(r4)),
		value = unlist(r4[rv], use.names=FALSE)
	)
	rt <- reshape(rt, idvar=c("form_id", "stratum_", "point_"), timevar="var", direction="wide")
	rt <- data.frame(
		form_id = rt$form_id,
		stratum_ = rt$stratum_,
		point_ = rt$point_,
		tillers_point_ = rt$value.total,
		injury_count_ = rt$value.injured,
		pest_injury_incidence_ = round(100 * rt$value.injured / rt$value.total, 2),
		injury_ = "rat injury",
		pest_species = "rats",
		organ_ = "tiller"
	)
	rt <- merge(rt, data.frame(form_id=v4$form_id, record_id=v4$record_id, date=v4$date, DVS_=v4$DVS, growth_stage=dvs[as.character(v4$DVS)]), by="form_id")

	fv <- lv[, c("form_id", "record_id", "date")]
	dd <- carobiner::bindr(lv, merge(fv, hc, by="form_id"), merge(fv, hi, by="form_id"), merge(fv, ha, by="form_id"), merge(fv, hw, by="form_id"), rt)
	dd$form_id <- NULL

	carobiner::write_files(path, meta, d, long=dd)
}
