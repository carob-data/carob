# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. 24 restricted Dataverse files (11 IRRI 2014 WS raw workbooks, 1 IRRI 2015 DS workbook, 12 ADN 2012 FP/PRI workbooks,
#    and the concept note PDF) were not downloaded; ADN 2012 DS/WS therefore only include the RICE-PRE treatment.
# 2. Yields are missing for: IRRI 2012 DS (blank in Form 1), IRRI 2014 WS (restricted workbooks), PhilRice NE/ADN 2015 WS
#    ("WS2015 harvest data.gsheet" is only a Google Sheets link), 3 ADN 2015 DS plots, and 2014 DS ADN (9 files report
#    whole-plot fresh weights of 121-362 kg at 22.33% MC without stating plot area; if 750 m2 as in other ADN seasons,
#    adjusted yields would be 1.46-4.36 t/ha).
# 3. Yield / harvest anomalies kept as reported for review:
#    - 2014 WS PhilRice NE "Famers practice II_1.xlsx": 2nd crop-cut area is entered as 53 (vs 5600 and 5800 g/10 m2),
#      likely missing two zeros (5300); included as reported.
#    - 2015 WS IRRI ODK crop cut: primitive yield rep 4 has 674 kg/ha with three identical crop cuts.
#    - 2015 DS IRRI harvest xlsx: plot 402 harvest moisture is 14.7% vs ~25% for all other plots.
# 4. Fertilizer rates are missing (NA) for ODK-only seasons (IRRI 2014 WS, IRRI 2015 DS, all 2015 WS), 2014 DS ADN,
#    2013 WS ADN (except 2 plots), and 2014 WS / 2015 DS ADN (product weights per plot are given without plot area).
#    Form 1 N/P/K rates follow Philippine oxide convention (N-P2O5-K2O) and are converted to elemental P (/2.29) and K (/1.2051).
# 5. Date typos corrected from station/season context:
#    - 2011 WS IRRI (7 .xls files): Form 2 Visit 3/4 dates entered as 2010-09-14/24/28 -> corrected to 2011.
#    - 2012 DS PhilRice Negros (9 .xls files): CED entered as 2012-12-27 (Visit 1 is 2012-02-09) -> corrected to 2011-12-27.
#    - 2013 DS PhilRice NE: Form 2 Visit dates "3/115/2013" and "4/116/2013" -> corrected to "2013-03-15" / "2013-04-16".
#    - 2014 DS PhilRice NE: Form 2 Visit 2/3 dates entered as 2013-03-12 / 2013-04-01 in 2 files -> corrected to 2014.
#    - 2015 DS PhilRice ADN ("Primitive R1_1.xls"): Form 2 Visit 2 date entered as 2020-04-14 -> corrected to 2015-04-14.
#    - 2015 WS PhilRice NE ODK: one assessment date "10-Sep-04" -> corrected to "2015-09-10".
# 6. Duplicate/incomplete ODK submissions remain: 2015 DS IRRI FP rep 2 visit 2 submitted 3 times (PRI/RP rep 2 missing at
#    visit 2); some 2015 DS visit 3 records dated 2015-04-13 (= visit 4 date); 2015 WS PhilRice NE FP visit 1 has rep 3
#    twice and no rep 2; 2015 WS rat form IRRI RP rep 1 stratum 3 point 4 has 2025 total tillers (likely typo).
# 7. Unlabeled ADN 2014 DS / 2014 WS / 2015 DS workbooks ("Farmers Practice R1..3.xls", "Primitive R1..3.xls",
#    "Rice Pre R1..3.xls") have blank village/field fields; station assigned to PhilRice Agusan (ADN) based on the
#    chronological station upload sequence (ADN -> IRRI -> NE -> NEG) in the Dataverse manifest. One 2014 WS ADN file
#    ("Farmers practice R2.xls") was overwritten/absent due to case-insensitive filename collision on download.
# 8. Weed cover ratings use two documented scales: 0-4 classes in Excel Form 2 and 2014/2015 DS ODK (0=0%, 1=<10%,
#    2=10-30%, 3=30-60%, 4=>60% per DP18 p. 11) vs % cover class midpoints (0/5/15/45/80) in 2015 WS ODK.
#    Weed species use free-text scientific/common names in Excel and 2014/2015 DS ODK vs EPPO codes in 2015 WS ODK.

## NEW
# Variables (wide):
# - fallow_weeks_: duration of fallow period prior to rice crop (weeks)
# - lodging_: percentage of field area lodged at last visit (%)
# Variables (long):
# - visit_: crop health assessment visit number (1-4; "2b" recorded as 2.5 where present)
# - DVS_: rice development stage code (10-100 per DP18 survey portfolio; decoded in growth_stage)
# - water_status_: field water status code (1-11 per DP18 p. 10: 1-5 no standing water, 6-7 <5 cm, 8-9 5-15 cm, 10-11 >15 cm)
# - hill_: sampled hill/quadrat number (1-10) within the plot
# - tillers_hill_, panicles_hill_: count of tillers / panicles per hill
# - leaves_tiller_: mean number of leaves per tiller on the sampled hill
# - injury_: name of pest or disease injury (e.g. deadheart, whitehead, bugburn, leaf blast)
# - organ_: assessed plant unit (tiller, leaf, panicle, hill, insect)
# - injury_count_: count of injured organs per hill, injured hills per sub-area, or rat-injured tillers per sampling point
# - pest_injury_incidence_: percentage of organs (tillers, panicles, leaves) injured by animal pests (%)
# - sweep_: sweep-net sample number (1-5, each 10 strokes) in 2011-2012 Form 2
# - subarea_: plot sub-area (A-E for systemic/area injuries, A-C for weeds, or "50 hills" in 2011-2012 Form 2)
# - weed_above_rating_, weed_below_rating_: weed cover rating above / below rice canopy (0-4 classes in Excel and 2014-2015 DS ODK; % cover midpoints in 2015 WS ODK)
# - weed_rank_broadleaved_, weed_rank_grass_, weed_rank_sedge_, weed_rank_small_: dominance rank of weed types (0=absent, 1=most frequent)
# - stratum_, point_: stratum (1-3) and sampling point (1-8) of the 2015 WS rat injury form
# - tillers_point_: total number of tillers assessed at a rat-form sampling point
# Unmatched controlled-vocabulary values:
# - fungicide_product: edifenphos (Hinosan)
# - pest_species: armyworm, brown planthopper, defoliators, gall midge, golden apple snail, green leafhopper,
#     leafminer, planthoppers, rats, rice black bug, rice bug, rice caseworm, rice grain bug, rice hispa,
#     rice leaffolder, rice whorl maggot, thrips, white leafhopper, whitebacked planthopper, zigzag leafhopper
# - disease: bacterial leaf blight, bacterial leaf streak, bakanae, brown spot, dirty panicle, false smut,
#     leaf blast, leaf scald, leaf smut, narrow brown spot, neck blast, red stripe, rice grassy stunt,
#     rice orange leaf, rice ragged stunt, rice tungro, rice yellow dwarf, sheath rot, yellowing syndrome

## NOTES
# Multi-season RICE-PRE station experiments (2011 WS - 2015 WS, 9 seasons) at IRRI Los Baños (Laguna) and three PhilRice
# stations (Central Experiment Station in Nueva Ecija, Agusan del Norte, and Negros Occidental), comparing three management
# packages: farmers' practice (FP), primitive yield (PRI), and RICE-PRE prescription (RP) in 3-4 replicates.
# Standardizes ALL downloadable data files: 251 Excel crop-health workbooks (Form 1 management/yield + Form 2 Visit 1-4
# hill/area/weed/insect assessments), the 2014 DS PhilRice NE ODK workbook (Crop_and_Injuries_Oct_23_2014_03_27.xlsx,
# which supplies Visits 1 & 4 complementing Excel Visits 2 & 3), 5 ODK CSVs (2014 WS - 2015 WS), and 2015 DS IRRI harvest xlsx.
# Scales and abbreviations follow bundled "DP18_rev_survey_portfolio.pdf" (Savary & Castilla 2009) and "Crop Health Abbreviation Key.doc".
# Coordinates use recorded GPS where available (geo_from_source = TRUE) and station coordinates otherwise (geo_from_source = FALSE).

## NOT PROCESSED
# - Summary Visit 1-4 and Synthesis sheets in the 251 Excel workbooks: redundant formula summaries of Form 1 and Form 2.
# - Individual crop-cut sub-samples (3 cuts per plot in Form 1, RICE_PRE_Crop_Cut.csv, RICEPRE 2015DS- harvest.xlsx):
#   averaged to plot-level yield (kg/ha at 14% MC); individual cut weights not stored separately.
# - Form 1 chemical dose/volume strings (mixed tank-load units) and individual fertilizer/pesticide application dates.
# - Form 2 free-text "Others [2]" remarks, beneficial insect remarks, and ODK area_pest_others / area_disease_others free text.
# - Non-data reference documents: DP18_rev_survey_portfolio.pdf, RICE-PRE Generic Protocol.docx, RICE-PRE Prescriptions.doc,
#   Snail management.doc, Crop Health Abbreviation Key.doc, WS2015 harvest data.gsheet (broken link), Icon (0 bytes).

carob_script <- function(path) {

"
RICE-PRE - a concept for crop health syndrome model

RICE-PRE, a concept for crop health syndrome model  Adam Sparks and Serge Savary  Background  Plant disease management recommendations are a central premise of botanic epidemiology. Because many rice growers and their extension support systems are increasingly unable to accurately diagnose crop health issues it is important to find other ways to make recommen- dations which will be useful in preventing crop yield losses.  RICE-PRE was inspired by the EPIPRE model created by Zadoks [1981]. The premise of RICE-PRE is based upon agroecologies of rice growing areas as defined by Andy Nelson (IRRI, SSD GIS and Remote Sensing Lab). For each agroecology there is a combination of two agricultural objectives from these three: a - maximum yield; b - maximum quality of grain; c - minimal environmental impact, and the season: wet or dry. The combination of these factors allows for the construction of a crop health syndrome profile for the production system whereby a 'prescription' can be made. The prescription being field operations and crop protection strategies for what we predict to be the major causes of yield reductions.  Statistical analysis of survey data from 456 lowland rice farmers' fields in tropical and sub-tropical Asia indicate that despite a broad range of environments and possible yield- reducing factors, very strong statistical links were indicated between these syndromes and the production situations [Savary et al., 2011].  RICE-PRE is meant to be    strategic (before the season starts),  based on strong statistical bases making use of observational survey data collected in South and Southeast Asia in 450 rice fields,  make use of prophylactic and preventive tools, especially resistant varieties,  but can make use of preventive chemical protection as well, when the risks involved are too high to be accepted,  based on the recently developed typology of rice ecologies developed at IRRI,  which we combine with agricultural objectives and externalities (positive or negative).

Bibliography  Serge Savary, Asimina Mila, Laetitia Willocquet, Paul Esker, Odile Carisse, and Neil McRoberts. Risk factors for crop health under global change and agricul- tural shifts: a framework of analyses using rice in tropical and subtropical asia as a model. Phytopathology, (ja), 2011. doi: 10.1094/PHYTO-07-10-0183. URL http://apsjournals.apsnet.org/doi/abs/10.1094/PHYTO-07-10-0183.  Jan C. Zadoks. EPIPRE: a disease and pest management system for winter wheat developed in the netherlands. EPPO Bulletin, 11(3):365-369, 1981. ISSN 1365-2338. doi: 10.1111/j.1365- 2338.1981.tb01945.x. URL http://dx.doi.org/10.1111/j.1365-2338.1981.tb01945.x.

Trials were conducted at IRRI starting in 2011 until current date of upload.

Trials were also conducted in conjunction with PhilRice at their stations,    Nueva Ecjia (NE),  Agusan del Norte (ADN), and  Negros (NEG).
"

	uri <- "doi:10.7910/DVN/VZP5CG"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = NA,
		project = "RICE-PRE",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield;disease_incidence;pest_incidence",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	bn <- basename(ff)

	## ---- shared lookup tables ----
	trt_map <- c(
		"Farmers' Practice"="farmers' practice", "FPRAC"="farmers' practice", "Farmer's practice"="farmers' practice",
		"Primitive Yield"="primitive yield", "PRIY"="primitive yield", "Primitive yield"="primitive yield",
		"RICE-PRE"="RICE-PRE", "RPRE"="RICE-PRE", "RICE PRE"="RICE-PRE"
	)
	visitno <- c("1st"=1, "2nd"=2, "3rd"=3, "4th"=4)
	# development stages (DVS) from DP18 p. 9 and "Crop Health Abbreviation Key.doc"
	dvs_map <- c(
		"10"="seedling", "20"="tillering", "30"="stem elongation/panicle initiation", "40"="booting",
		"50"="heading", "60"="flowering", "70"="milk", "80"="dough", "90"="ripening", "100"="fully mature"
	)
	status_from_score <- function(x) {
		x <- as.numeric(x)
		ifelse(is.na(x), NA_character_,
		ifelse(x >= 15, "very good",
		ifelse(x >= 12, "good",
		ifelse(x >= 8, "average",
		ifelse(x >= 6, "poor", "very poor")))))
	}
	clean_variety <- function(x) {
		x <- trimws(x)
		x[x %in% c("", "Traditional variety", "Modern variety", "Hybrid variety")] <- NA_character_
		x <- sub("\\s+(GS|RS)$", "", x)
		x <- gsub("NSIC\\s*Rv\\s*222", "NSIC Rc222", x, ignore.case=TRUE)
		x <- gsub("NSIC\\s*(Rc\\s*)?222", "NSIC Rc222", x, ignore.case=TRUE)
		x <- gsub("^(Rc\\s*)?222$", "NSIC Rc222", x, ignore.case=TRUE)
		x <- gsub("NSIC\\s*Rc\\s*238", "NSIC Rc238", x, ignore.case=TRUE)
		x <- gsub("(PSB|NSIC)\\s*Rc\\s*18", "PSB Rc18", x, ignore.case=TRUE)
		x
	}
	# Safe date parser for Excel serials and text dates ("17-Jan, 2012", "11-Sept,2013", "Apr-28-2014", etc.)
	parse_xl_date <- function(x) {
		if (is.na(x)) return(NA_character_)
		s <- trimws(as.character(x))
		if (!nzchar(s) || grepl("Harvest|Date|sowing|Visit", s, ignore.case=TRUE)) return(NA_character_)
		if (grepl("^[0-9]+(\\.[0-9]+)?$", s)) {
			num <- as.numeric(s)
			if (num > 35000 && num < 46000) return(as.character(as.Date(num, origin="1899-12-30")))
			return(NA_character_)
		}
		# Fix isolated typos in Form 2 visit dates ("3/115/2013", "4/116/2013", "22-Oc,2013", "Sept")
		s2 <- sub("^3/115/2013$", "3/15/2013", s)
		s2 <- sub("^4/116/2013$", "4/16/2013", s2)
		s2 <- sub("(?i)Sept", "Sep", s2, perl=TRUE)
		s2 <- sub("(?i)-Oc,", "-Oct,", s2, perl=TRUE)
		s2 <- gsub(",\\s*", "-", s2)
		s2 <- gsub("\\s+", "-", s2)
		for (fmt in c("%d-%b-%Y", "%d-%B-%Y", "%b-%d-%Y", "%d-%b-%y", "%d-%B-%y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%d-%m-%y")) {
			d <- as.Date(s2, format=fmt)
			if (!is.na(d) && d >= as.Date("2010-01-01") && d <= as.Date("2017-12-31")) return(as.character(d))
		}
		NA_character_
	}
	# Safe numeric parser for clean numeric strings (with optional known trailing unit)
	parse_num <- function(x) {
		if (is.na(x)) return(NA_real_)
		s <- trimws(as.character(x))
		if (!nzchar(s)) return(NA_real_)
		s <- sub("\\s*(days?|weeks?|%|kg/plot|kg|sq\\.?m\\.?|sg\\.?m\\.?)$", "", s, ignore.case=TRUE)
		s <- trimws(s)
		if (grepl("^-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", s)) as.numeric(s) else NA_real_
	}

	# Pesticide brand -> active ingredient & class (per Philippine FPA registrations & terminag values)
	map_pesticides <- function(raw_names) {
		out <- list(
			herb = character(), ins = character(), fung = character(), moll = character(),
			h_n = 0L, i_n = 0L, f_n = 0L, m_n = 0L
		)
		for (nm in raw_names) {
			s <- tolower(trimws(nm))
			if (!nzchar(s) || s %in% c("none", "0", "(g or ml /ha)")) next
			if (grepl("machete", s)) {
				out$herb <- c(out$herb, "butachlor"); out$h_n <- out$h_n + 1L
			} else if (grepl("nominee", s)) {
				out$herb <- c(out$herb, "bispyribac-sodium"); out$h_n <- out$h_n + 1L
			} else if (grepl("pyanchor", s)) {
				out$herb <- c(out$herb, "pyribenzoxim"); out$h_n <- out$h_n + 1L
			} else if (grepl("agroxone", s)) {
				out$herb <- c(out$herb, "mcpa"); out$h_n <- out$h_n + 1L
			} else if (grepl("exceed|advance|rouge|rogue", s)) {
				out$herb <- c(out$herb, "butachlor", "propanil"); out$h_n <- out$h_n + 1L
			} else if (grepl("brodan", s) && grepl("kocide", s)) {
				out$ins <- c(out$ins, "chlorpyrifos", "fenobucarb"); out$i_n <- out$i_n + 1L
				out$fung <- c(out$fung, "copper hydroxide"); out$f_n <- out$f_n + 1L
			} else if (grepl("brodan", s)) {
				out$ins <- c(out$ins, "chlorpyrifos", "fenobucarb"); out$i_n <- out$i_n + 1L
			} else if (grepl("cymbush|cypermethrin", s)) {
				out$ins <- c(out$ins, "cypermethrin"); out$i_n <- out$i_n + 1L
			} else if (grepl("cartap", s)) {
				out$ins <- c(out$ins, "cartab"); out$i_n <- out$i_n + 1L
			} else if (grepl("virtako", s)) {
				out$ins <- c(out$ins, "chlorantraniliprole", "thiamethoxam"); out$i_n <- out$i_n + 1L
			} else if (grepl("contact insecticide", s)) {
				out$ins <- c(out$ins, "unknown"); out$i_n <- out$i_n + 1L
			} else if (grepl("armure|amure", s)) {
				out$fung <- c(out$fung, "difenoconazole", "propiconazole"); out$f_n <- out$f_n + 1L
			} else if (grepl("score|difenocono", s)) {
				out$fung <- c(out$fung, "difenoconazole"); out$f_n <- out$f_n + 1L
			} else if (grepl("kocide|^kop$", s)) {
				out$fung <- c(out$fung, "copper hydroxide"); out$f_n <- out$f_n + 1L
			} else if (grepl("fungitox", s)) {
				out$fung <- c(out$fung, "thiophanate-methyl"); out$f_n <- out$f_n + 1L
			} else if (grepl("hinosan", s)) {
				out$fung <- c(out$fung, "edifenphos"); out$f_n <- out$f_n + 1L
			} else if (grepl("baylu|shatter", s)) {
				out$moll <- c(out$moll, "niclosamide"); out$m_n <- out$m_n + 1L
			} else if (grepl("kuhol|metaldehyde", s)) {
				out$moll <- c(out$moll, "metaldehyde"); out$m_n <- out$m_n + 1L
			} else if (grepl("molluscicide", s)) {
				out$moll <- c(out$moll, "unknown"); out$m_n <- out$m_n + 1L
			}
		}
		out
	}

	# Fertilizer product text -> terminag fertilizer_type terms & nutrient fractions (N, P2O5, K2O)
	map_fert_types <- function(raw_names) {
		s <- tolower(paste(raw_names, collapse=" "))
		types <- character()
		if (grepl("14-14-14|t14|complete", s)) types <- c(types, "NPK")
		if (grepl("16-20-0|ammonium phosphate", s)) types <- c(types, "NP")
		if (grepl("urea|46-0-0", s)) types <- c(types, "urea")
		if (grepl("21-0-0|ammosul", s)) types <- c(types, "DAS")
		if (grepl("0-0-60|kcl|mop|muriate", s)) types <- c(types, "KCl")
		if (length(types)) paste(unique(types), collapse="; ") else NA_character_
	}
	prod_npk_frac <- function(nm) {
		s <- tolower(trimws(nm))
		if (grepl("14-14-14|t14|complete", s)) return(c(0.14, 0.14, 0.14))
		if (grepl("16-20-0|ammonium phosphate", s)) return(c(0.16, 0.20, 0.00))
		if (grepl("urea|46-0-0", s)) return(c(0.46, 0.00, 0.00))
		if (grepl("21-0-0|ammosul", s)) return(c(0.21, 0.00, 0.00))
		if (grepl("0-0-60|kcl|mop|muriate", s)) return(c(0.00, 0.00, 0.60))
		c(NA_real_, NA_real_, NA_real_)
	}

	# Form 2 injury / insect row-label lookup (covers both 2011-2012 abbreviation template and 2013-2015 full-name template)
	f2_key <- data.frame(
		key = tolower(c(
			"SNL", "DH", "WH", "GM", "RT", "WM", "LF", "DEF", "Others Defoliators [2]",
			"BlB", "LB", "BS", "BLS", "NBS", "RS", "LS", "ShB", "ShR", "SR", "FSm", "NB", "DP", "RTD",
			"Deadheart", "Rat", "Silvershoot", "Whitehead",
			"Leaffolder", "Leafminer", "Rice hispa", "Whorl maggot", "Others(defoliators)[2]",
			"Bugburn", "Hopperburn",
			"Bacterial Leaf blight", "Bacterial leaf streak", "Brown spot", "Leaf blast", "Leaf scald",
			"Narrow brown spot", "Red stripe",
			"Dirty panicle", "False smut", "Neck blast", "Sheath blight", "Sheath rot",
			"Grassy stunt", "Ragged stunt", "Rice tungro", "Yellowing syndrome",
			"BPH", "WPH", "AW", "RB", "RBB", "ZZ", "GLH", "RC", "WLH", "ZLH", "WSB",
			"Others [2]Zigzag", "Others [2] GLH", "Others [2]  LM"
		)),
		injury_ = c(
			"golden apple snail injury", "deadheart", "whitehead", "silvershoot", "rat injury",
			"whorl maggot injury", "leaffolder injury", "defoliator injury", "defoliator injury",
			"bacterial leaf blight", "leaf blast", "brown spot", "bacterial leaf streak", "narrow brown spot",
			"red stripe", "leaf smut", "sheath blight", "sheath rot", "stem rot", "false smut", "neck blast",
			"dirty panicle", "rice tungro",
			"deadheart", "rat injury", "silvershoot", "whitehead",
			"leaffolder injury", "leafminer injury", "rice hispa injury", "whorl maggot injury", "defoliator injury",
			"bugburn", "hopperburn",
			"bacterial leaf blight", "bacterial leaf streak", "brown spot", "leaf blast", "leaf scald",
			"narrow brown spot", "red stripe",
			"dirty panicle", "false smut", "neck blast", "sheath blight", "sheath rot",
			"rice grassy stunt", "rice ragged stunt", "rice tungro", "yellowing syndrome",
			NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "leafminer injury"
		),
		organ_ = c(
			"hill", "tiller", "panicle", "tiller", "tiller",
			"leaf", "leaf", "leaf", "leaf",
			"leaf", "leaf", "leaf", "leaf", "leaf",
			"leaf", "leaf", "tiller", "tiller", "tiller", "panicle", "panicle",
			"panicle", "hill",
			"tiller", "tiller", "tiller", "panicle",
			"leaf", "leaf", "leaf", "leaf", "leaf",
			"hill", "hill",
			"leaf", "leaf", "leaf", "leaf", "leaf",
			"leaf", "leaf",
			"panicle", "panicle", "panicle", "tiller", "tiller",
			"hill", "hill", "hill", "hill",
			rep("insect", 13), "leaf"
		),
		pest_species = c(
			"golden apple snail", "stem borer", "stem borer", "gall midge", "rats",
			"rice whorl maggot", "rice leaffolder", "defoliators", "defoliators",
			rep(NA, 14),
			"stem borer", "rats", "gall midge", "stem borer",
			"rice leaffolder", "leafminer", "rice hispa", "rice whorl maggot", "defoliators",
			"rice black bug", "planthoppers",
			rep(NA, 16),
			"brown planthopper", "whitebacked planthopper", "armyworm", "rice bug", "rice black bug",
			"zigzag leafhopper", "green leafhopper", "rice caseworm", "white leafhopper",
			"zigzag leafhopper", "stem borer", "zigzag leafhopper", "green leafhopper", "leafminer"
		),
		disease = c(
			rep(NA, 9),
			"bacterial leaf blight", "leaf blast", "brown spot", "bacterial leaf streak", "narrow brown spot",
			"red stripe", "leaf smut", "sheath blight", "sheath rot", "stem rot", "false smut", "neck blast",
			"dirty panicle", "rice tungro",
			rep(NA, 11),
			"bacterial leaf blight", "bacterial leaf streak", "brown spot", "leaf blast", "leaf scald",
			"narrow brown spot", "red stripe",
			"dirty panicle", "false smut", "neck blast", "sheath blight", "sheath rot",
			"rice grassy stunt", "rice ragged stunt", "rice tungro", "yellowing syndrome",
			rep(NA, 14)
		),
		stringsAsFactors = FALSE
	)

	## ---- 1. Parse all 251 Excel crop-health workbooks (Form 1 + Form 2 Visit 1..4) ----
	xl_files <- ff[grepl("\\.xlsx?$", bn, ignore.case=TRUE) &
		!bn %in% c("Crop_and_Injuries_Oct_23_2014_03_27.xlsx", "RICEPRE 2015DS- harvest.xlsx")]

	parse_workbook <- function(f) {
		fn <- basename(f)
		shs <- readxl::excel_sheets(f)
		s1 <- as.matrix(carobiner::read.excel(f, sheet="Form 1", col_names=FALSE, col_types="text"))
		s1[is.na(s1)] <- ""
		s1 <- trimws(s1)
		nr <- nrow(s1); nc <- ncol(s1)
		rstr <- apply(s1, 1, paste, collapse=" ")
		find_r <- function(pat) {
			i <- grep(pat, rstr, ignore.case=TRUE)
			if (length(i)) i[1] else NA_integer_
		}
		get_after <- function(r, pat, max_c=nc) {
			if (is.na(r) || r < 1L || r > nr) return(NA_character_)
			v <- s1[r, ]
			i <- grep(pat, v, ignore.case=TRUE)
			if (!length(i)) return(NA_character_)
			cc <- seq_len(min(nc, max_c))
			rest <- v[cc[cc > i[1]]]
			rest <- rest[nzchar(rest)]
			if (length(rest)) rest[1] else NA_character_
		}
		is_checked <- function(r, pat) {
			if (is.na(r) || r < 1L || r > nr) return(FALSE)
			v <- s1[r, ]
			i <- grep(pat, v, ignore.case=TRUE)
			if (!length(i) || i[1] >= nc) return(FALSE)
			nxt <- v[seq(i[1] + 1L, min(nc, i[1] + 4L))]
			any(tolower(nxt) %in% c("x", "/", "1", "yes"))
		}

		# Treatment & rep from filename
		trt_val <- if (grepl("^(FP|FPR|Famers|Farmers)", fn, ignore.case=TRUE) || grepl("_FP", fn, ignore.case=TRUE)) {
			"farmers' practice"
		} else if (grepl("^(PRI|PP|PPR|Primitive)", fn, ignore.case=TRUE) || grepl("_PRI", fn, ignore.case=TRUE)) {
			"primitive yield"
		} else {
			"RICE-PRE"
		}
		# Roman numeral reps (PhilRice NE: "Famers practice II_1.xlsx", "Primitive  III.xlsx")
		# and Negros rep-in-filename ("FP_1.xlsx", "FP_1_1.xlsx")
		fn_base <- sub("\\.xlsx?$", "", fn)
		fn_nodup <- sub("_[0-9]$", "", fn_base)
		rep_val <- if (grepl("^(FP|PRI|RP|PP)_([1-4])(_[0-9])?$", fn_base)) {
			as.integer(sub("^(?:FP|PRI|RP|PP)_([1-4])(?:_[0-9])?$", "\\1", fn_base))
		} else if (grepl("\\bIII\\b", fn_nodup)) { 3L
		} else if (grepl("\\bII\\b", fn_nodup)) { 2L
		} else if (grepl("\\bI\\b", fn_nodup)) { 1L
		} else if (grepl("(FP|PRI|RP|RICEPRE|PP)R?([1-4])", fn_nodup, ignore.case=TRUE)) {
			as.integer(sub(".*?(?:FP|PRI|RP|RICEPRE|PP)R?([1-4]).*", "\\1", fn_nodup, ignore.case=TRUE))
		} else if (grepl("R([1-4])", fn_nodup)) {
			as.integer(sub(".*?R([1-4]).*", "\\1", fn_nodup))
		} else if (grepl("_([1-4])[0-9]{2}", fn_nodup)) {
			as.integer(sub(".*_([1-4])[0-9]{2}.*", "\\1", fn_nodup))
		} else if (grepl(" ([1-4])[0-9]{2}$", fn_nodup)) {
			as.integer(sub(".* ([1-4])[0-9]{2}$", "\\1", fn_nodup))
		} else if (grepl("_([1-4])$", fn_nodup)) {
			as.integer(sub(".*_([1-4])$", "\\1", fn_nodup))
		} else NA_integer_

		fld <- get_after(find_r("Field no"), "Field no")
		vil <- get_after(find_r("Village name"), "Village name")
		if (is.na(rep_val) && !is.na(vil) && grepl("(?:REP|Replication)\\s*([1-4])", vil, ignore.case=TRUE)) {
			rep_val <- as.integer(sub(".*?(?:REP|Replication)\\s*([1-4]).*", "\\1", vil, ignore.case=TRUE))
		}
		if (is.na(rep_val) && !is.na(fld) && grepl("^([1-4])[0-9]{2}$", fld)) {
			rep_val <- as.integer(substr(fld, 1, 1))
		}
		r_area <- find_r("Approximate field area")
		farmer <- get_after(r_area, "Farmer's name")
		area_raw <- get_after(r_area, "Approximate field area", max_c=12)
		area_ha <- if (is.na(area_raw)) {
			NA_real_
		} else if (grepl("sq|sg", area_raw, ignore.case=TRUE)) {
			parse_num(area_raw) / 10000
		} else {
			parse_num(area_raw)
		}

		# Identify station cleanly (filename + Field no take precedence over copy-pasted template Village text)
		sig_hi <- paste(fn, ifelse(is.na(fld), "", fld), ifelse(is.na(farmer), "", farmer))
		sig_lo <- ifelse(is.na(vil), "", vil)
		station <- if (grepl("IRRI|WS2013|RICEPRE_DS2014|Bethany", sig_hi, ignore.case=TRUE) || grepl("IRRI|Los Banos", sig_lo, ignore.case=TRUE)) {
			"IRRI_LB"
		} else if (grepl("\\bNE\\b|_NE\\b|NE_|Marquez", sig_hi, ignore.case=TRUE) || grepl("^(Famers|Primitive  ?|Rice pre) I", fn) || grepl("Nueva Ecija|Maligay|PhilRice-CES", sig_lo, ignore.case=TRUE)) {
			"PhilRice_NE"
		} else if (grepl("ADN|\\bAND_|Ortiz|DOC GERRY|^(Farmers [Pp]ractice|Primitive|Rice ?Pre)[- ]R[1-3]", sig_hi, ignore.case=TRUE) || grepl("Agusan|ADN|Basilisa", sig_lo, ignore.case=TRUE)) {
			"PhilRice_ADN"
		} else if (grepl("NEG|ENDINO|Cherry|Negros|^(FP|PRI|RP|PP|FPR|PPR|RPR)[_1-4]", sig_hi, ignore.case=TRUE) || grepl("Negros|Cansilayan", sig_lo, ignore.case=TRUE)) {
			"PhilRice_Negros"
		} else {
			NA_character_
		}

		# Plot ID (numeric plot code or field label)
		plot_id <- if (!is.na(fld) && nzchar(fld)) {
			fld
		} else if (grepl("[1-4][0-9]{2}", fn_nodup)) {
			sub(".*?([1-4][0-9]{2}).*", "\\1", fn_nodup)
		} else NA_character_

		# Coordinates in Form 1 (present in some IRRI and NE workbooks)
		r_lat <- find_r("Latitude")
		lat_raw <- get_after(r_lat, "Latitude", max_c=17)
		lon_raw <- get_after(r_lat, "Longitude")
		lat_val <- NA_real_; lon_val <- NA_real_
		if (!is.na(lat_raw) && !is.na(lon_raw)) {
			if (grepl("[°'\"]", lat_raw)) {
				lat_val <- carobiner::dms_to_dd(lat_raw, default_hem="N")
				lon_val <- carobiner::dms_to_dd(lon_raw, default_hem="E")
			} else {
				lat_val <- parse_num(lat_raw)
				lon_val <- parse_num(lon_raw)
			}
		}

		# Agronomic & management fields in Form 1
		r_prev <- find_r("Previous crop")
		prev_crop <- tolower(get_after(r_prev, "Previous crop", max_c=20))
		if (!is.na(prev_crop) && !grepl("rice", prev_crop)) prev_crop <- NA_character_
		fallow_wk <- parse_num(get_after(r_prev, "Duration of fallow"))

		r_tr <- find_r("Transplanted")
		r_ds <- find_r("Direct-seeded")
		pmeth <- if (is_checked(r_tr, "Transplanted")) {
			"transplanting"
		} else if (is_checked(r_ds, "Direct-seeded")) {
			"direct seeding"
		} else NA_character_
		tr_days <- parse_num(get_after(r_tr, "Age of seedlings"))
		hills_sqm <- parse_num(get_after(r_ds, "No\\. of plants per sqm", max_c=19))
		if (is.na(hills_sqm)) hills_sqm <- parse_num(get_after(r_tr + 1L, "No\\. of hills per sqm", max_c=19))
		sp_txt <- if (!is.na(r_ds)) paste(s1[r_ds, ], collapse=" ") else ""
		r_sp <- if (grepl("20\\s*cm\\s*x\\s*20\\s*cm", sp_txt, ignore.case=TRUE)) 20 else NA_real_

		r_ced <- find_r("Crop establishment date")
		ced_date <- parse_xl_date(get_after(r_ced, "Crop establishment date", max_c=22))
		hd_date <- parse_xl_date(get_after(r_ced, "Harvest date"))

		r_var <- find_r("Rice variety")
		var_val <- clean_variety(get_after(r_var, "Rice variety", max_c=11))
		vtype <- if (is_checked(r_var, "Modern variety")) {
			"modern"
		} else if (is_checked(r_var, "Traditional variety")) {
			"traditional"
		} else if (is_checked(r_var, "Hybrid variety")) {
			"hybrid"
		} else NA_character_

		# Manure
		r_man <- find_r("Manure")
		man_raw <- get_after(r_man, "Manure")
		om_used <- NA; om_amt <- NA_real_
		if (!is.na(man_raw)) {
			if (tolower(man_raw) %in% c("none", "0", "no")) {
				om_used <- FALSE; om_amt <- 0
			} else {
				mn <- parse_num(man_raw)
				if (!is.na(mn)) { om_used <- mn > 0; om_amt <- mn }
			}
		}

		# Inorganic fertilizer (Form 1 reports N, P2O5, K2O in kg/ha, or kg product/plot in NE 2013-14 & ADN 2013-15)
		r_ntot <- find_r("\\bNtot\\b")
		r_ptot <- find_r("\\bPtot\\b")
		r_ktot <- find_r("\\bKtot\\b")
		r_finp <- find_r("Fertilizer input")
		fert_none <- is_checked(r_finp, "none")

		Ntot <- parse_num(get_after(r_ntot, "^Ntot$"))
		Ptot <- parse_num(get_after(r_ptot, "^Ptot$"))
		Ktot <- parse_num(get_after(r_ktot, "^Ktot$"))

		# Collect split values (cols 10..15) and fertilizer product names (cols 18..nc)
		fert_prods <- character()
		n_raw <- p_raw <- k_raw <- rep(NA_character_, 4)
		n_prd <- p_prd <- k_prd <- rep(NA_character_, 4)
		if (!is.na(r_ntot) && r_ntot > 1L && nc >= 10L) {
			sub_r <- max(1L, r_ntot - 13L):(r_ntot - 1L)
			for (rr in sub_r) {
				lbl <- s1[rr, min(9L, nc)]
				val <- s1[rr, 10:min(15L, nc)]; val <- val[nzchar(val)]
				val <- if (length(val)) val[1] else NA_character_
				prd <- if (nc >= 19L) s1[rr, 19:nc] else ""
				prd <- prd[nzchar(prd) & !grepl("^[NPK][1-4]$", prd)]
				if (length(prd)) fert_prods <- c(fert_prods, prd)
				prd1 <- if (length(prd)) prd[1] else NA_character_
				if (grepl("^N[1-4]$", lbl)) { kk <- as.integer(substr(lbl, 2, 2)); n_raw[kk] <- val; n_prd[kk] <- prd1 }
				if (grepl("^P[1-4]$", lbl)) { kk <- as.integer(substr(lbl, 2, 2)); p_raw[kk] <- val; p_prd[kk] <- prd1 }
				if (grepl("^K[1-4]$", lbl)) { kk <- as.integer(substr(lbl, 2, 2)); k_raw[kk] <- val; k_prd[kk] <- prd1 }
			}
		}
		ftype <- map_fert_types(fert_prods)
		N_kg <- P_kg <- K_kg <- NA_real_
		if (!is.na(Ntot) || !is.na(Ptot) || !is.na(Ktot)) {
			N_kg <- Ntot
			P_kg <- if (!is.na(Ptot)) Ptot / 2.29 else NA_real_
			K_kg <- if (!is.na(Ktot)) Ktot / 1.2051 else NA_real_
		} else if (fert_none) {
			N_kg <- 0; P_kg <- 0; K_kg <- 0; ftype <- "none"
		} else if (any(!is.na(c(n_raw, p_raw, k_raw)))) {
			is_per_plot_txt <- any(grepl("kg|plot", c(n_raw, p_raw, k_raw), ignore.case=TRUE))
			n_nums <- sapply(n_raw, parse_num)
			p_nums <- sapply(p_raw, parse_num)
			k_nums <- sapply(k_raw, parse_num)
			# Detect PhilRice NE 2013-2014 DS per-plot product kg convention (N1=P1=K1 in {20.8, 24.9, 29.1} on 0.083 ha)
			is_ne_per_plot <- !is.na(area_ha) && abs(area_ha - 0.083) < 0.002 &&
				!is.na(n_nums[1]) && n_nums[1] %in% c(20.8, 24.9, 29.1) &&
				!is.na(p_nums[1]) && p_nums[1] == n_nums[1]
			if (is_per_plot_txt || is_ne_per_plot) {
				if (!is.na(area_ha) && area_ha > 0) {
					# Sum unique product applications per split (complete fertilizer appears on N1/P1/K1 with same kg/plot)
					N_acc <- 0; P2O5_acc <- 0; K2O_acc <- 0; ok_f <- FALSE
					for (kk in 1:4) {
						# Complete / NP / urea on N row
						if (!is.na(n_nums[kk]) && !is.na(n_prd[kk])) {
							fr <- prod_npk_frac(n_prd[kk])
							if (!any(is.na(fr))) {
								N_acc <- N_acc + n_nums[kk] * fr[1] / area_ha
								P2O5_acc <- P2O5_acc + n_nums[kk] * fr[2] / area_ha
								K2O_acc <- K2O_acc + n_nums[kk] * fr[3] / area_ha
								ok_f <- TRUE
							}
						}
						# Additional P-only product if different from N product
						if (!is.na(p_nums[kk]) && !is.na(p_prd[kk]) && (is.na(n_prd[kk]) || tolower(p_prd[kk]) != tolower(n_prd[kk]))) {
							fr <- prod_npk_frac(p_prd[kk])
							if (!any(is.na(fr))) {
								N_acc <- N_acc + p_nums[kk] * fr[1] / area_ha
								P2O5_acc <- P2O5_acc + p_nums[kk] * fr[2] / area_ha
								K2O_acc <- K2O_acc + p_nums[kk] * fr[3] / area_ha
								ok_f <- TRUE
							}
						}
						# Additional K-only product (MOP/KCl) if different from N product
						if (!is.na(k_nums[kk]) && !is.na(k_prd[kk]) && (is.na(n_prd[kk]) || tolower(k_prd[kk]) != tolower(n_prd[kk]))) {
							fr <- prod_npk_frac(k_prd[kk])
							if (!any(is.na(fr))) {
								N_acc <- N_acc + k_nums[kk] * fr[1] / area_ha
								P2O5_acc <- P2O5_acc + k_nums[kk] * fr[2] / area_ha
								K2O_acc <- K2O_acc + k_nums[kk] * fr[3] / area_ha
								ok_f <- TRUE
							}
						}
					}
					if (ok_f) {
						N_kg <- round(N_acc, 2)
						P_kg <- round(P2O5_acc / 2.29, 2)
						K_kg <- round(K2O_acc / 1.2051, 2)
					}
				}
			} else {
				# Already kg N, P2O5, K2O per ha per split
				N_kg <- if (all(is.na(n_nums))) NA_real_ else sum(n_nums, na.rm=TRUE)
				P_kg <- if (all(is.na(p_nums))) 0 else sum(p_nums, na.rm=TRUE) / 2.29
				K_kg <- if (all(is.na(k_nums))) 0 else sum(k_nums, na.rm=TRUE) / 1.2051
			}
		}
		if (!is.na(P_kg)) P_kg <- round(P_kg, 2)
		if (!is.na(K_kg)) K_kg <- round(K_kg, 2)
		fert_used <- if (!is.na(N_kg)) (N_kg > 0 || isTRUE(P_kg > 0) || isTRUE(K_kg > 0)) else if (fert_none) FALSE else if (!is.na(ftype)) TRUE else NA

		# Pesticides
		r_puse <- find_r("Pesticide use")
		pest_none <- is_checked(r_puse, "none")
		r_chem <- find_r("Name of chemical")
		chem_raw <- character()
		if (!is.na(r_chem) && r_chem < nr && nc >= 10L) {
			for (rr in (r_chem + 1L):min(nr, r_chem + 6L)) {
				v <- s1[rr, 10:min(24L, nc)]
				v <- v[nzchar(v) & !grepl("^[1-6]$|^Rate|^\\(g or ml", v)]
				if (length(v)) chem_raw <- c(chem_raw, v)
			}
		}
		pm <- map_pesticides(chem_raw)
		r_iu <- find_r("\\bIU\\b")
		iu_n <- parse_num(get_after(r_iu, "^IU$", max_c=14))
		hu_n <- parse_num(get_after(r_iu, "^HU$", max_c=18))
		fu_n <- parse_num(get_after(r_iu, "^FU$", max_c=22))
		ins_times <- if (!is.na(iu_n)) as.integer(iu_n) else if (pm$i_n > 0L) pm$i_n else if (pest_none) 0L else NA_integer_
		herb_times <- if (!is.na(hu_n)) as.integer(hu_n) else if (pm$h_n > 0L) pm$h_n else if (pest_none) 0L else NA_integer_
		fung_times <- if (!is.na(fu_n)) as.integer(fu_n) else if (pm$f_n > 0L) pm$f_n else if (pest_none) 0L else NA_integer_
		moll_times <- if (pm$m_n > 0L) pm$m_n else if (pest_none) 0L else NA_integer_

		# Crop status at booting & lodging at last visit
		r_cs <- find_r("Crop status at booting")
		crop_health <- NA_character_
		if (!is.na(r_cs)) {
			r_vg <- find_r("Very good")
			r_pr <- find_r("^.*Poor.*Very poor")
			if (is_checked(r_vg, "Very good")) crop_health <- "very good"
			else if (is_checked(r_vg, "^Good$")) crop_health <- "good"
			else if (is_checked(r_vg, "Average")) crop_health <- "average"
			else if (is_checked(r_pr, "^Poor$")) crop_health <- "poor"
			else if (is_checked(r_pr, "Very poor")) crop_health <- "very poor"
		}
		r_ldg <- find_r("Lodging at last visit")
		lodging <- parse_num(get_after(r_ldg, "Lodging at last visit", max_c=13))

		# Yield & moisture content in Form 1
		r_yld <- find_r("Yield")
		r_mc <- find_r("Moisture [Cc]ontent")
		yld_val <- NA_real_; yld_mc <- NA_real_
		if (!is.na(r_yld)) {
			c10 <- if (nc >= 10L) s1[r_yld, 10L] else ""
			y10 <- parse_num(c10)
			ycols <- intersect(c(14L, 18L, 22L, 27L), seq_len(nc))
			yraw <- s1[r_yld, ycols]
			mvals <- if (!is.na(r_mc)) sapply(s1[r_mc, intersect(c(14L, 18L, 22L, 27L), seq_len(nc))], parse_num) else rep(NA_real_, length(ycols))

			if (!is.na(y10) && y10 > 0) {
				# "Yield estimates (t/ha)" template with summary cell in C10 (already 14% MC adjusted)
				# 2012 DS Negros entered kg/10m2 into a g/10m2 formula -> y10 is ~0.003-0.005 instead of 3-5 t/ha
				yld_val <- if (y10 < 0.05) y10 * 1e6 else y10 * 1000
				yld_mc <- 14
			} else if (length(yraw) >= 1L && grepl("N99", yraw[1])) {
				# RICEPRE_DS2014_101_FP.xls: literal un-evaluated formula "(((100-N99)/86)*((N97/650)*10000))/1000"
				w_raw <- if (r_yld > 1L && nc >= 14L) parse_num(s1[r_yld - 1L, 14L]) else NA_real_
				mc1 <- if (length(mvals) >= 1L) mvals[1] else NA_real_
				if (!is.na(w_raw) && !is.na(mc1) && !is.na(area_ha) && area_ha > 0) {
					yld_val <- ((100 - mc1) / 86) * (w_raw / area_ha)
					yld_mc <- 14
				}
			} else if (any(grepl("kg", yraw, ignore.case=TRUE))) {
				# Whole-plot kg weights without plot area (2014 DS ADN): keep NA (documented in ISSUES)
				yld_val <- NA_real_
			} else {
				ynums <- sapply(yraw, parse_num)
				ok <- !is.na(ynums) & ynums > 0
				if (any(ok)) {
					# Special case: RICEPRE_DS2014_401_RP.xls (400 kg) and 402_PRI.xls (98 kg) on 650 m2 plot
					if (sum(ok) == 1L && ynums[ok][1] > 20 && ynums[ok][1] < 1000 && !is.na(area_ha) && abs(area_ha - 0.065) < 0.002) {
						mc1 <- if (!is.na(mvals[ok][1])) mvals[ok][1] else 14
						yld_val <- ((100 - mc1) / 86) * (ynums[ok][1] / area_ha)
						yld_mc <- 14
					} else if (any(ynums[ok] > 20)) {
						# Values in g/10 m2 (= kg/ha at measured MC), e.g. 2014 WS PhilRice NE (4200-6800)
						m_use <- mvals[ok]
						if (all(!is.na(m_use))) {
							yld_val <- mean(ynums[ok] * (100 - m_use) / 86)
							yld_mc <- 14
						} else {
							yld_val <- mean(ynums[ok])
							yld_mc <- NA_real_
						}
					} else {
						# Values in t/ha (already adjusted to 14% MC in Form 1)
						yld_val <- mean(ynums[ok]) * 1000
						yld_mc <- 14
					}
				}
			}
		}

		## ---- Parse Form 2 Visit sheets in this workbook ----
		f2_sheets <- grep("^Form 2", shs, ignore.case=TRUE, value=TRUE)
		lv_list <- list(); hc_list <- list(); hi_list <- list(); ha_list <- list(); hw_list <- list(); hp_list <- list()
		v1_date <- NA_character_

		for (sh in f2_sheets) {
			s2 <- as.matrix(carobiner::read.excel(f, sheet=sh, col_names=FALSE, col_types="text"))
			nr2 <- nrow(s2); nc2 <- ncol(s2)
			if (nr2 < 5L || nc2 < 10L) next
			s2[is.na(s2)] <- ""
			s2 <- trimws(s2)
			r2str <- apply(s2, 1, paste, collapse=" ")

			# Visit number from sheet name (e.g. "Form 2 Visit 1", "Form 2 Visit 2b")
			vnum <- if (grepl("2b", sh, ignore.case=TRUE)) 2.5 else {
				as.numeric(sub(".*Visit\\s*([1-4]).*", "\\1", sh, ignore.case=TRUE))
			}
			# Visit date from row 1
			dt_i <- grep("Date", s2[1, ], ignore.case=TRUE)
			vdate <- NA_character_
			if (length(dt_i)) {
				rest <- s2[1, seq_len(nc2) > dt_i[1]]
				rest <- rest[nzchar(rest)]
				if (length(rest)) vdate <- parse_xl_date(rest[1])
			}
			if (is.na(v1_date) && !is.na(vdate)) v1_date <- vdate

			# DVS & water status
			r_dvs <- grep("development stage", r2str, ignore.case=TRUE)
			dvs_val <- NA_integer_; ws_val <- NA_integer_
			if (length(r_dvs)) {
				r_d <- r_dvs[1]
				v_d <- s2[r_d, ]
				i1 <- grep("development stage", v_d, ignore.case=TRUE)[1]
				i2 <- grep("Water status", v_d, ignore.case=TRUE)
				max1 <- if (length(i2)) i2[1] - 1L else nc2
				r1v <- v_d[seq_len(max1) > i1]; r1v <- r1v[nzchar(r1v)]
				if (length(r1v)) dvs_val <- as.integer(parse_num(r1v[1]))
				if (length(i2)) {
					r2v <- v_d[seq_len(nc2) > i2[1]]; r2v <- r2v[nzchar(r2v)]
					if (length(r2v)) ws_val <- as.integer(parse_num(r2v[1]))
				}
			}

			# Locate hill header ("Hill or Quadrat No." with 1..10)
			r_hill <- grep("Hill or", r2str, ignore.case=TRUE)
			if (!length(r_hill)) next
			c1 <- which(s2[r_hill[1], ] == "1")
			if (!length(c1) || c1[1] < 2L) next
			c1 <- c1[1]
			c_lbl <- c1 - 1L
			c_hills <- c1:min(nc2, c1 + 9L)
			if (length(c_hills) < 10L) next

			# Crop growth counts (tillers, panicles, leaves per tiller)
			r_til <- grep("No\\. of tillers per hill", r2str, ignore.case=TRUE)
			r_pan <- grep("No\\. of panicles per hill", r2str, ignore.case=TRUE)
			r_lea <- grep("No\\. of leaves per tiller", r2str, ignore.case=TRUE)
			til_v <- if (length(r_til)) sapply(s2[r_til[1], c_hills], parse_num) else rep(NA_real_, 10)
			pan_v <- if (length(r_pan)) sapply(s2[r_pan[1], c_hills], parse_num) else rep(NA_real_, 10)
			lea_v <- if (length(r_lea)) sapply(s2[r_lea[1], c_hills], parse_num) else rep(NA_real_, 10)

			# Skip completely empty Form 2 sheets (no date, no DVS, no tillers)
			if (is.na(vdate) && is.na(dvs_val) && all(is.na(til_v))) next

			fid <- paste0(fn, "::", sh)
			lv_list[[sh]] <- data.frame(
				form_id = fid,
				date = vdate,
				visit_ = vnum,
				DVS_ = dvs_val,
				growth_stage = unname(dvs_map[as.character(dvs_val)]),
				water_status_ = ws_val,
				stringsAsFactors = FALSE
			)

			ok_hc <- !is.na(til_v) | !is.na(pan_v) | !is.na(lea_v)
			if (any(ok_hc)) {
				hc_list[[sh]] <- data.frame(
					form_id = fid,
					hill_ = which(ok_hc),
					tillers_hill_ = til_v[ok_hc],
					panicles_hill_ = pan_v[ok_hc],
					leaves_tiller_ = lea_v[ok_hc],
					stringsAsFactors = FALSE
				)
			}

			# Area-level denominator rows ("No. of hills or quadrats" in new template, cols c1..(c1+4) = A..E)
			r_area_hdr <- grep("^Area \\[3\\]", s2[, c_lbl], ignore.case=TRUE)
			area_den_rows <- grep("^No\\. of hills or quadrats$", s2[, c_lbl], ignore.case=TRUE)

			# Sweep section row (old template: "Sweep No" -> cols c1..(c1+4) = sweeps 1..5)
			r_swp <- grep("^Sweep No", s2[, c_lbl], ignore.case=TRUE)
			r_dis_hdr <- grep("^Diseases$", s2[, 1L], ignore.case=TRUE)
			swp_rows <- if (length(r_swp) && r_swp[1] < nr2) {
				end_swp <- if (length(r_dis_hdr) && any(r_dis_hdr > r_swp[1])) r_dis_hdr[r_dis_hdr > r_swp[1]][1] else min(nr2, r_swp[1] + 5L)
				(r_swp[1] + 1L):end_swp
			} else integer()

			# Scan all rows after crop growth for injuries, insects, and area injuries
			start_r <- max(c(r_til, r_pan, r_lea, r_hill[1]), na.rm=TRUE) + 1L
			if (start_r <= nr2) {
				for (rr in start_r:nr2) {
					lbl <- tolower(s2[rr, c_lbl])
					if (!nzchar(lbl)) next
					km <- match(lbl, f2_key$key)
					if (is.na(km)) next
					info <- f2_key[km, ]

					if (info$organ_ == "insect") {
						# Insect counts: either 5 sweeps (swp_rows) or 10 hills
						if (rr %in% swp_rows) {
							vals <- sapply(s2[rr, c1:(c1 + 4L)], parse_num)
							ok <- !is.na(vals)
							if (any(ok)) {
								hp_list[[length(hp_list) + 1L]] <- data.frame(
									form_id = fid,
									sweep_ = which(ok),
									hill_ = NA_integer_,
									pest_species = info$pest_species,
									pest_incidence = as.integer(round(vals[ok])),
									organ_ = "insect",
									stringsAsFactors = FALSE
								)
							}
						} else {
							vals <- sapply(s2[rr, c_hills], parse_num)
							ok <- !is.na(vals)
							if (any(ok)) {
								hp_list[[length(hp_list) + 1L]] <- data.frame(
									form_id = fid,
									sweep_ = NA_integer_,
									hill_ = which(ok),
									pest_species = info$pest_species,
									pest_incidence = as.integer(round(vals[ok])),
									organ_ = "insect",
									stringsAsFactors = FALSE
								)
							}
						}
					} else if (info$organ_ == "hill") {
						if (lbl == "snl") {
							# SNL in old template is 10 hills (0/1 or damaged hills per hill/quadrat)
							vals <- sapply(s2[rr, c_hills], parse_num)
							ok <- !is.na(vals)
							if (any(ok)) {
								hi_list[[length(hi_list) + 1L]] <- data.frame(
									form_id = fid, hill_ = which(ok), injury_count_ = vals[ok],
									injury_ = info$injury_, organ_ = "hill",
									pest_species = info$pest_species, disease = info$disease,
									disease_incidence = NA_character_, pest_injury_incidence_ = NA_real_,
									stringsAsFactors = FALSE
								)
							}
						} else if (lbl == "rtd") {
							# RTD in old template: infected hills out of 50 hills (single value in c1)
							val <- parse_num(s2[rr, c1])
							if (!is.na(val)) {
								ha_list[[length(ha_list) + 1L]] <- data.frame(
									form_id = fid, subarea_ = "50 hills", injury_count_ = val,
									injury_ = info$injury_, organ_ = "hill",
									pest_species = NA_character_, disease = info$disease,
									disease_incidence = as.character(round(100 * val / 50, 2)),
									stringsAsFactors = FALSE
								)
							}
						} else {
							# New template areas A-E (cols c1..(c1+4))
							vals <- sapply(s2[rr, c1:(c1 + 4L)], parse_num)
							ok <- !is.na(vals)
							if (any(ok)) {
								# Find nearest preceding denominator row if present
								den_r <- area_den_rows[area_den_rows < rr]
								dens <- if (length(den_r)) sapply(s2[tail(den_r, 1), c1:(c1 + 4L)], parse_num) else rep(NA_real_, 5)
								dens[!is.na(dens) & dens <= 0] <- NA_real_
								inc <- round(100 * vals[ok] / dens[ok], 2)
								ha_list[[length(ha_list) + 1L]] <- data.frame(
									form_id = fid,
									subarea_ = LETTERS[1:5][ok],
									injury_count_ = vals[ok],
									injury_ = info$injury_,
									organ_ = "hill",
									pest_species = info$pest_species,
									disease = info$disease,
									disease_incidence = if (is.na(info$disease)) rep(NA_character_, sum(ok)) else ifelse(is.na(inc), NA_character_, as.character(inc)),
									stringsAsFactors = FALSE
								)
							}
						}
					} else {
						# Hill-level tiller / panicle / leaf injury counts (10 hills)
						vals <- sapply(s2[rr, c_hills], parse_num)
						ok <- !is.na(vals)
						if (any(ok)) {
							nu <- if (info$organ_ == "tiller") til_v[ok] else if (info$organ_ == "panicle") pan_v[ok] else til_v[ok] * lea_v[ok]
							nu[!is.na(nu) & nu <= 0] <- NA_real_
							inc <- round(100 * vals[ok] / nu, 2)
							hi_list[[length(hi_list) + 1L]] <- data.frame(
								form_id = fid,
								hill_ = which(ok),
								injury_count_ = vals[ok],
								injury_ = info$injury_,
								organ_ = info$organ_,
								pest_species = info$pest_species,
								disease = info$disease,
								disease_incidence = if (is.na(info$disease)) rep(NA_character_, sum(ok)) else ifelse(is.na(inc), NA_character_, as.character(inc)),
								pest_injury_incidence_ = if (is.na(info$disease)) inc else rep(NA_real_, sum(ok)),
								stringsAsFactors = FALSE
							)
						}
					}
				}
			}

			# Weed section (sub-areas A, B, C)
			r_wa <- grep("^above the rice canopy$", s2[, 1L], ignore.case=TRUE)
			r_wb <- grep("^below the rice canopy$", s2[, 1L], ignore.case=TRUE)
			if (length(r_wa) && length(r_wb)) {
				ra <- r_wa[1]; rb <- r_wb[1]
				# A, B, C ratings in cols 3, 4, 5
				wa_v <- sapply(s2[ra, 3:5], parse_num)
				wb_v <- sapply(s2[rb, 3:5], parse_num)
				# Weed type ranks (S, BD, G, SD/SW) on rows ra..(ra+3)
				c_wt <- if (c1 == 5L) 10L else 11L
				w_ranks <- list(S=rep(NA_real_, 3), BD=rep(NA_real_, 3), G=rep(NA_real_, 3), SD=rep(NA_real_, 3))
				w_spp <- character()
				for (rr in ra:min(nr2, ra + 3L)) {
					wt <- toupper(s2[rr, min(c_wt, nc2)])
					if (wt == "SW") wt <- "SD"
					if (wt %in% names(w_ranks) && nc2 >= c_wt + 3L) {
						w_ranks[[wt]] <- sapply(s2[rr, (c_wt + 1L):(c_wt + 3L)], parse_num)
					}
					# Main weed species listed to the right of ranks
					if (nc2 >= c_wt + 5L) {
						sp_c <- s2[rr, (c_wt + 5L):nc2]
						sp_c <- sp_c[nzchar(sp_c) & !grepl("^0$|^none$|^Main weed", sp_c, ignore.case=TRUE)]
						if (length(sp_c)) w_spp <- c(w_spp, sp_c)
					}
				}
				w_spp_str <- if (length(w_spp)) paste(unique(w_spp), collapse="; ") else NA_character_
				ok_w <- !is.na(wa_v) | !is.na(wb_v) | !is.na(w_ranks$S) | !is.na(w_ranks$BD) | !is.na(w_ranks$G) | !is.na(w_ranks$SD)
				if (any(ok_w)) {
					hw_list[[sh]] <- data.frame(
						form_id = fid,
						subarea_ = c("A", "B", "C")[ok_w],
						weed_above_rating_ = wa_v[ok_w],
						weed_below_rating_ = wb_v[ok_w],
						weed_rank_sedge_ = w_ranks$S[ok_w],
						weed_rank_broadleaved_ = w_ranks$BD[ok_w],
						weed_rank_grass_ = w_ranks$G[ok_w],
						weed_rank_small_ = w_ranks$SD[ok_w],
						weed_species = rep(w_spp_str, sum(ok_w)),
						stringsAsFactors = FALSE
					)
				}
			}
		}

		# Fix known 1-year CED typo in 2012 DS Negros (CED 2012-12-27 while Visit 1 is 2012-02-09)
		if (!is.na(ced_date) && !is.na(v1_date) && ced_date > v1_date && substr(ced_date, 1, 4) == "2012" && substr(v1_date, 1, 4) == "2012") {
			ced_date <- sub("^2012", "2011", ced_date)
		}

		# Infer season and harvest year from CED (or Visit 1 date when CED is absent)
		ref_date <- if (!is.na(ced_date)) ced_date else v1_date
		yr <- NA_integer_; ssn <- NA_character_
		if (!is.na(ref_date)) {
			ry <- as.integer(substr(ref_date, 1, 4))
			rm <- as.integer(substr(ref_date, 6, 7))
			if (!is.na(ced_date)) {
				if (rm %in% c(11L, 12L)) { ssn <- "dry"; yr <- ry + 1L }
				else if (rm %in% 1:3) { ssn <- "dry"; yr <- ry }
				else { ssn <- "wet"; yr <- ry }
			} else {
				# Visit 1 date (usually 30-50 DAS)
				if (rm %in% 1:5) { ssn <- "dry"; yr <- ry }
				else { ssn <- "wet"; yr <- ry }
			}
		}

		# Fix known year typos in Form 2 visit dates:
		# - 2011 WS IRRI: Visit 3/4 entered as 2010-09-14/24/28 -> 2011
		# - 2014 DS PhilRice NE: Visit 2/3 entered as 2013-03-12 / 2013-04-01 -> 2014
		# - 2015 DS PhilRice ADN ("Primitive R1_1.xls"): Visit 2 entered as 2020-04-14 -> 2015
		lv_df <- if (length(lv_list)) do.call(rbind, lv_list) else NULL
		if (!is.null(lv_df) && !is.na(yr)) {
			if (yr == 2011L && identical(station, "IRRI_LB") && identical(ssn, "wet")) {
				bad_dt <- !is.na(lv_df$date) & grepl("^2010-09-", lv_df$date)
				lv_df$date[bad_dt] <- sub("^2010", "2011", lv_df$date[bad_dt])
			} else if (yr == 2014L && identical(station, "PhilRice_NE") && identical(ssn, "dry")) {
				bad_dt <- !is.na(lv_df$date) & grepl("^2013-(03|04)-", lv_df$date)
				lv_df$date[bad_dt] <- sub("^2013", "2014", lv_df$date[bad_dt])
			} else if (yr == 2015L && identical(station, "PhilRice_ADN") && identical(ssn, "dry")) {
				bad_dt <- !is.na(lv_df$date) & grepl("^2020-04-", lv_df$date)
				lv_df$date[bad_dt] <- sub("^2020", "2015", lv_df$date[bad_dt])
			}
		}

		wide <- data.frame(
			station = station, season = ssn, year = yr, treatment = trt_val, rep = rep_val,
			plot_id = plot_id,
			plot_area = if (!is.na(area_ha)) round(area_ha * 10000, 1) else NA_real_,
			latitude = lat_val, longitude = lon_val,
			geo_from_source = !is.na(lat_val) & !is.na(lon_val),
			previous_crop = prev_crop, fallow_weeks_ = fallow_wk,
			planting_method = pmeth, transplanting_days = as.integer(tr_days),
			plant_density = if (!is.na(hills_sqm)) hills_sqm * 10000 else NA_real_,
			row_spacing = r_sp, plant_spacing = r_sp,
			planting_date = if (!is.na(ced_date)) ced_date else as.character(yr),
			harvest_date = hd_date,
			variety = var_val, variety_type = vtype,
			OM_used = om_used, OM_amount = om_amt,
			fertilizer_used = fert_used, fertilizer_type = ftype,
			N_fertilizer = N_kg, P_fertilizer = P_kg, K_fertilizer = K_kg,
			insecticide_used = if (!is.na(ins_times)) ins_times > 0L else if (length(pm$ins)) TRUE else NA,
			insecticide_times = ins_times,
			insecticide_product = if (length(pm$ins)) paste(unique(pm$ins), collapse="; ") else NA_character_,
			herbicide_used = if (!is.na(herb_times)) herb_times > 0L else if (length(pm$herb)) TRUE else NA,
			herbicide_times = herb_times,
			herbicide_product = if (length(pm$herb)) paste(unique(pm$herb), collapse="; ") else NA_character_,
			fungicide_used = if (!is.na(fung_times)) fung_times > 0L else if (length(pm$fung)) TRUE else NA,
			fungicide_times = fung_times,
			fungicide_product = if (length(pm$fung)) paste(unique(pm$fung), collapse="; ") else NA_character_,
			molluscicide_used = if (!is.na(moll_times)) moll_times > 0L else if (length(pm$moll)) TRUE else NA,
			molluscicide_times = moll_times,
			molluscicide_product = if (length(pm$moll)) paste(unique(pm$moll), collapse="; ") else NA_character_,
			crop_health = crop_health, lodging_ = lodging,
			yield = if (!is.na(yld_val)) round(yld_val, 1) else NA_real_,
			yield_moisture = yld_mc,
			stringsAsFactors = FALSE
		)
		if (!is.null(lv_df)) {
			lv_df$station <- station; lv_df$season <- ssn; lv_df$year <- yr
			lv_df$treatment <- trt_val; lv_df$rep <- rep_val
		}
		list(
			wide = wide,
			lv = lv_df,
			hc = if (length(hc_list)) do.call(rbind, hc_list) else NULL,
			hi = if (length(hi_list)) do.call(rbind, hi_list) else NULL,
			ha = if (length(ha_list)) do.call(rbind, ha_list) else NULL,
			hw = if (length(hw_list)) do.call(rbind, hw_list) else NULL,
			hp = if (length(hp_list)) do.call(rbind, hp_list) else NULL
		)
	}

	xl_parsed <- lapply(xl_files, parse_workbook)
	d_xl <- do.call(rbind, lapply(xl_parsed, `[[`, "wide"))
	lv_xl <- do.call(rbind, Filter(Negate(is.null), lapply(xl_parsed, `[[`, "lv")))
	hc_xl <- do.call(rbind, Filter(Negate(is.null), lapply(xl_parsed, `[[`, "hc")))
	hi_xl <- do.call(rbind, Filter(Negate(is.null), lapply(xl_parsed, `[[`, "hi")))
	ha_xl <- do.call(rbind, Filter(Negate(is.null), lapply(xl_parsed, `[[`, "ha")))
	hw_xl <- do.call(rbind, Filter(Negate(is.null), lapply(xl_parsed, `[[`, "hw")))
	hp_xl <- do.call(rbind, Filter(Negate(is.null), lapply(xl_parsed, `[[`, "hp")))

	## ---- 2. Parse ODK CSVs + Oct 2014 PhilRice NE ODK xlsx + 2015 DS harvest xlsx ----
	# ODK crop health form, IRRI Los Baños, 2014 wet season
	f1 <- ff[bn == "RICEPRE_CropHealth_2014WS_2014_10_02_23_32_28_completedata.csv"]
	r1 <- read.csv(f1, check.names=FALSE, na.strings=c("", "n/a"))
	# Same ODK form name ("2014WS"), visits Feb-Apr 2015: IRRI Los Baños, 2015 dry season
	f2 <- ff[bn == "RICEPRE_CropHealth_2014WS_2015_04_21_23_40_32.csv"]
	r2 <- read.csv(f2, check.names=FALSE, na.strings=c("", "n/a"))
	# ODK crop and injuries form, 2015 wet season, IRRI, PhilRice NE and PhilRice ADN
	f3 <- ff[bn == "_home_formcontroller_ODK_briefcase_RICEPRE_Crop_and_Injuries.csv"]
	r3 <- read.csv(f3, check.names=FALSE, na.strings="")
	f4 <- ff[bn == "_home_formcontroller_ODK_briefcase_RICEPRE_Rat_Injuries.csv"]
	r4 <- read.csv(f4, check.names=FALSE, na.strings="")
	f5 <- ff[bn == "_home_formcontroller_ODK_briefcase_RICE_PRE_Crop_Cut.csv"]
	r5 <- read.csv(f5, check.names=FALSE)
	f6 <- ff[bn == "RICEPRE 2015DS- harvest.xlsx"]
	r6 <- carobiner::read.excel(f6, sheet="per plot")
	# ODK Visits 1 & 4 for PhilRice NE 2014 DS (complements Excel Visits 2 & 3)
	f7 <- ff[bn == "Crop_and_Injuries_Oct_23_2014_03_27.xlsx"]
	r7 <- carobiner::read.excel(f7, col_types="text")
	r7$`_uuid` <- paste0("oct2014_", seq_len(nrow(r7)))

	visits14 <- function(r, season, year) {
		pm <- tolower(trimws(r$`group_crop_info/planting_method`))
		pm <- ifelse(pm == "transplanted", "transplanting", ifelse(pm == "direct_seeded", "direct seeding", NA_character_))
		data.frame(
			form_id = r$`_uuid`,
			researcher = r$`group_contact/researcher_name`,
			station = r$`group_contact/station_name`,
			season = season,
			year = as.integer(year),
			treatment = unname(trt_map[r$`group_contact/treatment_name`]),
			rep = as.integer(r$`group_contact/replicate_name`),
			visit = unname(visitno[r$`group_crop_info/visitNo_label`]),
			date = substr(r$datetime, 1, 10),
			DVS = as.integer(r$`group_crop_info/crop_stage`),
			water_status = as.integer(r$`group_crop_info/water_status`),
			variety = clean_variety(r$`group_crop_info/rice_variety`),
			planting_method = pm,
			crop_health = status_from_score(r$`crop_status/overall_cropstatus`),
			latitude = as.numeric(r$`_gps1_latitude`),
			longitude = as.numeric(r$`_gps1_longitude`),
			stringsAsFactors = FALSE
		)
	}
	v1 <- visits14(r1, "wet", 2014)
	v2 <- visits14(r2, "dry", 2015)

	# One 2015 assessment date entered as "10-Sep-04"; all other visit 2 records at PhilRice NE are "10-Sep-15"
	adate <- r3$`GenInfo-date_assessment`
	adate[adate == "10-Sep-04"] <- "10-Sep-15"
	v3 <- data.frame(
		form_id = r3$KEY,
		researcher = r3$`GenInfo-researcher_name`,
		station = r3$`GenInfo-station_name`,
		season = "wet",
		year = 2015L,
		treatment = unname(trt_map[r3$`GenInfo-treatment_name`]),
		rep = as.integer(r3$`GenInfo-replicate_name`),
		visit = unname(visitno[r3$`GenInfo-visitNo_label`]),
		date = as.character(as.Date(adate, "%d-%b-%y")),
		DVS = as.integer(r3$`group_crop_info-crop_growth_stage`),
		water_status = NA_integer_,
		variety = NA_character_,
		planting_method = NA_character_,
		crop_health = status_from_score(r3$`crop_status-overall_cropstatus`),
		latitude = r3$`barcode_number-gps1-Latitude`,
		longitude = r3$`barcode_number-gps1-Longitude`,
		stringsAsFactors = FALSE
	)

	# Oct 2014 ODK workbook = PhilRice NE 2014 DS, Visits 1 & 4
	hl7 <- r7$`group_contact/Hill_label`
	v7 <- data.frame(
		form_id = r7$`_uuid`,
		researcher = r7$`group_contact/contact_person`,
		station = "PhilRice_NE",
		season = "dry",
		year = 2014L,
		treatment = ifelse(grepl("Farmer", hl7, ignore.case=TRUE), "farmers' practice",
			ifelse(grepl("Primitive", hl7, ignore.case=TRUE), "primitive yield", "RICE-PRE")),
		rep = as.integer(sub(".*?([1-3])$", "\\1", trimws(hl7))),
		visit = unname(visitno[r7$`group_contact/visitNo_label`]),
		date = substr(r7$datetime, 1, 10),
		DVS = as.integer(r7$`group_crop_info/crop_stage`),
		water_status = as.integer(r7$`group_crop_info/water_status`),
		variety = NA_character_,
		planting_method = ifelse(tolower(r7$`group_crop_info/planting_method`) == "transplanted", "transplanting", NA_character_),
		crop_health = NA_character_,
		latitude = as.numeric(r7$`_gps1_latitude`),
		longitude = as.numeric(r7$`_gps1_longitude`),
		stringsAsFactors = FALSE
	)

	v4 <- data.frame(
		form_id = r4$KEY,
		station = r4$`barcode_number-station_name`,
		season = "wet",
		year = 2015L,
		treatment = unname(trt_map[r4$`barcode_number-treatment_name`]),
		rep = as.integer(r4$`barcode_number-replicate_name`),
		date = as.character(as.Date(r4$`barcode_number-date`, "%d-%b-%y")),
		DVS = as.integer(r4$`group_crop_info-crop_growth_stage`),
		latitude = r4$`gps1-Latitude`,
		longitude = r4$`gps1-Longitude`,
		stringsAsFactors = FALSE
	)

	# Crop cuts, IRRI 2015 WS; mean of 3 x 10 m2 cuts adjusted to 14% moisture in source
	y5 <- data.frame(
		station = r5$`Geninfo-station_name`,
		season = "wet",
		year = 2015L,
		treatment = unname(trt_map[r5$`Geninfo-treatment_name`]),
		rep = as.integer(r5$`Geninfo-replicate_name`),
		yield = round(r5$avecropcutyield * 1000, 1),
		yield_moisture = 14,
		harvest_date = as.character(as.Date(r5$`Geninfo-datecropcut`, "%d-%b-%y")),
		stringsAsFactors = FALSE
	)
	# Plot harvest, IRRI 2015 DS; mean of 3 samples per plot adjusted to 14% moisture in source
	y6 <- data.frame(
		station = "IRRI_LB",
		season = "dry",
		year = 2015L,
		treatment = unname(trt_map[r6$Treatment]),
		rep = as.integer(r6$Replicate),
		plot_id = as.character(r6$`Plot ID`),
		yield = round(r6$`T/HA` * 1000, 1),
		yield_moisture = 14,
		# Weighing date of all samples in sheet "RICE PRE harvest"
		harvest_date = "2015-04-20",
		stringsAsFactors = FALSE
	)

	# Drop ODK training/test submissions (duplicates of real plot visits)
	vv <- carobiner::bindr(v1, v2, v3, v7)
	vv <- vv[!grepl("^training|^test", vv$researcher, ignore.case=TRUE), ]
	vv$researcher <- NULL

	## ---- 3. Combine wide records (Excel Form 1 + ODK-only seasons) ----
	keys <- c("station", "season", "year", "treatment", "rep")
	odk_keys <- unique(rbind(vv[, keys], v4[, keys], y5[, keys], y6[, keys]))
	# Keep ODK plots not already present in d_xl (v7 PhilRice NE 2014 DS merges onto existing d_xl rows!)
	xl_kstr <- do.call(paste, d_xl[, keys])
	odk_kstr <- do.call(paste, odk_keys)
	d_odk <- odk_keys[!(odk_kstr %in% xl_kstr), , drop=FALSE]
	d_odk <- merge(d_odk, rbind(y5[, c(keys, "yield", "yield_moisture", "harvest_date")],
		y6[, c(keys, "yield", "yield_moisture", "harvest_date")]), by=keys, all.x=TRUE)
	d_odk <- merge(d_odk, y6[, c(keys, "plot_id")], by=keys, all.x=TRUE)

	agg_first <- function(df, col) {
		sub <- df[!is.na(df[[col]]), c(keys, col), drop=FALSE]
		if (!nrow(sub)) return(NULL)
		aggregate(sub[[col]], by=sub[, keys], FUN=function(x) paste(unique(x), collapse="; "))
	}
	for (col in c("variety", "planting_method", "crop_health")) {
		ag <- agg_first(vv, col)
		if (!is.null(ag)) {
			names(ag)[ncol(ag)] <- col
			d_odk <- merge(d_odk, ag, by=keys, all.x=TRUE)
		}
	}
	d_odk$planting_date <- as.character(d_odk$year)

	# GPS from ODK forms averaged per station x season x year (also supplies PhilRice NE 2014 DS GPS)
	gps_odk <- rbind(vv[, c("station", "season", "year", "latitude", "longitude")],
		v4[, c("station", "season", "year", "latitude", "longitude")])
	gps_odk <- gps_odk[!is.na(gps_odk$latitude) & !is.na(gps_odk$longitude), ]
	gps_odk <- aggregate(cbind(latitude, longitude) ~ station + season + year, data=gps_odk, FUN=mean)
	d_odk <- merge(d_odk, gps_odk, by=c("station", "season", "year"), all.x=TRUE)
	d_odk$geo_from_source <- !is.na(d_odk$latitude) & !is.na(d_odk$longitude)

	d <- carobiner::bindr(d_xl, d_odk)
	# Fill PhilRice NE 2014 DS GPS from v7 ODK GPS where Form 1 had blank lat/lon
	m_gps <- match(paste(d$station, d$season, d$year), paste(gps_odk$station, gps_odk$season, gps_odk$year))
	need_gps <- is.na(d$latitude) & !is.na(m_gps)
	d$latitude[need_gps] <- gps_odk$latitude[m_gps[need_gps]]
	d$longitude[need_gps] <- gps_odk$longitude[m_gps[need_gps]]
	d$geo_from_source[need_gps] <- TRUE

	# Station-level admin hierarchy & fallback coordinates for Excel seasons without per-file GPS
	# IRRI_LB, PhilRice_NE, PhilRice_ADN fallbacks = mean of GPS measured in this dataset (geo_from_source = FALSE, uncertainty 500 m);
	# PhilRice_Negros (Cansilayan, Murcia, Negros Occidental) = Wikimapia/PhilAtlas station coordinates (geo_from_source = FALSE, uncertainty 1000 m).
	geo <- data.frame(
		station = c("IRRI_LB", "PhilRice_NE", "PhilRice_ADN", "PhilRice_Negros"),
		adm1 = c("Laguna", "Nueva Ecija", "Agusan del Norte", "Negros Occidental"),
		adm2 = c("Los Baños", "Science City of Muñoz", "Remedios T. Romualdez", "Murcia"),
		location = c("IRRI, Los Baños", "PhilRice Central Experiment Station, Maligaya",
			"PhilRice Agusan, Basilisa", "PhilRice Negros, Cansilayan"),
		lat_fb = c(14.1650, 15.6743, 9.0660, 10.5622),
		lon_fb = c(121.2610, 120.8946, 125.5865, 122.9925),
		unc_fb = c(500, 500, 500, 1000),
		src_fb = c("dataset station GPS mean", "dataset station GPS mean",
			"dataset station GPS mean", "Wikimapia/PhilAtlas"),
		stringsAsFactors = FALSE
	)
	d <- merge(d, geo, by="station", all.x=TRUE)
	miss_geo <- is.na(d$latitude) | is.na(d$longitude)
	d$latitude[miss_geo] <- d$lat_fb[miss_geo]
	d$longitude[miss_geo] <- d$lon_fb[miss_geo]
	d$geo_from_source[miss_geo] <- FALSE
	d$geo_uncertainty <- ifelse(miss_geo, d$unc_fb, NA_real_)
	d$geo_source <- ifelse(miss_geo, d$src_fb, NA_character_)
	d$lat_fb <- d$lon_fb <- d$unc_fb <- d$src_fb <- NULL

	d$trial_id <- paste(d$station, d$year, d$season, sep="_")
	d$country <- "Philippines"
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	# "RICE-PRE Generic Protocol.docx": Agroecology 3, irrigated rice (confirmed by Form 1 Irrigated checkboxes)
	d$irrigated <- TRUE
	d$stress <- "pests;disease;weeds"

	# Sort deterministically and assign record_id
	d <- d[order(d$trial_id, d$treatment, d$rep), ]
	rownames(d) <- NULL
	d$record_id <- seq_len(nrow(d))

	## ---- 4. Build long table dd (Excel Form 2 + ODK hill/area/weed/rat observations) ----
	rid_of <- function(x) {
		tid <- paste(x$station, x$year, x$season, sep="_")
		d$record_id[match(paste(tid, x$treatment, x$rep), paste(d$trial_id, d$treatment, d$rep))]
	}
	lv_xl$record_id <- rid_of(lv_xl)
	vv$record_id <- rid_of(vv)
	v4$record_id <- rid_of(v4)

	# Visit rows from ODK
	lv_odk <- data.frame(
		form_id = vv$form_id,
		record_id = vv$record_id,
		date = vv$date,
		visit_ = vv$visit,
		DVS_ = vv$DVS,
		growth_stage = unname(dvs_map[as.character(vv$DVS)]),
		water_status_ = vv$water_status,
		stringsAsFactors = FALSE
	)
	lv_xl_clean <- lv_xl[, c("form_id", "record_id", "date", "visit_", "DVS_", "growth_stage", "water_status_")]

	# ODK hill-level observations (10 hills per plot and visit, across r1, r2, r3, r7)
	odk_hills <- function(r, id) {
		hv <- grep("^group_[0-9]+[-/](group_number|group_pest|group_diseases|hill|group_others)", names(r), value=TRUE)
		hv <- hv[!grepl("note_", hv)]
		data.frame(
			form_id = rep(id, length(hv)),
			hill_ = rep(as.integer(sub("^group_([0-9]+).*", "\\1", hv)), each=length(id)),
			var = rep(sub("^others[0-9]+_", "others_", sub("[0-9]+$", "", sub(".*[-/]", "", hv))), each=length(id)),
			value = trimws(as.character(unlist(r[hv], use.names=FALSE))),
			stringsAsFactors = FALSE
		)
	}
	h <- rbind(
		odk_hills(r1, r1$`_uuid`),
		odk_hills(r2, r2$`_uuid`),
		odk_hills(r3, r3$KEY),
		odk_hills(r7, r7$`_uuid`)
	)
	h <- h[!is.na(h$value) & nzchar(h$value) & h$form_id %in% vv$form_id, ]
	# Fix typos in Oct 2014 ODK column names ("bacterialleaftblight" / "bacterialleaftstreak")
	h$var[h$var == "disease_bacterialleaftblight"] <- "disease_bacterialleafblight"
	h$var[h$var == "disease_bacterialleaftstreak"] <- "disease_bacterialleafstreak"

	hc_odk <- h[h$var %in% c("tiller_hill", "panicle_hill", "leaves_tiller"), ]
	hc_odk$value <- as.numeric(hc_odk$value)
	hc_odk <- reshape(hc_odk, idvar=c("form_id", "hill_"), timevar="var", direction="wide")
	names(hc_odk) <- c("form_id", "hill_", "tillers_hill_", "panicles_hill_", "leaves_tiller_")[
		match(names(hc_odk), c("form_id", "hill_", "value.tiller_hill", "value.panicle_hill", "value.leaves_tiller"))
	]

	# 2015 WS ODK "other" tiller injuries (all are rat: "Rat", "Rt", "Ra")
	ho <- h[h$var %in% c("others_nametiller", "others_numbertiller"), ]
	ho <- reshape(ho, idvar=c("form_id", "hill_"), timevar="var", direction="wide")
	ho <- data.frame(
		form_id = ho$form_id,
		hill_ = ho$hill_,
		var = ifelse(tolower(ho$value.others_nametiller) %in% c("rat", "rt", "ra"), "pest_rat", ho$value.others_nametiller),
		value = ho$value.others_numbertiller,
		stringsAsFactors = FALSE
	)

	ik <- data.frame(
		var = c("pest_bird", "pest_ricebug", "pest_ricegrainbug", "pest_whiteheadstemborer", "pest_whiteheadblackbug", "pest_whitehead",
			"disease_dirtypanicle", "disease_falsesmut", "disease_neckblast",
			"pest_leaffolder", "pest_leafminer", "pest_ricehispa", "pest_thrip", "pest_whorlmaggot", "pest_defoliators",
			"disease_bacterialleafblight", "disease_bacterialleafstreak", "disease_brownspot", "disease_leafblast",
			"disease_leafscald", "disease_narrowbrownspot", "disease_redstripe",
			"pest_deadheartstemborer", "pest_deadheartblackbug", "pest_deadheart", "pest_silvershoot", "pest_rat",
			"disease_bakanae", "disease_sheathblight", "disease_sheathrot", "disease_stemrot"),
		injury_ = c("bird injury", "rice bug injury", "rice grain bug injury", "whitehead", "whitehead", "whitehead",
			"dirty panicle", "false smut", "neck blast",
			"leaffolder injury", "leafminer injury", "rice hispa injury", "thrips injury", "whorl maggot injury", "defoliator injury",
			"bacterial leaf blight", "bacterial leaf streak", "brown spot", "leaf blast",
			"leaf scald", "narrow brown spot", "red stripe",
			"deadheart", "deadheart", "deadheart", "silvershoot", "rat injury",
			"bakanae", "sheath blight", "sheath rot", "stem rot"),
		organ_ = rep(c("panicle", "leaf", "tiller"), c(9, 13, 9)),
		pest_species = c("birds", "rice bug", "rice grain bug", "stem borer", "rice black bug", NA,
			NA, NA, NA,
			"rice leaffolder", "leafminer", "rice hispa", "thrips", "rice whorl maggot", "defoliators",
			rep(NA, 7),
			"stem borer", "rice black bug", NA, "gall midge", "rats",
			NA, NA, NA, NA),
		disease = c(rep(NA, 6), "dirty panicle", "false smut", "neck blast",
			rep(NA, 6),
			"bacterial leaf blight", "bacterial leaf streak", "brown spot", "leaf blast",
			"leaf scald", "narrow brown spot", "red stripe",
			rep(NA, 5),
			"bakanae", "sheath blight", "sheath rot", "stem rot"),
		stringsAsFactors = FALSE
	)
	hi_odk <- rbind(h[h$var %in% ik$var, ], ho)
	hi_odk$injury_count_ <- as.numeric(hi_odk$value)
	hi_odk <- merge(hi_odk[, c("form_id", "hill_", "var", "injury_count_")], ik, by="var")
	hi_odk <- merge(hi_odk, hc_odk, by=c("form_id", "hill_"), all.x=TRUE)
	nunits <- ifelse(hi_odk$organ_ == "tiller", hi_odk$tillers_hill_,
		ifelse(hi_odk$organ_ == "panicle", hi_odk$panicles_hill_, hi_odk$tillers_hill_ * hi_odk$leaves_tiller_))
	nunits[nunits == 0] <- NA
	inc <- round(100 * hi_odk$injury_count_ / nunits, 2)
	hi_odk$disease_incidence <- ifelse(is.na(hi_odk$disease), NA_character_, ifelse(is.na(inc), NA_character_, as.character(inc)))
	hi_odk$pest_injury_incidence_ <- ifelse(is.na(hi_odk$disease), inc, NA_real_)
	hi_odk$var <- hi_odk$tillers_hill_ <- hi_odk$panicles_hill_ <- hi_odk$leaves_tiller_ <- NULL

	# ODK whole-area injuries (areas A-E)
	odk_areas <- function(r, id) {
		av <- grep("[-/]area_(bugburn|hopperburn|gas|tungro|grassy|ragged|orangeleaf|oramgeleaf|yellowdwarf|yellowing)[A-E]$", names(r), value=TRUE)
		data.frame(
			form_id = rep(id, length(av)),
			subarea_ = rep(sub(".*([A-E])$", "\\1", av), each=length(id)),
			var = rep(sub(".*area_([a-z]+)[A-E]$", "\\1", av), each=length(id)),
			injury_count_ = as.numeric(unlist(r[av], use.names=FALSE)),
			stringsAsFactors = FALSE
		)
	}
	ha_odk <- rbind(odk_areas(r1, r1$`_uuid`), odk_areas(r2, r2$`_uuid`), odk_areas(r3, r3$KEY), odk_areas(r7, r7$`_uuid`))
	ha_odk <- ha_odk[!is.na(ha_odk$injury_count_) & ha_odk$form_id %in% vv$form_id, ]
	ak <- data.frame(
		var = c("bugburn", "hopperburn", "gas", "tungro", "grassy", "ragged", "orangeleaf", "oramgeleaf", "yellowdwarf", "yellowing"),
		injury_ = c("bugburn", "hopperburn", "golden apple snail injury", "rice tungro", "rice grassy stunt",
			"rice ragged stunt", "rice orange leaf", "rice orange leaf", "rice yellow dwarf", "yellowing syndrome"),
		pest_species = c("rice black bug", "planthoppers", "golden apple snail", rep(NA, 7)),
		disease = c(NA, NA, NA, "rice tungro", "rice grassy stunt", "rice ragged stunt",
			"rice orange leaf", "rice orange leaf", "rice yellow dwarf", "yellowing syndrome"),
		stringsAsFactors = FALSE
	)
	ha_odk <- merge(ha_odk, ak, by="var")
	ha_odk$organ_ <- "hill"
	ha_odk$var <- NULL

	# ODK weeds by area (A-C)
	odk_weeds <- function(r, id) {
		last <- sub(".*[-/]", "", names(r))
		wv <- grep("^(weedabove_area|weedbelow_area|above_rice|below_rice|weed_broadleaved|broad_leaved|weed_grass|weed_sedge|weed_small|water_status|weeds_broadleaved|weeds_grass|weeds_sedge|weed_otherarea)[ABC]$", last)
		sv <- grep("weed_listarea[ABC]/", names(r))
		vraw <- sub("_?area$", "", sub("[ABC]$", "", last[wv]))
		vraw[vraw == "above_rice"] <- "weedabove"
		vraw[vraw == "below_rice"] <- "weedbelow"
		vraw[vraw == "broad_leaved"] <- "weed_broadleaved"
		x <- data.frame(
			form_id = rep(id, length(wv)),
			subarea_ = rep(sub(".*([ABC])$", "\\1", last[wv]), each=length(id)),
			var = rep(vraw, each=length(id)),
			value = trimws(as.character(unlist(r[wv], use.names=FALSE))),
			stringsAsFactors = FALSE
		)
		if (length(sv) > 0) {
			s <- data.frame(
				form_id = rep(id, length(sv)),
				subarea_ = rep(sub(".*weed_listarea([ABC])/.*", "\\1", names(r)[sv]), each=length(id)),
				var = "weeds_list",
				value = rep(gsub("_", " ", sub(".*/", "", names(r)[sv])), each=length(id)),
				present = as.logical(unlist(r[sv], use.names=FALSE)),
				stringsAsFactors = FALSE
			)
			s <- s[s$present %in% TRUE, ]
			s$present <- NULL
			x <- rbind(x, s)
		}
		x[!is.na(x$value) & nzchar(x$value), ]
	}
	hw_odk <- rbind(odk_weeds(r1, r1$`_uuid`), odk_weeds(r2, r2$`_uuid`), odk_weeds(r3, r3$KEY), odk_weeds(r7, r7$`_uuid`))
	hw_odk <- hw_odk[hw_odk$form_id %in% vv$form_id, ]
	spp <- hw_odk[hw_odk$var %in% c("weeds_broadleaved", "weeds_grass", "weeds_sedge", "weed_other", "weeds_list"), ]
	# Drop placeholder "none"/"0"/"O" typed into weed_otherarea* in 2015 DS ODK
	spp <- spp[!tolower(spp$value) %in% c("0", "o", "none", "non", "nene", "nome"), ]
	spp <- aggregate(value ~ form_id + subarea_, data=spp,
		FUN=function(x) paste(unique(unlist(strsplit(x, " (?=[A-Z]{3})", perl=TRUE))), collapse="; "))
	names(spp)[3] <- "weed_species"
	wr <- hw_odk[hw_odk$var %in% c("weedabove", "weedbelow", "weed_broadleaved", "weed_grass", "weed_sedge", "weed_small", "water_status"), ]
	wr$value <- as.numeric(wr$value)
	wr <- reshape(wr, idvar=c("form_id", "subarea_"), timevar="var", direction="wide")
	wnames <- c(
		value.weedabove="weed_above_rating_", value.weedbelow="weed_below_rating_",
		value.weed_broadleaved="weed_rank_broadleaved_", value.weed_grass="weed_rank_grass_",
		value.weed_sedge="weed_rank_sedge_", value.weed_small="weed_rank_small_", value.water_status="water_status_"
	)
	i <- names(wr) %in% names(wnames)
	names(wr)[i] <- wnames[names(wr)[i]]
	hw_odk <- merge(wr, spp, by=c("form_id", "subarea_"), all=TRUE)

	# 2015 WS rat injury form (3 strata x 8 sampling points)
	rv <- grep("^stratum_[0-9]-sp[0-9]_[0-9]?(total|injured)$", names(r4), value=TRUE)
	rt <- data.frame(
		form_id = rep(r4$KEY, length(rv)),
		stratum_ = rep(as.integer(sub("^stratum_([0-9]).*", "\\1", rv)), each=nrow(r4)),
		point_ = rep(as.integer(sub("^stratum_[0-9]-sp([0-9]).*", "\\1", rv)), each=nrow(r4)),
		var = rep(sub(".*(total|injured)$", "\\1", rv), each=nrow(r4)),
		value = unlist(r4[rv], use.names=FALSE),
		stringsAsFactors = FALSE
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
		organ_ = "tiller",
		stringsAsFactors = FALSE
	)
	rt <- merge(rt, data.frame(form_id=v4$form_id, record_id=v4$record_id, date=v4$date,
		DVS_=v4$DVS, growth_stage=unname(dvs_map[as.character(v4$DVS)]), stringsAsFactors=FALSE), by="form_id")

	# Combine all long-table observation types and attach record_id + date + visit_
	lv_all <- carobiner::bindr(lv_xl_clean, lv_odk)
	fv <- lv_all[, c("form_id", "record_id", "date", "visit_")]
	hc_all <- merge(fv, carobiner::bindr(hc_xl, hc_odk), by="form_id")
	hi_all <- merge(fv, carobiner::bindr(hi_xl, hi_odk), by="form_id")
	ha_all <- merge(fv, carobiner::bindr(ha_xl, ha_odk), by="form_id")
	hw_all <- merge(fv, carobiner::bindr(hw_xl, hw_odk), by="form_id")
	hp_all <- if (!is.null(hp_xl) && nrow(hp_xl) > 0) merge(fv, hp_xl, by="form_id") else NULL

	dd <- carobiner::bindr(lv_all, hc_all, hi_all, ha_all, hw_all, hp_all, rt)
	dd$form_id <- NULL

	d$station <- NULL
	d$year <- NULL

	carobiner::write_files(path, meta, d, long=dd)
}
