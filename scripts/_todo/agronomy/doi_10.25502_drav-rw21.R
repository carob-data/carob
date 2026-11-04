# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2020 on-station Monsoon trials at IITA, Ijaye, Ago-Owu and Onne. Cassava was harvested in 2021 (Year on the harvest file).
# Cassava yield is from cassava-harvest-monocrop.csv, which also holds the intercrop plots.
# cassava-harvest-intercrop.csv repeats those intercrop rows (all shared measurements matched) and was not read again.
# Maize grain yield is Gyieldtha (t/ha -> kg/ha) with Perc_mc as yield_moisture. StGraintha is close but not identical and was not used.
# Stem weight is fwy_stems from StmWttha. The dictionary calls the per-plot stem weight fresh.
# Plot-level weed scores, sprout counts and biomass, and species densities, are in the long table (DAP = WAP * 7).
# Weed scores are mapped as percent efficacy. This dictionary does not say percent; values in the monocrop sprout file are 80-100, and the 2018/2019 files of the project call the same ratings percent.
# Biomass is g/m2 * 10 = kg/ha and is stored as dry matter because the study abstract says weed dry matter. This dictionary only says g/m2.
# Species are numeric codes. Names are not in this download.
# Observation files in this package label the observation date HarvestDate. That column is an observation date except in the two harvest files.
# Dates are parsed per column: a value with day > 12 is day/month/year (25/10/2020); a value with month > 12 is month/day/year (6/16/2021, 9/14/2020). If a column has only one of those, the remaining ambiguous values follow it.
# Monocrop treatment codes are 1-8 and intercrop codes are 1-5. They are kept as codes. trial_id separates monocrop and intercrop so the codes are not pooled.
# States for IITA (Oyo), Ago-Owu (Osun) and Onne (Rivers) are the states used for those site names in doi:10.25502/rnbm-3x44/d and doi:10.25502/g4vs-5z61/d. Ijaye is not in those files.
# Net plot area is the NetPltm2 column (19.2 m2).
# Long rows are attached to the cassava row. The maize row of an intercrop plot has the same plot_id.

## ISSUES
# No treatment legend, so Monsoon, paraquat, hoe and the 2020 no-sprayhood treatment cannot be assigned to codes 1-8. herbicide_product is omitted. Do not guess.
# No crop-injury column, no fertilizer, variety, irrigation, or planting date. No coordinates. Do not use the CKAN coverage point (9.60 E, 8.00 N); that is not a trial site.
# Georeference: IITA (Oyo), Ijaye (state not in this file or the 2018/2019 site lists), Ago-Owu (Osun), Onne (Rivers).
# Onne has weed and sprout records but no cassava harvest. Maize harvest covers IITA and Ijaye only, so Ago-Owu intercrop maize yield is missing.
# Some observation dates fall in January 2020 because that column is month/day/year. Left as parsed; they sit oddly next to 8-15 WAP.
# fwy_stems exceeds 21000 kg/ha on some plots (StmWttha up to about 25.5 t/ha). Left as recorded.
# cyprosulfamide is not in this file (treatments are codes).
# No publication DOI.
# write_files: missing carob_contributor, longitude, latitude, irrigated, N/P/K, planting_date;
#   NA yield (Onne, and Ago-Owu maize); out of bounds fwy_stems (up to about 25.5 t/ha);
#   unknown variables in NEW VARIABLES. sprout_density_ is a sprout count, not shrub_density; maize_density_ is not vine_density.
# Terminag GitHub 403 warnings, if printed, are from the vocabulary check.

## NOT PROCESSED
# cassava-harvest-intercrop.csv: duplicate of the intercrop rows in cassava-harvest-monocrop.csv.
# TrtNo: a running index, not the treatment (TRT is the treatment).
# Per-plot plant, stem and root counts and weights (NoPlt, NoStem, StmWtkg, NoOkRt, OkRtWtKg, NoBdRt, BdRtWtkg): per-ha columns are used, and plot_area is kept.
# BdRtWttha: damaged root weight; yield is total roots and yield_marketable is the marketable weight.
# Row sprout counts (CassSprtCntR*, MzSprtCntR*) and MzCnt: per-ha columns are used.
# Quadrat counts (Q1-Q3, Bl_q*, GR_q*, Sg_q*): per-m2 columns are used.
# Maize subsample cob and grain weights, StoverWtKg, StovDwtKg, MC/Mc (about 100 minus Perc_mc), GrainsMC (about 250, not a percent), StGraintha.
# Empty extra columns on the density files.
# bayer-trials-on-monsoon_data_dictionary.csv: definitions only.
# Id / ID: row numbers.

## NEW VARIABLES
# marketable_root_density_: marketable cassava roots per ha.
# damaged_root_density_: damaged cassava roots per ha.
# weed_control_: total visual control, treated as percent (see NOTES).
# weed_control_broadleaf_: broadleaf control, same scale.
# weed_control_grass_: grass control, same scale.
# weed_control_sedge_: sedge control, same scale.
# weed_dmy_broadleaf_: broadleaf weed biomass (kg/ha), treated as dry matter.
# weed_dmy_grass_: grass weed biomass (kg/ha).
# weed_dmy_sedge_: sedge weed biomass (kg/ha).
# weed_dmy_: total weed biomass (kg/ha).
# sprout_density_: cassava sprouts (plants/ha) at that DAP.
# maize_density_: maize plants/ha at that DAP.


carob_script <- function(path) {

"
Evaluation of foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide for post-emergence weed control in cassava (Bayer_trials_on_Monsoon)

Monsoon Active [foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide ] was evaluated for post-emergence weed control cassava and cassava/maize intercrop for three years at six different sites across five States [Benue, Ogun, Osun, Oyo, and the Rivers States] in Nigeria. Monsoon was compared with Paraquat, a toxic herbicide already marked for de-registration in Nigeria. The evaluation included manual hoe weeding treatments. All post-emergence herbicides treatments in the assessment were applied under the canopy with a backpack spray fitted with a sprayhood. In 2020, a treatment where Monsoon was used under cassava canopy without a sprayhood was included in the assessment. All the trials were set up in a randomized complete block design and replicated four times. Cassava was monitored for injury. Treatment efficacy was assessed visually and from weed dry matter. Cassava and maize population at crop harvest and yield were assessed. The result showed that Monsoon was superior to Paraquat and manual hoe weeding in reducing weed pressure at most sites. Monsoon provided excellent control of major cassava weeds at all the sites. Monsoon did not affect crop stand negatively or depress cassava yield in both monocrop and intercropped plots. No crop injury was observed when Monsoon was applied under cassava canopy with or without a sprayhood at 7 to 8 weeks after planting cassava, indicating that Monsoon is suitable for post-emergence weed control in cassava and a potential replacement for Paraquat.
"

	uri <- "doi:10.25502/drav-rw21"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "PJ-NA2910",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield;fwy_stems;plant_density",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 58,
		carob_effort = 1.2
	)

	readf <- function(bn) {
		f <- ff[basename(ff) == bn]
		read.csv(f, na.strings = c("", " ", "NA", "na"), check.names = FALSE)
	}
	colv <- function(r, nms) {
		nm <- nms[nms %in% names(r)]
		if (length(nm) == 0) return(rep(NA, nrow(r)))
		r[[nm[1]]]
	}
	num <- function(x) {
		x <- trimws(as.character(x))
		x[x == ""] <- NA
		as.numeric(x)
	}
	norm_loc <- function(x) {
		k <- gsub("[^a-z]", "", tolower(as.character(x)))
		ifelse(k %in% c("iita", "iitaibadan"), "IITA",
			ifelse(k == "agoowu", "Ago-Owu",
			ifelse(k == "onne", "Onne",
			ifelse(k == "ijaye", "Ijaye", trimws(as.character(x))))))
	}
	norm_sys <- function(x) {
		k <- gsub("[^a-z]", "", tolower(as.character(x)))
		ifelse(k %in% c("monocrop", "cassava"), "monocrop",
			ifelse(k %in% c("intercrop", "intecrop", "cassmaizentecrop", "cassavamaize"), "intercrop", NA_character_))
	}
	adm_of <- function(loc) {
		ifelse(loc == "IITA", "Oyo", ifelse(loc == "Ago-Owu", "Osun", ifelse(loc == "Onne", "Rivers", NA_character_)))
	}
	parse_dates <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | x == ""] <- NA
		parts <- strsplit(x, "/", fixed = TRUE)
		kind <- rep(NA_character_, length(x))
		aa <- bb <- yy <- rep(NA_integer_, length(x))
		for (i in seq_along(parts)) {
			if (is.na(x[i]) || length(parts[[i]]) != 3) next
			aa[i] <- as.integer(parts[[i]][1])
			bb[i] <- as.integer(parts[[i]][2])
			yy[i] <- as.integer(parts[[i]][3])
			if (is.na(aa[i]) || is.na(bb[i]) || is.na(yy[i])) next
			if (aa[i] > 12) kind[i] <- "dmy"
			else if (bb[i] > 12) kind[i] <- "mdy"
			else kind[i] <- "amb"
		}
		known <- unique(kind[kind %in% c("dmy", "mdy")])
		fmt <- if (length(known) == 1) known else NA_character_
		out <- rep(NA_character_, length(x))
		for (i in seq_along(parts)) {
			k <- kind[i]
			if (is.na(k)) next
			if (k == "amb") k <- fmt
			if (is.na(k) || is.na(aa[i])) next
			if (k == "dmy") out[i] <- sprintf("%04d-%02d-%02d", yy[i], bb[i], aa[i])
			if (k == "mdy") out[i] <- sprintf("%04d-%02d-%02d", yy[i], aa[i], bb[i])
		}
		out
	}
	keys_of <- function(r) {
		data.frame(
			location = norm_loc(colv(r, "Loc")),
			system = norm_sys(colv(r, "CropSystem")),
			plot_id = trimws(as.character(colv(r, c("PlotNo", "Plot")))),
			rep = as.integer(num(colv(r, "Rep"))),
			treatment = trimws(as.character(colv(r, c("TRT", "Trt", "TrT"))))
		)
	}

	obs_names <- c("sproutweed-controlefficacy_mono.csv", "sproutweedcontrolefficacyinter.csv",
		"weedbiomass_mono.csv", "weedbiomass_inter.csv", "weeddensity_mono.csv", "weeddensity_inter.csv",
		"maizeexterbishment.csv", "maizeharvest.csv")
	key_list <- lapply(obs_names, function(bn) keys_of(readf(bn)))
	if (any(basename(ff) == "cassava-harvest-monocrop.csv")) {
		cas <- readf("cassava-harvest-monocrop.csv")
		kcas <- keys_of(cas)
		kcas$yield <- num(cas$TRtYldtha) * 1000
		kcas$yield_marketable <- num(cas$OkRtWttha) * 1000
		kcas$fwy_stems <- num(cas$StmWttha) * 1000
		kcas$plant_density <- num(cas$NoPltha)
		kcas$stem_density <- num(cas$NoStemha)
		kcas$marketable_root_density_ <- num(cas$NoOkRtha)
		kcas$damaged_root_density_ <- num(cas$NoBdRtha)
		kcas$plot_area <- num(cas$NetPltm2)
		kcas$harvest_date <- parse_dates(cas$HarvestDate)
		key_list <- c(list(kcas), key_list)
	} else {
		kcas <- NULL
	}
	mz <- readf("maizeharvest.csv")
	kmz <- keys_of(mz)
	kmz$mz_yield <- num(mz$Gyieldtha) * 1000
	kmz$mz_moisture <- num(mz$Perc_mc)
	kmz$mz_density <- num(mz$Mstandha)
	kmz$mz_residue <- num(mz$StvYldtha) * 1000
	kmz$mz_harvest <- parse_dates(colv(mz, c("HarvestDate", "Date")))

	allk <- do.call(rbind, lapply(key_list, function(k) k[, c("location", "system", "plot_id", "rep", "treatment")]))
	allk <- allk[!is.na(allk$plot_id) & allk$plot_id != "" & !is.na(allk$system) & !is.na(allk$location), ]
	allk <- allk[order(is.na(allk$treatment), is.na(allk$rep)), ]
	allk <- allk[!duplicated(allk[, c("location", "system", "plot_id")]), ]

	if (is.null(kcas)) {
		yld <- data.frame(location = character(), system = character(), plot_id = character(),
			yield = numeric(), yield_marketable = numeric(), fwy_stems = numeric(),
			plant_density = numeric(), stem_density = numeric(), marketable_root_density_ = numeric(),
			damaged_root_density_ = numeric(), plot_area = numeric(), harvest_date = character()
		)
	} else {
		yld <- kcas[!is.na(kcas$plot_id) & kcas$plot_id != "",
			c("location", "system", "plot_id", "yield", "yield_marketable", "fwy_stems", "plant_density",
				"stem_density", "marketable_root_density_", "damaged_root_density_", "plot_area", "harvest_date")]
	}
	mzv <- kmz[!is.na(kmz$plot_id) & kmz$plot_id != "",
		c("location", "system", "plot_id", "mz_yield", "mz_moisture", "mz_density", "mz_residue", "mz_harvest")]
	d <- merge(allk, yld, by = c("location", "system", "plot_id"), all.x = TRUE)
	d <- merge(d, mzv, by = c("location", "system", "plot_id"), all.x = TRUE)
	d$rep <- as.integer(d$rep)
	d$plot_key <- paste(d$location, d$system, d$plot_id, sep = "|")
	d$adm1 <- adm_of(d$location)
	d$country <- "Nigeria"
	d$crop <- "cassava"
	d$intercrops <- ifelse(d$system == "intercrop", "maize", "none")
	d$yield_part <- "roots"
	d$yield_isfresh <- TRUE
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	inter <- d$system == "intercrop"
	mzrows <- d[inter, ]
	mzrows$crop <- "maize"
	mzrows$intercrops <- "cassava"
	mzrows$yield <- mzrows$mz_yield
	mzrows$yield_part <- "grain"
	mzrows$yield_isfresh <- FALSE
	mzrows$yield_moisture <- mzrows$mz_moisture
	mzrows$plant_density <- mzrows$mz_density
	mzrows$dmy_residue <- mzrows$mz_residue
	mzrows$fwy_stems <- NA_real_
	mzrows$stem_density <- NA_real_
	mzrows$yield_marketable <- NA_real_
	mzrows$marketable_root_density_ <- NA_real_
	mzrows$damaged_root_density_ <- NA_real_
	mzrows$harvest_date <- mzrows$mz_harvest
	d$yield_moisture <- NA_real_
	d$dmy_residue <- NA_real_
	d <- rbind(d, mzrows)
	d$mz_yield <- d$mz_moisture <- d$mz_density <- d$mz_residue <- d$mz_harvest <- NULL
	d$location <- carobiner::fix_name(d$location, "title")
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$trial_id <- paste(d$location, d$system, "2020", sep = "_")
	d$system <- NULL
	d$record_id <- as.integer(seq_len(nrow(d)))
	for (nm in c("yield_marketable", "fwy_stems", "stem_density", "marketable_root_density_", "damaged_root_density_", "plot_area", "dmy_residue")) {
		if (nm %in% names(d) && all(is.na(d[[nm]]))) d[[nm]] <- NULL
	}

	meas_row <- function(r, sprout, maize) {
		k <- keys_of(r)
		ok <- !is.na(k$plot_id) & k$plot_id != "" & !is.na(k$system)
		wap <- num(colv(r, "WAP"))
		data.frame(
			plot_key = paste(k$location, k$system, k$plot_id, sep = "|")[ok],
			DAP = as.integer(wap * 7)[ok],
			date = parse_dates(colv(r, c("Date", "HarvestDate")))[ok],
			weed_control_ = if (sprout) num(colv(r, "WCRTotal"))[ok] else rep(NA_real_, sum(ok)),
			weed_control_broadleaf_ = if (sprout) num(colv(r, "WCRBL"))[ok] else rep(NA_real_, sum(ok)),
			weed_control_grass_ = if (sprout) num(colv(r, "WCRGR"))[ok] else rep(NA_real_, sum(ok)),
			weed_control_sedge_ = if (sprout) num(colv(r, "WCRSG"))[ok] else rep(NA_real_, sum(ok)),
			sprout_density_ = if (sprout) num(colv(r, "CaSprtha"))[ok] else rep(NA_real_, sum(ok)),
			maize_density_ = if (maize) num(colv(r, "MzCntha"))[ok] else rep(NA_real_, sum(ok)),
			weed_dmy_broadleaf_ = if (!sprout && !maize) num(colv(r, "BLBiogm2"))[ok] * 10 else rep(NA_real_, sum(ok)),
			weed_dmy_grass_ = if (!sprout && !maize) num(colv(r, "GrBiogm2"))[ok] * 10 else rep(NA_real_, sum(ok)),
			weed_dmy_sedge_ = if (!sprout && !maize) num(colv(r, "SgBiogm2"))[ok] * 10 else rep(NA_real_, sum(ok)),
			weed_dmy_ = if (!sprout && !maize) num(colv(r, "TBiogm2"))[ok] * 10 else rep(NA_real_, sum(ok))
		)
	}
	blocks <- list(
		meas_row(readf("sproutweed-controlefficacy_mono.csv"), TRUE, FALSE),
		meas_row(readf("sproutweedcontrolefficacyinter.csv"), TRUE, FALSE),
		meas_row(readf("maizeexterbishment.csv"), FALSE, TRUE),
		meas_row(readf("weedbiomass_mono.csv"), FALSE, FALSE),
		meas_row(readf("weedbiomass_inter.csv"), FALSE, FALSE)
	)
	plot_long <- blocks[[1]]
	for (i in 2:length(blocks)) {
		b <- blocks[[i]]
		plot_long <- merge(plot_long, b, by = c("plot_key", "DAP"), all = TRUE, suffixes = c("", ".b"))
		if ("date.b" %in% names(plot_long)) {
			plot_long$date <- ifelse(is.na(plot_long$date) | plot_long$date == "", plot_long$date.b, plot_long$date)
			plot_long$date.b <- NULL
		}
		bnms <- names(plot_long)[grepl("\\.b$", names(plot_long))]
		for (nm in bnms) {
			base <- sub("\\.b$", "", nm)
			plot_long[[base]] <- ifelse(is.na(plot_long[[base]]), plot_long[[nm]], plot_long[[base]])
			plot_long[[nm]] <- NULL
		}
	}

	sp_pieces <- list()
	for (bn in c("weeddensity_mono.csv", "weeddensity_inter.csv")) {
		r <- readf(bn)
		k <- keys_of(r)
		ok <- !is.na(k$plot_id) & k$plot_id != "" & !is.na(k$system) & !is.na(num(colv(r, "Denm2")))
		sp_pieces[[bn]] <- data.frame(
			plot_key = paste(k$location, k$system, k$plot_id, sep = "|")[ok],
			DAP = as.integer(num(colv(r, "WAP")) * 7)[ok],
			date = parse_dates(colv(r, c("Date", "HarvestDate")))[ok],
			weed_species = as.character(colv(r, "SpeciesCode"))[ok],
			weed_density = num(colv(r, "Denm2"))[ok] * 10000
		)
	}
	sp <- do.call(rbind, sp_pieces)
	dlong <- carobiner::bindr(plot_long, sp)
	mcols <- c("weed_control_", "weed_control_broadleaf_", "weed_control_grass_", "weed_control_sedge_",
		"sprout_density_", "maize_density_", "weed_dmy_broadleaf_", "weed_dmy_grass_", "weed_dmy_sedge_", "weed_dmy_", "weed_density")
	dlong <- dlong[rowSums(!is.na(dlong[, mcols])) > 0, ]
	cas_key <- d[d$crop == "cassava", c("plot_key", "record_id")]
	dlong <- merge(dlong, cas_key, by = "plot_key", all.x = TRUE)
	dlong$plot_key <- NULL
	d$plot_key <- NULL

	carobiner::write_files(path, meta, d, long = dlong)
}
