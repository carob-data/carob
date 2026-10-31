# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm alternative cropping-system trial, rabi 2015-16, in five Dhanusha nodes. The dataset title names Nepal. geographicCoverage is empty and no sheet has a country column.
# The measured crop is the rabi crop. Treatment is the field-book system name (tillage and rotation).
# Grain yield is the air-dry t/ha column, stored as kg/ha. Moisture is not reported.
# P and K are the P2O5 and K2O columns divided by 2.29 and 1.2051. N is already elemental kg/ha.
# Zero-tillage treatments are land_prep_method "none" (the terminag term for zero tillage). Conventional tillage is "conventional".
# Harvest index in the sheet is a fraction. It is stored as a percent.
# Row spacing 0 is broadcast, so it is left empty. Thousand-grain weight 0 is left empty.
# "Rice-potato" is the same treatment as "Rice-Potato". "Rice-CTMustard" is the same spacing as "Rice-CT Mustard".

## ISSUES
# Latitude and longitude columns on the site sheet are empty. In Phulgama those columns hold cropping-system text, not coordinates.
# Plot size is 400 m2 on every row. The vocabulary maximum for plot_area is 350.
# Biomass and straw are not labeled fresh or dry. Harvest index equals grain yield divided by biomass.
# Sinojoda phenology has no treatment header. Rows are aligned to the grain-harvest sheet by site and order.
# Mustard crop-failure rows have no crop name. The crop is taken from the treatment "Rice-Mustard". Those yields are 0. The low-yield message is that failure, not a tons/kg mix-up.
# Bean in "Rice-Maize+Bean" is not identified beyond "bean".
# write_files: missing variables: yield_moisture, country, latitude, longitude, irrigated. unknown variables: biomass_kg_ha_, straw_kg_ha_. out of bounds: plot_area (400, 400). invalid terms: crop_rotation: bean. invalid terms: intercrops: bean. low yield (tons not kg?): mustard. contributor:

## NEW VARIABLES
# biomass_kg_ha_: Biomass.t.ha converted to kg/ha. Fresh or dry is not stated.
# straw_kg_ha_: Straw.yield.t.ha converted to kg/ha. Fresh or dry is not stated.

## NOT PROCESSED
# Daily rainfall, land preparation operations, irrigation, ponding, pesticides, weeds, pests, diseases.
# Biomass subsample weights, soil analysis, farm-gate prices, and fertilizer product prices.
# Farmer names. Previous-season history on the site sheet (mostly empty).
# Plant counts and days-after-seeding calculations.


carob_script <- function(path) {

"
4.3-Rabi (winter)crops-all nodes-Alternative cropping systems trial-Dhanusha-Nepal

Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to existing systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10547996"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT;NARC",
		publication = NA,
		project = "Alternative cropping systems trial, Dhanusha",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.6
	)

	to_date <- function(x) {
		if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.character(as.Date(x)))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		x <- trimws(as.character(x))
		x[x %in% c("", "-", ".", "NA", "nd")] <- NA
		as.character(as.Date(x))
	}

	pick_num <- function(df, idx, nms) {
		n <- nms[nms %in% names(df)]
		if (length(n) == 0 || length(idx) == 0) return(rep(NA_real_, length(idx)))
		out <- rep(NA_real_, length(idx))
		ok <- !is.na(idx)
		out[ok] <- as.numeric(df[[n[1]]][idx[ok]])
		out
	}

	crop_of <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x[x %in% c("", "-", "na", "0")] <- NA
		x[x %in% c("rabi maize", "maize")] <- "maize"
		x[x == "wheat"] <- "wheat"
		x[x == "lentil"] <- "lentil"
		x[x == "mustard"] <- "mustard"
		x[x == "potato"] <- "potato"
		x
	}

	rotation_of <- function(tmnt) {
		x <- tolower(tmnt)
		out <- rep(NA_character_, length(x))
		out[grepl("mung", x) & grepl("wheat", x)] <- "rice;wheat;mung bean"
		out[grepl("mung", x) & grepl("lentil", x)] <- "rice;lentil;mung bean"
		out[grepl("spinach", x)] <- "rice;maize_spinach"
		out[grepl("bean", x) & !grepl("mung", x)] <- "rice;maize_bean"
		left <- is.na(out)
		out[left & grepl("mustard", x)] <- "rice;mustard"
		out[left & grepl("lentil", x)] <- "rice;lentil"
		out[left & grepl("potato", x)] <- "rice;potato"
		out[left & grepl("wheat", x)] <- "rice;wheat"
		out[left & grepl("maize", x)] <- "rice;maize"
		out
	}

	one <- function(f) {
		ph <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		gh <- as.data.frame(carobiner::read.excel.hdr(f, sheet="14 - Grain Harvest ", skip=4, hdr=2))
		ft <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		ph <- ph[!is.na(ph$Site.No) & !is.na(ph$Node), ]
		gh <- gh[!is.na(gh$Site.No) & !is.na(gh$Tmnt), ]
		ft <- ft[!is.na(ft$Site.No) & !is.na(ft$Tmnt), ]
		# Sinojoda phenology has no treatment name. Within a site, row order matches the harvest sheet.
		add_seq <- function(x) ave(seq_len(nrow(x)), x$Site.No, FUN=seq_along)
		ph$seq <- add_seq(ph)
		gh$seq <- add_seq(gh)
		ft$seq <- add_seq(ft)
		key <- function(x) paste(x$Site.No, x$seq)
		ig <- match(key(ph), key(gh))
		ifr <- match(key(ph), key(ft))

		tmnt <- trimws(as.character(gh$Tmnt[ig]))
		tmnt[tmnt == "Rice-potato"] <- "Rice-Potato"
		tmnt[tmnt == "Rice-CTMustard"] <- "Rice-CT Mustard"

		crop <- crop_of(ph$Crop)
		if ("Crops" %in% names(ph)) {
			alt <- crop_of(ph$Crops)
			crop[is.na(crop)] <- alt[is.na(crop)]
		}
		crop[is.na(crop) & grepl("mustard", tolower(tmnt))] <- "mustard"
		crop[is.na(crop) & grepl("lentil", tolower(tmnt))] <- "lentil"
		crop[is.na(crop) & grepl("potato", tolower(tmnt))] <- "potato"
		crop[is.na(crop) & grepl("wheat", tolower(tmnt))] <- "wheat"
		crop[is.na(crop) & grepl("maize", tolower(tmnt))] <- "maize"

		part <- ifelse(crop == "potato", "tubers", ifelse(crop %in% c("lentil", "mustard"), "seed", "grain"))
		prep <- rep(NA_character_, length(tmnt))
		prep[grepl("ZT", tmnt)] <- "none"
		prep[grepl("CT", tmnt)] <- "conventional"

		inter <- rep(NA_character_, length(tmnt))
		inter[grepl("Bean", tmnt) & !grepl("mung", tmnt, ignore.case=TRUE)] <- "bean"
		inter[grepl("Spinach", tmnt)] <- "spinach"

		seed_nm <- grep("^Date.of.seeding", names(ph), value=TRUE)
		harv_nm <- grep("harvest", names(ph), value=TRUE, ignore.case=TRUE)
		harv_nm <- harv_nm[!grepl("DAS", harv_nm)]
		planting <- if (length(seed_nm)) to_date(ph[[seed_nm[1]]]) else rep(NA_character_, nrow(ph))
		harvest <- if (length(harv_nm)) to_date(ph[[harv_nm[1]]]) else rep(NA_character_, nrow(ph))

		spacing <- as.numeric(ph$Row.spacing.cm)
		spacing[spacing == 0] <- NA

		tgw <- pick_num(gh, ig, "TGW.g")
		tgw[tgw == 0] <- NA

		prod_nm <- grep("Product.used", names(ft), value=TRUE)
		fert <- rep(NA_character_, nrow(ph))
		if (length(prod_nm) > 0) {
			for (i in seq_len(nrow(ph))) {
				j <- ifr[i]
				if (is.na(j)) next
				p <- trimws(as.character(unlist(ft[j, prod_nm])))
				p[p %in% c("", "-", "NA", "0", "na")] <- NA
				p <- p[!is.na(p)]
				p[grepl("^DAP", p)] <- "DAP"
				p[grepl("^Urea", p, ignore.case=TRUE)] <- "urea"
				p[grepl("^Muriate|^MOP|^Mop", p)] <- "KCl"
				if (length(p)) fert[i] <- paste(unique(p), collapse=";")
			}
		}

		N <- pick_num(ft, ifr, c("N.kg.ha", "Kg.ha_N.kg.ha"))
		P <- pick_num(ft, ifr, "P2O5.kg.ha") / 2.29
		K <- pick_num(ft, ifr, "K2O.kg.ha") / 1.2051
		fert[is.na(fert) & !is.na(N) & N == 0 & !is.na(P) & P == 0 & !is.na(K) & K == 0] <- "none"

		variety <- trimws(as.character(ph$Variety))
		variety[variety %in% c("", "-", "NA", "0")] <- NA

		data.frame(
			location = carobiner::fix_name(ph$Node, "title"),
			season = "rabi",
			trial_id = paste(ph$Node, ph$Year),
			plot_id = paste(ph$Site.No, tmnt, ph$seq),
			treatment = tmnt,
			land_prep_method = prep,
			crop = crop,
			crop_rotation = rotation_of(tmnt),
			intercrops = inter,
			intercropped = !is.na(inter),
			variety = variety,
			yield_part = part,
			planting_date = planting,
			harvest_date = harvest,
			row_spacing = spacing,
			plot_area = as.numeric(ph$Plot.size.m2),
			seed_rate = pick_num(ph, seq_len(nrow(ph)), c("Seed.rate.kg.ha", "Seedrate.kg.ha")),
			yield = pick_num(gh, ig, "Calculation_Grain.yield.t.ha") * 1000,
			seed_weight = tgw,
			harvest_index = {
				hi <- pick_num(gh, ig, "HI")
				ifelse(!is.na(hi) & hi <= 1, hi * 100, hi)
			},
			biomass_kg_ha_ = pick_num(gh, ig, "Biomass.t.ha") * 1000,
			straw_kg_ha_ = pick_num(gh, ig, "Straw.yield.t.ha") * 1000,
			N_fertilizer = N,
			P_fertilizer = P,
			K_fertilizer = K,
			fertilizer_type = fert,
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	d <- lapply(ff, one)
	d <- do.call(rbind, d)
	carobiner::write_files(path, meta, d)
}
