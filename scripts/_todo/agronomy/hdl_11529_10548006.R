# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm maize and leafy-vegetable intercropping at Malda nodes, rabi 2016-17.
# The title names Malda, West Bengal. The site sheet puts West Bengal in the Country cell and Malda in the District cell. Country is India from that place.
# Grain yield is Calculation_Grain.yield.t.ha of air-dry grain, stored as kg/ha. yield_isfresh is FALSE.
# P2O5.kg.ha is oxide and is divided by 2.29. K.kg.ha is elemental K.
# Intercrop rows use the leafy crop and Intercrop.t.ha. Fenugreek is harvested as leaves in this trial.
# Napashak is stored as napa shak.

## ISSUES
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated out of bounds: plot_area (666.5, 666.5) high crop yield: fenugreek: 17320 contributor:

## NOT PROCESSED
# Irrigation, ponding, weeds, pesticides, biomass subsamples, soil analysis, and prices.
# Fertilizer product names.
# Farmer names.
# Daily rainfall sheet.


carob_script <- function(path) {

"
5.6-Rabi (winter) crops-all nodes- Inter-cropping trial -Malda-West Bengal

Farmer participatory on-farm trials with CA technologies comparing with farmers' practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory intercropping leafy vegetables in maize trials were conducted comparing to exiting practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10548006"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Inter-cropping trial, Malda, West Bengal",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.5
	)

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & nchar(x) >= 10 & substr(x, 5, 5) == "-"
		out[iso] <- substr(x[iso], 1, 10)
		slash <- !is.na(x) & grepl("/", x)
		if (any(slash)) {
			p <- strsplit(x[slash], "/")
			n <- lengths(p)
			keep <- n == 3
			idx <- which(slash)[keep]
			pp <- p[keep]
			dd <- as.integer(vapply(pp, function(z) z[1], ""))
			mm <- as.integer(vapply(pp, function(z) z[2], ""))
			yy <- as.integer(vapply(pp, function(z) z[3], ""))
			yy[!is.na(yy) & yy < 100] <- yy[!is.na(yy) & yy < 100] + 2000
			good <- !is.na(dd) & !is.na(mm) & !is.na(yy) & mm >= 1 & mm <= 12 & dd >= 1 & dd <= 31
			out[idx[good]] <- sprintf("%04d-%02d-%02d", yy[good], mm[good], dd[good])
		}
		out
	}
	num <- function(x) {
		if (is.null(x)) return(NA_real_)
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		as.numeric(x)
	}
	blank <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}

	second <- c(
		"Maize+Red Amaranth"="red amaranth",
		"Maize+Spinach"="spinach",
		"Maize+Napashak"="napa shak",
		"Maize+Coriander"="coriander",
		"Maize+Fenugreek"="fenugreek"
	)
	leaf_part <- c(
		"red amaranth"="leaves",
		"spinach"="leaves",
		"napa shak"="leaves",
		"coriander"="leaves",
		"fenugreek"="leaves"
	)

	one <- function(f) {
		ph <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		gh <- as.data.frame(carobiner::read.excel.hdr(f, sheet="13 - Grain Harvest ", skip=4, hdr=2))
		ft <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		ic <- as.data.frame(carobiner::read.excel.hdr(f, sheet="15-Intercrop", skip=4, hdr=2))
		ph <- ph[!is.na(ph$Tmnt) & !is.na(ph$Node), ]
		gh <- gh[!is.na(gh$Tmnt), ]
		ft <- ft[!is.na(ft$Tmnt), ]
		ic <- ic[!is.na(ic$Tmnt), ]
		add_seq <- function(x) ave(seq_len(nrow(x)), x$Site.No, x$Tmnt, FUN=seq_along)
		ph$seq <- add_seq(ph)
		gh$seq <- add_seq(gh)
		ft$seq <- add_seq(ft)
		ic$seq <- add_seq(ic)
		key <- function(x) paste(x$Site.No, x$Tmnt, x$seq)
		ig <- match(key(ph), key(gh))
		ifr <- match(key(ph), key(ft))
		ii <- match(key(ph), key(ic))

		hv_name <- intersect(c("Datw.of.harvest.dd.mm.yy", "Date.of.harvest.dd.mm.yy"), names(ph))[1]
		sp <- num(ph$Row.spacing.cm)
		sp[!is.na(sp) & sp == 0] <- NA_real_
		hi <- num(gh$HI[ig])
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		veg <- unname(second[as.character(ph$Tmnt)])
		loc <- carobiner::fix_name(trimws(as.character(ph$Node)), "title")
		base <- data.frame(
			country = "India",
			adm1 = "West Bengal",
			adm2 = "Malda",
			location = loc,
			trial_id = paste(loc, ph$Year, ph$seq),
			plot_id = paste(ph$Site.No, ph$Tmnt, ph$seq),
			season = "rabi",
			treatment = as.character(ph$Tmnt),
			variety = blank(ph$Variety),
			plot_area = num(ph$Plot.size.m2),
			row_spacing = sp,
			planting_date = as_date(ph$Date.of.seeding.dd.mm.yy),
			harvest_date = as_date(ph[[hv_name]]),
			anthesis_days = num(ph$X50pct.anthesis.DAS),
			maturity_days = num(ph$X80pct.physiological.maturity.DAS),
			seed_rate = num(ph$Seedrate.kg.ha),
			yield = num(gh$Calculation_Grain.yield.t.ha[ig]) * 1000,
			harvest_index = hi,
			N_fertilizer = num(ft$N.kg.ha[ifr]),
			P_fertilizer = num(ft$P2O5.kg.ha[ifr]) / 2.29,
			K_fertilizer = num(ft$K.kg.ha[ifr]),
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE,
			yield_isfresh = FALSE
		)
		base$crop <- "maize"
		base$yield_part <- "grain"
		veg_rows <- !is.na(veg)
		leaf <- base[veg_rows, ]
		leaf$crop <- veg[veg_rows]
		leaf$yield_part <- unname(leaf_part[leaf$crop])
		leaf$yield <- num(ic$Intercrop.t.ha[ii])[veg_rows] * 1000
		leaf$seed_rate <- num(ic$Seed.rate.of.intercrop.kg.ha[ii])[veg_rows]
		leaf$variety <- NA_character_
		leaf$anthesis_days <- NA_real_
		leaf$harvest_index <- NA_real_
		rbind(base, leaf)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	d <- do.call(rbind, lapply(ff, one))
	carobiner::write_files(path, meta, d)
}
