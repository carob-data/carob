# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm maize intercropping at Premtoli, Rajshahi, rabi 2014-15 and 2015-16.
# The site-sheet header says Country: Bangladesh and District: Rajshahi. The node column says Premtoli.
# Grain yield is Grain.yield.t.ha of air-dry grain, stored as kg/ha. yield_isfresh is FALSE for maize.
# Intercrop yield is Ccalculaation_Intercrop.t.ha, calculated from fresh above-ground samples. yield_isfresh is TRUE for those rows.
# P2O5.kg.ha is oxide and is divided by 2.29. K2O.kg.ha (2015-16 column name K2.Okg.ha) is oxide and is divided by 1.2051.
# Gypsum.kg.ha is the product. Elemental S is 19 percent of gypsum (terminag fertilizer type gypsum).
# ZnSO4 summary is 0, so zinc fertilizer is 0.
# Sites 1 and 2 have latitude and longitude in the site sheet. Site 3 does not.
# The same site and treatment is one row per replicate. plot_id keeps the replicate.

## ISSUES
# Plot area is 400 m2. Anthesis is 127 to 164 days. Site 3 has no coordinates. Those values were not changed.
# write_files: missing variables: yield_moisture. out of bounds: plot_area (400, 400), anthesis_days (127, 164). NA detected: latitude, longitude. contributor:

## NOT PROCESSED
# Irrigation, ponding, weeds, pesticides, biomass subsamples, soil analysis, prices, and fertilizer product names.
# Straw yield.
# Farmer names.
# Daily rainfall sheets.
# Field history.


carob_script <- function(path) {

"
5.1-Rabi (winter) crops-all nodes-Inter-cropping trial-Rajshahi-Bangladesh

Farmer participatory on-farm trials with CA technologies comparing with farmers' practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory intercropping leafy vegetables in maize trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10548001"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Inter-cropping trial, Rajshahi, Bangladesh",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & nchar(x) >= 10 & substr(x, 5, 5) == "-"
		out[iso] <- substr(x[iso], 1, 10)
		out
	}
	num <- function(x) {
		if (is.null(x)) return(rep(NA_real_, 1))
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
	pick <- function(x, nms) {
		n <- nms[nms %in% names(x)]
		if (length(n) == 0) return(rep(NA_real_, nrow(x)))
		num(x[[n[1]]])
	}

	second <- c(
		"Maize+Red Amaranth"="red amaranth",
		"Maize+Spinach"="spinach",
		"Maize+Green pea"="pea"
	)
	part <- c("red amaranth"="leaves", "spinach"="leaves", "pea"="pod")

	one <- function(f) {
		r1 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		r2 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="13 - Grain Harvest ", skip=4, hdr=2))
		r3 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		r4 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="15-Intercrop", skip=4, hdr=2))
		r5 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="2 - Site information", skip=4, hdr=2))
		r1 <- r1[!is.na(r1$Tmnt) & !is.na(r1$Site.No), ]
		r2 <- r2[!is.na(r2$Tmnt), ]
		r3 <- r3[!is.na(r3$Tmnt), ]
		r4 <- r4[!is.na(r4$Tmnt), ]
		add_seq <- function(x) ave(seq_len(nrow(x)), x$Site.No, x$Tmnt, FUN=seq_along)
		r1$seq <- add_seq(r1)
		r2$seq <- add_seq(r2)
		r3$seq <- add_seq(r3)
		r4$seq <- add_seq(r4)
		key <- function(x) paste(x$Site.No, x$Tmnt, x$seq)
		ig <- match(key(r1), key(r2))
		ifr <- match(key(r1), key(r3))
		ii <- match(key(r1), key(r4))

		site <- data.frame(
			Site.No = r5$Site.No.Unique.farmer.ID,
			latitude = num(r5$Latitude.North.Degree.and.decimal.format.only),
			longitude = num(r5$Longitude.East.Degree.and.decimal.format.only)
		)
		site <- site[!is.na(site$Site.No), ]
		site <- site[!duplicated(site$Site.No), ]
		isite <- match(r1$Site.No, site$Site.No)

		K <- pick(r3, c("K2O.kg.ha", "K2.Okg.ha")) / 1.2051
		gypsum <- pick(r3, c("gypsum.kg.ha", "Gypsum.Kg.ha"))
		base <- data.frame(
			country = "Bangladesh",
			adm2 = "Rajshahi",
			location = "Premtoli",
			season = "rabi",
			latitude = site$latitude[isite],
			longitude = site$longitude[isite],
			site_no = as.character(r1$Site.No),
			year = as.character(r1$Year),
			seq = r1$seq,
			treatment = as.character(r1$Tmnt),
			variety = blank(r1$Variety),
			plot_area = num(r1$Plot.size.m2),
			row_spacing = num(r1$Row.spacing.cm),
			planting_date = as_date(r1$Date.of.seeding.dd.mm.yy),
			harvest_date = as_date(r1$Date.of.harvest.dd.mm.yy),
			anthesis_days = num(r1$X50pct.anthesis.DAS),
			maturity_days = num(r1$X80pct.physiological.maturity.DAS),
			seed_rate = num(r1$Seedrate.kg.ha),
			yield = pick(r2, "Grain.yield.t.ha")[ig] * 1000,
			N_fertilizer = pick(r3, "N.kg.ha")[ifr],
			P_fertilizer = pick(r3, "P2O5.kg.ha")[ifr] / 2.29,
			K_fertilizer = K[ifr],
			# Gypsum is CaSO4.2H2O. terminag gives 19 percent S.
			S_fertilizer = gypsum[ifr] * 0.19,
			Zn_fertilizer = pick(r3, "ZnSO4.kg.ha.1")[ifr],
			crop = "maize",
			yield_part = "grain",
			yield_isfresh = FALSE
		)
		veg <- unname(second[base$treatment])
		veg_rows <- !is.na(veg)
		leaf <- base[veg_rows, ]
		leaf$crop <- veg[veg_rows]
		leaf$yield_part <- unname(part[leaf$crop])
		leaf$yield <- pick(r4, "Ccalculaation_Intercrop.t.ha")[ii][veg_rows] * 1000
		leaf$seed_rate <- pick(r4, "Seed.rate.of.intercrop.kg.ha")[ii][veg_rows]
		leaf$variety <- blank(r4$Variety[ii])[veg_rows]
		leaf$anthesis_days <- NA_real_
		leaf$yield_isfresh <- TRUE
		rbind(base, leaf)
	}

	ff <- ff[basename(ff) %in% c("Inter-cropping 2014-15-Premtoli-Rajshahi.xlsx", "Inter-cropping 2015-16-Premtoli-Rajshahi.xlsx")]
	d <- do.call(rbind, lapply(ff, one))
	d$trial_id <- paste(d$location, d$year, sep="_")
	d$plot_id <- paste(d$site_no, d$treatment, d$seq, sep="_")
	d$site_no <- NULL
	d$year <- NULL
	d$seq <- NULL
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$geo_from_source[is.na(d$latitude) | is.na(d$longitude)] <- NA

	carobiner::write_files(path, meta, d)
}
