# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm maize and leafy-vegetable intercropping at Cooch Behar nodes, rabi 2015-16 and 2016-17.
# The dataset title names Choochbehar, West Bengal. Country is India from that place. The 2015 workbook header still says Bangladesh and Rangpur.
# Grain yield is Calculation_Grain.yield.t.ha of air-dry grain, stored as kg/ha. yield_isfresh is FALSE.
# P2O5.kg.ha is oxide and is divided by 2.29. K.kg.ha is elemental K.
# Sole maize has intercrop yield 0. Intercrop rows use the leafy crop and Intercrop.t.ha.
# Napashak is stored as napa shak. Bathua is not in the crop list.

## ISSUES
# Plot size is 666.5 or 1333 m2. Bathua is kept as recorded. The 2016-17 book repeats some site and treatment rows, including a year labeled 2016-18.
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated. out of bounds: plot_area (666.5, 1333). invalid terms: crop: bathua.

## NOT PROCESSED
# Irrigation, ponding, weeds, pesticides, biomass subsamples, soil analysis, and prices.
# Fertilizer product names. Boric acid and zinc columns are empty.
# Farmer names.


carob_script <- function(path) {

"
5.5-Rabi (winter) crops-all nodes- Inter-cropping trial -Choochbehar-West Bengal

Farmer participatory on-farm trials with CA technologies comparing with farmers' practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to exiting systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10548005"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Inter-cropping trial, Choochbehar, West Bengal",
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
		"Maize+Bathua"="bathua",
		"Maize+Coriander"="coriander"
	)
	leaf_part <- c(
		"red amaranth"="leaves",
		"spinach"="leaves",
		"napa shak"="leaves",
		"bathua"="leaves",
		"coriander"="leaves"
	)

	one <- function(f) {
		r1 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		r2 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="13 - Grain Harvest ", skip=4, hdr=2))
		r3 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		r4 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="15-Intercrop", skip=4, hdr=2))
		r1 <- r1[!is.na(r1$Tmnt) & !is.na(r1$Node), ]
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

		hv_name <- intersect(c("Datw.of.harvest.dd.mm.yy", "Date.of.harvest.dd.mm.yy"), names(r1))[1]
		hd <- as_date(r1[[hv_name]])
		hd[!is.na(hd) & hd > "2018-06-05"] <- NA_character_
		sp <- num(r1$Row.spacing.cm)
		sp[!is.na(sp) & sp == 0] <- NA_real_

		hi <- num(r2$HI[ig])
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		veg <- unname(second[as.character(r1$Tmnt)])
		base <- data.frame(
			country = "India",
			adm1 = "West Bengal",
			location = carobiner::fix_name(trimws(as.character(r1$Node)), "title"),
			trial_id = paste(carobiner::fix_name(trimws(as.character(r1$Node)), "title"), r1$Year, r1$seq),
			plot_id = paste(r1$Site.No, r1$Tmnt, r1$seq),
			season = "rabi",
			treatment = as.character(r1$Tmnt),
			variety = blank(r1$Variety),
			plot_area = num(r1$Plot.size.m2),
			row_spacing = sp,
			planting_date = as_date(r1$Date.of.seeding.dd.mm.yy),
			harvest_date = hd,
			anthesis_days = num(r1$X50pct.anthesis.DAS),
			maturity_days = num(r1$X80pct.physiological.maturity.DAS),
			seed_rate = num(r1$Seedrate.kg.ha),
			yield = num(r2$Calculation_Grain.yield.t.ha[ig]) * 1000,
			harvest_index = hi,
			N_fertilizer = num(r3$N.kg.ha[ifr]),
			P_fertilizer = num(r3$P2O5.kg.ha[ifr]) / 2.29,
			K_fertilizer = num(r3$K.kg.ha[ifr]),
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
		leaf$yield <- num(r4$Intercrop.t.ha[ii])[veg_rows] * 1000
		leaf$seed_rate <- num(r4$Seed.rate.of.intercrop.kg.ha[ii])[veg_rows]
		leaf$variety <- NA_character_
		leaf$anthesis_days <- NA_real_
		leaf$harvest_index <- NA_real_
		rbind(base, leaf)
	}

	ff <- ff[basename(ff) %in% c("Inter-cropping 2015-15-all nodes-Choochbehar.xlsx", "Inter-cropping 2016-17-all nodes-Choochbehar.xlsx")]
	d <- do.call(rbind, lapply(ff, one))
	carobiner::write_files(path, meta, d)
}
