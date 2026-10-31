# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm alternative cropping-system trials, Rajshahi, 2014-15.
# The site sheet header says Country: Bangladesh and District: Rajshahi. Latitude and longitude cells are empty.
# Grain yield is t/ha of air-dry grain, stored as kg/ha. yield_isfresh is FALSE.
# P2O5.kg.ha is oxide and is divided by 2.29. The potassium summary column is labeled K.kg.ha, so it is stored as elemental K.
# S, Zn, and B summary columns are labeled as the elements.

## ISSUES
# write_files: missing variables: yield_moisture, latitude, longitude. out of bounds: plot_area (200, 400). duplicates: duplicate records detected.
# The site sheet has latitude and longitude columns and they are empty. Plot size reaches 400 m2. Some site and treatment rows are repeated in the workbooks.
## NOT PROCESSED
# Irrigation, ponding, weeds, pesticides, biomass, soil analysis, prices, and fertilizer product names.
# Farmer names.
# Daily rainfall sheets.


carob_script <- function(path) {

"
4.1-Rabi (winter) crops-all nodes-Alternative cropping systems trial-Rajshahi-Bangladesh

Farmer participatory on-farm trials with CA technologies comparing with farmers' practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory cropping systems trials were conducted comparing to exiting systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10547992"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Alternative cropping systems trial, Rajshahi, Bangladesh",
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
		iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
		if (any(iso)) out[iso] <- substr(x[iso], 1, 10)
		out
	}
	num <- function(x) {
		if (is.null(x)) return(NA_real_)
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		as.numeric(x)
	}
	header_value <- function(cells, label) {
		i <- which(cells == label)
		if (!length(i)) return(NA_character_)
		rest <- cells[(i[1] + 1):length(cells)]
		rest <- rest[!is.na(rest) & nzchar(rest)]
		if (!length(rest)) return(NA_character_)
		rest[1]
	}

	one <- function(f) {
		ph <- carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2)
		gh <- carobiner::read.excel.hdr(f, sheet="14 - Grain Harvest ", skip=4, hdr=2)
		ft <- carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2)
		si <- carobiner::read.excel(f, sheet="2 - Site information", col_names=FALSE)
		cells <- trimws(as.character(unlist(si[1, ])))
		cells[is.na(cells)] <- ""
		country <- header_value(cells, "Country:")
		adm2 <- header_value(cells, "District:")
		ph <- ph[!is.na(ph$Crop) & !is.na(ph$Tmnt), ]
		gh <- gh[!is.na(gh$Tmnt), ]
		ft <- ft[!is.na(ft$Tmnt), ]
		key <- function(x) paste(x$Site.No, x$Tmnt)
		ig <- match(key(ph), key(gh))
		ifr <- match(key(ph), key(ft))
		crop <- tolower(trimws(ph$Crop))
		crop[crop == "chick pea"] <- "chickpea"
		part <- rep("seed", length(crop))
		part[crop == "maize"] <- "grain"
		part[crop == "wheat"] <- "grain"
		part[crop == "potato"] <- "tubers"
		data.frame(
			country = country,
			adm2 = adm2,
			location = ph$Node,
			trial_code = as.character(ph$Trial.Code),
			season = ifelse(grepl("rabi", ph$Season, ignore.case=TRUE), "rabi", tolower(trimws(ph$Season))),
			year = as.character(ph$Year),
			site_no = as.character(ph$Site.No),
			treatment = ph$Tmnt,
			crop = crop,
			variety = ph$Variety,
			plot_area = num(ph$Plot.size.m2),
			row_spacing = num(ph$Row.spacing.cm),
			seed_rate = num(ph$Seed.rate.kg.ha),
			planting_date = as_date(ph$Date.of.seeding),
			harvest_date = as_date(ph$Datw.of.harvest.mm.dd.yy),
			anthesis_days = num(ph$X50pct.anthesis.DAS),
			maturity_days = num(ph$X80pct.physiological.maturity.DAS),
			yield = num(gh$Calculation_Grain.yield.t.ha[ig]) * 1000,
			yield_part = part,
			N_fertilizer = num(ft$N.kg.ha[ifr]),
			P_fertilizer = num(ft$P2O5.kg.ha[ifr]) / 2.29,
			K_fertilizer = num(ft$K.kg.ha[ifr]),
			S_fertilizer = num(ft$S.kg.ha[ifr]),
			Zn_fertilizer = num(ft$Zn.kg.ha[ifr]),
			B_fertilizer = num(ft$B.kg.ha[ifr])
		)
	}

	d <- do.call(rbind, lapply(ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)], one))
	d$trial_id <- paste(d$location, d$year, d$trial_code, sep="_")
	d$plot_id <- d$site_no
	d$trial_code <- NULL
	d$year <- NULL
	d$site_no <- NULL
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- FALSE
	d$yield_isfresh <- FALSE

	carobiner::write_files(path, meta, d)
}
