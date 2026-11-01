# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm long-term aman rice trials in Madhubani, Bihar, 2015-2017.
# The site sheet puts Bihar in the Country cell and Madhubani in the District cell. The title names Madhubani-Bihar, so the country is India and Bihar is adm1.
# Grain yield is t/ha, stored as kg/ha. The sheet is the SRFSI air-dry grain calculation. yield_isfresh is FALSE.
# P2O5.kg.ha is oxide and is divided by 2.29. K2O.kg.ha is oxide and is divided by 1.2051.
# Zinc sulfate is not stored, because the hydrate is not stated.
# The same site and treatment can be entered twice. Those rows are kept as separate plots.
# Row spacing 0 is empty. A harvest date after the 2018-06-05 publication date is not stored.
# Negative anthesis and maturity values are Excel dates stored in the days columns. They are not kept.
# Korahiya 2015 and the 2017 Khiari, Korahiya, and Shukhet workbooks name the harvest sheet "13 - Grain Harvest ". The others use "14 - Grain Harvest ".

## ISSUES
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated
# write_files: unknown variables: biomass_kg_ha_
# write_files: out of bounds: plot_area (182, 3640)
# write_files: NA detected: yield

## NEW VARIABLES
# biomass_kg_ha_: above-ground biomass from the grain sheet, t/ha converted to kg/ha. Fresh or dry is not stated on the column.

## NOT PROCESSED
# Irrigation, ponding, weeds, pesticides, biomass subsamples, soil analysis, prices, and fertilizer product names.
# Farmer names.
# Daily rainfall sheets.
# ZnSO4.kg.ha, because the hydrate is not stated.


carob_script <- function(path) {

"
1.3-Kharif (summer) rice-all nodes-Long-term trial (LT)-Madhubani-Bihar

Farmers' participatory researcher-managed long-term trials of aman rice in Madhubani, Bihar, India.
"

	uri <- "hdl:11529/10547956"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Long-term trial, Madhubani, Bihar",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.6
	)

	as_date <- function(x) {
		if (is.null(x)) return(NA_character_)
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
		if (is.numeric(x) && !inherits(x, "POSIXt")) return(as.numeric(x))
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		as.numeric(x)
	}
	take <- function(df, idx, nms) {
		n <- nms[nms %in% names(df)]
		if (length(n) == 0) return(rep(NA_real_, length(idx)))
		num(df[[n[1]]])[idx]
	}
	header_value <- function(cells, label) {
		i <- which(cells == label)
		if (!length(i)) return(NA_character_)
		rest <- cells[(i[1] + 1):length(cells)]
		rest <- rest[!is.na(rest) & nzchar(rest)]
		if (!length(rest)) return(NA_character_)
		rest[1]
	}
	grain_sheet <- function(bn) {
		if (grepl("2015-LT-Korahiya", bn)) return("13 - Grain Harvest ")
		if (grepl("2017-LT-Khiari|2017-LT-Korahiya|2017-LT-Shukhet", bn)) return("13 - Grain Harvest ")
		"14 - Grain Harvest "
	}

	one <- function(f) {
		bn <- basename(f)
		ph <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		gh <- as.data.frame(carobiner::read.excel.hdr(f, sheet=grain_sheet(bn), skip=4, hdr=2))
		ft <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		si <- carobiner::read.excel(f, sheet="2 - Site information", col_names=FALSE)
		cells <- trimws(as.character(unlist(si[1, ])))
		cells[is.na(cells)] <- ""
		raw_c <- header_value(cells, "Country:")
		adm2 <- header_value(cells, "District:")
		country <- raw_c
		adm1 <- NA_character_
		if (raw_c %in% c("Bihar", "West Bengal")) {
			country <- "India"
			adm1 <- raw_c
		}
		if (!is.na(raw_c) && toupper(raw_c) == "INDIA") {
			country <- "India"
			adm1 <- "Bihar"
		}
		ph <- ph[!is.na(ph$Tmnt) & !is.na(ph$Node), ]
		gh <- gh[!is.na(gh$Tmnt), ]
		ft <- ft[!is.na(ft$Tmnt.No), ]
		if (!"Site.No" %in% names(ph) && "Unique.Farmer.s.code" %in% names(ph)) ph$Site.No <- ph$Unique.Farmer.s.code
		if (!"Site.No" %in% names(gh) && "Unique.Farmer.s.code" %in% names(gh)) gh$Site.No <- gh$Unique.Farmer.s.code
		if (!"Site.No" %in% names(ft) && "Unique.Farmer.s.code" %in% names(ft)) ft$Site.No <- ft$Unique.Farmer.s.code
		add_seq <- function(x, tm) ave(seq_len(nrow(x)), x$Site.No, x[[tm]], FUN=seq_along)
		ph$seq <- add_seq(ph, "Tmnt")
		gh$seq <- add_seq(gh, "Tmnt")
		ft$seq <- add_seq(ft, "Tmnt.No")
		keyp <- paste(ph$Site.No, ph$Tmnt.No, ph$seq)
		ig <- match(keyp, paste(gh$Site.No, gh$Tmnt, gh$seq))
		ifr <- match(keyp, paste(ft$Site.No, ft$Tmnt.No, ft$seq))
		y <- take(gh, ig, c("Grain.yield.t.ha", "Calculation_Grain.yield.t.ha", "Calculation_Grain.Yield.t.ha", "Calulation_Grain.Yield.t.ha"))
		sp <- num(ph$Row.spacing.cm)
		sp[!is.na(sp) & sp < 1] <- NA_real_
		hd <- as_date(ph$Datw.of.harvest.dd.mm.yy)
		hd[!is.na(hd) & hd > "2018-06-05"] <- NA_character_
		crop <- tolower(trimws(as.character(ph$Crop)))
		crop[grepl("rice", crop)] <- "rice"
		crop[grepl("wheat", crop)] <- "wheat"
		crop[grepl("lentil", crop)] <- "lentil"
		crop[grepl("maize", crop)] <- "maize"
		crop[grepl("chick", crop)] <- "chickpea"
		crop[grepl("potato", crop)] <- "potato"
		part <- rep("grain", length(crop))
		part[crop %in% c("lentil", "chickpea", "wheat")] <- "seed"
		part[crop == "potato"] <- "tubers"
		hi <- take(gh, ig, "HI")
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		ad <- num(ph$X50pct.anthesis.DAS)
		md <- num(ph$X80pct.physiological.maturity.DAS)
		ad[!is.na(ad) & ad < 0] <- NA_real_
		md[!is.na(md) & md < 0] <- NA_real_
		data.frame(
			country = country,
			adm1 = adm1,
			adm2 = carobiner::fix_name(adm2, "title"),
			location = carobiner::fix_name(ph$Node, "title"),
			trial_id = paste(ph$Year, ph$Node, ph$Trial.Code),
			plot_id = paste(ph$Site.No, ph$Tmnt, ph$seq),
			treatment = ph$Tmnt,
			crop = crop,
			variety = ph$Variety,
			yield_part = part,
			planting_date = as_date(ph$Date.of.seeding.dd.mm.yy),
			harvest_date = hd,
			row_spacing = sp,
			seed_rate = num(ph$Seedrate.kg.ha),
			plot_area = num(ph$Plot.size.m2),
			yield = y * 1000,
			yield_isfresh = FALSE,
			biomass_kg_ha_ = take(gh, ig, "Biomass.t.ha") * 1000,
			harvest_index = hi,
			anthesis_days = ad,
			maturity_days = md,
			N_fertilizer = take(ft, ifr, "N.kg.ha"),
			P_fertilizer = take(ft, ifr, "P2O5.kg.ha") / 2.29,
			K_fertilizer = take(ft, ifr, "K2O.kg.ha") / 1.2051,
			gypsum = take(ft, ifr, c("Gypsum.kg.ha", "Gypsum.Kg.ha")),
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	d <- do.call(rbind, lapply(ff, one))
	carobiner::write_files(path, meta, d)
}
