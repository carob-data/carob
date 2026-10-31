# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm maize and potato intercropping trial, Madhubani, Bihar, 2015-16. The site-sheet country cell says Bihar, which is the state. The district cell says Madhubani.
# Grain and tuber yield is t/ha, stored as kg/ha.
# P2O5.kg.ha is oxide and is divided by 2.29. The summary column is K.kg.ha, so potassium is elemental. Zn.kg.ha is elemental zinc. Zinc sulfate product weight is not stored.
# Boric acid is converted to elemental B (10.81/61.83).
# Row spacing below 1 cm is empty. A harvest date after the 2018-06-05 publication date is not stored.
# Intercrop yield is the Intercrop.t.ha column. The biomass subsample is labeled fresh, so yield_isfresh is TRUE for the intercrop and not set for maize and potato.

## ISSUES
# missing variables: yield_moisture, latitude, longitude, irrigated
# unknown variables: biomass_kg_ha_
# out of bounds: plot_area (455, 455)

## NOT PROCESSED
# Irrigation, ponding, weeds, pesticides, soil analysis, prices, and fertilizer product names.
# Farmer names.
# Daily rainfall.
# ZnSO4.kg.ha.


carob_script <- function(path) {

"
5.3-Rabi (winter) crops-all nodes- Inter-cropping trial -Madhubani-Bihar

Farmer participatory on-farm intercropping of leafy vegetables in maize, compared with existing practices, Madhubani, Bihar.
"

	uri <- "hdl:11529/10548003"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Inter-cropping trial, Madhubani, Bihar",
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
	blank <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c("-", "NA")] <- NA_character_
		x
	}

	f <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	ph <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
	gh <- as.data.frame(carobiner::read.excel.hdr(f, sheet="13 - Grain Harvest ", skip=4, hdr=2))
	ft <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
	ic <- as.data.frame(carobiner::read.excel.hdr(f, sheet="15-Intercrop", skip=4, hdr=2))
	ph <- ph[!is.na(ph$Tmnt) & !is.na(ph$Crop), ]
	gh <- gh[!is.na(gh$Tmnt), ]
	ft <- ft[!is.na(ft$Tmnt), ]
	add_seq <- function(x) ave(seq_len(nrow(x)), x$Site.No, x$Tmnt, FUN=seq_along)
	ph$seq <- add_seq(ph)
	gh$seq <- add_seq(gh)
	ft$seq <- add_seq(ft)
	key <- function(x) paste(x$Site.No, x$Tmnt, x$seq)
	ig <- match(key(ph), key(gh))
	ifr <- match(key(ph), key(ft))
	Kox <- take(ft, ifr, "K2O.kg.ha")
	kplain <- take(ft, ifr, "K.kg.ha")
	K <- ifelse(is.na(Kox), kplain, Kox / 1.2051)
	B <- take(ft, ifr, c("Boric.acid.kg.ha", "Boric.Acid.kg.ha"))
	sp <- num(ph$Row.spacing.cm)
	sp[!is.na(sp) & sp < 1] <- NA_real_
	hd <- as_date(ph$Datw.of.harvest.dd.mm.yy)
	hd[!is.na(hd) & hd > "2018-06-05"] <- NA_character_
	crop <- tolower(trimws(ph$Crop))
	part <- ifelse(crop == "potato", "tubers", "grain")
	hi <- take(gh, ig, "HI")
	hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
	d <- data.frame(
		country = "India",
		adm1 = "Bihar",
		adm2 = "Madhubani",
		location = ph$Node,
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
		yield = take(gh, ig, c("Grain.yield.t.ha", "Calculation_Grain.Yield.t.ha")) * 1000,
		biomass_kg_ha_ = take(gh, ig, "Biomass.t.ha") * 1000,
		harvest_index = hi,
		seed_weight = take(gh, ig, "TGW.g"),
		N_fertilizer = take(ft, ifr, "N.kg.ha"),
		P_fertilizer = take(ft, ifr, "P2O5.kg.ha") / 2.29,
		K_fertilizer = K,
		Zn_fertilizer = take(ft, ifr, "Zn.kg.ha"),
		gypsum = take(ft, ifr, c("Gypsum.kg.ha", "Gypsum.Kg.ha")),
		B_fertilizer = B * 10.81 / 61.83,
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	iname <- ic$Crop.information.and.phenology.of.inter.crop_Name.of.Inter.Crop
	ic <- ic[!is.na(ic$Tmnt) & !is.na(iname) & nzchar(trimws(as.character(iname))), ]
	if (nrow(ic) > 0) {
		icn <- tolower(trimws(as.character(ic$Crop.information.and.phenology.of.inter.crop_Name.of.Inter.Crop)))
		ihd <- as_date(ic$Date.of.harvest.dd.mm.yy)
		ihd[!is.na(ihd) & ihd > "2018-06-05"] <- NA_character_
		loc <- ph$Node[match(paste(ic$Site.No, ic$Tmnt), paste(ph$Site.No, ph$Tmnt))]
		d2 <- data.frame(
			country = "India",
			adm1 = "Bihar",
			adm2 = "Madhubani",
			location = loc,
			trial_id = paste(ic$Year, "intercrop", ic$Site.No, ic$Tmnt),
			plot_id = paste(ic$Site.No, ic$Tmnt, "intercrop"),
			treatment = ic$Tmnt,
			crop = icn,
			variety = ic$Variety,
			yield_part = "aboveground biomass",
			planting_date = as_date(ic$Date.of.seeding.dd.mm.yy),
			harvest_date = ihd,
			row_spacing = num(ic$Row.spacing.cm),
			yield = num(ic$Intercrop.t.ha) * 1000,
			yield_isfresh = TRUE,
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
		d2$row_spacing[!is.na(d2$row_spacing) & d2$row_spacing < 1] <- NA_real_
		d <- carobiner::bindr(d, d2)
	}

	carobiner::write_files(path, meta, d)
}
