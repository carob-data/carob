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

	f <- ff[basename(ff) == "Inter-cropping 2015-16-all nodes-Madhubani.xlsx"]
	r1 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
	r2 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="13 - Grain Harvest ", skip=4, hdr=2))
	r3 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
	r4 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="15-Intercrop", skip=4, hdr=2))
	r1 <- r1[!is.na(r1$Tmnt) & !is.na(r1$Crop), ]
	r2 <- r2[!is.na(r2$Tmnt), ]
	r3 <- r3[!is.na(r3$Tmnt), ]
	add_seq <- function(x) ave(seq_len(nrow(x)), x$Site.No, x$Tmnt, FUN=seq_along)
	r1$seq <- add_seq(r1)
	r2$seq <- add_seq(r2)
	r3$seq <- add_seq(r3)
	key <- function(x) paste(x$Site.No, x$Tmnt, x$seq)
	ig <- match(key(r1), key(r2))
	ifr <- match(key(r1), key(r3))
	Kox <- take(r3, ifr, "K2O.kg.ha")
	kplain <- take(r3, ifr, "K.kg.ha")
	K <- ifelse(is.na(Kox), kplain, Kox / 1.2051)
	B <- take(r3, ifr, c("Boric.acid.kg.ha", "Boric.Acid.kg.ha"))
	sp <- num(r1$Row.spacing.cm)
	sp[!is.na(sp) & sp < 1] <- NA_real_
	hd <- as_date(r1$Datw.of.harvest.dd.mm.yy)
	hd[!is.na(hd) & hd > "2018-06-05"] <- NA_character_
	crop <- tolower(trimws(r1$Crop))
	part <- ifelse(crop == "potato", "tubers", "grain")
	hi <- take(r2, ig, "HI")
	hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
	d <- data.frame(
		country = "India",
		adm1 = "Bihar",
		adm2 = "Madhubani",
		location = r1$Node,
		trial_id = paste(r1$Year, r1$Node, r1$Trial.Code),
		plot_id = paste(r1$Site.No, r1$Tmnt, r1$seq),
		treatment = r1$Tmnt,
		crop = crop,
		variety = r1$Variety,
		yield_part = part,
		planting_date = as_date(r1$Date.of.seeding.dd.mm.yy),
		harvest_date = hd,
		row_spacing = sp,
		seed_rate = num(r1$Seedrate.kg.ha),
		plot_area = num(r1$Plot.size.m2),
		yield = take(r2, ig, c("Grain.yield.t.ha", "Calculation_Grain.Yield.t.ha")) * 1000,
		biomass_kg_ha_ = take(r2, ig, "Biomass.t.ha") * 1000,
		harvest_index = hi,
		seed_weight = take(r2, ig, "TGW.g"),
		N_fertilizer = take(r3, ifr, "N.kg.ha"),
		P_fertilizer = take(r3, ifr, "P2O5.kg.ha") / 2.29,
		K_fertilizer = K,
		Zn_fertilizer = take(r3, ifr, "Zn.kg.ha"),
		gypsum = take(r3, ifr, c("Gypsum.kg.ha", "Gypsum.Kg.ha")),
		B_fertilizer = B * 10.81 / 61.83,
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	iname <- r4$Crop.information.and.phenology.of.inter.crop_Name.of.Inter.Crop
	r4 <- r4[!is.na(r4$Tmnt) & !is.na(iname) & nzchar(trimws(as.character(iname))), ]
	if (nrow(r4) > 0) {
		icn <- tolower(trimws(as.character(r4$Crop.information.and.phenology.of.inter.crop_Name.of.Inter.Crop)))
		ihd <- as_date(r4$Date.of.harvest.dd.mm.yy)
		ihd[!is.na(ihd) & ihd > "2018-06-05"] <- NA_character_
		loc <- r1$Node[match(paste(r4$Site.No, r4$Tmnt), paste(r1$Site.No, r1$Tmnt))]
		d2 <- data.frame(
			country = "India",
			adm1 = "Bihar",
			adm2 = "Madhubani",
			location = loc,
			trial_id = paste(r4$Year, "intercrop", r4$Site.No, r4$Tmnt),
			plot_id = paste(r4$Site.No, r4$Tmnt, "intercrop"),
			treatment = r4$Tmnt,
			crop = icn,
			variety = r4$Variety,
			yield_part = "aboveground biomass",
			planting_date = as_date(r4$Date.of.seeding.dd.mm.yy),
			harvest_date = ihd,
			row_spacing = num(r4$Row.spacing.cm),
			yield = num(r4$Intercrop.t.ha) * 1000,
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
