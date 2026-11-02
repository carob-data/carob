# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm long-term aman rice trials in Sunsari, Nepal, 2014-2017. The site sheet header says Country: Nepal and District: Sunsari.
# Grain yield is the air-dry calculation in t/ha, stored as kg/ha. yield_isfresh is FALSE.
# P2O5.kg.ha is oxide and is divided by 2.29. K2O.kg.ha is oxide and is divided by 1.2051.
# Boric acid is converted to elemental B (10.81/61.83). Zinc sulfate is not stored, because the hydrate is not stated.
# The same site and treatment can be entered twice. Those rows are kept as separate plots.
# Row spacing 0 is empty. Values such as 20x15 are a plot size, not a spacing, and are left empty. A harvest date after the 2018-06-05 publication date is not stored.
# Simariya workbooks have no node column. The node is taken from the file name.
# Two Bhokraha workbooks name the harvest sheet "13.Grain Harvest " and spell the yield column Calulation. Two Saalbani workbooks name it "13 -  Grain harvest".

## ISSUES
# missing variables: yield_moisture, latitude, longitude, irrigated
# unknown variables: biomass_kg_ha_
# out of bounds: plot_area (400, 800)
# NA detected: yield

## NOT PROCESSED
# Irrigation, ponding, weeds, pesticides, biomass subsamples, soil analysis, prices, and fertilizer product names.
# Farmer names.
# Daily rainfall sheets.
# ZnSO4.kg.ha, because the hydrate is not stated.


carob_script <- function(path) {

"
1.6-Kharif (summer) rice-all nodes-Long term trial (LT)-Sunsari-Nepal

Farmers' participatory researcher-managed long-term trials of aman rice in Sunsari, Nepal.
"

	uri <- "hdl:11529/10547960"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Long-term trial, Sunsari, Nepal",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.7
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
	grain_sheet <- function(bn) {
		if (grepl("Bhokraha", bn) && grepl("2016|2017", bn)) return("13.Grain Harvest ")
		if (grepl("Saalbani", bn) && grepl("2016|2017", bn)) return("13 -  Grain harvest")
		"14 - Grain Harvest "
	}

	one <- function(f) {
		ph <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		gh <- as.data.frame(carobiner::read.excel.hdr(f, sheet=grain_sheet(basename(f)), skip=4, hdr=2))
		ft <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		if (!"Node" %in% names(ph) || all(is.na(ph$Node))) {
			bn <- basename(f)
			node <- NA_character_
			if (grepl("Bhaluwa", bn)) node <- "Bhaluwa"
			if (grepl("Bhokraha", bn)) node <- "Bhokraha"
			if (grepl("Kaptanganj", bn)) node <- "Kaptanganj"
			if (grepl("Saalbani", bn)) node <- "Saalbani"
			if (grepl("Simariya", bn)) node <- "Simariya"
			ph$Node <- node
		}
		ph <- ph[!is.na(ph$Tmnt) & !is.na(ph$Crop) & !is.na(ph$Node), ]
		ph <- ph[!trimws(as.character(ph$Crop)) %in% c("0", "NA"), ]
		if (nrow(ph) == 0) return(NULL)
		gh <- gh[!is.na(gh$Tmnt), ]
		ft <- ft[!is.na(ft$Tmnt), ]
		add_seq <- function(x) ave(seq_len(nrow(x)), x$Site.No, x$Tmnt, FUN=seq_along)
		ph$seq <- add_seq(ph)
		gh$seq <- add_seq(gh)
		ft$seq <- add_seq(ft)
		key <- function(x) paste(x$Site.No, x$Tmnt, x$seq)
		ig <- match(key(ph), key(gh))
		ifr <- match(key(ph), key(ft))
		y <- take(gh, ig, c("Calculation_Grain.Yield.t.ha", "Calulation_Grain.Yield.t.ha", "Grain.yield.t.ha"))
		Kox <- take(ft, ifr, "K2O.kg.ha")
		kplain <- take(ft, ifr, "K.kg.ha")
		K <- ifelse(is.na(Kox), kplain, Kox / 1.2051)
		B <- take(ft, ifr, c("Boric.acid.kg.ha", "Boric.Acid.kg.ha"))
		spx <- trimws(as.character(ph$Row.spacing.cm))
		spx[is.na(spx) | !nzchar(spx) | spx %in% c("-", "NA", "0") | grepl("x", spx, ignore.case=TRUE)] <- NA_character_
		sp <- as.numeric(spx)
		sp[!is.na(sp) & sp < 1] <- NA_real_
		hcol <- intersect(c("Datw.of.harvest.dd.mm.yy", "Date.of.harvest.dd.mm.yy"), names(ph))
		hd <- as_date(ph[[hcol[1]]])
		hd[!is.na(hd) & hd > "2018-06-05"] <- NA_character_
		crop <- tolower(trimws(ph$Crop))
		crop[grepl("rice", crop)] <- "rice"
		hi <- take(gh, ig, "HI")
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		data.frame(
			country = "Nepal",
			adm2 = "Sunsari",
			location = ph$Node,
			trial_id = paste(ph$Year, ph$Node, ph$Trial.Code),
			plot_id = paste(ph$Site.No, ph$Tmnt, ph$seq),
			treatment = ph$Tmnt,
			crop = crop,
			variety = ph$Variety,
			yield_part = "grain",
			planting_date = as_date(ph$Date.of.seeding.dd.mm.yy),
			harvest_date = hd,
			row_spacing = sp,
			seed_rate = num(ph[[intersect(c("Seedrate.kg.ha", "Seed.rate.kg.ha"), names(ph))[1]]]),
			plot_area = { pa <- num(ph$Plot.size.m2); pa[!is.na(pa) & pa < 1] <- NA_real_; pa },
			yield = y * 1000,
			yield_isfresh = FALSE,
			biomass_kg_ha_ = take(gh, ig, "Biomass.t.ha") * 1000,
			harvest_index = hi,
			seed_weight = take(gh, ig, "TGW.g"),
			N_fertilizer = take(ft, ifr, "N.kg.ha"),
			P_fertilizer = take(ft, ifr, "P2O5.kg.ha") / 2.29,
			K_fertilizer = K,
			gypsum = take(ft, ifr, c("Gypsum.kg.ha", "Gypsum.Kg.ha")),
			B_fertilizer = B * 10.81 / 61.83,
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	parts <- lapply(ff, one)
	parts <- parts[!vapply(parts, is.null, logical(1))]
	d <- do.call(rbind, parts)
	carobiner::write_files(path, meta, d)
}
