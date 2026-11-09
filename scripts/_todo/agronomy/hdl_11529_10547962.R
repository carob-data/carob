# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm long-term aman rice trials in Malda, West Bengal, 2014-2017.
# The site sheet puts West Bengal in the Country cell. The title names Malda-West Bengal, so the country is India and West Bengal is adm1.
# Aman rice 2017-Manikchak- Malda.xlsx is not the standard field book. Yield in that file is q/ha (1 q = 100 kg). Its fertilizer grades increment by one across rows and are not used.
# Grain yield is t/ha, stored as kg/ha. The sheet is the SRFSI air-dry grain calculation. yield_isfresh is FALSE.
# P2O5.kg.ha is oxide and is divided by 2.29. K2O.kg.ha is oxide and is divided by 1.2051.
# Zinc sulfate is not stored, because the hydrate is not stated.
# The same site and treatment can be entered twice. Those rows are kept as separate plots.
# Row spacing 0 is empty. A harvest date after the 2018-06-05 publication date is not stored.
# Negative anthesis and maturity values are Excel dates stored in the days columns. They are not kept.
# The standard workbooks use the sheet "14 - Grain Harvest ".

## ISSUES
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated
# write_files: unknown variables: biomass_kg_ha_
# write_files: out of bounds: plot_area (1, 1333), anthesis_days (78, 278)
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
1.8-Kharif (summer) rice-all nodes-Long term (LT) trial-Malda-West Bengal

Farmers' participatory researcher-managed long-term trials of aman rice in Malda, West Bengal, India.
"

	uri <- "hdl:11529/10547962"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Long-term trial, Malda, West Bengal",
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
	grain_sheet <- function(bn) "14 - Grain Harvest "

	manikchak <- function(f) {
		r1 <- carobiner::read.excel(f, sheet="Crop Parameter", col_names=FALSE)
		hdr <- as.character(unlist(r1[1, ]))
		hdr[is.na(hdr)] <- ""
		body <- r1[-1, ]
		col <- function(lab) {
			i <- which(hdr == lab)[1]
			if (is.na(i)) return(rep(NA, nrow(body)))
			body[[i]]
		}
		crop <- tolower(trimws(as.character(col("Crop"))))
		crop[grepl("rice", crop)] <- "rice"
		y <- num(col("Yield (q/ha)")) * 100
		bio <- num(col("Biomass (q/ha)")) * 100
		sp <- num(col("Spacing (m)")) * 100
		hi <- num(col("Harvest Index"))
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		data.frame(
			country = "India",
			adm1 = "West Bengal",
			adm2 = "Malda",
			location = "Manikchak",
			trial_id = paste(col("Year"), "Manikchak", col("Site No.")),
			plot_id = paste(col("Site No."), col("Tmnt")),
			treatment = as.character(col("Tmnt")),
			crop = crop,
			variety = as.character(col("Variety")),
			yield_part = "grain",
			planting_date = as_date(col("DOS")),
			row_spacing = sp,
			plot_area = num(col("Area Harvested (sq.m.)")),
			seed_weight = num(col("1000 grain wt (g)")),
			yield = y,
			yield_isfresh = FALSE,
			biomass_kg_ha_ = bio,
			harvest_index = hi,
			anthesis_days = num(col("Days of 50% Anthesis")),
			maturity_days = NA_real_,
			N_fertilizer = NA_real_,
			P_fertilizer = NA_real_,
			K_fertilizer = NA_real_,
			gypsum = NA_real_,
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	one <- function(f) {
		bn <- basename(f)
		r2 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		r3 <- as.data.frame(carobiner::read.excel.hdr(f, sheet=grain_sheet(bn), skip=4, hdr=2))
		r4 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		r5 <- carobiner::read.excel(f, sheet="2 - Site information", col_names=FALSE)
		cells <- trimws(as.character(unlist(r5[1, ])))
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
			adm1 <- "West Bengal"
		}
		if (!is.na(raw_c) && toupper(raw_c) %in% c("NEPAL", "BANGLADESH")) {
			country <- ifelse(toupper(raw_c) == "NEPAL", "Nepal", "Bangladesh")
		}
		r2 <- r2[!is.na(r2$Tmnt) & !is.na(r2$Node), ]
		r3 <- r3[!is.na(r3$Tmnt), ]
		r4 <- r4[!is.na(r4$Tmnt.No), ]
		if (!"Site.No" %in% names(r2) && "Unique.Farmer.s.code" %in% names(r2)) r2$Site.No <- r2$Unique.Farmer.s.code
		if (!"Site.No" %in% names(r3) && "Unique.Farmer.s.code" %in% names(r3)) r3$Site.No <- r3$Unique.Farmer.s.code
		if (!"Site.No" %in% names(r4) && "Unique.Farmer.s.code" %in% names(r4)) r4$Site.No <- r4$Unique.Farmer.s.code
		add_seq <- function(x, tm) ave(seq_len(nrow(x)), x$Site.No, x[[tm]], FUN=seq_along)
		r2$seq <- add_seq(r2, "Tmnt")
		r3$seq <- add_seq(r3, "Tmnt")
		r4$seq <- add_seq(r4, "Tmnt.No")
		keyp <- paste(r2$Site.No, r2$Tmnt.No, r2$seq)
		ig <- match(keyp, paste(r3$Site.No, r3$Tmnt, r3$seq))
		ifr <- match(keyp, paste(r4$Site.No, r4$Tmnt.No, r4$seq))
		y <- take(r3, ig, c("Grain.yield.t.ha", "Calculation_Grain.yield.t.ha", "Calculation_Grain.Yield.t.ha", "Calulation_Grain.Yield.t.ha"))
		sp <- num(r2$Row.spacing.cm)
		sp[!is.na(sp) & sp < 1] <- NA_real_
		hd <- as_date(r2$Datw.of.harvest.dd.mm.yy)
		hd[!is.na(hd) & hd > "2018-06-05"] <- NA_character_
		crop <- tolower(trimws(as.character(r2$Crop)))
		crop[grepl("rice", crop)] <- "rice"
		crop[grepl("wheat", crop)] <- "wheat"
		crop[grepl("lentil", crop)] <- "lentil"
		crop[grepl("maize", crop)] <- "maize"
		crop[grepl("chick", crop)] <- "chickpea"
		crop[grepl("potato", crop)] <- "potato"
		part <- rep("grain", length(crop))
		part[crop %in% c("lentil", "chickpea", "wheat")] <- "seed"
		part[crop == "potato"] <- "tubers"
		hi <- take(r3, ig, "HI")
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		ad <- num(r2$X50pct.anthesis.DAS)
		md <- num(r2$X80pct.physiological.maturity.DAS)
		ad[!is.na(ad) & ad < 0] <- NA_real_
		md[!is.na(md) & md < 0] <- NA_real_
		data.frame(
			country = country,
			adm1 = adm1,
			adm2 = carobiner::fix_name(adm2, "title"),
			location = carobiner::fix_name(r2$Node, "title"),
			trial_id = paste(r2$Year, r2$Node, r2$Trial.Code),
			plot_id = paste(r2$Site.No, r2$Tmnt, r2$seq),
			treatment = r2$Tmnt,
			crop = crop,
			variety = r2$Variety,
			yield_part = part,
			planting_date = as_date(r2$Date.of.seeding.dd.mm.yy),
			harvest_date = hd,
			row_spacing = sp,
			seed_rate = num(r2$Seedrate.kg.ha),
			plot_area = num(r2$Plot.size.m2),
			yield = y * 1000,
			yield_isfresh = FALSE,
			biomass_kg_ha_ = take(r3, ig, "Biomass.t.ha") * 1000,
			harvest_index = hi,
			anthesis_days = ad,
			maturity_days = md,
			N_fertilizer = take(r4, ifr, "N.kg.ha"),
			P_fertilizer = take(r4, ifr, "P2O5.kg.ha") / 2.29,
			K_fertilizer = take(r4, ifr, "K2O.kg.ha") / 1.2051,
			gypsum = take(r4, ifr, c("Gypsum.kg.ha", "Gypsum.Kg.ha")),
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	ff <- ff[basename(ff) %in% c(
		"Aman rice 2014-LT-All nodes-Malda.xlsx",
		"Aman rice 2015-LT-all nodes-Malda.xlsx",
		"Aman rice 2016-LT-All nodes-Malda.xlsx",
		"Aman rice 2017-LT-Urgitola-Malda.xlsx",
		"Aman rice 2017-Manikchak- Malda.xlsx"
	)]
	read_one <- function(f) {
		if (grepl("Manikchak", basename(f))) return(manikchak(f))
		one(f)
	}
	parts <- lapply(ff, read_one)
	cols <- unique(unlist(lapply(parts, names)))
	parts <- lapply(parts, function(x) {
		miss <- setdiff(cols, names(x))
		if (length(miss)) x[miss] <- NA
		x[cols]
	})
	d <- do.call(rbind, parts)
	carobiner::write_files(path, meta, d)
}
