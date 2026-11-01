# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm long-term wheat trials at Madhubani, Bihar. geographicCoverage is India, Madhubani. The title names Bihar.
# Grain yield is t/ha and is stored as kg/ha. The harvest sheets describe air-dry grain. Moisture is not in the yield columns used here.
# These wheat files label potassium as K2O, so P and K are converted from the oxides. The 2014 sheet spells that column K2O5. A K.kg.ha column, when that is the name present, is elemental K.
# Boric acid is converted to elemental B (10.81/61.83). Zinc sulfate is not stored.
# The same Site No and treatment can occur twice. Those rows stay separate.
# Row spacing 0 is empty. Anthesis, maturity, and plot area of 0 are empty. A harvest date after the 2018-06-05 publication date is not stored.
# The 2014 workbook names the phenology sheet "4...- Stand counts & Phenology". Korahiya and the 2016-17 books use "13 - Grain Harvest ". The other books use "14 - Grain Harvest ".

## ISSUES
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated. unknown variables: biomass_kg_ha_. out of bounds: plot_area (364, 4800). NA detected: yield.

## NEW VARIABLES
# biomass_kg_ha_: biomass column in t/ha, stored as kg/ha. Fresh or dry is not stated on every sheet. The grain is described as air dry.

## NOT PROCESSED
# Irrigation, ponding, weeds, pesticide applications, biomass subsamples, soil analysis, and prices.
# Fertilizer product names and zinc sulfate. Farmer names. Daily rainfall.


carob_script <- function(path) {

"
On-farm long-term wheat trials at Madhubani, Bihar, rabi seasons 2014-15, 2015-16, and 2016-17.
"

	uri <- "hdl:11529/10547965"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Rabi long-term trial, Madhubani, Bihar",
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
		num <- x
		num[grepl("[A-Za-z]", x)] <- NA_character_
		v <- as.numeric(num)
		excel <- !is.na(v) & v > 20000 & v < 80000
		if (any(excel)) out[excel] <- as.character(as.Date(v[excel], origin="1899-12-30"))
		out
	}
	fix_names <- function(x) {
		nms <- gsub("%", "pct", x)
		nms <- make.names(nms, unique=TRUE)
		nms <- gsub("\\.+", ".", nms)
		nms <- gsub("\\.$", "", nms)
		make.names(nms, unique=TRUE)
	}
	read_field <- function(f, sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE)
		lab <- trimws(as.character(r[[1]]))
		lab[is.na(lab)] <- ""
		hdr <- which(lab == "Year")[1]
		after <- which(grepl("^20[0-9]{2}-", lab))
		after <- after[after > hdr][1]
		block <- r[hdr:(after - 1), , drop=FALSE]
		nms <- apply(block, 2, function(i) {
			i <- trimws(as.character(i))
			i <- i[!is.na(i) & nzchar(i)]
			if (!length(i)) return("")
			paste(i, collapse="_")
		})
		body <- as.data.frame(r[after:nrow(r), , drop=FALSE])
		names(body) <- fix_names(nms)
		body
	}
	last_name <- function(nms, pat) {
		n <- grep(pat, nms, ignore.case=TRUE, value=TRUE)
		if (!length(n)) return(NA_character_)
		n[length(n)]
	}
	date_name <- function(nms, pat) {
		n <- grep(pat, nms, ignore.case=TRUE, value=TRUE)
		n <- n[grepl("date|datw", n, ignore.case=TRUE)]
		if (!length(n)) return(NA_character_)
		n[1]
	}
	col_or_na <- function(df, nm) {
		if (is.na(nm) || !nm %in% names(df)) return(rep(NA, nrow(df)))
		df[[nm]]
	}
	take_named <- function(df, idx, nm) {
		if (is.na(nm) || !nm %in% names(df)) return(rep(NA_real_, length(idx)))
		as.numeric(df[[nm]][idx])
	}

	one <- function(f) {
		bn <- basename(f)
		ph_sheet <- "4- Stand counts & Phenology"
		if (grepl("2014-15", bn)) ph_sheet <- "4...- Stand counts & Phenology"
		gh_sheet <- "14 - Grain Harvest "
		if (grepl("2016-17", bn) || grepl("Korahiya", bn)) gh_sheet <- "13 - Grain Harvest "
		ph <- read_field(f, ph_sheet)
		gh <- read_field(f, gh_sheet)
		ft <- read_field(f, "6 - Fertilizer amounts ")
		ph <- ph[!is.na(ph$Tmnt) & !is.na(ph$Node) & nzchar(as.character(ph$Tmnt)), ]
		gh <- gh[!is.na(gh$Tmnt) & nzchar(as.character(gh$Tmnt)), ]
		ft <- ft[!is.na(ft$Tmnt) & nzchar(as.character(ft$Tmnt)), ]
		add_seq <- function(x) ave(seq_len(nrow(x)), x$Site.No, x$Tmnt, FUN=seq_along)
		ph$seq <- add_seq(ph)
		gh$seq <- add_seq(gh)
		ft$seq <- add_seq(ft)
		key <- function(x) paste(x$Site.No, x$Tmnt, x$seq)
		ig <- match(key(ph), key(gh))
		ifr <- match(key(ph), key(ft))
		y <- take_named(gh, ig, last_name(names(gh), "grain.yield.t.ha|sun.dry.grain"))
		bio <- take_named(gh, ig, last_name(names(gh), "biomass.t.ha"))
		hi <- take_named(gh, ig, last_name(names(gh), "^HI$"))
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		Kox <- take_named(ft, ifr, last_name(names(ft), "K2O"))
		Kplain <- take_named(ft, ifr, last_name(names(ft), "^K.kg.ha$"))
		K <- ifelse(!is.na(Kox), Kox / 1.2051, Kplain)
		B <- take_named(ft, ifr, last_name(names(ft), "Boric"))
		spn <- last_name(names(ph), "Row.spacing")
		sp <- if (is.na(spn)) rep(NA_character_, nrow(ph)) else as.character(ph[[spn]])
		sp[sp %in% c("0", "30-40", "40-50")] <- NA_character_
		hd <- as_date(col_or_na(ph, date_name(names(ph), "harvest")))
		hd[!is.na(hd) & hd > "2018-06-05"] <- NA_character_
		pd <- as_date(col_or_na(ph, date_name(names(ph), "seed")))
		cropn <- if ("Crop" %in% names(ph)) "Crop" else last_name(names(ph), "Trial.Type")
		crop <- tolower(trimws(as.character(ph[[cropn]])))
		data.frame(
			country = "India",
			adm1 = "Bihar",
			adm2 = "Madhubani",
			location = as.character(ph$Node),
			trial_id = paste(ph$Year, ph$Node),
			plot_id = paste(ph$Site.No, ph$Tmnt, ph$seq),
			treatment = as.character(ph$Tmnt),
			crop = crop,
			variety = as.character(ph$Variety),
			yield_part = "grain",
			planting_date = pd,
			harvest_date = hd,
			row_spacing = as.numeric(sp),
			seed_rate = take_named(ph, seq_len(nrow(ph)), last_name(names(ph), "Seed.rate|Seedrate")),
			plot_area = take_named(ph, seq_len(nrow(ph)), last_name(names(ph), "Plot.size")),
			anthesis_days = take_named(ph, seq_len(nrow(ph)), last_name(names(ph), "anthesis.DAS")),
			maturity_days = take_named(ph, seq_len(nrow(ph)), last_name(names(ph), "maturity.DAS")),
			yield = y * 1000,
			biomass_kg_ha_ = bio * 1000,
			harvest_index = hi,
			N_fertilizer = take_named(ft, ifr, last_name(names(ft), "^N\\.")),
			P_fertilizer = take_named(ft, ifr, last_name(names(ft), "P2O5")) / 2.29,
			K_fertilizer = K,
			gypsum = take_named(ft, ifr, last_name(names(ft), "Gypsum")),
			B_fertilizer = B * 10.81 / 61.83,
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE,
			yield_isfresh = FALSE
		)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	d <- do.call(rbind, lapply(ff, one))
	d <- d[!is.na(d$crop) & d$crop != "na" & nzchar(d$crop), ]
	d <- d[!is.na(d$treatment) & nzchar(d$treatment), ]
	d$plot_area[d$plot_area == 0] <- NA_real_
	d$anthesis_days[d$anthesis_days == 0] <- NA_real_
	d$maturity_days[d$maturity_days == 0] <- NA_real_
	d$row_spacing[d$row_spacing == 0] <- NA_real_
	if (all(is.na(d$gypsum))) d$gypsum <- NULL
	if (all(is.na(d$B_fertilizer))) d$B_fertilizer <- NULL
	if (all(is.na(d$anthesis_days))) d$anthesis_days <- NULL
	if (all(is.na(d$maturity_days))) d$maturity_days <- NULL
	carobiner::write_files(path, meta, d)
}
