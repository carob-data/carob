# R script for "carob"
# license: GPL (>=3)

## NOTES
# Long-term rabi trials at Sunsari, Nepal. The site-sheet country cell is Nepal and the district cell is Sunsari. The dataset title also names Nepal.
# Grain yield columns are t/ha of air-dry grain and are stored as kg/ha. yield_isfresh is FALSE. Moisture percent is not given.
# P2O5.kg.ha and K2O.kg.ha are oxides and are converted to elemental P and K. N.kg.ha is elemental.
# The crop is taken from the column whose values are crop names. File names supply the crop when that column is a place name.
# Three 2016-17 wheat workbooks use a grain-harvest sheet numbered 13. Latitude and longitude were not filled in on the site sheets that were checked.

## ISSUES
# No coordinates in the site sheets that were checked.
# Plot area is 400 to 440 m2, above the usual plot-area range. The values are kept.
# Harvest cells that are not dates in 2010-2020 are left empty.
# Gypsum and zinc sulphate are product rates, and the hydrate is not stated, so they are not stored as nutrient rates.
# Some site and treatment rows are repeated, and some grain-yield cells are empty.
# One district cell says Sunsaril. It is stored as Sunsari.
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated. out of bounds: plot_area (400, 440). duplicates: duplicate records detected. NA detected: yield.

## NOT PROCESSED
# Rainfall, land preparation, irrigation, ponding, pesticides, weeds, biomass samples, soil analysis, and prices.
# Farmer names.
# Gypsum.kg.ha and ZnSO4.kg.ha.
# Straw and total biomass. The sheets do not say whether those weights are fresh or dry.


carob_script <- function(path) {

"
2.6-Rabi (winter) crops-all nodes-Long term trial (LT)-Sunsari-Nepal

Farmers’ participatory researchers managed long-term trails aimed at improving the productivity, profitability, and sustainability of smallholder agriculture in the EGP by addressing the following objectives: 
1.	Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation. 
2.	Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders. 
3.	Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems.
"

	uri <- "hdl:11529/10548382"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "SRFSI long-term trial, Sunsari",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.8
	)

	header_value <- function(cells, label) {
		i <- which(cells == label)
		if (!length(i)) return(NA_character_)
		rest <- cells[(i[1] + 1):length(cells)]
		rest <- rest[!is.na(rest) & nzchar(rest)]
		if (!length(rest)) return(NA_character_)
		rest[1]
	}
	num <- function(x) {
		if (is.null(x)) return(rep(NA_real_, 1))
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		x[grepl("[A-Za-z]", x)] <- NA_character_
		as.numeric(x)
	}
	sheets_of <- function(f) {
		tmp <- tempfile()
		dir.create(tmp)
		utils::unzip(f, files="xl/workbook.xml", exdir=tmp)
		xml <- paste(readLines(file.path(tmp, "xl", "workbook.xml"), warn=FALSE), collapse="")
		unlink(tmp, recursive=TRUE)
		m <- gregexpr('name="[^"]+"', xml)
		hit <- sub('^name="([^"]+)"$', "\\1", unlist(regmatches(xml, m)))
		gsub("&amp;", "&", hit, fixed=TRUE)
	}
	read_block <- function(f, sheet) {
		top <- carobiner::read.excel(f, sheet=sheet, n_max=12, col_names=FALSE)
		first <- trimws(as.character(top[[1]]))
		i <- which(first == "Year")[1]
		if (length(i) == 0 || is.na(i)) i <- 5
		sk <- i - 1
		if (sk < 2) sk <- 2
		carobiner::read.excel.hdr(f, sheet=sheet, skip=sk, hdr=2)
	}
	col_one <- function(x, pattern) {
		nm <- grep(pattern, names(x), value=TRUE)
		if (!length(nm)) return(rep(NA, nrow(x)))
		nm <- nm[order(nchar(nm))]
		x[[nm[1]]]
	}
	norm_crop <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x[is.na(x) | x %in% c("", "na", "0", "-")] <- NA_character_
		x[x %in% c("kidney bean", "kidneybean")] <- "kidney bean"
		x[x %in% c("chick pea", "chickpea")] <- "chickpea"
		x[x %in% c("paddy", "aman rice", "aman")] <- "rice"
		x
	}
	known <- c("rice", "wheat", "maize", "lentil", "kidney bean", "chickpea", "mustard", "mung bean", "potato")
	pick_crop <- function(x) {
		nm <- grep("Crop", names(x), value=TRUE)
		nm <- nm[!grepl("System|sequence|rotation", nm, ignore.case=TRUE)]
		best <- rep(NA_character_, nrow(x))
		bestn <- 0
		for (k in nm) {
			v <- norm_crop(x[[k]])
			n <- sum(v %in% known, na.rm=TRUE)
			if (n > bestn) {
				best <- v
				bestn <- n
			}
		}
		best
	}
	pick_tmnt <- function(x) {
		nm <- grep("Tmnt", names(x), value=TRUE)
		nm <- nm[!grepl("No", nm)]
		if (!length(nm)) nm <- grep("Tmnt", names(x), value=TRUE)
		if (!length(nm)) return(rep(NA_character_, nrow(x)))
		best <- nm[1]
		bestn <- -1
		for (k in nm) {
			v <- trimws(as.character(x[[k]]))
			v <- v[!is.na(v) & nzchar(v)]
			n <- length(unique(v))
			if (n > bestn && n < max(nrow(x), 1)) {
				best <- k
				bestn <- n
			}
		}
		trimws(as.character(x[[best]]))
	}
	as_date <- function(x) {
		if (is.null(x)) return(NA_character_)
		if (inherits(x, "POSIXt") || inherits(x, "Date")) {
			out <- as.character(as.Date(x))
		} else if (is.numeric(x)) {
			out <- as.character(as.Date(x, origin="1899-12-30"))
		} else {
			x <- trimws(as.character(x))
			x[!nzchar(x) | x %in% c("-", "NA")] <- NA_character_
			out <- rep(NA_character_, length(x))
			iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
			if (any(iso)) out[iso] <- substr(x[iso], 1, 10)
		}
		bad <- !is.na(out) & (out < "2010-01-01" | out > "2020-12-31")
		out[bad] <- NA_character_
		out
	}
	crop_from_name <- function(bn) {
		bn <- tolower(bn)
		if (grepl("rice", bn)) return("rice")
		if (grepl("wheat", bn)) return("wheat")
		if (grepl("maize", bn)) return("maize")
		if (grepl("lentil", bn)) return("lentil")
		if (grepl("kidney", bn)) return("kidney bean")
		NA_character_
	}

	one <- function(f) {
		sh <- sheets_of(f)
		gs <- sh[grepl("Grain Harvest", sh)]
		ps <- sh[grepl("Stand counts", sh)]
		fs <- sh[grepl("Fertilizer amount", sh)]
		if (!length(gs) || !length(ps)) return(NULL)
		gh <- read_block(f, gs[1])
		ph <- read_block(f, ps[1])
		ft <- NULL
		if (length(fs)) ft <- read_block(f, fs[1])
		si <- carobiner::read.excel(f, sheet=sh[grepl("Site information", sh)][1], col_names=FALSE)
		cells <- trimws(as.character(unlist(si[1, ])))
		cells[is.na(cells)] <- ""
		raw_country <- header_value(cells, "Country:")
		country <- raw_country
		adm1 <- NA_character_
		if (!is.na(raw_country) && grepl("India", raw_country)) {
			country <- "India"
			if (grepl("Bihar", raw_country)) adm1 <- "Bihar"
		}
		gh$crop <- pick_crop(gh)
		if (all(is.na(gh$crop))) gh$crop <- pick_crop(ph)[seq_len(nrow(gh))]
		if (all(is.na(gh$crop))) gh$crop <- crop_from_name(basename(f))
		gh$tmnt <- pick_tmnt(gh)
		gh$site <- as.character(col_one(gh, "^Site"))
		gh$year <- as.character(col_one(gh, "^Year"))
		gh$season <- as.character(col_one(gh, "^Season"))
		gh$trial_code <- as.character(col_one(gh, "Trial.Code"))
		ycol <- grep("yield", names(gh), ignore.case=TRUE, value=TRUE)
		ycol <- ycol[grepl("t.ha|t/ha", ycol, ignore.case=TRUE)]
		pref <- ycol[grepl("Grain|Sun.dry|Calculation", ycol)]
		if (length(pref)) ycol <- pref
		ycol <- ycol[!grepl("Straw|Biomass|HI", ycol)]
		yield <- rep(NA_real_, nrow(gh))
		if (length(ycol)) yield <- num(gh[[ycol[1]]])
		ok <- yield[!is.na(yield)]
		if (length(ok) && median(ok) < 40) yield <- yield * 1000
		nfer <- rep(NA_real_, nrow(gh))
		pfer <- nfer
		kfer <- nfer
		if (!is.null(ft)) {
			ft$tmnt <- pick_tmnt(ft)
			ft$site <- as.character(col_one(ft, "^Site"))
			ft$year <- as.character(col_one(ft, "^Year"))
			keyg <- paste(gh$site, gh$tmnt, gh$year)
			keyf <- paste(ft$site, ft$tmnt, ft$year)
			ix <- match(keyg, keyf)
			nc <- grep("N\\.kg\\.ha", names(ft), value=TRUE)
			pc <- grep("P2O5\\.kg\\.ha", names(ft), value=TRUE)
			kc <- grep("K2O\\.kg\\.ha", names(ft), value=TRUE)
			if (length(nc)) nfer <- num(ft[[nc[1]]][ix])
			if (length(pc)) pfer <- num(ft[[pc[1]]][ix]) / 2.29
			if (length(kc)) kfer <- num(ft[[kc[1]]][ix]) / 1.2051
		}
		ph$tmnt <- pick_tmnt(ph)
		ph$site <- as.character(col_one(ph, "^Site"))
		ixp <- match(paste(gh$site, gh$tmnt), paste(ph$site, ph$tmnt))
		variety <- as.character(col_one(ph, "^Variety"))[ixp]
		variety <- trimws(variety)
		variety[!nzchar(variety) | variety %in% c("NA", "0")] <- NA_character_
		season <- tolower(trimws(gh$season))
		season[grepl("kharif|monsoon", season)] <- "kharif"
		season[grepl("rabi|winter", season)] <- "rabi"
		season[!season %in% c("kharif", "rabi")] <- NA_character_
		crop <- norm_crop(gh$crop)
		part <- ifelse(crop %in% c("rice", "wheat", "maize"), "grain", "seed")
		data.frame(
			country = country,
			adm1 = adm1,
			adm2 = header_value(cells, "District:"),
			location = header_value(cells, "Community/Node:"),
			year = gh$year,
			season = season,
			trial_code = gh$trial_code,
			plot_id = gh$site,
			treatment = gh$tmnt,
			crop = crop,
			variety = variety,
			yield = yield,
			yield_part = part,
			plot_area = num(col_one(gh, "Plot.size")),
			row_spacing = num(col_one(ph, "Row.spacing")[ixp]),
			seed_rate = num(col_one(ph, "Seedrate|Seed.rate")[ixp]),
			planting_date = as_date(col_one(ph, "Date.of.seeding")[ixp]),
			harvest_date = as_date(col_one(ph, "Datw.of.harvest|Date.of.harvest")[ixp]),
			anthesis_days = num(col_one(ph, "anthesis.DAS")[ixp]),
			maturity_days = num(col_one(ph, "maturity.DAS")[ixp]),
			N_fertilizer = nfer,
			P_fertilizer = pfer,
			K_fertilizer = kfer
		)
	}

	parts <- lapply(ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)], one)
	parts <- parts[!vapply(parts, is.null, logical(1))]
	d <- do.call(rbind, parts)
	d <- d[!is.na(d$treatment) & nzchar(d$treatment) & !is.na(d$crop), ]
	d$adm2 <- carobiner::fix_name(d$adm2, "title")
	d$adm2[d$adm2 %in% c("Susari", "Sunsaril")] <- "Sunsari"
	d$location <- carobiner::fix_name(d$location, "title")
	d$variety <- carobiner::fix_name(d$variety, "title")
	d$trial_id <- paste(d$location, d$year, d$season, sep="_")
	d$year <- NULL
	d$trial_code <- NULL
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	d$yield_isfresh <- FALSE

	carobiner::write_files(path, meta, d)
}
