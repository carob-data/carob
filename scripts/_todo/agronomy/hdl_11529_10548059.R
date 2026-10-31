# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station split-plot maize herbicide trials, rabi 2015-16 and 2016-17. The Rangpur general sheet says Design: Split plot and names Bangladesh.
# The BAU Bihar site sheet says Country: India and District: Bhagalpur. The Sunsari site sheet says Country: Bangladesh. That cell is stored as written.
# Sunsari 2015 records spacing as "60 x 20". The first number is row_spacing and the second is plant_spacing, both cm.
# Sunsari has no TGW column. Seed weight there is 1000-grain weight(g).
# Rajshahi, Dinajpur, and Cooch Behar workbooks have no country cell.
# Grain yield is the Grain yield (t/ha) column, stored as kg/ha. Dry stover t/ha is dmy_residue.
# The 2015 harvest header says plant height in m. The values are about 190, so they are stored as cm.
# Harvest index in the field books is a fraction and is stored as a percent.
# Oven-dry weed biomass is g/m2. It is stored as kg/ha in weed_biomass_dry_. The vocabulary weed_biomass is fresh weight.
# Rangpur is a Stat sheet, not a field book. gy is unlabeled. It is treated as t/ha because the field books call the same quantity Grain yield (t/ha).
# Herbicide names are the subplot and main-plot text. A "No pre-emergence" or "No post emergence" level is none.
# The fertilizer sheet has product names and no applied amounts.

## ISSUES
# Plant height is 0 to 320 cm. Those values were not changed. Several stations have no country cell, and some plots have no yield.
# write_files: missing variables: latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variable: weed_biomass_dry_ (weed_biomass?). unknown variables: pre_herbicide_, post_herbicide_. out of bounds: plant_height (0, 320). NA detected: yield, country. contributor:

## NEW VARIABLES
# pre_herbicide_: pre-emergence herbicide, as named in the field book. none means unsprayed.
# post_herbicide_: post-emergence herbicide, as named in the field book. none means unsprayed.
# weed_biomass_dry_: oven-dry weed biomass at harvest, kg/ha. The source is g/m2.

## NOT PROCESSED
# Fertilizer amounts. The application cells are empty.
# Weed counts at 15, 30, and 60 days, and the Rangpur count columns.
# Quadrat biomass subsamples, cob counts, and prices.
# Farmer or station contact names.
# Daily rainfall sheets.


carob_script <- function(path) {

"
8.3-Zero tillage maize weed management-on-station research trials-SRFSI Project-ACIAR-CIMMYT

The direct seeded maize in no-till/strip till/permanent beds or tilled soil with fresh beds is an alternate option through mechanized precision planting in a single pass. The CA-based crop management techniques will be faced major concern of weed management initially. To address the weed management problems in CA-based maize production under no-till systems, different weed management options trial is designed to convince different stakeholders including farmers, input dealers, extension officers, private sectors, researchers, and policymakers.
"

	uri <- "hdl:11529/10548059"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "CIMMYT",
		publication = NA,
		project = "SRFSI zero-tillage maize weed management",
		design = "split plot",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.8
	)

	num <- function(x) {
		if (is.null(x)) return(NA_real_)
		if (is.numeric(x)) return(as.numeric(x))
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		as.numeric(x)
	}
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
	header_value <- function(cells, label) {
		cells <- trimws(as.character(cells))
		cells[is.na(cells)] <- ""
		i <- which(cells == label)
		if (!length(i)) return(NA_character_)
		rest <- cells[(i[1] + 1):length(cells)]
		rest <- rest[nzchar(rest)]
		if (!length(rest)) return(NA_character_)
		rest[1]
	}
	block <- function(f, sheet) {
		r <- try(carobiner::read.excel(f, sheet=sheet, col_names=FALSE), silent=TRUE)
		if (inherits(r, "try-error")) return(NULL)
		c1 <- trimws(as.character(r[[1]]))
		hdr <- which(c1 == "Year")
		if (!length(hdr)) return(NULL)
		hdr <- hdr[1]
		nms <- trimws(as.character(unlist(r[hdr, ])))
		empty <- is.na(nms) | !nzchar(nms)
		nms[empty] <- paste0("blank", seq_len(sum(empty)))
		nms <- make.unique(nms)
		x <- r[(hdr + 1):nrow(r), , drop=FALSE]
		names(x) <- nms
		x
	}
	colx <- function(x, label=NULL, pattern=NULL) {
		if (is.null(x)) return(NULL)
		nms <- names(x)
		if (!is.null(label)) i <- which(nms == label) else i <- grep(pattern, nms)
		if (!length(i)) return(rep(NA, nrow(x)))
		x[[i[1]]]
	}
	clean_herb <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | !nzchar(x)] <- NA_character_
		x[grepl("^No pre|^No post", x)] <- "none"
		x
	}

	spacing <- function(x) {
		x <- trimws(as.character(x))
		bits <- strsplit(x, "x", fixed=TRUE)
		first <- vapply(bits, function(p) trimws(p[1]), "")
		second <- vapply(bits, function(p) {
			if (length(p) < 2) return(NA_character_)
			trimws(p[2])
		}, "")
		list(row_spacing=num(first), plant_spacing=num(second))
	}

	fieldbook <- function(f) {
		ph <- block(f, "4- Stand counts & Phenology")
		if (is.null(ph)) ph <- block(f, "4- Stand counts, PH & Phenology")
		hv <- block(f, "16.WeedBio & cropData (harvest)")
		if (is.null(hv)) return(NULL)
		si <- try(carobiner::read.excel(f, sheet="2 - Site information", col_names=FALSE), silent=TRUE)
		country <- NA_character_
		adm2 <- NA_character_
		if (!inherits(si, "try-error")) {
			country <- header_value(unlist(si[1, ]), "Country:")
			adm2 <- header_value(unlist(si[1, ]), "District:")
		}
		pre <- clean_herb(colx(hv, "Main plot trt (pre-emergence herbicides)"))
		post <- clean_herb(colx(hv, "Sub plot trt (Post-emergence herbicides)"))
		keep <- !is.na(pre) & !is.na(post)
		hv <- hv[keep, ]
		pre <- pre[keep]
		post <- post[keep]
		ht <- num(colx(hv, pattern="^Plant height"))
		if (any(!is.na(ht)) && median(ht, na.rm=TRUE) < 5) ht <- ht * 100
		hi <- num(colx(hv, "Hi"))
		if (all(is.na(hi))) hi <- num(colx(hv, "HI"))
		if (any(!is.na(hi)) && median(hi, na.rm=TRUE) <= 1.5) hi <- hi * 100
		mc1 <- num(colx(hv, pattern="^MC% of grain sample1"))
		mc2 <- num(colx(hv, pattern="^MC% of grain sample 2"))
		moist <- (mc1 + mc2) / 2
		one_mc <- is.na(moist) & !is.na(mc1)
		moist[one_mc] <- mc1[one_mc]
		weed <- num(colx(hv, "Oven dry weed biomass at harvest (g/m2)")) * 10
		d <- data.frame(
			country = country,
			adm2 = adm2,
			location = trimws(as.character(colx(hv, "Name of research station"))),
			season = "rabi",
			year = trimws(as.character(colx(hv, "Year"))),
			rep = as.integer(num(colx(hv, "Replication"))),
			plot_id = trimws(as.character(colx(hv, "Plot Id"))),
			treatment = paste(pre, post, sep="; "),
			pre_herbicide_ = pre,
			post_herbicide_ = post,
			herbicide_used = !(pre == "none" & post == "none"),
			plot_area = num(colx(hv, pattern="^Plot size")),
			plant_height = ht,
			seed_weight = NA_real_,
			yield_moisture = moist,
			yield = num(colx(hv, "Grain yield (t/ha)")) * 1000,
			dmy_residue = num(colx(hv, "Dry stover wt (t/ha)")) * 1000,
			harvest_index = hi,
			weed_biomass_dry_ = weed,
			variety = NA_character_,
			planting_date = NA_character_,
			row_spacing = NA_real_,
			plant_spacing = NA_real_
		)
		sw <- num(colx(hv, "TGW"))
		if (all(is.na(sw))) sw <- num(colx(hv, "1000-grain weight(g)"))
		d$seed_weight <- sw
		if (!is.null(ph)) {
			pre_p <- clean_herb(colx(ph, "Main plot trt (pre-emergence herbicides)"))
			post_p <- clean_herb(colx(ph, "Sub plot trt (Post-emergence herbicides)"))
			idx <- !is.na(pre_p) & !is.na(post_p)
			ph <- ph[idx, ]
			pre_p <- pre_p[idx]
			post_p <- post_p[idx]
			keyp <- paste(colx(ph, "Replication"), colx(ph, "Plot Id"), pre_p, post_p)
			variety <- trimws(as.character(colx(ph, "Variety")))
			pdate <- as_date(colx(ph, pattern="^Date of seeding"))
			space <- spacing(colx(ph, pattern="^Row spacing"))
			yr <- trimws(as.character(colx(ph, "Year")))
			lok <- trimws(as.character(colx(ph, "Name of research station")))
			keyh <- paste(d$rep, d$plot_id, d$pre_herbicide_, d$post_herbicide_)
			i <- match(keyh, keyp)
			d$variety <- variety[i]
			d$planting_date <- pdate[i]
			d$row_spacing <- space$row_spacing[i]
			d$plant_spacing <- space$plant_spacing[i]
			miss_year <- is.na(d$year) | !nzchar(d$year)
			d$year[miss_year] <- yr[i][miss_year]
			miss_loc <- is.na(d$location) | !nzchar(d$location)
			d$location[miss_loc] <- lok[i][miss_loc]
		}
		d
	}

	rangpur <- function(f) {
		r <- try(carobiner::read.excel(f, sheet="Stat"), silent=TRUE)
		if (inherits(r, "try-error")) return(NULL)
		if (!("gy" %in% names(r))) return(NULL)
		g <- carobiner::read.excel(f, sheet="General", col_names=FALSE)
		txt <- paste(as.character(unlist(g)), collapse=" ")
		country <- NA_character_
		if (grepl("BANGLADESH", txt)) country <- "Bangladesh"
		keep <- !is.na(r$Year) & !is.na(r$gy)
		r <- r[keep, ]
		pre_code <- c(PH1="none", PH2="Pendimethalin", PH3="Atrazine")
		post_code <- c(
			PO1="none", PO2="Atrazine", PO3="Laudis (Tembotrione)",
			PO4="Sempra (Halosulfuron)", PO5="Laudis + Atrazine", PO6="Sempra + Atrazine"
		)
		pre <- unname(pre_code[as.character(r$mtrt)])
		post <- unname(post_code[as.character(r$strt)])
		gy <- num(r$gy)
		if (median(gy, na.rm=TRUE) < 40) gy <- gy * 1000
		sy <- num(r$sy)
		if (median(sy, na.rm=TRUE) < 40) sy <- sy * 1000
		hi <- num(r$hi)
		if (median(hi, na.rm=TRUE) <= 1.5) hi <- hi * 100
		data.frame(
			country = country,
			adm2 = NA_character_,
			location = trimws(as.character(r$site)),
			season = "rabi",
			year = trimws(as.character(r$Year)),
			rep = as.integer(num(r$rep)),
			plot_id = as.character(r$trt),
			treatment = paste(pre, post, sep="; "),
			pre_herbicide_ = pre,
			post_herbicide_ = post,
			herbicide_used = !(pre == "none" & post == "none"),
			plot_area = NA_real_,
			plant_height = num(r$pht),
			seed_weight = num(r$tgw),
			yield_moisture = NA_real_,
			yield = gy,
			dmy_residue = sy,
			harvest_index = hi,
			weed_biomass_dry_ = NA_real_,
			variety = NA_character_,
			planting_date = NA_character_,
			row_spacing = NA_real_,
			plant_spacing = NA_real_
		)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	parts <- list()
	for (f in ff) {
		hv <- fieldbook(f)
		if (!is.null(hv)) {
			parts[[length(parts) + 1]] <- hv
		} else {
			st <- rangpur(f)
			if (!is.null(st)) parts[[length(parts) + 1]] <- st
		}
	}
	d <- do.call(rbind, parts)
	okloc <- !is.na(d$location) & nzchar(d$location)
	d$location[okloc] <- carobiner::fix_name(d$location[okloc], "title")
	okadm <- !is.na(d$adm2) & nzchar(d$adm2)
	d$adm2[okadm] <- carobiner::fix_name(d$adm2[okadm], "title")
	d <- d[!is.na(d$treatment) & nzchar(d$treatment), ]
	d$trial_id <- paste(d$location, d$year, sep="_")
	d$plot_id <- paste(d$trial_id, d$rep, d$plot_id, d$treatment, sep="_")
	d$year <- NULL
	d$rep <- NULL
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$stress <- "weeds"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
