# R script for "carob"
# license: GPL (>=3)

## NOTES
# ICRISAT station trial of rabi sorghum at Patancheru. Each wide-table row is one destructively sampled plant. plant_number_ is the sample index within the plot, not one plant followed through the season.
# The wide table is the experiment sheet, with final-harvest total-crop measurements joined on plot and plant. The long table is every growth sample. Weather is passed separately.
# Plant density in the sheet is plants/m2 and is stored as plants/ha. Row spacing stays in cm.
# A fertilizer grade in the sheet is N-P2O5-K2O. Elemental P is P2O5/2.29 and K is K2O/1.2051. Product amounts are kg/ha. Urea with no grade uses 46% N. DAP with no grade uses the terminag composition (18% N, 20.1% P). Ammonium phosphate with no grade is stored as MAP at the terminag composition (11% N, 26.9% P). K_fertilizer is 0 when only those products were applied.
# Dates are Excel serials (origin 1899-12-30), a rabi day-of-year (1-366: day >= 180 is the season start year, otherwise the next year), or d-m-yyyy.
# Harvest index is a fraction in the sheet and is stored as a percent. A 50-grain mass (g) times 20, or a 100-grain mass times 10, is the 1000-seed weight.
# Biomass columns are not labeled fresh or dry. Harvest index matches grain/total, so grain and biomass are stored as dry matter (yield_isfresh FALSE). Moisture is not in the files and yield_moisture is omitted.
# Solar radiation is about 6-22 in every year. The 2017-20 descriptions call that MJ/m2, and the older sheets have the same magnitude, so values below 100 are multiplied by 1000 to kJ/m2.
# Flood, Flooded, Flood Irrigation, Full and Sprinkler are methods, not millimetres. irrigation_amount is omitted when any amount is one of those words. A plot irrigated both by flood and by sprinkler has no irrigation_method, because that variable takes one value.
# Location PA in the sheet is Patancheru. adm1 Telangana and adm2 Sangareddy are the GADM units for that place; they are not written in the files. Latitude and longitude are copied only when the experiment sheet has numbers.
# on_farm is FALSE, is_survey is FALSE, crop is sorghum, yield_part is grain, and trial_id is 1. The maturity date is used as the harvest date.
# stress is drought only for a WS code or the words water stress, or when plot details name that water regime. Well-watered plots are left blank. A treatment that only says Stress is not labeled drought unless plot details say so.
# Season 2014-15, field BM8-C. Treatment is Control or Stress, crossed with nitrogen and density. Coordinates are the sheet values 17.53oN and 78.27oE.
# The complete-dataset workbook is not used when the experiment, growth, plot and weather files already exist.

## ISSUES
# The sheet never says whether biomass is fresh or dry. Dry matter is an assumption from the harvest index, and it should be confirmed.
# Radiation is assumed to be MJ/m2 before converting to kJ/m2. Confirm for the years whose weather sheet does not name the unit.
# adm1 and adm2 are not in the files.
# carob_contributor is unset until someone has read this script.
# write_files: missing carob_contributor and yield_moisture; unknown panicle_yield_, plant_number_, tiller_number_, sla_; NA yield; site BM8-C flagged as all uppercase; harvest columns are also on the long table.
# Plants with no grain weight are kept. Flowering dates outside the terminag range are kept as recorded.
# Ammonium phosphate (150 kg/ha) has no grade. It is stored as MAP at 11% N and 26.9% P. The urea row is grade 46-0-0. HN and LN do not differ in the fertilizer columns; both are 62.5 kg N/ha. Density does differ.
# Plot-details water regime is Stress on the Stress treatments, and those rows are stress drought.
# Six plants have no grain yield.
# The weather date header says mm/dd/yyyy. The values behave as Excel serials and were read with origin 1899-12-30.

## NOT PROCESSED
# Culm splits (Main Plant, Main stem, Tiller 1-3, All tillers, and the MS-, T1-, T2-, T3-, AT-, NT- columns): the same biomass measurements already taken from the total-crop columns.
# g/plant columns where the matching kg/ha column was used.
# Field plan: a spatial layout, not the measurements.
# Soil bulk density workbook: can weights and fresh and dry soil by depth. It is not one bulk density per plot, and core volume was not calculated.
# Soil chemistry sheets inside the complete workbook.
# Genotype glossary: the variety names used here are already on the experiment sheet.
# Weather wind, evaporation and sunshine. The 2014-15 wind column is labeled km/h. Julian day repeats the date.
# Plot-details genotypes that were never sampled in the experiment or growth sheets.
# Complete dataset workbook: it duplicates the standalone experiment, growth, plot and weather files.

## NEW VARIABLES
# plant_number_: destructive sample index within the plot
# grain_number_: grains per plant, total crop
# sla_: specific leaf area (cm2/g)
# panicle_yield_: panicle weight (kg/ha)
# tiller_number_: tillers per plant
# Kept only when that sheet has the measurement.

carob_script <- function(path) {

"
Data on multi factorial treatments in Rabi sorghum for 2014-15 at Patancheru

Sequential observations of agronomicaly important traits during cropping season for Rabi Sorghum Crop during 2014 and 2015 season in ICRISAT Patancheru campus research fields.
"

	# Shared reader for the ICRISAT Patancheru rabi-sorghum multi-factorial series.
	# Column positions differ by year; matching is by header text.
	
	norm_txt <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x[is.na(x)] <- ""
		gsub("[^a-z0-9]+", " ", x)
	}
	
	forward_fill <- function(x) {
		x <- as.character(x)
		x[is.na(x)] <- ""
		last <- ""
		for (i in seq_along(x)) {
			if (nzchar(trimws(x[i]))) {
				last <- trimws(x[i])
			} else {
				x[i] <- last
			}
		}
		x
	}
	
	num_clean <- function(x) {
		if (is.numeric(x)) return(as.numeric(x))
		x <- trimws(as.character(x))
		low <- tolower(x)
		# Grades (11-52-0) and explicit missing tokens are not amounts.
		x[low %in% c("", "na", "no", "none", "-", "?", "no irrigation", "abcd",
			"flood", "flooded", "flood irrigation", "full", "sprinkler") |
			grepl("^[0-9.]+-[0-9.]+-[0-9.]+$", x)] <- NA
		as.numeric(x)
	}
	
	id_chr <- function(x) {
		if (is.numeric(x)) {
			return(as.character(as.integer(round(x))))
		}
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		x <- sub("\\.0+$", "", x)
		x
	}
	
	parse_ll <- function(x) {
		if (is.numeric(x)) return(as.numeric(x))
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "NaN")] <- NA_character_
		out <- rep(NA_real_, length(x))
		ok <- !is.na(x) & grepl("[0-9]", x)
		cleaned <- gsub("[^0-9.\\-]", "", x)
		good <- ok & grepl("^-?[0-9]+(\\.[0-9]+)?$", cleaned)
		out[good] <- as.numeric(cleaned[good])
		out
	}
	
	# Dates are Excel serials, day-of-year (rabi spans two calendar years), or d-m-yyyy.
	parse_dates <- function(x, start_year) {
		if (inherits(x, "Date") || inherits(x, "POSIXt")) {
			return(as.character(as.Date(x)))
		}
		x <- trimws(as.character(x))
		x[tolower(x) %in% c("", "na", "no", "none", "-", "?", "no irrigation")] <- NA_character_
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
		out[iso] <- substr(x[iso], 1, 10)
		dmy <- !is.na(x) & !iso & grepl("^[0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{4}$", x)
		if (any(dmy)) {
			out[dmy] <- as.character(as.Date(gsub("/", "-", x[dmy]), format = "%d-%m-%Y"))
		}
		num <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
		xn <- rep(NA_real_, length(x))
		if (any(num)) xn[num] <- as.numeric(x[num])
		ex <- num & !is.na(xn) & xn > 20000 & xn < 80000
		if (any(ex)) out[ex] <- as.character(as.Date(xn[ex], origin = "1899-12-30"))
		doy <- num & !is.na(xn) & xn >= 1 & xn <= 366 & !ex
		if (any(doy) && !is.na(start_year)) {
			yr <- ifelse(xn[doy] >= 180, start_year, start_year + 1L)
			out[doy] <- as.character(as.Date(paste0(yr, "-01-01")) + (xn[doy] - 1))
		}
		out
	}
	
	start_year_of <- function(x) {
		x <- paste(x, collapse = " ")
		m <- regexpr("(19|20)[0-9]{2}", x)
		if (m < 0) return(NA_integer_)
		as.integer(regmatches(x, m)[1])
	}
	
	parse_grade <- function(g) {
		g <- trimws(as.character(g))
		if (is.na(g) || !grepl("^[0-9.]+-[0-9.]+-[0-9.]+$", g)) return(NULL)
		p <- strsplit(g, "-", fixed = TRUE)[[1]]
		# Grade is N-P2O5-K2O. Elemental P = P2O5/2.29, K = K2O/1.2051.
		c(N = as.numeric(p[1]) / 100,
			P = (as.numeric(p[2]) / 2.29) / 100,
			K = (as.numeric(p[3]) / 1.2051) / 100)
	}
	
	product_name <- function(type) {
		t <- tolower(trimws(as.character(type)))
		if (is.na(t) || t %in% c("", "na", "no", "none", "-", "?")) return(NA_character_)
		if (grepl("urea", t)) return("urea")
		if (grepl("dap|diammonium|di-ammonium|di ammonium", t)) return("DAP")
		if (grepl("ammonium phosphate|ammonium phosp|^ap$", t)) return("MAP")
		NA_character_
	}
	
	frac_of <- function(prod, grade) {
		g <- parse_grade(grade)
		if (!is.null(g)) return(g)
		if (is.na(prod)) return(c(N = NA_real_, P = NA_real_, K = NA_real_))
		if (prod == "urea") return(c(N = 0.46, P = 0, K = 0))
		# terminag values_fertilizer_type: DAP 18% N, 20.1% P; MAP 11% N, 26.9% P
		if (prod == "DAP") return(c(N = 0.18, P = 0.201, K = 0))
		if (prod == "MAP") return(c(N = 0.11, P = 0.269, K = 0))
		c(N = NA_real_, P = NA_real_, K = NA_real_)
	}
	
	paste_ok <- function(x) {
		x <- x[!is.na(x) & x != ""]
		x <- unique(x)
		if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
	}
	
	col_hit <- function(nm, pat, excl = NULL) {
		hit <- grep(pat, nm)
		if (!is.null(excl) && length(hit)) hit <- hit[!grepl(excl, nm[hit])]
		if (length(hit) == 0) return(NA_integer_)
		hit[1]
	}
	
	read_grid <- function(f, sheet = 1) {
		carobiner::read.excel(f, sheet = sheet, col_names = FALSE)
	}
	
	experiment_header <- function(e) {
		n <- min(30, nrow(e))
		cands <- integer(0)
		for (i in seq_len(n)) {
			vals <- norm_txt(unlist(e[i, , drop = TRUE]))
			has_id <- any(vals %in% c("code", "experiment code"))
			has_key <- any(grepl("plot|trt|genotype|geno|entry|latitude|sowing|density", vals))
			if (has_id && has_key) cands <- c(cands, i)
		}
		if (length(cands) == 0) return(NA_integer_)
		# The usable header is the last candidate above the first data row.
		data_row <- which(grepl("^PA", as.character(e[[1]])))[1]
		if (!is.na(data_row)) {
			before <- cands[cands < data_row]
			if (length(before)) return(max(before))
		}
		cands[length(cands)]
	}
	
	read_experiment <- function(f, sheet = 1) {
		e <- read_grid(f, sheet)
		hdr <- experiment_header(e)
		if (is.na(hdr)) stop("No experiment header in ", basename(f))
		labs <- as.character(unlist(e[hdr, , drop = TRUE]))
		labs[is.na(labs)] <- ""
		# 2015-16 splits plot/genotype onto an earlier header row than the dates.
		if (hdr > 1) {
			for (k in (hdr - 1):max(1, hdr - 6)) {
				prev <- as.character(unlist(e[k, , drop = TRUE]))
				prev[is.na(prev)] <- ""
				prev[tolower(prev) %in% c("experiment", "code")] <- ""
				unit <- grepl("kg/ha|^mm$|per m2|^cm$|^number$|^amt$", trimws(prev), ignore.case = TRUE)
				prev[unit] <- ""
				empty <- !nzchar(trimws(labs))
				labs[empty & nzchar(prev)] <- prev[empty & nzchar(prev)]
			}
		}
		grp <- rep("", length(labs))
		if (hdr > 1) {
			grp <- forward_fill(as.character(unlist(e[hdr - 1, , drop = TRUE])))
		}
		lab_n <- norm_txt(labs)
		full <- labs
		generic <- lab_n %in% c("date", "type", "amount", "amt", "code", "")
		full[generic] <- trimws(paste(grp[generic], labs[generic]))
		nm <- norm_txt(full)
		# A bare "date" sitting between emergence and maturity is flowering (2013-14 sheet).
		em <- col_hit(nm, "emergence")
		mat <- col_hit(nm, "maturity")
		fl <- col_hit(nm, "flowering", excl = "julian")
		if (is.na(fl) && !is.na(em) && !is.na(mat) && mat > em + 1) {
			mid <- (em + 1):(mat - 1)
			mid <- mid[grepl("^date( date)*$", nm[mid]) | nm[mid] == ""]
			if (length(mid) == 1) {
				full[mid] <- "Flowering date"
				nm[mid] <- "flowering date"
			}
		}
		dat <- e[(hdr + 1):nrow(e), , drop = FALSE]
		keep <- apply(dat, 1, function(r) any(!is.na(r) & as.character(r) != ""))
		dat <- dat[keep, , drop = FALSE]
		jp <- which(nm %in% c("plot", "plot id"))[1]
		if (length(jp) == 1 && !is.na(jp)) {
			pv <- trimws(as.character(dat[[jp]]))
			dat <- dat[grepl("^[0-9]+(\\.[0-9]+)?$", pv), , drop = FALSE]
		}
		names(dat) <- make.unique(ifelse(nzchar(full), full, paste0("blank", seq_along(full))))
		list(dat = dat, nm = nm, full = full, hdr = hdr)
	}
	
	pick_variety <- function(dat, nm) {
		cands <- which(nm %in% c("genotype", "entry", "geno", "cultivar"))
		if (length(cands) == 0) return(rep(NA_character_, nrow(dat)))
		score <- sapply(cands, function(j) {
			v <- as.character(dat[[j]])
			mean(grepl("[A-Za-z]", v), na.rm = TRUE)
		})
		as.character(dat[[cands[which.max(score)]]])
	}
	
	build_experiment <- function(f, sheet = 1) {
		ex <- read_experiment(f, sheet)
		dat <- ex$dat
		nm <- ex$nm
		n <- nrow(dat)
		col <- function(pat, excl = NULL) col_hit(nm, pat, excl)
		year_raw <- if (!is.na(col("^year$"))) as.character(dat[[col("^year$")]]) else NA_character_
		sy <- start_year_of(year_raw)
		if (is.na(sy) && !is.na(col("experiment code|^code$"))) {
			sy <- start_year_of(as.character(dat[[col("experiment code|^code$")]]))
		}
		trt <- if (!is.na(col("^trt$|^treatment$"))) trimws(as.character(dat[[col("^trt$|^treatment$")]])) else rep(NA_character_, n)
		sub <- if (!is.na(col("sub trt"))) trimws(as.character(dat[[col("sub trt")]])) else rep(NA_character_, n)
		# 2013-14 stores HN/HD in an unlabeled column between TRT and sowing.
		if (all(is.na(sub) | sub == "")) {
			jsow <- col("sowing")
			jtrt <- col("^trt$|^treatment$")
			if (!is.na(jsow) && !is.na(jtrt) && jsow > jtrt + 1) {
				for (j in (jtrt + 1):(jsow - 1)) {
					v <- trimws(as.character(dat[[j]]))
					if (mean(grepl("HN|LN|HD|LD", toupper(v)), na.rm = TRUE) > 0.4) sub <- v
				}
			}
		}
		sub[tolower(sub) %in% c("na", "no", "")] <- NA_character_
		treatment <- trt
		both <- !is.na(sub) & !is.na(trt) & !mapply(function(a, b) grepl(a, b, fixed = TRUE), sub, trt)
		treatment[both] <- paste(trt[both], sub[both])
		# 2013-14 puts WW/WS in the unlabeled column immediately before TRT.
		jtrt <- col("^trt$|^treatment$")
		if (!is.na(jtrt) && jtrt > 1) {
			v <- trimws(as.character(dat[[jtrt - 1]]))
			if (mean(grepl("^(WW|WS)$", toupper(v)), na.rm = TRUE) > 0.4) {
				add <- !is.na(v) & nzchar(v)
				treatment[add] <- paste(v[add], treatment[add])
			}
		}
		variety <- trimws(pick_variety(dat, nm))
		loc <- if (!is.na(col("^location$"))) trimws(as.character(dat[[col("^location$")]])) else rep("Patancheru", n)
		loc[loc %in% c("PA", "Pa")] <- "Patancheru"
		country <- if (!is.na(col("^country$"))) carobiner::fix_name(tolower(trimws(as.character(dat[[col("^country$")]]))), "title") else rep("India", n)
		field <- if (!is.na(col("^field$"))) trimws(as.character(dat[[col("^field$")]])) else rep(NA_character_, n)
		plot <- if (!is.na(col("^plot$"))) id_chr(dat[[col("^plot$")]]) else rep(NA_character_, n)
		plant <- if (!is.na(col("^plant$|^plant no$|^plantno$|^plnt no$|^plant number$"))) as.integer(round(num_clean(dat[[col("^plant$|^plant no$|^plantno$|^plnt no$|^plant number$")]]))) else rep(NA_integer_, n)
		repn <- if (!is.na(col("^rep$"))) as.integer(round(num_clean(dat[[col("^rep$")]]))) else rep(NA_integer_, n)
		dens <- if (!is.na(col("density"))) num_clean(dat[[col("density")]]) else rep(NA_real_, n)
		# plants/m2 -> plants/ha
		spacing <- if (!is.na(col("spacing"))) num_clean(dat[[col("spacing")]]) else rep(NA_real_, n)
		soil <- if (!is.na(col("soil type"))) carobiner::fix_name(tolower(trimws(as.character(dat[[col("soil type")]]))), "title") else rep(NA_character_, n)
		lat <- if (!is.na(col("^latitude$"))) parse_ll(dat[[col("^latitude$")]]) else rep(NA_real_, n)
		lon <- if (!is.na(col("^longitude$"))) parse_ll(dat[[col("^longitude$")]]) else rep(NA_real_, n)
		lat[!is.na(lat) & (lat < -90 | lat > 90)] <- NA_real_
		lon[!is.na(lon) & (lon < -180 | lon > 180)] <- NA_real_
		sowing <- if (!is.na(col("sowing"))) parse_dates(dat[[col("sowing")]], sy) else rep(NA_character_, n)
		emerg <- if (!is.na(col("emergence"))) parse_dates(dat[[col("emergence")]], sy) else rep(NA_character_, n)
		flow <- if (!is.na(col("flowering"))) parse_dates(dat[[col("flowering")]], sy) else rep(NA_character_, n)
		matur <- if (!is.na(col("maturity"))) parse_dates(dat[[col("maturity")]], sy) else rep(NA_character_, n)
	
		N <- P <- K <- rep(0, n)
		ftypes <- vector("list", n)
		fdates <- vector("list", n)
		famt <- rep(0, n)
		map_assumed <- FALSE
		grade_used <- FALSE
		unknown_prod <- character(0)
		for (k in 1:4) {
			jt <- col_hit(nm, paste0("(fertilizer|fert)", k, " type"))
			jd <- col_hit(nm, paste0("(fertilizer|fert)", k, " date"))
			jg <- col_hit(nm, paste0("(n p k|npk) ?", k))
			# 2015-16 labels the N-P-K grade as a second "fertilizerN amount" column.
			amt_idx <- grep(paste0("(fertilizer|fert)", k, " (amount|amt)"), nm)
			ja <- NA_integer_
			for (j in amt_idx) {
				vals <- trimws(as.character(dat[[j]]))
				vals <- vals[!is.na(vals) & nzchar(vals)]
				if (!length(vals)) next
				if (mean(grepl("^[0-9.]+-[0-9.]+-[0-9.]+$", vals)) > 0.5) {
					if (is.na(jg)) jg <- j
				} else if (is.na(ja)) {
					ja <- j
				}
			}
			if (is.na(jt) && is.na(ja)) next
			type <- if (!is.na(jt)) as.character(dat[[jt]]) else rep(NA_character_, n)
			amt <- if (!is.na(ja)) num_clean(dat[[ja]]) else rep(NA_real_, n)
			grade <- if (!is.na(jg)) as.character(dat[[jg]]) else rep(NA_character_, n)
			dt <- if (!is.na(jd)) parse_dates(dat[[jd]], sy) else rep(NA_character_, n)
			for (i in seq_len(n)) {
				prod <- product_name(type[i])
				if (is.na(prod)) {
					if (!is.na(type[i]) && nzchar(type[i]) && !tolower(type[i]) %in% c("na", "no", "none")) {
						unknown_prod <- c(unknown_prod, type[i])
					}
					next
				}
				fr <- frac_of(prod, grade[i])
				if (is.na(fr["N"])) next
				if (!is.null(parse_grade(grade[i]))) grade_used <- TRUE
				if (prod == "MAP" && is.null(parse_grade(grade[i]))) map_assumed <- TRUE
				if (!is.na(amt[i])) {
					N[i] <- N[i] + amt[i] * fr["N"]
					P[i] <- P[i] + amt[i] * fr["P"]
					K[i] <- K[i] + amt[i] * fr["K"]
					famt[i] <- famt[i] + amt[i]
				}
				ftypes[[i]] <- c(ftypes[[i]], prod)
				if (!is.na(dt[i])) fdates[[i]] <- c(fdates[[i]], dt[i])
			}
		}
		fert_type <- vapply(ftypes, paste_ok, "")
		fert_date <- vapply(fdates, paste_ok, "")
	
		ir_dates <- vector("list", n)
		ir_mm <- rep(NA_real_, n)
		ir_n <- rep(0L, n)
		ir_method <- vector("list", n)
		wordy <- FALSE
		for (k in 1:16) {
			jd <- col_hit(nm, paste0("(irrigation|irigation|irri)", k, " date"))
			ja <- col_hit(nm, paste0("(irrigation|irigation|irri)", k, " (amount|amt)"))
			if (is.na(jd) && is.na(ja)) next
			dt <- if (!is.na(jd)) parse_dates(dat[[jd]], sy) else rep(NA_character_, n)
			raw <- if (!is.na(ja)) trimws(as.character(dat[[ja]])) else rep(NA_character_, n)
			low <- tolower(raw)
			mm <- num_clean(raw)
			for (i in seq_len(n)) {
				is_word <- !is.na(low[i]) && low[i] %in% c("flood", "flooded", "flood irrigation", "full", "sprinkler")
				is_none <- !is.na(low[i]) && low[i] %in% c("no irrigation", "no", "na", "none", "")
				has <- (!is.na(dt[i])) || is_word || (!is.na(mm[i]))
				if (is_none || !has) next
				ir_n[i] <- ir_n[i] + 1L
				if (!is.na(dt[i])) ir_dates[[i]] <- c(ir_dates[[i]], dt[i])
				if (!is.na(mm[i])) {
					ir_mm[i] <- ifelse(is.na(ir_mm[i]), mm[i], ir_mm[i] + mm[i])
				}
				if (is_word) {
					wordy <- TRUE
					meth <- if (grepl("sprinkler", low[i])) "sprinkler" else "flood"
					ir_method[[i]] <- c(ir_method[[i]], meth)
				}
			}
		}
		d <- data.frame(
			plot_id = plot,
			plant_number_ = plant,
			rep = repn,
			variety = variety,
			treatment = treatment,
			planting_date = sowing,
			emergence_date = emerg,
			flowering_date = flow,
			maturity_date = matur,
			harvest_date = matur,
			plant_density = dens * 10000,
			row_spacing = spacing,
			soil_type = soil,
			N_fertilizer = N,
			P_fertilizer = P,
			K_fertilizer = K,
			fertilizer_amount = famt,
			fertilizer_type = fert_type,
			fertilizer_date = fert_date,
			fertilizer_used = famt > 0,
			irrigation_number = ir_n,
			irrigated = ir_n > 0,
			country = country,
			location = loc,
			site = field,
			latitude = lat,
			longitude = lon
		)
		d$irrigation_dates <- vapply(ir_dates, paste_ok, "")
		# irrigation_method does not allow multiple values. Leave mixed plots blank.
		d$irrigation_method <- vapply(ir_method, function(x) {
			x <- unique(x[!is.na(x) & nzchar(x)])
			if (length(x) == 1) x else NA_character_
		}, "")
		d$irrigation_amount <- ir_mm
		tt <- tolower(d$treatment)
		d$stress <- ifelse(grepl("(^| )ws( |$)|water stress", tt), "drought", NA_character_)
		d$flowering_days <- as.numeric(as.Date(d$flowering_date) - as.Date(d$planting_date))
		d$emergence_days <- as.numeric(as.Date(d$emergence_date) - as.Date(d$planting_date))
		d$maturity_days <- as.numeric(as.Date(d$maturity_date) - as.Date(d$planting_date))
		# Drop legend rows that leaked through.
		ok <- !is.na(d$plot_id) & !grepl("[A-Za-z]", d$plot_id)
		d <- d[ok, ]
		list(d = d, map_assumed = map_assumed, grade_used = grade_used,
			unknown_prod = unique(unknown_prod), wordy_irri = wordy, start_year = sy)
	}
	
	growth_header <- function(g) {
		n <- min(12, nrow(g))
		for (i in seq_len(n)) {
			vals <- norm_txt(unlist(g[i, , drop = TRUE]))
			has_meas <- any(grepl("grain wt|stover|tc grain|tc stover", vals))
			has_id <- any(grepl("^plot|plot id|cultivar|location", vals))
			if (has_meas && has_id) return(i)
		}
		NA_integer_
	}
	
	read_growth <- function(f, sheet = 1) {
		g <- read_grid(f, sheet)
		hdr <- growth_header(g)
		if (is.na(hdr)) stop("No growth header in ", basename(f))
		name <- as.character(unlist(g[hdr, , drop = TRUE]))
		name[is.na(name)] <- ""
		unit <- rep("", length(name))
		if (hdr < nrow(g)) {
			nxt <- as.character(unlist(g[hdr + 1, , drop = TRUE]))
			nxt[is.na(nxt)] <- ""
			if (any(grepl("kg/ha|g/plant|g/Plant|plant no|m2/m2", nxt, ignore.case = TRUE))) {
				unit <- nxt
				empty <- !nzchar(name)
				name[empty] <- nxt[empty]
				unit[empty] <- ""
				data_row <- hdr + 2
			} else {
				data_row <- hdr + 1
			}
		} else {
			data_row <- hdr + 1
		}
		part <- rep("", length(name))
		part_row <- NA_integer_
		if (hdr > 1) {
			for (k in (hdr - 1):max(1, hdr - 4)) {
				vals <- as.character(unlist(g[k, , drop = TRUE]))
				if (any(grepl("total crop", vals, ignore.case = TRUE))) {
					part <- forward_fill(vals)
					part_row <- k
					break
				}
			}
		}
		# A row between the plant-part labels and the variable names carries
		# "50 grain", "grain", "HI". Paste it onto generic names such as "mass".
		if (!is.na(part_row) && part_row < hdr - 1) {
			prefix <- rep("", length(name))
			for (k in (part_row + 1):(hdr - 1)) {
				pref <- as.character(unlist(g[k, , drop = TRUE]))
				pref[is.na(pref)] <- ""
				pref[grepl("total crop|main plant|main stem|tiller [0-9]|all tiller", pref, ignore.case = TRUE)] <- ""
				hit <- nzchar(trimws(pref))
				prefix[hit] <- trimws(paste(prefix[hit], trimws(pref[hit])))
			}
			use <- nzchar(prefix)
			name[use] <- trimws(paste(prefix[use], name[use]))
		}
		nn <- norm_txt(name)
		un <- norm_txt(unit)
		pn <- norm_txt(part)
		total <- grepl("^tc ", nn) | pn == "total crop"
		# Identity columns sit left of the total-crop block.
		dat <- g[data_row:nrow(g), , drop = FALSE]
		keep <- apply(dat, 1, function(r) any(!is.na(r) & as.character(r) != ""))
		dat <- dat[keep, , drop = FALSE]
		jp <- col_hit(nn, "^plot")
		if (!is.na(jp)) {
			pv <- trimws(as.character(dat[[jp]]))
			dat <- dat[grepl("^[0-9]+(\\.[0-9]+)?$", pv), , drop = FALSE]
		}
	
		take <- function(pred) {
			hit <- which(pred)
			if (length(hit) == 0) return(rep(NA_real_, nrow(dat)))
			num_clean(dat[[hit[1]]])
		}
		take_chr <- function(idx) {
			if (is.na(idx)) return(rep(NA_character_, nrow(dat)))
			trimws(as.character(dat[[idx]]))
		}
		kg <- function(meas_pat) {
			pred <- total & grepl(meas_pat, nn) & grepl("kg", un)
			if (!any(pred)) {
				# Modern names carry the unit: tc-grain-wt-ha
				pred <- total & grepl(meas_pat, nn) & grepl(" ha$", nn)
			}
			take(pred)
		}
		plot <- take_chr(col_hit(nn, "^plot"))
		ip <- col_hit(nn, "^plant$|plant no|plant number")
		if (is.na(ip)) {
			plant <- rep(NA_integer_, nrow(dat))
		} else {
			plant <- as.integer(round(num_clean(dat[[ip]])))
		}
		variety <- take_chr(col_hit(nn, "cultivar|genotype"))
		trt_i <- col_hit(nn, "treatment combination|^treatment$")
		treatment <- take_chr(trt_i)
		das <- take(nn == "das")
		samp_i <- col_hit(nn, "sampling|sample collection")
		sampling <- take_chr(samp_i)
		grain <- kg("grain wt|grain wt ha")
		if (all(is.na(grain))) grain <- kg("grain")
		stover <- kg("stover")
		tot <- kg("tot biom|total biom")
		leaf <- kg("leaf biom")
		stem <- kg("stem biom")
		pan <- kg("pan wt|panicle")
		lai <- take(total & grepl("lai", nn))
		hi <- take(total & (nn == "hi" | grepl("^hi ", nn) | grepl("harvest index", nn)))
		if (all(is.na(hi))) hi <- take(nn == "hi" | grepl("harvest index", nn))
		g50 <- take(total & grepl("50 grain|50grain", nn))
		g100 <- take(total & grepl("100 grain|100grain", nn))
		gno <- take(total & grepl("grain no|grain number", nn) & !grepl("50|100", nn))
		sla <- take(total & grepl("^sla| sla", nn))
		till <- take(total & grepl("till", nn) & grepl("tot|total|no", nn))
		dens <- take(nn == "tc density" | nn == "density")
	
		out <- data.frame(
			plot_id = id_chr(plot),
			plant_number_ = plant,
			variety = variety,
			treatment = treatment,
			DAP = as.integer(round(das)),
			growth_stage = sampling,
			yield = grain,
			dmy_residue = stover,
			dmy_total = tot,
			dmy_leaves = leaf,
			dmy_stems = stem,
			panicle_yield_ = pan,
			LAI = lai,
			harvest_index = hi,
			grain50 = g50,
			grain100 = g100,
			grain_number_ = gno,
			sla_ = sla,
			tiller_number_ = till,
			density_m2 = dens
		)
		out <- out[!is.na(out$plot_id) & !grepl("[A-Za-z]", out$plot_id), ]
		# HI is stored as a fraction. terminag harvest_index is a percent.
		med <- median(out$harvest_index, na.rm = TRUE)
		if (is.finite(med) && med <= 1.5) out$harvest_index <- out$harvest_index * 100
		out$seed_weight <- ifelse(!is.na(out$grain50), out$grain50 * 20, out$grain100 * 10)
		out
	}
	
	read_reps <- function(f) {
		if (is.na(f) || !nzchar(f) || !file.exists(f)) return(NULL)
		p <- read_grid(f, 1)
		hdr <- NA_integer_
		for (i in seq_len(min(8, nrow(p)))) {
			vals <- norm_txt(unlist(p[i, , drop = TRUE]))
			if (any(vals == "plot" | grepl("^plot ", vals)) && any(vals == "rep" | grepl("^rep", vals))) {
				hdr <- i
				break
			}
		}
		if (is.na(hdr)) return(NULL)
		labs <- as.character(unlist(p[hdr, , drop = TRUE]))
		labs[is.na(labs)] <- ""
		nm <- norm_txt(labs)
		# First plot and first rep (sheets repeat the pair).
		jp <- which(nm == "plot" | nm == "plot id")[1]
		jr <- which(nm == "rep" | nm == "rep no")[1]
		jb <- which(nm == "block")[1]
		jw <- which(nm == "water regime")[1]
		dat <- p[(hdr + 1):nrow(p), , drop = FALSE]
		pv <- trimws(as.character(dat[[jp]]))
		dat <- dat[grepl("^[0-9]+(\\.[0-9]+)?$", pv), , drop = FALSE]
		out <- data.frame(
			plot_id = id_chr(dat[[jp]]),
			rep = as.integer(round(num_clean(dat[[jr]])))
		)
		if (!is.na(jb)) out$block_id <- trimws(as.character(dat[[jb]]))
		if (!is.na(jw)) out$water_regime <- trimws(as.character(dat[[jw]]))
		out <- out[!is.na(out$plot_id), ]
		out <- out[!duplicated(out$plot_id), ]
		out
	}
	
	read_weather <- function(f, start_year) {
		if (is.na(f) || !nzchar(f) || !file.exists(f)) return(NULL)
		w <- carobiner::read.excel(f)
		nm <- norm_txt(names(w))
		if (!any(grepl("rain|prec", nm))) return(NULL)
		col <- function(pat) {
			hit <- grep(pat, nm)
			if (length(hit) == 0) return(rep(NA, nrow(w)))
			w[[hit[1]]]
		}
		rad <- col("^radn$|solar|^radiation")
		srad <- num_clean(rad)
		# Description files for 2017-20 state radiation as MJ/m2. Older sheets use the same magnitude.
		if (!all(is.na(srad)) && median(srad, na.rm = TRUE) < 100) srad <- srad * 1000
		data.frame(
			station_name = as.character(col("metstation|station")),
			date = parse_dates(col("^date"), start_year),
			prec = num_clean(col("^rain|prec")),
			tmax = num_clean(col("^maxt$|max temp|max t")),
			tmin = num_clean(col("^mint$|min temp|min t")),
			rhmx = num_clean(col("maxrh|max rh|humidity1|rel humidity1")),
			rhmn = num_clean(col("minrh|min rh|humidity2|rel humidity2")),
			srad = srad
		)
	}
	
	rabi_records <- function(exp_file, gro_file, plot_file = NA, wth_file = NA, exp_sheet = 1, gro_sheet = 1) {
		ex <- build_experiment(exp_file, exp_sheet)
		d <- ex$d
		g <- read_growth(gro_file, gro_sheet)
		reps <- read_reps(plot_file)
		if (!is.null(reps)) {
			if (all(is.na(d$rep))) {
				d$rep <- NULL
				d <- merge(d, reps[, intersect(names(reps), c("plot_id", "rep", "block_id")), drop = FALSE], by = "plot_id", all.x = TRUE)
			} else if ("block_id" %in% names(reps)) {
				d <- merge(d, reps[, c("plot_id", "block_id")], by = "plot_id", all.x = TRUE)
			}
			if ("water_regime" %in% names(reps)) {
				wr <- reps[, c("plot_id", "water_regime")]
				d <- merge(d, wr, by = "plot_id", all.x = TRUE)
				ws <- tolower(d$water_regime) %in% c("stress", "ws", "water stress")
				d$stress[ws] <- "drought"
				d$water_regime <- NULL
			}
		}
		gs <- tolower(g$growth_stage)
		is_h <- !is.na(gs) & grepl("harvest|maturity|final", gs) & !grepl("nutrition", gs)
		# If the sheet never names a harvest, use the last sample that has grain weight.
		if (!any(is_h)) {
			okg <- !is.na(g$yield)
			key <- paste(g$plot_id, g$plant_number_)
			dap <- ifelse(is.na(g$DAP), -1L, g$DAP)
			dap[!okg] <- -1L
			is_h <- ave(dap, key, FUN = function(z) z == max(z) & z >= 0) == 1
		}
		h <- g[is_h, ]
		h <- h[order(h$plot_id, h$plant_number_, h$DAP), ]
		h <- h[!duplicated(paste(h$plot_id, h$plant_number_), fromLast = TRUE), ]
		meas <- c("yield", "dmy_residue", "dmy_total", "dmy_leaves", "dmy_stems", "panicle_yield_",
			"LAI", "harvest_index", "seed_weight", "grain_number_", "sla_", "tiller_number_")
		meas <- meas[meas %in% names(h)]
		d <- merge(d, h[, c("plot_id", "plant_number_", meas)], by = c("plot_id", "plant_number_"), all.x = TRUE)
	
		# LAI at final harvest is often empty; do not keep an all-NA column on the wide table.
		for (v in meas) {
			if (v %in% names(d) && all(is.na(d[[v]]))) d[[v]] <- NULL
		}
		# "Full" / "Flood" are not millimetres. A lone numeric beside those words is not the seasonal total.
		if (isTRUE(ex$wordy_irri) || ("irrigation_amount" %in% names(d) && all(is.na(d$irrigation_amount)))) {
			d$irrigation_amount <- NULL
		}
		if ("irrigation_method" %in% names(d) && all(is.na(d$irrigation_method) | d$irrigation_method == "")) d$irrigation_method <- NULL
		if ("irrigation_dates" %in% names(d) && all(is.na(d$irrigation_dates) | d$irrigation_dates == "")) d$irrigation_dates <- NULL
		if ("fertilizer_date" %in% names(d) && all(is.na(d$fertilizer_date) | d$fertilizer_date == "")) d$fertilizer_date <- NULL
		if ("block_id" %in% names(d) && all(is.na(d$block_id) | d$block_id == "")) d$block_id <- NULL
		if ("stress" %in% names(d) && all(is.na(d$stress))) d$stress <- NULL
		if (all(is.na(d$latitude)) || all(is.na(d$longitude))) {
			d$latitude <- NULL
			d$longitude <- NULL
			geo <- FALSE
		} else {
			geo <- TRUE
		}
		d$crop <- "sorghum"
		d$yield_part <- "grain"
		d$on_farm <- FALSE
		d$is_survey <- FALSE
		d$trial_id <- "1"
		d$geo_from_source <- geo
		d$yield_isfresh <- FALSE
		d$adm1 <- "Telangana"
		d$adm2 <- "Sangareddy"
		# Maturity date is the sheet's maturity column. Descriptions say plants were harvested at maturity.
		if (!"harvest_date" %in% names(d) && "maturity_date" %in% names(d)) d$harvest_date <- d$maturity_date
	
		g$trial_id <- "1"
		g$crop <- "sorghum"
		g$yield_part <- "grain"
		# Keep the time series even when grain is still absent.
		long_vars <- c("trial_id", "plot_id", "plant_number_", "variety", "treatment", "DAP", "growth_stage",
			"yield", "dmy_residue", "dmy_total", "dmy_leaves", "dmy_stems", "panicle_yield_", "LAI",
			"harvest_index", "seed_weight", "grain_number_", "sla_", "tiller_number_")
		long <- g[, intersect(long_vars, names(g))]
		empty_meas <- c("yield", "dmy_residue", "dmy_total", "dmy_leaves", "dmy_stems", "LAI")
		empty_meas <- intersect(empty_meas, names(long))
		if (length(empty_meas)) {
			has <- apply(long[, empty_meas, drop = FALSE], 1, function(r) any(!is.na(r)))
			long <- long[has, ]
		}
	
		wth <- read_weather(wth_file, ex$start_year)
		if (!is.null(wth)) {
			wth <- wth[!is.na(wth$date), ]
			if (nrow(wth) == 0) wth <- NULL
		}
		if (!is.null(wth)) {
			wth$country <- "India"
			wth$location <- "Patancheru"
			wth$adm1 <- "Telangana"
			wth$adm2 <- "Sangareddy"
			wth$geo_from_source <- geo
			if (geo) {
				wth$latitude <- d$latitude[1]
				wth$longitude <- d$longitude[1]
			}
			if ("site" %in% names(d)) wth$site <- d$site[1]
		}
		list(d = d, long = long, wth = wth, info = ex)
	}

	uri <- "doi:10.21421/D2/S4FIRW"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	pick <- function(name) {
		hit <- ff[basename(ff) == name]
		if (length(hit) != 1) stop("Expected one file named ", name)
		hit
	}
	fexp <- pick("Experiment.xls")
	fgro <- pick("Growth&development.xls")
	fplot <- pick("Plot details.xlsx")
	fwth <- pick("Weather.xlsx")
	rec <- rabi_records(fexp, fgro, fplot, fwth)
	d <- rec$d
	long <- rec$long
	wth <- rec$wth
	if ("site" %in% names(d) && all(is.na(d$site))) d$site <- NULL
	if ("rep" %in% names(d) && all(is.na(d$rep))) d$rep <- NULL
	if (!is.null(wth) && "site" %in% names(wth) && all(is.na(wth$site))) wth$site <- NULL
	id_cols <- c("trial_id", "plot_id", "plant_number_", "variety", "treatment", "DAP", "growth_stage", "crop", "yield_part")
	for (v in setdiff(names(long), id_cols)) {
		if (all(is.na(long[[v]]))) long[[v]] <- NULL
	}
	tv <- c("variety", "treatment")
	if (length(unique(d$plant_density[!is.na(d$plant_density)])) > 1) tv <- c(tv, "plant_density")
	if (length(unique(round(d$N_fertilizer[!is.na(d$N_fertilizer)], 1))) > 1) tv <- c(tv, "N_fertilizer")
	resp <- intersect(c("yield", "dmy_total", "dmy_leaves", "dmy_stems", "dmy_residue", "seed_weight", "harvest_index"), names(d))

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = paste(tv, collapse = ";"),
		response_vars = paste(resp, collapse = ";"),
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	carobiner::write_files(path, meta, d, long = long, wth = wth)
}
