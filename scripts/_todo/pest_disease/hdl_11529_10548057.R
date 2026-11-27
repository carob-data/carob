# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Several workbooks contain copy-paste or template errors:
# - Aman rice 2015-herbicide trial-DSR-Dhanusha.xlsx lists "Country: Bangladesh" in sheet 2 even though the station is NRRP-Hardinath (Dhanusha, Nepal); corrected to Nepal.
# - Aman rice 2016-herbicides trial-SRFSI-WRC-Rajshahi.xlsx has WRC-Dinajpur coordinates in sheet 2, Year = 2015 on the harvest sheet, and identical plant height, panicle density, grains/panicle, and weed biomass values to the 2016 WRC-Dinajpur workbook (grain yield, straw, and TGW differ); verify with the data provider.
# - Aman rice 2016-herbicide trial-SRFSI-BAU-Bihar.xlsx has identical plant height and TGW columns to the 2015 BAU-Bihar workbook, while panicle density and grain yield differ.
# - Aman rice 2016-herbicide trial-SRFSI-RARS-Nepal.xlsx row 1 (plot 1) has weighing moisture entered as 154 instead of 15.4, which caused the Excel formula to calculate a depressed grain yield (1.133 t/ha instead of ~2.50 t/ha); original spreadsheet yield is kept here for reviewer decision.
# - Fertilizer sheets are blank or incomplete for RARS-Sunsari 2015, WRC-Dinajpur 2015/2016, WRC-Rajshahi 2016, and UBKV 2015/2016 (no N reported); RWRC-Rajshahi 2015 g/plot amounts appear to apply to a 90 m2 main plot rather than 15 m2 subplots and are left NA.
# - In UBKV 2016 and Rangpur 2016 (TGW only), Excel formulas returned 0 where raw measurement cells were empty; those formula zeros are set to NA.
# - Excel autofill date drags (+1 day per row across all 45-60 rows) occur in planting_date for RARS-Nepal 2016 (starting 2016-07-27) and harvest_date for Dhanusha 2015 (starting 2015-11-06) and Dinajpur 2016 (starting 2016-10-16); corrected to the first date of each series.

## NEW
# dry_weed_biomass_: oven-dry weed biomass at harvest (kg/ha).
# herbicide_product: "ethoxysulfuron" is not yet in values_herbicides.csv.

## NOTES
# On-station dry direct-seeded aman rice herbicide trials under SRFSI (2015 and 2016) across seven research stations in Bangladesh, India, and Nepal (12 workbooks, 572 plots).
# Grain yield is reported at 14% moisture in most workbooks and at 12% moisture in WRC-Dinajpur 2016 and WRC-Rajshahi 2016 (converted from t/ha to kg/ha).
# Shifted treatment/plot columns in RWRC-Rajshahi 2015, WRC-Rajshahi 2016, and WRC-Dinajpur 2015/2016 are aligned by content rather than column position so no trials are dropped.
# Coordinates come from sheet 2 (geo_from_source = TRUE) for OFRD-Rangpur, RARS-Tarahara, and WRC-Dinajpur, and are georeferenced from station names (geo_from_source = FALSE) for NRRP-Hardinath, RWRC-Rajshahi, BAU-Purnea, and UBKV Cooch Behar.

## NOT PROCESSED
# Sheets 1 (Daily rainfall: mostly empty templates), 3 (Land Prep Operations), 5 (Crop Mgmt Operations), 8 (Water ponding), 9 (Pesticide applications), 10 (Weeds, pests and diseases), 11 (Harvest operations), 12-14 (intermediate weed counts/biomass at 15/30/60 DAS with inconsistent layouts across workbooks), 15 (Visual scores), 17 (Soil analysis: empty across all workbooks).
# Individual herbicide doses: only pendimethalin (1000 g a.i./ha) is stated consistently across the 11 main workbooks.


carob_script <- function(path) {

"
8.1-Direct seeded rice weed management-on-station research trials-SRFSI Project-ACIAR-CIMMYT

Dry direct-seeded rice herbicide trials on research stations, comparing pre-emergence and post-emergence weed-control options.
"

	uri <- "hdl:11529/10548057"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=5,
		data_organization = "CIMMYT;BARI;NARC;UBKV;RDRS",
		publication = NA,
		project = "SRFSI",
		design = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_product;weeding_done",
		response_vars = "yield;dmy_residue;plant_height;seed_weight;dry_weed_biomass_",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 70,
		carob_effort = 0
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x <- sub("cm$", "", x, ignore.case=TRUE)
		x <- trimws(x)
		x[x %in% c("", "-", "NA", "N/A", "na", "#DIV/0!", "#VALUE!", "#REF!")] <- NA_character_
		as.numeric(x)
	}

	parse_dt <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA", "N/A")] <- NA_character_
		x <- gsub("--", "-", x, fixed=TRUE)
		out <- rep(NA_character_, length(x))
		is_ser <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
		if (any(is_ser)) {
			out[is_ser] <- as.character(as.Date(as.numeric(x[is_ser]), origin="1899-12-30"))
		}
		is_dmy <- !is.na(x) & grepl("^[0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{2,4}$", x)
		if (any(is_dmy)) {
			s <- gsub("/", "-", x[is_dmy])
			d4 <- as.Date(s, format="%d-%m-%Y")
			d2 <- as.Date(s, format="%d-%m-%y")
			dd <- ifelse(nchar(sub("^.*-", "", s)) == 4, as.character(d4), as.character(d2))
			out[is_dmy] <- dd
		}
		out
	}

	pre_ai <- function(x) {
		x <- trimws(as.character(x))
		out <- rep(NA_character_, length(x))
		out[grepl("No pre|No-spray|^-$", x, ignore.case=TRUE)] <- "none"
		out[grepl("Pend", x, ignore.case=TRUE)] <- "pendimethalin"
		out[grepl("Oxadiazon", x, ignore.case=TRUE)] <- "oxadiazon"
		out[grepl("Pretilachlor", x, ignore.case=TRUE)] <- "pretilachlor"
		out
	}

	post_ai <- function(x) {
		x <- trimws(as.character(x))
		out <- rep(NA_character_, length(x))
		out[grepl("No post|No-spray|^-$", x, ignore.case=TRUE)] <- "none"
		out[grepl("Pyrazosulfuron", x, ignore.case=TRUE)] <- "bispyribac-sodium;pyrazosulfuron"
		out[grepl("Ethoxysulfuron", x, ignore.case=TRUE)] <- "fenoxaprop-p-ethyl;ethoxysulfuron"
		out[grepl("Halosulfuron", x, ignore.case=TRUE)] <- "fenoxaprop-p-ethyl;halosulfuron-methyl"
		out[grepl("Bispyribac", x, ignore.case=TRUE) & is.na(out)] <- "bispyribac-sodium"
		out
	}

	join_ai <- function(a, b) {
		out <- paste(a, b, sep=";")
		out <- gsub("(^none;|;none$)", "", out)
		out[a == "none" & b == "none"] <- "none"
		out[is.na(a) | is.na(b)] <- NA_character_
		out
	}

	fix_texture <- function(x) {
		if (is.na(x) || !nzchar(x)) return(NA_character_)
		x <- tolower(trimws(x))
		if (grepl("sandy loam", x)) return("sandy loam")
		if (grepl("clay loam", x)) return("clay loam")
		x
	}

	# Station metadata table: verified from sheet 2 GPS coordinates (geo_from_source = TRUE)
	# and station locations (geo_from_source = FALSE)
	stn_meta <- data.frame(
		file = c(
			"Aman rice  2016-herbicide trial-SRFSI-OFRD-Rangpur.xlsx",
			"Aman rice 2015-herbicide trial-DSR-Dhanusha.xlsx",
			"Aman rice 2015-herbicide trial-RARS-Sunsari.xlsx",
			"Aman rice 2015-herbicide trial-RWRC-Rajshahi.xlsx",
			"Aman rice 2015-herbicide trial-SRFSI-BAU-Bihar.xlsx",
			"Aman rice 2015-herbicide trial-UBKV-C'behar.xlsx",
			"Aman rice 2015-herbicide trial-WRC-Dinajpur.xlsx",
			"Aman rice 2016-herbicide trial-SRFSI-BAU-Bihar.xlsx",
			"Aman rice 2016-herbicide trial-SRFSI-RARS-Nepal.xlsx",
			"Aman rice 2016-Herbicide trial-SRFSI-UVKV-C'Behar.xlsx",
			"Aman rice 2016-herbicide trial-SRFSI-WRC-Dinajpur.xlsx",
			"Aman rice 2016-herbicides trial-SRFSI-WRC-Rajshahi.xlsx"
		),
		year = c(2016, 2015, 2015, 2015, 2015, 2015, 2015, 2016, 2016, 2016, 2016, 2016),
		country = c(
			"Bangladesh", "Nepal", "Nepal", "Bangladesh", "India", "India",
			"Bangladesh", "India", "Nepal", "India", "Bangladesh", "Bangladesh"
		),
		adm1 = c(
			"Rangpur", "Madhesh", "Koshi", "Rajshahi", "Bihar", "West Bengal",
			"Rangpur", "Bihar", "Koshi", "West Bengal", "Rangpur", "Rajshahi"
		),
		adm2 = c(
			"Rangpur", "Dhanusha", "Sunsari", "Rajshahi", "Purnea", "Cooch Behar",
			"Dinajpur", "Purnea", "Sunsari", "Cooch Behar", "Dinajpur", "Rajshahi"
		),
		location = c(
			"OFRD Rangpur", "NRRP Hardinath", "RARS Tarahara", "RWRC Rajshahi",
			"BAU Purnea", "UBKV Pundibari", "WRC Dinajpur", "BAU Purnea",
			"RARS Tarahara", "UBKV Pundibari", "WRC Dinajpur", "RWRC Rajshahi"
		),
		# GPS from sheet 2: Rangpur (25.72048, 89.26385), RARS Tarahara (26°42.367'N, 87°16.754'E),
		# WRC Dinajpur (25°44.710'N, 88°40.500'E); others georeferenced from research station names
		latitude = c(
			25.72048, 26.796, 26.70612, 24.367, 25.777, 26.404,
			25.74517, 25.777, 26.70612, 26.404, 25.74517, 24.367
		),
		longitude = c(
			89.26385, 85.970, 87.27923, 88.659, 87.475, 89.386,
			88.67500, 87.475, 87.27923, 89.386, 88.67500, 88.659
		),
		geo_from_source = c(
			TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
			TRUE, FALSE, TRUE, FALSE, TRUE, FALSE
		),
		geo_uncertainty = c(
			NA_real_, 2000, NA_real_, 2000, 3000, 2000,
			NA_real_, 3000, NA_real_, 2000, NA_real_, 2000
		),
		geo_source = c(
			NA_character_, "NRRP Hardinath, Dhanusha", NA_character_, "BARI RWRC Shyampur, Rajshahi",
			"BAU Purnea research station", "UBKV Pundibari, Cooch Behar",
			NA_character_, "BAU Purnea research station", NA_character_, "UBKV Pundibari, Cooch Behar",
			NA_character_, "BARI RWRC Shyampur, Rajshahi"
		),
		# Yield moisture standardization basis in Excel formulas
		ym_basis = c(14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 12, 12),
		# Reliable per-plot fertilizer calculation from sheet 6 (g/plot / plot_size * 10)
		fert_ok = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
	)

	get_fert <- function(f, plot_area_val) {
		shs <- readxl::excel_sheets(f)
		s6 <- grep("Fertilizer", shs, ignore.case=TRUE, value=TRUE)[1]
		if (is.na(s6) || is.na(plot_area_val) || plot_area_val <= 0) {
			return(list(N=NA_real_, P=NA_real_, K=NA_real_, ftype=NA_character_))
		}
		ft <- carobiner::read.excel(f, sheet=s6, col_names=FALSE, col_types="text")
		hr <- which(trimws(as.character(ft[[1]])) == "Year")[1]
		if (is.na(hr)) return(list(N=NA_real_, P=NA_real_, K=NA_real_, ftype=NA_character_))
		h1 <- ifelse(is.na(ft[hr, ]), "", trimws(as.character(unlist(ft[hr, ]))))
		dat <- ft[(hr + 2):nrow(ft), ]
		dat <- dat[grepl("^201[56]$", trimws(as.character(dat[[1]]))), ]
		if (!nrow(dat)) return(list(N=NA_real_, P=NA_real_, K=NA_real_, ftype=NA_character_))
		pcols <- grep("Product used", h1, ignore.case=TRUE)
		# Use first complete row (uniform blanket application across all plots)
		r1 <- dat[1, ]
		N_tot <- 0; P_tot <- 0; K_tot <- 0
		ftypes <- character()
		for (j in pcols) {
			pr <- trimws(as.character(r1[[j]]))
			amt <- num(r1[[j + 5]])
			if (is.na(amt) || amt <= 0) next
			kg_ha <- amt / plot_area_val * 10 # g/plot -> kg/ha
			pl <- tolower(pr)
			# Map standard nutrient fractions (note Rangpur sheet 6 mislabels TSP/MOP grades)
			if (grepl("urea", pl)) {
				N_tot <- N_tot + kg_ha * 0.46
				ftypes <- c(ftypes, "urea")
			} else if (grepl("dap", pl)) {
				N_tot <- N_tot + kg_ha * 0.18
				P_tot <- P_tot + kg_ha * 0.201
				ftypes <- c(ftypes, "DAP")
			} else if (grepl("tsp", pl)) {
				P_tot <- P_tot + kg_ha * 0.1923
				ftypes <- c(ftypes, "TSP")
			} else if (grepl("mop|muriate", pl)) {
				K_tot <- K_tot + kg_ha * 0.498
				ftypes <- c(ftypes, "KCl")
			} else if (grepl("gypsum", pl)) {
				ftypes <- c(ftypes, "gypsum")
			} else if (grepl("zn|zinc", pl)) {
				ftypes <- c(ftypes, "ZnSO4")
			} else if (is.na(pr) || !nzchar(pr)) {
				# RARS-Nepal 2016 3rd urea top-dress leaves Product cell blank
				if (j > pcols[1] && any(ftypes == "urea")) {
					N_tot <- N_tot + kg_ha * 0.46
				}
			}
		}
		if (!length(ftypes)) return(list(N=NA_real_, P=NA_real_, K=NA_real_, ftype=NA_character_))
		list(
			N = round(N_tot, 2),
			P = round(P_tot, 2),
			K = round(K_tot, 2),
			ftype = paste(unique(ftypes), collapse=";")
		)
	}

	one <- function(idx) {
		sm <- stn_meta[idx, ]
		f <- ff[basename(ff) == sm$file]
		shs <- readxl::excel_sheets(f)

		# Read soil texture from sheet 2
		si <- carobiner::read.excel(f, sheet="2 - Site information", col_names=FALSE, col_types="text")
		h4 <- trimws(as.character(unlist(si[4, ])))
		texc <- grep("texture", h4, ignore.case=TRUE)[1]
		tex_raw <- if (!is.na(texc)) na.omit(trimws(as.character(si[[texc]][7:nrow(si)]))) else character()
		tex_raw <- tex_raw[nzchar(tex_raw)]
		soil_tex <- if (length(tex_raw)) fix_texture(tex_raw[1]) else NA_character_

		# Read harvest sheet (all text to avoid mixed-type warnings)
		s16 <- grep("16\\.WeedBio", shs, ignore.case=TRUE, value=TRUE)[1]
		r <- carobiner::read.excel(f, sheet=s16, col_names=FALSE, col_types="text")
		hr <- which(trimws(as.character(r[[1]])) == "Year")[1]
		h1 <- ifelse(is.na(r[hr, ]), "", trimws(as.character(unlist(r[hr, ]))))
		h2 <- ifelse(is.na(r[hr + 1, ]), "", trimws(as.character(unlist(r[hr + 1, ]))))
		hdr <- trimws(paste(h1, h2))

		dat <- r[(hr + 2):nrow(r), ]
		ok <- grepl("^201[56]$", trimws(as.character(dat[[1]])))
		dat <- dat[ok, ]
		n <- nrow(dat)

		# Locate pre- and post-emergence herbicide columns by actual cell content
		# (handles 1-column left shifts in Rajshahi 2015/2016 and Dinajpur 2015/2016)
		pre_col <- which(sapply(dat[, 1:min(14, ncol(dat))], function(z) {
			any(grepl("Pend|Oxadiazon|Pretilachlor", z, ignore.case=TRUE))
		}))[1]
		post_col <- which(sapply(dat[, 1:min(14, ncol(dat))], function(z) {
			any(grepl("Bispyribac|Ethoxysulfuron|Halosulfuron", z, ignore.case=TRUE))
		}))[1]

		pre <- pre_ai(dat[[pre_col]])
		post <- post_ai(dat[[post_col]])
		prod <- join_ai(pre, post)

		# Manual weeding treatment column (only Rangpur 2016 varies MW vs No MW)
		mw_col <- grep("Manual weeding", hdr, ignore.case=TRUE)[1]
		if (!is.na(mw_col)) {
			mw_val <- trimws(as.character(dat[[mw_col]]))
			weeding_done <- ifelse(grepl("^MW$", mw_val, ignore.case=TRUE), TRUE,
				ifelse(grepl("No MW", mw_val, ignore.case=TRUE), FALSE, NA))
		} else {
			weeding_done <- rep(FALSE, n)
		}

		# Replication & plot_id
		if (sm$file == "Aman rice  2016-herbicide trial-SRFSI-OFRD-Rangpur.xlsx") {
			rep_val <- rep(1L, n)
			pid_col <- grep("^Plot Id$", hdr, ignore.case=TRUE)[1]
			pid_val <- trimws(as.character(dat[[pid_col]]))
		} else {
			# Replication column is immediately before pre_col
			rep_val <- as.integer(num(dat[[pre_col - 1]]))
			pid_col <- grep("^Plot Id$", hdr, ignore.case=TRUE)[1]
			pid_val <- if (!is.na(pid_col)) trimws(as.character(dat[[pid_col]])) else NA_character_
			# In shifted Rajshahi/Dinajpur files, plot_id is directly after post_col
			if (all(is.na(pid_val) | !nzchar(pid_val))) {
				pid_val <- trimws(as.character(dat[[post_col + 1]]))
			}
		}

		# Treatment description
		pre_raw <- trimws(as.character(dat[[pre_col]]))
		post_raw <- trimws(as.character(dat[[post_col]]))
		trt_desc <- paste(pre_raw, "+", post_raw)
		if (!is.na(mw_col)) trt_desc <- paste(trt_desc, "+", trimws(as.character(dat[[mw_col]])))

		# Plot size
		psz_col <- grep("^Plot size", hdr, ignore.case=TRUE)[1]
		psz <- if (!is.na(psz_col)) num(dat[[psz_col]]) else rep(NA_real_, n)
		if (all(is.na(psz))) {
			sub_col <- grep("Area of sub-plots", hdr, ignore.case=TRUE)[1]
			if (!is.na(sub_col)) psz <- num(dat[[sub_col]])
		}

		# Harvest summary columns (always in the last ~10 columns)
		nc <- ncol(dat)
		tail_idx <- max(1, nc - 10):nc
		find_tail <- function(pat) {
			j <- tail_idx[grep(pat, hdr[tail_idx], ignore.case=TRUE)]
			if (!length(j)) return(rep(NA_real_, n))
			num(dat[[j[1]]])
		}

		yld <- find_tail("Grain yield \\(t/ha\\)") * 1000
		dmy_res <- find_tail("Dry straw wt|Straw yield") * 1000
		hi <- find_tail("\\bHi\\b") * 100
		pht <- find_tail("Plant height|Planthight")
		tgw <- find_tail("TGW|1000-grain")
		weed_dry <- find_tail("Oven dry weed biomass") * 10 # g/m2 -> kg/ha
		g_pan <- find_tail("Grain.*panicle")
		# Panicle density only when reported per m2 (Sunsari 2015 and UBKV 2015 report no/hill)
		pan_m2 <- find_tail("Panicle density \\(no\\./m2\\)") * 10000

		# Remove Excel formula zeros where raw inputs were blank (UBKV 2016, Rangpur 2016 TGW, Dinajpur/Rajshahi 2016 empty plots)
		pht[!is.na(pht) & pht <= 0] <- NA_real_
		tgw[!is.na(tgw) & tgw <= 0] <- NA_real_
		pan_m2[!is.na(pan_m2) & pan_m2 <= 0] <- NA_real_
		g_pan[!is.na(g_pan) & g_pan <= 0] <- NA_real_
		if (all(!is.na(weed_dry) & weed_dry == 0)) weed_dry[] <- NA_real_
		if (all(!is.na(hi) & hi == 0)) hi[] <- NA_real_

		# Read phenology sheet for variety, planting/harvest dates, row spacing, seed rate, maturity days
		s4 <- grep("Phenology", shs, ignore.case=TRUE, value=TRUE)[1]
		p <- carobiner::read.excel(f, sheet=s4, col_names=FALSE, col_types="text")
		phr <- which(trimws(as.character(p[[1]])) == "Year")[1]
		ph1 <- ifelse(is.na(p[phr, ]), "", trimws(as.character(unlist(p[phr, ]))))
		ph2 <- ifelse(is.na(p[phr + 1, ]), "", trimws(as.character(unlist(p[phr + 1, ]))))
		phdr <- trimws(paste(ph1, ph2))
		pdat <- p[(phr + 2):nrow(p), ]
		pdat <- pdat[grepl("^201[56]$", trimws(as.character(pdat[[1]]))), ]

		pull_p <- function(pat) {
			j <- grep(pat, phdr, ignore.case=TRUE)
			if (!length(j) || nrow(pdat) == 0) return(rep(NA_character_, n))
			v <- trimws(as.character(pdat[[j[1]]]))
			v[v %in% c("", "-", "NA", "N/A")] <- NA_character_
			if (length(v) == n) return(v)
			# Broadcast single trial-level constant when row counts differ slightly
			uv <- unique(na.omit(v))
			if (length(uv) == 1) rep(uv, n) else rep(NA_character_, n)
		}

		var_val <- pull_p("Variety")
		var_val <- gsub("  +", " ", var_val)
		# Fill trial-level constants where only a single non-NA value exists (e.g. Rangpur 2016 MW half-plots)
		fill_const <- function(v) {
			uv <- unique(na.omit(v))
			if (length(uv) == 1) rep(uv, length(v)) else v
		}
		fix_autofill_date <- function(v) {
			if (length(v) >= 10 && all(!is.na(v))) {
				dv <- as.Date(v)
				if (all(diff(dv) == 1)) return(rep(v[1], length(v)))
			}
			v
		}
		var_val <- fill_const(var_val)
		pdate <- fix_autofill_date(fill_const(parse_dt(pull_p("Date of seeding"))))
		hdate <- fix_autofill_date(fill_const(parse_dt(pull_p("Dat[ew] of harvest"))))
		if (all(is.na(pdate))) pdate <- rep(as.character(sm$year), n)
		row_sp <- fill_const(num(pull_p("Row spacing")))
		srate <- fill_const(num(pull_p("Seedrate")))
		mat_d <- num(pull_p("maturity.*DAS|DAS.*maturity"))
		mat_d[!is.na(mat_d) & (mat_d < 45 | mat_d > 250)] <- NA_real_

		# Irrigation status from sheet 7
		s7 <- grep("Irrigation", shs, ignore.case=TRUE, value=TRUE)[1]
		ir <- carobiner::read.excel(f, sheet=s7, col_names=FALSE, col_types="text")
		ihr <- which(trimws(as.character(ir[[1]])) == "Year")[1]
		ih1 <- ifelse(is.na(ir[ihr, ]), "", trimws(as.character(unlist(ir[ihr, ]))))
		idat <- ir[(ihr + 2):nrow(ir), ]
		idat <- idat[grepl("^201[56]$", trimws(as.character(idat[[1]]))), ]
		id_cols <- grep("Date of irrigation", ih1, ignore.case=TRUE)
		has_irr <- length(id_cols) > 0 && any(sapply(id_cols, function(j) {
			any(!is.na(idat[[j]]) & nzchar(trimws(as.character(idat[[j]]))) & trimws(as.character(idat[[j]])) != "-")
		}))
		irrig <- if (has_irr) TRUE else NA

		# Fertilizer from sheet 6 (where units & plot size are verified)
		finfo <- if (sm$fert_ok) get_fert(f, stats::median(psz, na.rm=TRUE)) else list(N=NA_real_, P=NA_real_, K=NA_real_, ftype=NA_character_)

		data.frame(
			country = sm$country,
			adm1 = sm$adm1,
			adm2 = sm$adm2,
			location = sm$location,
			latitude = sm$latitude,
			longitude = sm$longitude,
			geo_from_source = sm$geo_from_source,
			geo_uncertainty = sm$geo_uncertainty,
			geo_source = sm$geo_source,
			trial_id = paste(sm$year, gsub(" ", "_", sm$location), sep="_"),
			plot_id = pid_val,
			rep = rep_val,
			season = "aman",
			crop = "rice",
			variety = var_val,
			planting_date = pdate,
			harvest_date = hdate,
			planting_method = "direct seeding",
			row_spacing = row_sp,
			seed_rate = srate,
			plot_area = psz,
			soil_texture = soil_tex,
			irrigated = irrig,
			fertilizer_used = if (!is.na(finfo$ftype)) TRUE else NA,
			fertilizer_type = finfo$ftype,
			N_fertilizer = finfo$N,
			P_fertilizer = finfo$P,
			K_fertilizer = finfo$K,
			treatment = trt_desc,
			herbicide_product = prod,
			herbicide_used = prod != "none",
			weeding_done = weeding_done,
			yield_part = "grain",
			yield = yld,
			yield_moisture = sm$ym_basis,
			yield_isfresh = TRUE,
			dmy_residue = dmy_res,
			harvest_index = hi,
			plant_height = pht,
			seed_weight = tgw,
			spike_density = pan_m2,
			grains_per_spike = g_pan,
			maturity_days = mat_d,
			dry_weed_biomass_ = weed_dry,
			stress = "weeds",
			on_farm = FALSE,
			is_survey = FALSE
		)
	}

	d <- do.call(rbind, lapply(seq_len(nrow(stn_meta)), one))
	carobiner::write_files(path, meta, d)
}
