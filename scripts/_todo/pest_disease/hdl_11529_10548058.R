# R script for "carob"
# license: GPL (>=3)

## ISSUES
# On the harvest sheet, the Season column says "Rabi (winter)" in three workbooks while sheet 2, filenames, and planting dates (July 2015) confirm these are Kharif/Aman wet-season rice trials; season is stored as "kharif".
# In Dhanusha (NRRP), fertilizer sheet 6 lists DAP and Muriate amounts but leaves Urea application amounts blank, so N_fertilizer is not computed for Dhanusha. In Rajshahi and Sunsari, fertilizer amounts are blank.
# Coordinates are absent in Dhanusha and Rajshahi workbooks and were georeferenced from the research station names (NRRP Hardinath and RWRC Rajshahi).

## NEW
# dry_weed_biomass_: oven-dry weed biomass at harvest (kg/ha).
# herbicide_product: "ethoxysulfuron" is not yet in values_herbicides.csv.

## NOTES
# Dry direct-seeded zero-tillage rice weed-management learning-module trials across four SRFSI nodes in 2015 (Rangpur at two farms, Dhanusha, Rajshahi, Sunsari; 31 plots total).
# Grain yield (t/ha, standardized to 14% moisture in Excel) and dry straw yield (t/ha) are converted to kg/ha; oven-dry weed biomass (g/m2) is converted to kg/ha (* 10).
# Herbicide active-ingredient doses (g a.i./ha) from the treatment columns are summed across pre- and post-emergence applications and converted to kg a.i./ha in herbicide_amount.
# Coordinates from sheet 2 are used for RDRS Farm Rangpur, OFRD Farm Rangpur, and RARS Tarahara Sunsari (geo_from_source = TRUE).

## NOT PROCESSED
# Sheets 1 (Daily rainfall: only Sunsari has 32 daily rows), 3 (Land Prep Operations), 5 (Crop Mgmt Operations), 8 (Water ponding), 9 (Pesticide applications), 10 (Weeds, pests and diseases), 11 (Harvest operations), 12-14 (intermediate quadrat weed counts/biomass at 15, 35, 60 DAS), 15 (Visual scores), 17 (Soil analysis: empty).


carob_script <- function(path) {

"
8.2-Zero tillage rice weed management learning module- -SRFSI Project-ACIAR-CIMMYT

Dry direct-seeded rice in no-till or tilled soil is an alternate option for transplanting rice but due to weed management constraints, farmers hesitate to adopt the direct seeded rice. To address the weed management problems in dry direct seeded rice, different weed management options trial is designed to convince different stakeholders including farmers, input dealers, extension officers, private sectors, researchers, and policymakers.
"

	uri <- "hdl:11529/10548058"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "CIMMYT;BARI;NARC;RDRS",
		publication = NA,
		project = "SRFSI",
		design = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_product;herbicide_amount",
		response_vars = "yield;dmy_residue;plant_height;seed_weight;dry_weed_biomass_",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 75,
		carob_effort = 0
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA", "N/A", "na", "#DIV/0!", "#VALUE!")] <- NA_character_
		as.numeric(x)
	}

	parse_dt <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA", "N/A")] <- NA_character_
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
			out[is_dmy] <- ifelse(nchar(sub("^.*-", "", s)) == 4, as.character(d4), as.character(d2))
		}
		out
	}

	blank_herb <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | x %in% c("", "-", "No-spray", "no-spray")] <- ""
		x
	}

	products <- function(pre, post) {
		src <- paste(pre, post)
		out <- rep("none", length(src))
		for (i in seq_along(src)) {
			s <- src[i]
			parts <- character(0)
			if (grepl("pendimethalin", s, ignore.case=TRUE)) parts <- c(parts, "pendimethalin")
			if (grepl("oxadiazon", s, ignore.case=TRUE)) parts <- c(parts, "oxadiazon")
			if (grepl("bispyribac", s, ignore.case=TRUE)) parts <- c(parts, "bispyribac-sodium")
			if (grepl("pyrazosulfuron", s, ignore.case=TRUE)) parts <- c(parts, "pyrazosulfuron")
			if (grepl("fenoxaprop", s, ignore.case=TRUE)) parts <- c(parts, "fenoxaprop-p-ethyl")
			if (grepl("ethoxysulfuron", s, ignore.case=TRUE)) parts <- c(parts, "ethoxysulfuron")
			if (grepl("halosulfuron", s, ignore.case=TRUE)) parts <- c(parts, "halosulfuron-methyl")
			if (length(parts)) out[i] <- paste(parts, collapse=";")
		}
		out
	}

	# Parse g a.i./ha values (e.g. "1000 g; ...", "25 g + 20 g; ...", "56 g at 15-20 DAS fb 67 g at 30 DAS")
	sum_dose_kg <- function(x) {
		x <- as.character(x)
		out <- rep(0, length(x))
		for (i in seq_along(x)) {
			s <- x[i]
			if (is.na(s) || !nzchar(trimws(s))) next
			m <- regmatches(s, gregexpr("[0-9]+(?:\\.[0-9]+)?\\s*g\\b", s, ignore.case=TRUE))[[1]]
			if (length(m)) {
				vals <- as.numeric(sub("\\s*g$", "", m, ignore.case=TRUE))
				out[i] <- sum(vals) / 1000
			}
		}
		out
	}

	fix_texture <- function(x) {
		if (is.na(x) || !nzchar(x)) return(NA_character_)
		x <- tolower(trimws(x))
		if (grepl("sandy loam", x)) return("sandy loam")
		if (grepl("clay loam", x)) return("clay loam")
		x
	}

	# Station metadata (Rangpur has two distinct sites with GPS in sheet 2)
	file_meta <- data.frame(
		file = c(
			"harif rice 2015-Weed management module-Rangpur.xlsx",
			"Kharif rice 2015-weed management module-Dhanusha.xlsx",
			"Kharif rice 2015-weed management module-Rajshahi.xlsx",
			"Kharif rice 2015-weed management module-Sunsari.xlsx"
		),
		country = c("Bangladesh", "Nepal", "Bangladesh", "Nepal"),
		adm1 = c("Rangpur", "Madhesh", "Rajshahi", "Koshi"),
		adm2 = c("Rangpur", "Dhanusha", "Rajshahi", "Sunsari"),
		location = c("Rangpur", "NRRP Hardinath", "RWRC Rajshahi", "RARS Tarahara"),
		latitude = c(25.72048, 26.796, 24.367, 26.70460),
		longitude = c(89.26385, 85.970, 88.659, 87.27917),
		geo_from_source = c(TRUE, FALSE, FALSE, TRUE),
		geo_uncertainty = c(NA_real_, 2000, 2000, NA_real_),
		geo_source = c(NA_character_, "NRRP Hardinath, Dhanusha", "BARI RWRC Shyampur, Rajshahi", NA_character_)
	)

	one <- function(idx) {
		fm <- file_meta[idx, ]
		f <- ff[basename(ff) == fm$file]
		shs <- readxl::excel_sheets(f)

		# Site info (soil texture & previous crop)
		si <- carobiner::read.excel(f, sheet="2 - Site information", col_names=FALSE, col_types="text")
		h4 <- trimws(as.character(unlist(si[4, ])))
		texc <- grep("texture", h4, ignore.case=TRUE)[1]
		tex_raw <- if (!is.na(texc)) na.omit(trimws(as.character(si[[texc]][7:nrow(si)]))) else character()
		tex_raw <- tex_raw[nzchar(tex_raw)]
		soil_tex <- if (length(tex_raw)) fix_texture(tex_raw[1]) else NA_character_
		prev_crop <- if (fm$adm2 == "Sunsari") "wheat" else NA_character_

		# Harvest sheet
		s16 <- grep("16\\.WeedBio", shs, ignore.case=TRUE, value=TRUE)[1]
		r <- carobiner::read.excel(f, sheet=s16, col_names=FALSE, col_types="text")
		hr <- which(trimws(as.character(r[[1]])) == "Year")[1]
		h1 <- ifelse(is.na(r[hr, ]), "", trimws(as.character(unlist(r[hr, ]))))
		h2 <- ifelse(is.na(r[hr + 1, ]), "", trimws(as.character(unlist(r[hr + 1, ]))))
		hdr <- trimws(paste(h1, h2))
		dat <- r[(hr + 2):nrow(r), ]
		dat <- dat[grepl("^201[56]$", trimws(as.character(dat[[1]]))), ]
		n <- nrow(dat)

		pre_c <- grep("Pre-emergence herb", hdr, ignore.case=TRUE)[1]
		post_c <- grep("Post-emergence herb", hdr, ignore.case=TRUE)[1]
		pre <- blank_herb(dat[[pre_c]])
		post <- blank_herb(dat[[post_c]])
		prod <- products(pre, post)
		dose_kg <- sum_dose_kg(dat[[pre_c + 1]]) + sum_dose_kg(dat[[post_c + 1]])
		dose_kg[prod == "none"] <- 0
		ylab <- ifelse(pre == "" & post == "", "no herbicide", trimws(paste(pre, post)))

		# Tail summary columns
		nc <- ncol(dat)
		tail_idx <- max(1, nc - 9):nc
		find_tail <- function(pat) {
			j <- tail_idx[grep(pat, hdr[tail_idx], ignore.case=TRUE)]
			if (!length(j)) return(rep(NA_real_, n))
			num(dat[[j[1]]])
		}

		yld <- find_tail("Grain yield \\(t/ha\\)") * 1000
		dmy_res <- find_tail("Dry straw wt") * 1000
		hi <- find_tail("\\bHi\\b") * 100
		pht <- find_tail("Plant height")
		tgw <- find_tail("TGW")
		tgw[!is.na(tgw) & tgw <= 0] <- NA_real_
		weed_dry <- find_tail("Oven dry weed biomass") * 10 # g/m2 -> kg/ha
		pan_m2 <- find_tail("Panicle density") * 10000
		g_pan <- find_tail("Grain.*panicle")

		# Phenology sheet
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
			uv <- unique(na.omit(v))
			if (length(uv) == 1) rep(uv, n) else rep(NA_character_, n)
		}

		var_val <- gsub("  +", " ", pull_p("Variety"))
		pdate <- parse_dt(pull_p("Date of seeding"))
		hdate <- parse_dt(pull_p("Dat[ew] of harvest"))
		if (all(is.na(pdate))) pdate <- rep("2015", n)
		row_sp <- num(pull_p("Row spacing"))
		srate <- num(pull_p("Seedrate"))
		mat_d <- num(pull_p("maturity.*DAS|DAS.*maturity"))

		# Location & coordinates (split Rangpur's two sites: RDRS Farm vs OFRD Farm)
		stn_raw <- trimws(as.character(dat[[5]]))
		loc <- rep(fm$location, n)
		lat <- rep(fm$latitude, n)
		lon <- rep(fm$longitude, n)
		if (fm$adm2 == "Rangpur") {
			is_rdrs <- grepl("RDRS", stn_raw, ignore.case=TRUE)
			loc[is_rdrs] <- "RDRS Farm, Monthona"
			loc[!is_rdrs] <- "OFRD Farm, Rangpur"
			lat[is_rdrs] <- 25.80442
			lon[is_rdrs] <- 89.17078
			lat[!is_rdrs] <- 25.72048
			lon[!is_rdrs] <- 89.26385
		}

		# Fertilizer (only Rangpur has complete N, P, K application amounts: 2000g Urea, 1335g TSP, 835g MOP, 835g Gypsum, 83g ZnSO4 per 90 m2 plot)
		N_fert <- rep(NA_real_, n)
		P_fert <- rep(NA_real_, n)
		K_fert <- rep(NA_real_, n)
		ftype <- rep(NA_character_, n)
		fused <- rep(NA, n)
		if (fm$adm2 == "Rangpur") {
			N_fert[] <- round(2000 / 90 * 10 * 0.46, 2)
			P_fert[] <- round(1335 / 90 * 10 * 0.1923, 2)
			K_fert[] <- round(835 / 90 * 10 * 0.498, 2)
			ftype[] <- "urea;TSP;KCl;gypsum;ZnSO4"
			fused[] <- TRUE
		} else if (fm$adm2 == "Dhanusha") {
			# Sheet 6 records 652 g DAP and 375 g Muriate per 75 m2 plot, but Urea amounts are blank
			P_fert[] <- round(652 / 75 * 10 * 0.201, 2)
			K_fert[] <- round(375 / 75 * 10 * 0.498, 2)
			ftype[] <- "DAP;KCl;urea"
			fused[] <- TRUE
		}

		# Irrigation (sheet 7 has irrigation events in Rangpur, Dhanusha, Sunsari)
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

		psz_c <- grep("^Plot size", hdr, ignore.case=TRUE)[1]
		pid_c <- grep("^Plot Id$", hdr, ignore.case=TRUE)[1]
		data.frame(
			country = fm$country,
			adm1 = fm$adm1,
			adm2 = fm$adm2,
			location = loc,
			latitude = lat,
			longitude = lon,
			geo_from_source = fm$geo_from_source,
			geo_uncertainty = fm$geo_uncertainty,
			geo_source = fm$geo_source,
			trial_id = paste("2015", gsub("[^A-Za-z0-9]+", "_", loc), sep="_"),
			plot_id = trimws(as.character(dat[[pid_c]])),
			season = "kharif",
			crop = "rice",
			variety = var_val,
			previous_crop = prev_crop,
			planting_date = pdate,
			harvest_date = hdate,
			planting_method = "direct seeding",
			land_prep_method = "none",
			row_spacing = row_sp,
			seed_rate = srate,
			plot_area = num(dat[[psz_c]]),
			soil_texture = soil_tex,
			irrigated = irrig,
			fertilizer_used = fused,
			fertilizer_type = ftype,
			N_fertilizer = N_fert,
			P_fertilizer = P_fert,
			K_fertilizer = K_fert,
			treatment = ylab,
			herbicide_product = prod,
			herbicide_used = prod != "none",
			herbicide_amount = dose_kg,
			yield_part = "grain",
			yield = yld,
			yield_moisture = 14,
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

	d <- do.call(rbind, lapply(seq_len(nrow(file_meta)), one))
	carobiner::write_files(path, meta, d)
}
