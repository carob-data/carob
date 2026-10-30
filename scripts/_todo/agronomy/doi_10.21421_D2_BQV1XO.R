# R script for "carob"
# license: GPL (>=3)

## NOTES
# Multi-year groundnut reference-collection trials under full irrigation (WW) and water stress (WS) in India and Niger, 2008-2013.
# Each sheet has a plot-level table on the left. A second block of genotype means (often labeled "mean non-sensitive", and sometimes repeated under the plot table) was not used.
# Pod and haulm weights in g/m2 were multiplied by 10 to kg/ha. Where the column is named DW, haulm is dmy_residue and yield_moisture is 0. Otherwise haulm is fwy_residue and moisture is unknown.
# On the HT sheets, columns marked "old-wrong" or "weight / 2m2" were skipped. The columns under "Correct data" are already g/m2 (half of the 2 m2 plot weight).
# Plants per m2 were multiplied by 10000 to plants/ha. Sheet codes: SC is read as the ICRISAT Sahelian Center at Sadore, Niger; sheets without SC are the India trials. HT is kept only in the trial id because the file does not define it.
# WW is irrigated. WS is not, and stress is drought. A row treatment of ww or ws overrides the sheet name (the 2012 WW sheet contains both).
# Plot area is set only where the sheet states it (4.8, 2.1, 2, 1.8 or 1.75 m2).

## ISSUES
# The India city is not in the file, and there are no coordinates for either country. SC as Sadore is an interpretation of the sheet code plus the title (Niger and India).
# HT is not defined in the file.
# One WS-SC 2008 pod weight is -3 g/m2 (ICG 1274). It was left as recorded, so yield is negative.
# WW-SC 2009 pod weights reach about 1200 g/m2 (about 12 t/ha), which is very high for groundnut.
# Niger 2008 sheets have six replications. Fertilizer is not reported.
# Shelling percent is 100 * subsample seed weight / subsample pod weight on the 2013-14 sheets only.
# The 2013-14 sheets also show an unlabeled 1.875 next to "per 1.8 m2" (WW) or "per 1.75 m2" (WS). Plot area uses 1.8 and 1.75.
# About 590 plot rows have no pod weight. One WS-SC 2008 pod weight is negative. Nine SC 2008 haulm weights are negative. One shelling percentage is about 100. Plant density is 0 on a few plots and 3882500 plants/ha for ICG 928, WS 09-10, rep 2.
# write_files: missing carob_contributor; missing latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown variable seed_yield_; out of bounds plant_density (0-3882500), yield (-30-12329), fwy_residue (-1980-13790), shelling_percentage (up to 100); high groundnut yield 12329 kg/ha; NA detected in yield.

## NOT PROCESSED
# Right-hand and lower blocks of genotype means, including the "mean non-sensitive" column.
# Columns marked old-wrong, and the weight/2m2 or g/plot columns, because the g/m2 (or DW/m2) columns were used.
# 2013-14: No of plants (a count, plot area ambiguous), SS pod number, SS seed number, total seed weight, haulm weight and total pod weight (the DW/m2 columns were used).
# Row, Sno and Genoid layout indexes.
# The unlabeled 1.875 factor.

## NEW VARIABLES
# seed_yield_: kernel weight, kg/ha, only on the 2012 SC-HT sheets that report seed g/m2. yield itself is pod weight.

carob_script <- function(path) {

"
Multi-year assessment of Groundnut reference collection in field conditions (water stress and full irrigation) in Niger and India

Yield and agronomic trait assessment
"

	uri <- "doi:10.21421/D2/BQV1XO"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;treatment",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 1.2
	)

	f <- ff[basename(ff) == "Groundnut Ref Set  Field WW WS - India-Niger 2008-13.xlsx"]
	sheets <- c(
		"WW 08-09", "WW 09-10", "WW 10-11", "WW 11-12", "WW 12-13", "WW 13-14",
		"WW-SC 2008", "WW-SC 2009",
		"WS 08-09", "WS 09-10", "WS 10-11", "WS 11-12", "WS 12-13", "WS 13-14",
		"WS-SC 2008", "WS-SC 2009",
		"WW-SC-HT-2009", "WW-SC-HT-2010", "WW-SC-HT-2012",
		"WS-SC-HT-2009", "WS-SC-HT-2010", "WS-SC-HT-2012"
	)

	norm <- function(x) {
		x <- tolower(trimws(x))
		x[is.na(x)] <- ""
		gsub("\\s+", " ", x)
	}

	pick_col <- function(hdr, super, pattern, require_area=FALSE) {
		nm <- norm(hdr)
		sp <- norm(super)
		hit <- grep(pattern, nm)
		if (!length(hit)) return(NA_integer_)
		bad <- grepl("wrong", sp[hit])
		if (any(!bad)) hit <- hit[!bad]
		areal <- grepl("m2|m²", nm[hit])
		if (require_area) {
			if (!any(areal)) return(NA_integer_)
			hit <- hit[areal]
		} else if (any(areal)) {
			hit <- hit[areal]
		}
		hit[length(hit)]
	}

	to_num <- function(x) {
		x <- trimws(x)
		x[is.na(x) | x %in% c("", "NA", "na")] <- NA
		as.numeric(x)
	}

	read_left <- function(sheet) {
		raw <- as.data.frame(carobiner::read.excel(f, sheet=sheet, col_names=FALSE))
		raw[] <- lapply(raw, as.character)
		score <- apply(raw, 1, function(row) {
			x <- norm(row)
			sum(x %in% c("plot", "plot no.", "plotno", "rep", "entryname", "entry name", "entry", "genotype", "geno name", "genoid"))
		})
		h <- which(score >= 2)[1]
		hdr <- trimws(raw[h, ])
		hdr[is.na(hdr)] <- ""
		super <- rep("", ncol(raw))
		if (h > 1) {
			super <- trimws(raw[h - 1, ])
			super[is.na(super)] <- ""
		}
		split_at <- ncol(raw) + 1
		if (ncol(raw) >= 5) {
			for (i in 5:ncol(raw)) {
				if (hdr[i] == "" && any(hdr[i:length(hdr)] != "")) {
					split_at <- i
					break
				}
			}
		}
		idx <- seq_len(split_at - 1)
		while (length(idx) && hdr[idx[length(idx)]] == "") idx <- idx[-length(idx)]
		banner <- paste(as.character(unlist(raw[seq_len(min(3, nrow(raw))), ])), collapse=" ")
		list(hdr=hdr[idx], super=super[idx], dat=raw[(h + 1):nrow(raw), idx, drop=FALSE], banner=banner)
	}

	one_sheet <- function(sheet) {
		x <- read_left(sheet)
		hdr <- x$hdr
		super <- x$super
		dat <- x$dat
		i_var <- pick_col(hdr, super, "entryname|entry name|^genotype$|geno name|^entry$")
		i_code <- pick_col(hdr, super, "^geno$|^genoid$|^number$")
		i_rep <- pick_col(hdr, super, "^rep$")
		i_plot <- pick_col(hdr, super, "^plot")
		i_block <- pick_col(hdr, super, "^block$")
		i_trt <- pick_col(hdr, super, "^trt$|^treat$|^rh$")
		i_pod <- pick_col(hdr, super, "pod")
		i_haulm <- pick_col(hdr, super, "haulm|halm|veg wt")
		i_plt <- pick_col(hdr, super, "noplt|plants", require_area=TRUE)
		i_mat <- pick_col(hdr, super, "daymat")
		i_fl <- pick_col(hdr, super, "dayfl")
		i_seed <- pick_col(hdr, super, "seed", require_area=TRUE)
		nm <- norm(hdr)
		i_sspod <- grep("ss ?pod wt", nm)[1]
		i_ssseed <- grep("ss seed wt", nm)[1]
		if (!length(i_sspod)) i_sspod <- NA_integer_
		if (!length(i_ssseed)) i_ssseed <- NA_integer_

		col <- function(i) {
			if (is.na(i)) return(rep(NA_character_, nrow(dat)))
			dat[[i]]
		}
		variety <- trimws(col(i_var))
		rep_chr <- trimws(col(i_rep))
		bad_name <- tolower(variety) %in% c("entry", "entryname", "entry name", "genotype", "geno name", "trt", "treat")
		ok <- !is.na(rep_chr) & grepl("^[0-9]+$", rep_chr) & !is.na(variety) & nzchar(variety) & !bad_name
		variety <- variety[ok]
		water <- tolower(trimws(col(i_trt)[ok]))
		water[!water %in% c("ww", "ws")] <- NA_character_
		sheet_w <- if (grepl("^WS", sheet)) "ws" else "ww"
		water[is.na(water)] <- sheet_w
		dry <- grepl("dw", norm(hdr[i_pod]))
		pod <- to_num(col(i_pod)[ok])
		haulm <- to_num(col(i_haulm)[ok])
		plants <- to_num(col(i_plt)[ok])
		sspod <- to_num(col(i_sspod)[ok])
		ssseed <- to_num(col(i_ssseed)[ok])
		shell <- rep(NA_real_, length(variety))
		use <- !is.na(sspod) & !is.na(ssseed) & sspod > 0
		shell[use] <- 100 * ssseed[use] / sspod[use]
		area <- NA_real_
		if (grepl("4\\.8\\s*m", x$banner)) area <- 4.8
		else if (grepl("2\\.1\\s*m", x$banner)) area <- 2.1
		else if (grepl("1\\.8\\s*m", x$banner)) area <- 1.8
		else if (grepl("1\\.75\\s*m", x$banner)) area <- 1.75
		else if (grepl("2\\s*m²|2m²|\\(2m", x$banner)) area <- 2
		if (grepl("20\\d{2}", sheet)) {
			planting <- sub(".*(20\\d{2}).*", "\\1", sheet)
		} else {
			planting <- paste0("20", sub(".*(\\d{2})-\\d{2}.*", "\\1", sheet))
		}
		niger <- grepl("SC", sheet)
		n <- length(variety)
		data.frame(
			country = rep(if (niger) "Niger" else "India", n),
			location = rep(if (niger) "Sadore" else NA_character_, n),
			trial_id = paste(sheet, toupper(water), sep="_"),
			planting_date = planting,
			treatment = toupper(water),
			irrigated = water == "ww",
			stress = ifelse(water == "ws", "drought", NA_character_),
			variety = variety,
			variety_code = trimws(col(i_code)[ok]),
			rep = as.integer(rep_chr[ok]),
			block_id = trimws(col(i_block)[ok]),
			plot_id = trimws(col(i_plot)[ok]),
			plot_area = area,
			flowering_days = to_num(col(i_fl)[ok]),
			maturity_days = to_num(col(i_mat)[ok]),
			plant_density = plants * 10000,
			yield = pod * 10,
			yield_moisture = ifelse(dry, 0, NA_real_),
			yield_isfresh = if (dry) FALSE else NA,
			fwy_residue = if (dry) rep(NA_real_, n) else haulm * 10,
			dmy_residue = if (dry) haulm * 10 else rep(NA_real_, n),
			shelling_percentage = shell,
			seed_yield_ = to_num(col(i_seed)[ok]) * 10
		)
	}

	d <- do.call(rbind, lapply(sheets, one_sheet))
	d$crop <- "groundnut"
	d$yield_part <- "pod"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	d$variety_code[!nzchar(d$variety_code)] <- NA_character_
	d$block_id[!nzchar(d$block_id)] <- NA_character_
	d$plot_id[!nzchar(d$plot_id)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
