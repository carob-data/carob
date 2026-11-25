# R script for "carob"
# license: GPL (>=3)

## NOTES
# Associated publication found by title/author match: Yactayo et al. (2013), Agricultural Water Management 123:65-72 (doi:10.1016/j.agwat.2013.03.009).
# La Molina, Lima, Peru. Coordinates (-12.076289, -76.948417) come from the CIP Dataverse geographic metadata.
# TY is fresh tuber yield in t/ha (converted to kg/ha * 1000). WUE (kg fresh tuber per m3 water) is mapped to terminag WP_irrigation.
# CWD (m3/ha) is cumulative irrigation water depth applied (verified by TY * 1000 / CWD = WUE and arid La Molina rainfall ~0 mm); CWD / 10 converts m3/ha to irrigation_amount in mm.
# WRIT6w and WRIT8w are water-restriction initiation at 6 weeks (42 DAP) and 8 weeks (56 DAP) after planting (note: the Read me sheet accidentally swaps 6w/8w text relative to the abstract and paper; abstract/paper definitions are used).
# RWC, SPAD, and leaflet osmotic potential (OP) were measured repeatedly by days after treatment (DAT); stored in long table linked via record_id with DAP = 42 + DAT (WRIT6w) or 56 + DAT (WRIT8w).
# Weather sheet GRad is MJ m-2 d-1, converted to srad in kJ m-2 (* 1000).
# Exact planting/harvest days are not in the workbook; collection window in Dataverse metadata is 2009-06-01 to 2009-11-30, so planting_date = "2009-06" and harvest_date = "2009-11".

## ISSUES
# No fertilizer rates or yield moisture reported in the workbook.
# write_files: missing carob_contributor, yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer; unknown variables: irrigation_timing_, irrigation_regime_, dat_, rwc_, op_.

## NOT PROCESSED
# Dictionary_OA_Yactayo.xlsx and Read me sheet: variable definitions used to verify units and treatments.

## NEW VARIABLES
# irrigation_timing_: water-restriction initiation timing (WRIT6w = 6 weeks after planting, WRIT8w = 8 weeks after planting).
# irrigation_regime_: irrigation treatment (FI = full irrigation, DI50 = 50% deficit, PRD50 = 50% partial root-zone drying, PRD25 = 25% PRD).
# dat_: days after initiation of water-restriction treatment.
# rwc_: leaf relative water content (%).
# op_: leaflet osmotic potential (MPa).


carob_script <- function(path) {

"
Replication data for: Effect of partial root-zone drying irrigation timing on potato tuber yield and water use efficiency

Partial root-zone drying (PRD) is an irrigation technique which has shown increased water use efficiency (WUE) without yield reductions in potato and other crops. However, questions remain as to the effect of the water restriction initiation timing and the level of alternate water restriction on the response of the potato crop. In this study, we tested: two PRD treatments with 25% (PRD25) and 50% (PRD50) of total water used in full irrigation (FI, as control), and a deficit irrigation treatment with 50% of water restriction (DI50). Two water restriction initiation timings were tested at: 6 weeks (WRIT6w) and 8 weeks (WRIT8w) after planting. Osmotic potential, osmotic adjustment, relative water content and chlorophyll concentration were assessed in four dates during the growing period. PRD50 initiated at WRIT6w showed the highest WUE without a tuber yield reduction respect to the control. While plants under PRDs and DI50 showed lower   than FI, PRDs treatments promoted higher osmotic adjustment particularly in WRIT6w. Our study suggests that early PRDs with mild water restriction allow drought hardiness (improving water stress response) and water saving avoiding a dramatic yield tuber reduction.
"

	uri <- "doi:10.21223/P3/MIVGMU"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIP",
		publication = "doi:10.1016/j.agwat.2013.03.009",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "irrigation_timing_;irrigation_regime_",
		response_vars = "yield;WP_irrigation;SPAD",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "Datos Yactayo et al_2013 (PRD).xlsx"]

	# Physiology sheets repeat the header for the second timing, with different DAT columns.
	sheet_blocks <- function(sheet) {
		r1 <- as.data.frame(carobiner::read.excel(f, sheet=sheet, col_names=FALSE))
		lab <- trimws(as.character(r1[[1]]))
		heads <- which(lab == "T")
		if (length(heads) == 0) stop("no header in ", sheet)
		parts <- list()
		for (k in seq_along(heads)) {
			i <- heads[k]
			end <- if (k < length(heads)) heads[k + 1] - 1 else nrow(r1)
			hdr <- trimws(as.character(unlist(r1[i, ])))
			empty <- is.na(hdr) | !nzchar(hdr)
			hdr[empty] <- paste0("c", which(empty))
			if (i == end) next
			y <- r1[(i + 1):end, , drop=FALSE]
			names(y) <- hdr
			keep <- !is.na(y$T) & nzchar(trimws(as.character(y$T)))
			y <- y[keep, ]
			if (nrow(y) > 0) parts[[length(parts) + 1]] <- y
		}
		parts
	}

	read_block <- function(sheet) {
		parts <- sheet_blocks(sheet)
		if (length(parts) == 1) return(parts[[1]])
		do.call(rbind, parts)
	}

	ty <- read_block("TY")
	wue <- read_block("WUE")
	cwd <- read_block("CWD")

	d <- data.frame(
		record_id = seq_len(nrow(ty)),
		crop = "potato",
		irrigation_timing_ = trimws(as.character(ty$T)),
		irrigation_regime_ = trimws(as.character(ty$TRT)),
		block_id = trimws(as.character(ty$B)),
		rep = as.integer(sub("^B", "", trimws(as.character(ty$B)))),
		yield = as.numeric(ty[["TY (t ha-1)"]]) * 1000,
		yield_part = "tubers",
		yield_isfresh = TRUE,
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = TRUE,
		geo_source = "Dataverse bounding box",
		trial_id = "LaMolina_2009",
		planting_date = "2009-06",
		harvest_date = "2009-11",
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = TRUE
	)
	d$treatment <- paste(d$irrigation_timing_, d$irrigation_regime_, sep="_")
	d$plot_id <- paste(d$treatment, d$block_id, sep="_")

	wue_key <- paste(trimws(as.character(wue$T)), trimws(as.character(wue$B)), trimws(as.character(wue$TRT)))
	cwd_key <- paste(trimws(as.character(cwd$T)), trimws(as.character(cwd$B)), trimws(as.character(cwd$TRT)))
	key <- paste(d$irrigation_timing_, d$block_id, d$irrigation_regime_)
	d$WP_irrigation <- as.numeric(wue[["WUE (Kg m-3)"]][match(key, wue_key)])
	# CWD (m3/ha) / 10 = mm cumulative irrigation applied
	d$irrigation_amount <- as.numeric(cwd[["CWD (m3 ha\u22121)"]][match(key, cwd_key)]) / 10

	phys_long <- function(sheet, value_name) {
		out <- list()
		for (r1 in sheet_blocks(sheet)) {
			id <- c("T", "B", "TRT")
			dats <- setdiff(names(r1), id)
			dats <- dats[grepl("DAT$", dats, ignore.case=TRUE)]
			for (j in dats) {
				timing <- trimws(as.character(r1$T))
				dat_val <- as.integer(sub("DAT$", "", j, ignore.case=TRUE))
				piece <- data.frame(
					plot_id = paste(timing, trimws(as.character(r1$TRT)), trimws(as.character(r1$B)), sep="_"),
					DAP = ifelse(timing == "WRIT6w", 42L, 56L) + dat_val,
					dat_ = dat_val
				)
				piece[[value_name]] <- as.numeric(r1[[j]])
				out[[length(out) + 1]] <- piece
			}
		}
		do.call(rbind, out)
	}

	rwc <- phys_long("RWC", "rwc_")
	sp <- phys_long("Chl SPAD", "SPAD")
	op <- phys_long("OP", "op_")
	dlong <- merge(rwc, sp, by=c("plot_id", "DAP", "dat_"), all=TRUE)
	dlong <- merge(dlong, op, by=c("plot_id", "DAP", "dat_"), all=TRUE)
	dlong <- dlong[dlong$plot_id %in% d$plot_id, ]
	dlong$record_id <- d$record_id[match(dlong$plot_id, d$plot_id)]
	dlong$plot_id <- NULL
	dlong <- dlong[order(dlong$record_id, dlong$DAP), ]
	rownames(dlong) <- NULL

	r2 <- carobiner::read.excel(f, sheet="Weather data")
	wth <- data.frame(
		date = as.character(as.Date(r2$Date)),
		tmax = as.numeric(r2[["T max(\u00b0C)"]]),
		tmin = as.numeric(r2[["T min (\u00b0C)"]]),
		srad = as.numeric(r2[["GRad (MJ*m2)"]]) * 1000,
		prec = as.numeric(r2[["PP (mm)"]]),
		rhum = as.numeric(r2[["RH (%)"]]),
		country = "Peru",
		location = "La Molina",
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
