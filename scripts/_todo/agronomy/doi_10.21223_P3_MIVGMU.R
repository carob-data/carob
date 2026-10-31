# R script for "carob"
# license: GPL (>=3)

## NOTES
# La Molina, Lima, Peru. Coordinates are the dataset bounding box (southLatitude -12.076289, westLongitude -76.948417).
# TY is t/ha (dictionary). Stored as kg/ha. Fresh tuber yield; moisture is not stated.
# WRIT6w and WRIT8w are water-restriction initiation at 6 and 8 weeks after planting (Read me sheet). PRD25, PRD50, DI50, and FI are the irrigation regimes.
# RWC, chlorophyll (SPAD), and osmotic potential are repeated on days after treatment (Read me: DAT). They stay in the long table.
# Weather sheet GRad is MJ m-2 d-1. Stored as srad in kJ m-2.

## ISSUES
# CWD is m3/ha. The dictionary does not say it is applied irrigation, so it is not converted to irrigation_amount.
# No planting or harvest date in the workbook. Collection window in the metadata is 2009-06-01 to 2009-11-30.
# No fertilizer rates.
# The site is not labeled on-farm. La Molina is treated as a station (on_farm FALSE).
# Long rows use dat_ (days after treatment). That name is not in the date/DAP/depth list, so the time check remains.
# write_files: ; yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variable irrigation_timing_ (timing?); unknown variables irrigation_regime_, wue_, cwd_, dat_, op_, rwc_.
# write_files: time/depth: no time/depth variables in long records?

## NOT PROCESSED
# Dictionary_OA_Yactayo.xlsx: variable definitions, used to read units.
# WUE is kept as wue_. Plot-level water-use efficiency, kg fresh tuber per m3.

## NEW VARIABLES
# irrigation_timing_: water-restriction initiation code (WRIT6w, WRIT8w)
# irrigation_regime_: PRD25, PRD50, DI50, or FI
# wue_: water use efficiency, kg/m3
# cwd_: recorded CWD, m3/ha
# dat_: days after treatment
# rwc_: relative water content, percent
# op_: leaflet osmotic potential, MPa

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
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "irrigation_timing_;irrigation_regime_",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.55
	)

	f <- ff[basename(ff) == "Datos Yactayo et al_2013 (PRD).xlsx"]

	# Physiology sheets repeat the header for the second timing, with different DAT columns.
	sheet_blocks <- function(sheet) {
		x <- as.data.frame(carobiner::read.excel(f, sheet=sheet, col_names=FALSE))
		lab <- trimws(as.character(x[[1]]))
		heads <- which(lab == "T")
		if (length(heads) == 0) stop("no header in ", sheet)
		parts <- list()
		for (k in seq_along(heads)) {
			i <- heads[k]
			end <- if (k < length(heads)) heads[k + 1] - 1 else nrow(x)
			hdr <- trimws(as.character(unlist(x[i, ])))
			empty <- is.na(hdr) | !nzchar(hdr)
			hdr[empty] <- paste0("c", which(empty))
			if (i == end) next
			y <- x[(i + 1):end, , drop=FALSE]
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
		crop = "potato",
		irrigation_timing_ = trimws(as.character(ty$T)),
		irrigation_regime_ = trimws(as.character(ty$TRT)),
		block_id = trimws(as.character(ty$B)),
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
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = TRUE
	)
	d$treatment <- paste(d$irrigation_timing_, d$irrigation_regime_, sep="_")
	d$plot_id <- paste(d$treatment, d$block_id, sep="_")

	wue_key <- paste(trimws(as.character(wue$T)), trimws(as.character(wue$B)), trimws(as.character(wue$TRT)))
	cwd_key <- paste(trimws(as.character(cwd$T)), trimws(as.character(cwd$B)), trimws(as.character(cwd$TRT)))
	key <- paste(d$irrigation_timing_, d$block_id, d$irrigation_regime_)
	d$wue_ <- as.numeric(wue[["WUE (Kg m-3)"]][match(key, wue_key)])
	d$cwd_ <- as.numeric(cwd[["CWD (m3 ha−1)"]][match(key, cwd_key)])

	phys_long <- function(sheet, value_name) {
		out <- list()
		for (x in sheet_blocks(sheet)) {
			id <- c("T", "B", "TRT")
			dats <- setdiff(names(x), id)
			dats <- dats[grepl("DAT$", dats, ignore.case=TRUE)]
			for (j in dats) {
				piece <- data.frame(
					plot_id = paste(trimws(as.character(x$T)), trimws(as.character(x$TRT)), trimws(as.character(x$B)), sep="_"),
					dat_ = as.integer(sub("DAT$", "", j, ignore.case=TRUE))
				)
				piece[[value_name]] <- as.numeric(x[[j]])
				out[[length(out) + 1]] <- piece
			}
		}
		do.call(rbind, out)
	}

	rwc <- phys_long("RWC", "rwc_")
	sp <- phys_long("Chl SPAD", "SPAD")
	op <- phys_long("OP", "op_")
	dlong <- merge(rwc, sp, by=c("plot_id", "dat_"), all=TRUE)
	dlong <- merge(dlong, op, by=c("plot_id", "dat_"), all=TRUE)
	dlong <- dlong[dlong$plot_id %in% d$plot_id, ]
	dlong$order <- match(dlong$dat_, sort(unique(dlong$dat_)))
	dlong <- dlong[order(dlong$plot_id, dlong$order), ]
	rownames(dlong) <- NULL

	wx <- carobiner::read.excel(f, sheet="Weather data")
	wth <- data.frame(
		date = as.character(as.Date(wx$Date)),
		tmax = as.numeric(wx[["T max(°C)"]]),
		tmin = as.numeric(wx[["T min (°C)"]]),
		srad = as.numeric(wx[["GRad (MJ*m2)"]]) * 1000,
		prec = as.numeric(wx[["PP (mm)"]]),
		rhum = as.numeric(wx[["RH (%)"]]),
		country = "Peru",
		location = "La Molina",
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
