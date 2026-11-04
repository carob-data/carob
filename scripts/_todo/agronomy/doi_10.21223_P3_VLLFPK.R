# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIP experimental field, La Molina, Lima, Peru. Unica potato, October 2017 to January 2018. Irrigation is drip (DR) or furrow (FU).
# T1 is full irrigation. T2, T3, and T4 are irrigated when CWSI is below 0.4, 0.6, and 0.7. Full irrigation is not stored as CWSI 0.
# Biomass is the mean of 4 plants per plot, in g per plant (dictionary). Plot area 45 m2 is stated for irrigation volume only. Plant density is not stated, so g per plant is not converted to kg/ha.
# Fresh and dry biomass use the dictionary names. The source spelling is "steam" for stem.
# Irrigation volume is m3 per plot. mm = m3 / 45 * 1000.
# Canopy cover is percent ground cover. Soil humidity is gravimetric percent at 30 cm.
# Biomass covers plots 1 to 32 (blocks 1 to 4). Canopy, irrigation, soil, and CWSI also include plots 33 to 40, which are block 5. Those eight plots have no biomass rows.
# Character token NA on the CWSI and soil sheets is missing.

## ISSUES
# No coordinates. The bounding box in the metadata is only a west longitude and a south latitude.
# No planting day or harvest day. The metadata window is 2017-10-05 to 2018-01-15.
# No fertilizer rate.
# No yield in kg/ha, because biomass is g per plant and density is not stated.
# Dry root biomass is often below 1 g per plant, as recorded.
# missing variables: yield, yield_moisture, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variables: cwsi_rule_
# unknown variable: irrigation_event_ (irrigation_amount?)
# unknown variables: tubers_per_plant_, cwsi_, foliage_temperature_, foliage_air_dt_

## NOT PROCESSED
# CWSI sheet: wet-surface temperature, air temperature, relative humidity, vapor-pressure deficit, solar radiation, and wind speed. They are station readings repeated on every plot, and there is no calendar date for a weather table.
# Days after treatment initiation. Days after planting is the time used.
# Harvest number. It matches the six biomass dates.
# Biomass, canopy, irrigation, soil, and CWSI identifier columns.


## NEW VARIABLES
# cwsi_rule_: full, or the CWSI level named in the dictionary (0.4, 0.6, 0.7).
# tubers_per_plant_: tuber count, mean of 4 plants. Not converted to tubers/ha.
# irrigation_event_: irrigation sequence number on the plot.
# cwsi_: crop water stress index.
# foliage_temperature_: foliage temperature, degrees C.
# foliage_air_dt_: foliage temperature minus air temperature, degrees C.


carob_script <- function(path) {

"
Dataset for: Yield Estimation of Unica Potato Crop Variety (Solanum tuberosum L.) Using a crop growth model

The objective of this study was to evaluate the performance of the SOLANUM model in the yield estimation of the UNICA potato crop variety subjected to different water stress levels and under two types of irrigation (drip and furrow irrigation). The trial was carried out in the experimental field of the International Potato Center during the months of October-2017 to January-2018. Four treatments of irrigation moments based on the Crop Water Stress Index (CWSI) were applied for this trial: Full irrigated (T1), CWSI less than 0.4 (T2), CWSI less than 0.6 (T3) and CWSI less than 0.7 (T4).
"

	uri <- "doi:10.21223/P3/VLLFPK"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = "Effective use of water in the potato crop in arid zones: Improving the management of irrigation by monitoring the water status to face climate change",
		design = NA,
		data_type = "experiment",
		treatment_vars = "irrigation_method;cwsi_rule_",
		response_vars = "fw_tubers;ground_cover;soil_GWC",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "Yield estimation of Unica potato.xls"]
	b <- carobiner::read.excel(f, sheet="Biomass")
	cc <- carobiner::read.excel(f, sheet="Canopy cover")
	ir <- carobiner::read.excel(f, sheet="Irrigation volume")
	so <- carobiner::read.excel(f, sheet="Soil humidity")
	cw <- carobiner::read.excel(f, sheet="CWSI")

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("NA", "")] <- NA_character_
		as.numeric(x)
	}
	plot_attr <- function(r) {
		data.frame(
			plot = as.character(r$plot),
			treatment = as.character(r$treatment),
			irrigation = as.character(r$irrigation),
			block = as.integer(num(r$block)),
			stringsAsFactors = FALSE
		)
	}
	plots <- rbind(plot_attr(b), plot_attr(cc), plot_attr(ir), plot_attr(so), plot_attr(cw))
	plots <- plots[!duplicated(plots$plot), ]
	plots <- plots[order(as.integer(plots$plot)), ]
	rule <- c("T1"="full", "T2"="0.4", "T3"="0.6", "T4"="0.7")
	meth <- c("DR"="drip", "FU"="furrow")

	d <- data.frame(
		trial_id = "LaMolina_2017",
		crop = "potato",
		variety = "Unica",
		treatment = as.character(plots$treatment),
		cwsi_rule_ = unname(rule[as.character(plots$treatment)]),
		irrigation_method = unname(meth[as.character(plots$irrigation)]),
		rep = plots$block,
		plot_id = as.character(plots$plot),
		plot_area = 45,
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		yield_part = "tubers",
		irrigated = TRUE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		record_id = seq_len(nrow(plots))
	)

	rid <- function(plot) {
		as.integer(match(as.character(plot), d$plot_id))
	}
	nblank <- function(n) {
		data.frame(
			record_id = rep(NA_integer_, n),
			DAP = rep(NA_integer_, n),
			time = rep(NA_character_, n),
			depth = rep(NA_real_, n),
			irrigation_event_ = rep(NA_integer_, n),
			fw_leaves = rep(NA_real_, n),
			fw_stems = rep(NA_real_, n),
			fw_roots = rep(NA_real_, n),
			fw_tubers = rep(NA_real_, n),
			dw_leaves = rep(NA_real_, n),
			dw_stems = rep(NA_real_, n),
			dw_roots = rep(NA_real_, n),
			dw_tubers = rep(NA_real_, n),
			tubers_per_plant_ = rep(NA_real_, n),
			ground_cover = rep(NA_real_, n),
			irrigation_amount = rep(NA_real_, n),
			soil_GWC = rep(NA_real_, n),
			cwsi_ = rep(NA_real_, n),
			foliage_temperature_ = rep(NA_real_, n),
			foliage_air_dt_ = rep(NA_real_, n)
		)
	}
	clock <- function(h) {
		if (inherits(h, "POSIXt")) {
			return(format(h, "%H:%M:%S"))
		}
		frac <- num(h)
		out <- rep(NA_character_, length(frac))
		ok <- !is.na(frac)
		secs <- round((frac[ok] - floor(frac[ok])) * 86400)
		secs[secs >= 86400] <- 86399
		hh <- secs %/% 3600
		mm <- (secs %% 3600) %/% 60
		ss <- secs %% 60
		out[ok] <- sprintf("%02d:%02d:%02d", hh, mm, ss)
		out
	}

	nb <- nrow(b)
	lb <- nblank(nb)
	lb$record_id <- rid(b$plot)
	lb$DAP <- as.integer(num(b$dap))
	lb$fw_leaves <- num(b$FL)
	lb$fw_stems <- num(b$FS)
	lb$fw_roots <- num(b$FR)
	lb$fw_tubers <- num(b$FT)
	lb$dw_leaves <- num(b$DL)
	lb$dw_stems <- num(b$DS)
	lb$dw_roots <- num(b$DR)
	lb$dw_tubers <- num(b$DT)
	lb$tubers_per_plant_ <- num(b$nT)

	nc <- nrow(cc)
	lc <- nblank(nc)
	lc$record_id <- rid(cc$plot)
	lc$DAP <- as.integer(num(cc$dap))
	lc$ground_cover <- num(cc$CC)

	ni <- nrow(ir)
	li <- nblank(ni)
	li$record_id <- rid(ir$plot)
	li$DAP <- as.integer(num(ir$dap))
	li$irrigation_event_ <- as.integer(num(ir$nIrri))
	li$irrigation_amount <- num(ir$V) / 45 * 1000

	ns <- nrow(so)
	ls <- nblank(ns)
	ls$record_id <- rid(so$plot)
	ls$DAP <- as.integer(num(so$dap))
	ls$depth <- 30
	ls$soil_GWC <- num(so$Hv)

	nw <- nrow(cw)
	lw <- nblank(nw)
	lw$record_id <- rid(cw$plot)
	lw$DAP <- as.integer(num(cw$dap))
	lw$time <- clock(cw$hour)
	lw$cwsi_ <- num(cw$CWSI)
	lw$foliage_temperature_ <- num(cw$Tf)
	lw$foliage_air_dt_ <- num(cw$dT)

	x <- rbind(lb, lc, li, ls, lw)
	rownames(x) <- NULL

	carobiner::write_files(path, meta, d, long=x)
}
