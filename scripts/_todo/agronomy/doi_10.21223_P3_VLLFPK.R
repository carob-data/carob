# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIP experimental field, La Molina, Lima, Peru. Unica potato, October 5, 2017 to January 15, 2018 (exact window from Dataverse metadata, matching E2-CIP in doi:10.21223/LNLR9B). Irrigation is drip (DR) or furrow (FU).
# T1 is full irrigation. T2, T3, and T4 are irrigated when CWSI is below 0.4, 0.6, and 0.7. Full irrigation is not stored as CWSI 0.
# Biomass is the mean of 4 plants per plot, in g per plant (dictionary). Plot area 45 m2 is stated for irrigation volume only. Plant density is not stated, so g per plant is not converted to kg/ha.
# Irrigation volume is m3 per plot: per-event depth (mm) = V / 45 * 1000 is stored in long as irrigation_event_mm_, and seasonal total irrigation_amount (mm) and irrigation_number are stored in wide d.
# Canopy cover is percent ground cover (1.7 to 100%). Soil humidity is gravimetric percent at 30 cm (divided by 100 to g/g for soil_GWC).
# Biomass covers plots 1 to 32 (blocks 1 to 4); canopy, irrigation, soil, and CWSI also include plots 33 to 40 (block 5).
# Estimated CIP La Molina coordinates (-12.076289, -76.948417) added with geo_from_source = FALSE and geo_uncertainty = 1000 m.

## ISSUES
# No fertilizer rate or yield moisture.
# No yield in kg/ha, because biomass is g per plant and plant density is not stated.
# Dry root biomass is often below 1 g per plant, as recorded.
# write_files: missing carob_contributor, yield, yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer; unknown variables: cwsi_rule_, irrigation_event_, irrigation_event_mm_, tubers_per_plant_, cwsi_, foliage_temperature_, foliage_air_dt_.

## NOT PROCESSED
# CWSI sheet: wet-surface temperature, air temperature, relative humidity, vapor-pressure deficit, solar radiation, and wind speed (station readings repeated on every plot without calendar dates).
# Days after treatment initiation (daTI): DAP is used as the common time variable.
# Harvest number (matches the six biomass DAPs).
# Data_Dictionary.xls: codebook.

## NEW VARIABLES
# cwsi_rule_: full, or the CWSI threshold named in the dictionary (0.4, 0.6, 0.7).
# tubers_per_plant_: tuber count, mean of 4 plants.
# irrigation_event_: irrigation sequence number on the plot.
# irrigation_event_mm_: per-event irrigation depth (mm = m3 / 45 * 1000).
# cwsi_: crop water stress index.
# foliage_temperature_: foliage temperature (degrees C).
# foliage_air_dt_: foliage temperature minus air temperature (degrees C).


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "Yield estimation of Unica potato.xls"]
	nas <- c("", "NA")
	r1 <- carobiner::read.excel(f, sheet="Biomass", na=nas)
	r2 <- carobiner::read.excel(f, sheet="Canopy cover", na=nas)
	r3 <- carobiner::read.excel(f, sheet="Irrigation volume", na=nas)
	r4 <- carobiner::read.excel(f, sheet="Soil humidity", na=nas)
	r5 <- carobiner::read.excel(f, sheet="CWSI", na=nas)

	plot_attr <- function(r) {
		data.frame(
			plot = as.character(r$plot),
			treatment = as.character(r$treatment),
			irrigation = as.character(r$irrigation),
			block = as.integer(r$block),
			stringsAsFactors = FALSE
		)
	}
	plots <- rbind(plot_attr(r1), plot_attr(r2), plot_attr(r3), plot_attr(r4), plot_attr(r5))
	plots <- plots[!duplicated(plots$plot), ]
	plots <- plots[order(as.integer(plots$plot)), ]
	rule <- c("T1"="full", "T2"="0.4", "T3"="0.6", "T4"="0.7")
	meth <- c("DR"="drip", "FU"="furrow")

	irr_mm_vec <- as.numeric(r3$V) / 45 * 1000
	irr_sum <- tapply(irr_mm_vec, as.character(r3$plot), sum, na.rm=TRUE)
	irr_cnt <- tapply(irr_mm_vec, as.character(r3$plot), function(z) sum(!is.na(z) & z > 0))

	d <- data.frame(
		record_id = seq_len(nrow(plots)),
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
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = FALSE,
		geo_uncertainty = 1000,
		planting_date = "2017-10-05",
		harvest_date = "2018-01-15",
		yield_part = "tubers",
		irrigated = TRUE,
		irrigation_amount = unname(irr_sum[as.character(plots$plot)]),
		irrigation_number = as.integer(unname(irr_cnt[as.character(plots$plot)])),
		on_farm = FALSE,
		is_survey = FALSE
	)

	rid <- function(plot) as.integer(match(as.character(plot), d$plot_id))

	clock <- function(h) {
		if (inherits(h, "POSIXt")) return(format(h, "%H:%M:%S", tz="UTC"))
		frac <- as.numeric(h)
		out <- rep(NA_character_, length(frac))
		ok <- !is.na(frac)
		secs <- round((frac[ok] - floor(frac[ok])) * 86400)
		secs[secs >= 86400] <- 86399
		sprintf("%02d:%02d:%02d", secs %/% 3600, (secs %% 3600) %/% 60, secs %% 60)
	}

	lb <- data.frame(
		record_id = rid(r1$plot),
		DAP = as.integer(r1$dap),
		fw_leaves = as.numeric(r1$FL),
		fw_stems = as.numeric(r1$FS),
		fw_roots = as.numeric(r1$FR),
		fw_tubers = as.numeric(r1$FT),
		dw_leaves = as.numeric(r1$DL),
		dw_stems = as.numeric(r1$DS),
		dw_roots = as.numeric(r1$DR),
		dw_tubers = as.numeric(r1$DT),
		tubers_per_plant_ = as.numeric(r1$nT)
	)
	lc <- data.frame(
		record_id = rid(r2$plot),
		DAP = as.integer(r2$dap),
		ground_cover = as.numeric(r2$CC)
	)
	li <- data.frame(
		record_id = rid(r3$plot),
		DAP = as.integer(r3$dap),
		irrigation_event_ = as.integer(r3$nIrri),
		irrigation_event_mm_ = irr_mm_vec
	)
	ls <- data.frame(
		record_id = rid(r4$plot),
		DAP = as.integer(r4$dap),
		depth = 30,
		# Hv is gravimetric moisture in %; terminag soil_GWC is g/g
		soil_GWC = as.numeric(r4$Hv) / 100
	)
	lw <- data.frame(
		record_id = rid(r5$plot),
		DAP = as.integer(r5$dap),
		time = clock(r5$hour),
		cwsi_ = as.numeric(r5$CWSI),
		foliage_temperature_ = as.numeric(r5$Tf),
		foliage_air_dt_ = as.numeric(r5$dT)
	)

	dlong <- carobiner::bindr(lb, lc, li, ls, lw)

	carobiner::write_files(path, meta, d, long=dlong)
}
