# R script for "carob"
# license: GPL (>=3)

## NOTES
# UNICA potato water-stress and photosynthesis recovery experiment at CIP La Molina, Lima, Peru (24 plots across 2 irrigation methods x 3 stress thresholds).
# Uses the consolidated 8-sheet workbook Photosynthesis recovery-2017_v2.xlsx with units documented in Documentation_v2.xlsx: Harvest fw/dw are g per plant (mapped to fw_leaves..dw_tubers); Canopy cover CC is % (ground_cover); Irrigation Vol IV is m3 per plot; Ecophysiology Pn (umol CO2 m-2 s-1), gs_max (mol H2O m-2 s-1), Fv'/Fm'; Isotopes and Recovery isotopes D13 (per mil); PRI (dimensionless).
# Irrigation methods: DR = drip, FU = furrow. Treatments: 1 = full irrigation, 2 = gs_max threshold 0.15, 3 = gs_max threshold 0.05 (per Data_Dictionary.xlsx).
# Exact planting date (2017-07-05) is verified directly from date - dap across all rows of Irrigation Vol; final harvest at 97 DAP gives harvest_date = "2017-10-10".
# All plant-level sequential harvests, canopy cover, irrigation events, ecophysiology, PRI, and isotope observations are preserved without aggregation in the long table linked via record_id.
# Hourly Weather (2352 rows, 2017-07-05 to 2017-10-10) is aggregated to daily wth: VPD kPa -> hPa (* 10), SR W/m2 hourly -> srad kJ/m2/day (sum * 3.6), Pa mbar = hPa, PP and Eo summed daily.
# Coordinates (-12.076289, -76.948417) come from the CIP Dataverse bounding box (south/west corner) with geo_uncertainty = 2000 m.

## ISSUES
# Plot area and plant density are not stated in this dataset's files (though the companion Cool Farm Tool dataset doi:10.21223/LNLR9B E1-CIP reports 45 m2), so per-plant tuber weights (g/plant) and per-plot irrigation volumes (m3) are not converted to kg/ha or mm.
# One cell in Harvest fw-tuber contains the raw typo "20.3.4", which is read as NA.
# Canopy cover CC reaches 112.69% in the raw sheet (kept as recorded).
# No fertilizer rates or yield moisture reported.
# write_files: missing carob_contributor, yield, yield_moisture, K_fertilizer, N_fertilizer, P_fertilizer; out of bounds: ground_cover (8.311, 112.692); unknown variables: irrigation_regime_, total_irrigation_volume_m3_, harvest_, tubers_per_plant_, irrigation_volume_m3_, photosynthesis_, gs_max_, fv_fm_, PRI_, sample_material_, sampling_moment_, d13C_discrimination_.

## NOT PROCESSED
# Legacy v1 individual workbooks (HARVEST-EX17-I.xlsx, CANOPY COVER-EX17-I.xlsx, ECOPHYSIOLOGY-EX17-I.xlsx, HARVEST ISOTOPES-EX17-I.xlsx, IRRIGATION VOLUME-EX17-I.xlsx, PRI EX17-I.xlsx, RECOVERY ISOTOPES-EX17-I.xlsx): superseded by the consolidated Photosynthesis recovery-2017_v2.xlsx.
# Data_Dictionary.xlsx and Documentation_v2.xlsx: codebooks used to verify units.
# Weather PAR (umol/s/m2): no matching column in terminag weather variables.
# Isotope sample dry weight dw (mg): analytical mass for IRMS.

## NEW VARIABLES
# irrigation_regime_: water-restriction treatment (full irrigation, gs_max 0.15, gs_max 0.05).
# total_irrigation_volume_m3_: seasonal total irrigation volume per plot (m3).
# harvest_: sequential harvest number (1 to 6).
# tubers_per_plant_: number of tubers per harvested plant (count).
# irrigation_volume_m3_: per-event irrigation volume per plot (m3).
# photosynthesis_: net photosynthetic rate Pn (umol CO2 m-2 s-1).
# gs_max_: maximum stomatal conductance (mol H2O m-2 s-1).
# fv_fm_: light-saturated PSII photochemical efficiency (Fv'/Fm').
# PRI_: photochemical reflectance index.
# sample_material_: plant organ sampled for isotope analysis (leaf, tuber).
# sampling_moment_: sampling timing relative to rewatering (BI = before irrigation, AI = after irrigation).
# d13C_discrimination_: carbon-13 isotopic discrimination D13 (per mil).


carob_script <- function(path) {

"
Dataset for: The photosynthesis recovery, a key trait as indicator of an appropriate irrigation criteria in potato crop

Field experiment to compare physiological performance and crop yield among different water stress levels before and after rehidratation periods in UNICA potato variety, to test the reliability of photosynthesis recovery as key trait for indicate an appropriate irrigation criteria, under two irrigation types.
"

	uri <- "doi:10.21223/P3/Q4KNAN"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "irrigation_method;irrigation_regime_",
		response_vars = "fw_tubers;dw_tubers;ground_cover",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Photosynthesis recovery-2017_v2.xlsx"]
	nas <- c("", "NA", "20.3.4")
	r_eco <- carobiner::read.excel(f, sheet="Ecophysiology", na=c("", "NA"))
	r_cc  <- carobiner::read.excel(f, sheet="Canopy cover", na=c("", "NA"))
	r_iso <- carobiner::read.excel(f, sheet="Isotopes", na=c("", "NA"))
	r_rec <- carobiner::read.excel(f, sheet="Revovery isotopes", na=c("", "NA"))
	r_pri <- carobiner::read.excel(f, sheet="PRI", na=c("", "NA"))
	r_irr <- carobiner::read.excel(f, sheet="Irrigation Vol", na=c("", "NA"))
	r_har <- carobiner::read.excel(f, sheet="Harvest", na=nas)
	r_wth <- carobiner::read.excel(f, sheet="Weather", na=c("", "NA"))

	meth_map <- c(DR="drip", FU="furrow")
	reg_map <- c("1"="full irrigation", "2"="gs_max 0.15", "3"="gs_max 0.05")

	plots <- unique(data.frame(
		plot = as.integer(r_har$plot),
		irr = trimws(as.character(r_har$irrigation)),
		trt = trimws(as.character(r_har$treatment))
	))
	plots <- plots[order(plots$plot), ]

	iv_sum <- tapply(as.numeric(r_irr$IV), as.integer(r_irr$plot), sum, na.rm=TRUE)
	iv_cnt <- tapply(as.numeric(r_irr$IV), as.integer(r_irr$plot), function(z) sum(!is.na(z) & z > 0))
	pchar <- as.character(plots$plot)

	d <- data.frame(
		record_id = seq_len(nrow(plots)),
		trial_id = "LaMolina_UNICA_2017",
		plot_id = pchar,
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = TRUE,
		geo_uncertainty = 2000,
		geo_source = "CIP Dataverse bounding box (south and west only)",
		crop = "potato",
		variety = "UNICA",
		yield_part = "tubers",
		treatment = paste(plots$irr, plots$trt, sep="_"),
		irrigation_method = unname(meth_map[plots$irr]),
		irrigation_regime_ = unname(reg_map[plots$trt]),
		irrigated = TRUE,
		irrigation_number = as.integer(unname(iv_cnt[pchar])),
		total_irrigation_volume_m3_ = unname(iv_sum[pchar]),
		planting_date = "2017-07-05",
		harvest_date = "2017-10-10",
		harvest_days = 97L,
		on_farm = FALSE,
		is_survey = FALSE
	)

	rid <- function(p) d$record_id[match(as.character(as.integer(p)), d$plot_id)]

	l_har <- data.frame(
		record_id = rid(r_har$plot),
		DAP = as.integer(r_har$dap),
		order = as.integer(r_har$plant),
		harvest_ = as.integer(r_har$harvest),
		fw_leaves = as.numeric(r_har[["fw-leaf"]]),
		fw_stems = as.numeric(r_har[["fw-stem"]]),
		fw_roots = as.numeric(r_har[["fw- root"]]),
		fw_tubers = as.numeric(r_har[["fw-tuber"]]),
		tubers_per_plant_ = as.numeric(r_har$at),
		dw_leaves = as.numeric(r_har[["dw-leaf"]]),
		dw_stems = as.numeric(r_har[["dw-stem"]]),
		dw_roots = as.numeric(r_har[["dw-root"]]),
		dw_tubers = as.numeric(r_har[["dw-tuber"]])
	)
	l_cc <- data.frame(
		record_id = rid(r_cc$plot),
		DAP = as.integer(r_cc$DAP),
		ground_cover = as.numeric(r_cc$CC)
	)
	l_irr <- data.frame(
		record_id = rid(r_irr$plot),
		date = format(as.Date(r_irr$date), "%Y-%m-%d"),
		DAP = as.integer(r_irr$dap),
		irrigation_volume_m3_ = as.numeric(r_irr$IV)
	)
	l_eco <- data.frame(
		record_id = rid(r_eco$plot),
		date = format(as.Date(r_eco$date), "%Y-%m-%d"),
		DAP = as.integer(r_eco$dap),
		order = as.integer(r_eco$plant),
		photosynthesis_ = as.numeric(r_eco$Pn),
		gs_max_ = as.numeric(r_eco$gs_max),
		fv_fm_ = as.numeric(r_eco[["Fv'/Fm'"]])
	)
	l_pri <- data.frame(
		record_id = rid(r_pri$plot),
		DAP = as.integer(r_pri$dap),
		order = as.integer(r_pri$plant),
		PRI_ = as.numeric(r_pri$PRI)
	)
	l_iso <- data.frame(
		record_id = rid(r_iso$plot),
		DAP = as.integer(r_iso$dap),
		harvest_ = as.integer(r_iso$harvest),
		sample_material_ = tolower(trimws(as.character(r_iso$tm))),
		d13C_discrimination_ = as.numeric(r_iso$D13)
	)
	l_rec <- data.frame(
		record_id = rid(r_rec$plot),
		DAP = as.integer(r_rec$dap),
		sample_material_ = tolower(trimws(as.character(r_rec$tm))),
		sampling_moment_ = trimws(as.character(r_rec$ms)),
		d13C_discrimination_ = as.numeric(r_rec$D13)
	)

	dlong <- carobiner::bindr(l_har, l_cc, l_irr, l_eco, l_pri, l_iso, l_rec)

	# Aggregate hourly Weather (2352 rows) to daily wth
	wdate <- sprintf("%04d-%02d-%02d", as.integer(r_wth$Year), as.integer(r_wth$Month), as.integer(r_wth$Day))
	ud <- sort(unique(wdate))
	agg_fun <- function(v, fn) {
		z <- tapply(as.numeric(v), wdate, function(x) if (all(is.na(x))) NA_real_ else fn(x, na.rm=TRUE))
		unname(z[ud])
	}
	# Circular mean for wind direction (degrees)
	wdir_mean <- tapply(as.numeric(r_wth$Dir), wdate, function(x) {
		x <- x[!is.na(x)]
		if (!length(x)) return(NA_real_)
		rad <- x * pi / 180
		ang <- atan2(mean(sin(rad)), mean(cos(rad))) * 180 / pi
		(ang + 360) %% 360
	})
	wth <- data.frame(
		date = ud,
		temp = agg_fun(r_wth$Ta, mean),
		tmin = agg_fun(r_wth$Ta, min),
		tmax = agg_fun(r_wth$Ta, max),
		rhum = agg_fun(r_wth$RH, mean),
		rhmn = agg_fun(r_wth$RH, min),
		rhmx = agg_fun(r_wth$RH, max),
		# VPD kPa -> hPa (* 10)
		vpd = agg_fun(r_wth$VPD, mean) * 10,
		wspd = agg_fun(r_wth$WS, mean),
		wdir = unname(wdir_mean[ud]),
		# Hourly SR (W/m2) summed over 24 h * 3600 s / 1000 = kJ/m2/day
		srad = agg_fun(r_wth$SR, sum) * 3.6,
		prec = agg_fun(r_wth$PP, sum),
		# Pa in mbar = hPa
		pressure = agg_fun(r_wth$Pa, mean),
		ETo = agg_fun(r_wth$Eo, sum),
		country = "Peru",
		location = "La Molina",
		latitude = -12.076289,
		longitude = -76.948417,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
