# R script for "carob"
# license: GPL (>=3)

## NOTES
# Crop-News long-term agroforestry experiment at CREAF/INERA Kamboinsé station, Burkina Faso (2018-2022): sorghum (variety Sariasso-18), cowpea (variety Beng-Raaga), and native evergreen woody shrub (Piliostigma sp.) under 2 tillage methods (Z = zaï pits -> "basins"; Nt = no-tillage -> "none") x 4 shrub density levels (NS = no shrub, LSD = low, MSD = medium, HSD = high).
# Coordinates (12.4467 N, -1.5625 E) are for CREAF/INERA Kamboinsé station (geo_from_source = FALSE, geo_uncertainty = 2000 m); verify exact trial coordinates if available.
# Per the .txt metadata files, each elementary plot had 24 plots across 4 blocks plus plot 26 ("hors bloc", rep = NA). Cowpea was sown on half of each elementary plot (141.44 m2) intercropped with sorghum; no chemical fertilizer was applied (N/P/K_fertilizer = 0), while each plot received its own sorghum straw and coppiced shrub biomass as organic mulch (OM_used = TRUE).
# CRITICAL SOURCE FIX: In Rendement-niebe_2018-2022.xlsx, columns for 2018-2019 are ordered (poids_gousses, rdt gousses, poids grain, rdt grain) whereas 2020-2022 are ordered (poids_gousses, poids grain, rdt gousses, rdt grain). Columns are matched by row-2 headers rather than fixed offsets so pod_yield_ is correctly in kg/ha for all 5 years.
# Sorghum grain and straw (Mg/ha * 1000 -> kg/ha; straw mapped to y_residue, missing in 2022), cowpea grain and sun-dried pod yields (kg/ha), and annual shrub dry aboveground biomass (Mg/ha * 1000 -> kg/ha, yield_moisture = 0) are stored in wide d. Per-coppicing shrub dry biomass and dates (Aboveground_DB_at_Coppicing sheet; 2 coppicings/yr in 2018-2020, 3/yr in 2021-2022) are stored in long linked via record_id.
# Sowing, weeding (3 weedings/season), and harvest dates are from Crop-Shrub-Operations_2018-2022.xlsx. Daily station weather (Climat_Daily_2018-2022.xlsx) is passed via wth.

## ISSUES
# Exact numeric shrub densities (plants/ha) for NS, LSD, MSD, and HSD are not in the open files; stored as categorical shrub_density_.
# Piliostigma sp. is not yet in terminag's crop vocabulary (retained as "piliostigma").
# Sorghum and cowpea grain moisture contents are not reported in the open files (cowpea was sun-dried); yield_moisture is NA for sorghum and cowpea (0 for oven-dry shrub biomass).
# Cowpea 2018 harvest date and shrub harvest date (multiple coppicings in long) are not single dates in the operations sheet, so harvest_date is NA for those rows.
# Coordinates for Kamboinsé station are estimated from station location; author should verify exact field coordinates.

## NEW VARIABLES
# tillage_code_: original tillage code ("Z" = zaï pits, "Nt" = no-tillage)
# shrub_density_: woody shrub density treatment level ("NS", "LSD", "MSD", "HSD")
# pod_yield_: sun-dried cowpea pod yield (kg/ha)
# coppice_number_: sequential shrub coppicing event within the year (1, 2, or 3)
# coppice_dmy_: dry aboveground shrub biomass harvested at each coppicing event (kg/ha)

## NOT PROCESSED
# Crop-Shrub-Operations_2018-2022.xlsx (Fertilization sheet): annual trial-wide averages of sorghum residue and shrub mulch (plot-specific sorghum straw and shrub biomass are already in d).
# Climat_Daily_2018-2022.xlsx: calculated Tmoy = (Tmin+Tmax)/2.
# Crop-Shrub-Operations_2018-2022.txt, Rendement-Niebe_2018-2023.txt, Rendement-Sorgho_2018-2023.txt, Shrub-Aboveground-Biomass_2018-2022.txt: French documentation/README files describing the Excel workbooks, not observation tables.


carob_script <- function(path) {

"
Crop-News: Experimenting intercropping Sorghum sp., cowpea and native evergreen woody shrub (Piliostigma sp) at Kamboinse - Burkina Faso - 2018-2022

Intercropping sorghum sp., cowpea and native evergreen woody shrub was tested during this period, using 2 soil tillage methods : Zai vs No-tillage, and 4 shrub densities (0, low, medium and high).
"

	uri <- "doi:10.18167/DVN1/MB93LU"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIRAD",
		publication = NA,
		project = "Crop-News",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "land_prep_method;shrub_density_",
		response_vars = "yield;y_residue;pod_yield_;dmy_total",
		carob_LLM = "Gemini 4 Neon",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_completion = 90,
		carob_effort = 0
	)

	excel_date <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("/", "", "NA", "na")] <- NA_character_
		as.character(as.Date(as.numeric(x), origin="1899-12-30"))
	}
	prep <- function(code) {
		code <- trimws(as.character(code))
		out <- rep(NA_character_, length(code))
		out[code == "Nt"] <- "none"
		# Zaï pits mapped to terminag's "basins" (tillage_code_ retains original "Z"/"Nt")
		out[code == "Z"] <- "basins"
		out
	}
	block_id <- function(x) {
		x <- trimws(as.character(x))
		x[tolower(x) %in% c("hors bloc", "hors blocs")] <- NA_character_
		as.integer(x)
	}
	read_sheet <- function(f, sheet) {
		as.data.frame(readxl::read_excel(f, sheet=sheet, col_names=FALSE, .name_repair="minimal"))
	}

	join_dates <- function(d1, d2, d3) {
		mapply(function(a, b, c) {
			v <- c(a, b, c)
			v <- v[!is.na(v) & nzchar(v)]
			if (length(v) == 0) NA_character_ else paste(v, collapse=";")
		}, d1, d2, d3, USE.NAMES=FALSE)
	}

	# Operations dates (sowing, 3 weedings, harvest)
	f1 <- ff[basename(ff) == "Crop-Shrub-Operations_2018-2022.xlsx"]
	sorg_op <- read_sheet(f1, "Sorghum")
	cow_op  <- read_sheet(f1, "Cowpea")
	yr_s <- as.character(sorg_op[3:nrow(sorg_op), 1])
	sow_s  <- setNames(excel_date(sorg_op[3:nrow(sorg_op), 4]), yr_s)
	harv_s <- setNames(excel_date(sorg_op[3:nrow(sorg_op), 12]), yr_s)
	weed_s <- setNames(join_dates(
		excel_date(sorg_op[3:nrow(sorg_op), 6]),
		excel_date(sorg_op[3:nrow(sorg_op), 8]),
		excel_date(sorg_op[3:nrow(sorg_op), 10])), yr_s)

	yr_c <- as.character(cow_op[3:nrow(cow_op), 1])
	sow_c  <- setNames(excel_date(cow_op[3:nrow(cow_op), 4]), yr_c)
	harv_c <- setNames(excel_date(cow_op[3:nrow(cow_op), 12]), yr_c)
	weed_c <- setNames(join_dates(
		excel_date(cow_op[3:nrow(cow_op), 6]),
		excel_date(cow_op[3:nrow(cow_op), 8]),
		excel_date(cow_op[3:nrow(cow_op), 10])), yr_c)

	# 1) Sorghum (grain and straw, Mg/ha -> kg/ha)
	f2 <- ff[basename(ff) == "Rendement-Sorgho_2018-2022.xlsx"]
	rs <- read_sheet(f2, "yield_2018-2022")
	id <- rs[3:nrow(rs), 1:4]
	names(id) <- c("block", "plot", "tillage", "shrub")
	gy <- apply(rs[3:nrow(rs), 5:9], 2, as.numeric)
	yrg <- as.integer(as.numeric(unlist(rs[2, 5:9])))
	st <- apply(rs[3:nrow(rs), 10:13], 2, as.numeric)
	yrs <- as.integer(as.numeric(unlist(rs[2, 10:13])))
	ds <- NULL
	for (i in seq_along(yrg)) {
		straw <- rep(NA_real_, nrow(id))
		j <- match(yrg[i], yrs)
		if (!is.na(j)) straw <- st[, j]
		sh_code <- trimws(as.character(id$shrub))
		one <- data.frame(
			year = yrg[i],
			trial_id = paste("Kamboinse", yrg[i], sep="_"),
			rep = block_id(id$block),
			plot_id = paste(yrg[i], trimws(as.character(id$plot)), sep="_"),
			plot_num = trimws(as.character(id$plot)),
			plot_area = NA_real_,
			crop = "sorghum",
			variety = "Sariasso-18",
			intercrops = ifelse(sh_code == "NS", "cowpea", "cowpea;piliostigma"),
			intercropped = TRUE,
			land_prep_method = prep(id$tillage),
			tillage_code_ = trimws(as.character(id$tillage)),
			shrub_density_ = sh_code,
			treatment = paste(trimws(id$tillage), sh_code, sep="-"),
			yield = gy[, i] * 1000,
			yield_part = "grain",
			yield_moisture = NA_real_,
			y_residue = straw * 1000,
			dmy_total = NA_real_,
			pod_yield_ = NA_real_,
			planting_date = unname(sow_s[as.character(yrg[i])]),
			harvest_date = unname(harv_s[as.character(yrg[i])]),
			weeding_dates = unname(weed_s[as.character(yrg[i])])
		)
		ds <- rbind(ds, one)
	}

	# 2) Cowpea (sun-dried grain and pod yields, kg/ha; match columns by header because 2020-2022 swap column order)
	f3 <- ff[basename(ff) == "Rendement-niebe_2018-2022.xlsx"]
	rc <- read_sheet(f3, "2018-22")
	idc <- rc[3:nrow(rc), 1:4]
	names(idc) <- c("block", "plot", "tillage", "shrub")
	hdr2 <- tolower(trimws(as.character(unlist(rc[2, ]))))
	dc <- NULL
	for (k in 0:4) {
		year <- 2018 + k
		cols_k <- (5 + k * 4):(8 + k * 4)
		h_k <- hdr2[cols_k]
		col_gy  <- cols_k[grepl("rdt.*grain", h_k)][1]
		col_pod <- cols_k[grepl("rdt.*gouss", h_k)][1]
		sh_code <- trimws(as.character(idc$shrub))
		one <- data.frame(
			year = year,
			trial_id = paste("Kamboinse", year, sep="_"),
			rep = block_id(idc$block),
			plot_id = paste(year, trimws(as.character(idc$plot)), sep="_"),
			plot_num = trimws(as.character(idc$plot)),
			# Half of elementary plot (13 rows x 17/16 cols = 141.44 m2 per Rendement-Niebe .txt)
			plot_area = 141.44,
			crop = "cowpea",
			variety = "Beng-Raaga",
			intercrops = ifelse(sh_code == "NS", "sorghum", "sorghum;piliostigma"),
			intercropped = TRUE,
			land_prep_method = prep(idc$tillage),
			tillage_code_ = trimws(as.character(idc$tillage)),
			shrub_density_ = sh_code,
			treatment = paste(trimws(idc$tillage), sh_code, sep="-"),
			yield = as.numeric(unlist(rc[3:nrow(rc), col_gy])),
			yield_part = "seed",
			yield_moisture = NA_real_,
			y_residue = NA_real_,
			dmy_total = NA_real_,
			pod_yield_ = as.numeric(unlist(rc[3:nrow(rc), col_pod])),
			planting_date = unname(sow_c[as.character(year)]),
			harvest_date = unname(harv_c[as.character(year)]),
			weeding_dates = unname(weed_c[as.character(year)])
		)
		dc <- rbind(dc, one)
	}

	# 3) Piliostigma shrub annual dry aboveground biomass (Mg/ha -> kg/ha)
	f4 <- ff[basename(ff) == "Shrub-Aboveground-Biomass_2018-2022.xlsx"]
	rb <- read_sheet(f4, "Annual_Aboveground_Dry-Biomass")
	idb <- rb[3:nrow(rb), 1:4]
	names(idb) <- c("block", "plot", "tillage", "shrub")
	bm <- apply(rb[3:nrow(rb), 5:9], 2, as.numeric)
	yrb <- as.integer(as.numeric(unlist(rb[2, 5:9])))
	db <- NULL
	for (i in seq_along(yrb)) {
		sh_code <- trimws(as.character(idb$shrub))
		one <- data.frame(
			year = yrb[i],
			trial_id = paste("Kamboinse", yrb[i], sep="_"),
			rep = block_id(idb$block),
			plot_id = paste(yrb[i], trimws(as.character(idb$plot)), sep="_"),
			plot_num = trimws(as.character(idb$plot)),
			plot_area = NA_real_,
			crop = "piliostigma",
			variety = "Piliostigma sp.",
			intercrops = "sorghum;cowpea",
			intercropped = TRUE,
			land_prep_method = prep(idb$tillage),
			tillage_code_ = trimws(as.character(idb$tillage)),
			shrub_density_ = sh_code,
			treatment = paste(trimws(idb$tillage), sh_code, sep="-"),
			yield = bm[, i] * 1000,
			yield_part = "aboveground biomass",
			yield_moisture = 0,
			y_residue = NA_real_,
			dmy_total = bm[, i] * 1000,
			pod_yield_ = NA_real_,
			planting_date = unname(sow_s[as.character(yrb[i])]),
			harvest_date = NA_character_,
			weeding_dates = unname(weed_s[as.character(yrb[i])])
		)
		db <- rbind(db, one)
	}

	d <- rbind(ds, dc, db)
	d$record_id <- seq_len(nrow(d))
	d$country <- "Burkina Faso"
	d$location <- "Kamboinse"
	# CREAF/INERA Kamboinsé station (12°26'48"N, 1°33'45"W)
	d$latitude <- 12.4467
	d$longitude <- -1.5625
	d$geo_from_source <- FALSE
	d$geo_uncertainty <- 2000
	d$fertilizer_used <- FALSE
	d$fertilizer_type <- "none"
	d$N_fertilizer <- 0
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	d$OM_used <- TRUE
	d$OM_type <- "crop residue"
	d$weeding_done <- TRUE
	d$weeding_times <- 3L
	d$irrigated <- FALSE
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	# Long table: per-coppicing shrub dry biomass linked to piliostigma records via record_id
	rcop <- read_sheet(f4, "Aboveground_DB_at_Coppicing")
	cop_plots <- trimws(as.character(rcop[4:nrow(rcop), 2]))
	cop_lbl <- trimws(as.character(unlist(rcop[2, 5:ncol(rcop)])))
	cop_dt  <- excel_date(unlist(rcop[3, 5:ncol(rcop)]))
	cop_val <- apply(rcop[4:nrow(rcop), 5:ncol(rcop)], 2, as.numeric)
	dlong <- NULL
	sh_key <- d[d$crop == "piliostigma", c("record_id", "trial_id", "plot_id", "year", "plot_num")]
	for (j in seq_along(cop_lbl)) {
		yr_j <- as.integer(substr(cop_dt[j], 1, 4))
		cnum <- as.integer(sub("^rec_([0-9]+).*", "\\1", cop_lbl[j]))
		tmp <- data.frame(
			year = yr_j,
			plot_num = cop_plots,
			date = cop_dt[j],
			coppice_number_ = cnum,
			coppice_dmy_ = cop_val[, j] * 1000
		)
		tmp <- merge(tmp, sh_key, by=c("year", "plot_num"), all.x=TRUE)
		tmp <- tmp[!is.na(tmp$record_id) & !is.na(tmp$coppice_dmy_),
			c("record_id", "trial_id", "plot_id", "date", "coppice_number_", "coppice_dmy_")]
		dlong <- rbind(dlong, tmp)
	}

	d$year <- NULL
	d$plot_num <- NULL

	# Daily weather at Kamboinsé station
	f5 <- ff[basename(ff) == "Climat_Daily_2018-2022.xlsx"]
	rw <- carobiner::read.excel(f5, sheet="donn jour", na=c("na", "NA"))
	wth <- data.frame(
		station_id = "Kamboinse",
		station_name = "Kamboinse",
		country = "Burkina Faso",
		latitude = 12.4467,
		longitude = -1.5625,
		geo_from_source = FALSE,
		date = as.character(as.Date(rw[[1]], tz="UTC")),
		tmin = as.numeric(rw$TMin),
		tmax = as.numeric(rw$Tmax),
		rhmn = as.numeric(rw[["Hum Min"]]),
		rhmx = as.numeric(rw[["Hum max"]]),
		prec = as.numeric(rw[["pluie (mm)"]])
	)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
