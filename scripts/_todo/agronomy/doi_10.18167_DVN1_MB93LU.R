# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sorghum and cowpea grain yields are the open Rendement workbooks, one row per plot, year, and crop. Sorghum grain and straw are Mg/ha in the sheet, times 1000 to kg/ha. Cowpea grain and pod yields are already kg/ha. Shrub dry biomass is Mg/ha, times 1000.
# Tillage is Z (zaï) or Nt (no-tillage). Shrub density codes are NS, LSD, MSD, and HSD. The open files do not give those densities in plants per ha.
# Sorghum and cowpea each have one plot labeled hors bloc, with no block number.
# Sowing and harvest dates are the début dates on Crop-Shrub-Operations. A slash is no date. Country is Burkina Faso from geographicCoverage. Kamboinse is the location. Kadiogo is the geographicCoverage state and is not used as adm1.
# Climat_Daily is a daily station series and is passed as weather. The column that averages Tmin and Tmax is not kept. The string na is missing.

## ISSUES
# Sorghum straw has no fresh or dry statement in the sorghum note, so it is not stored as dmy_residue. There is no straw value for 2022.
# The fertilization sheet gives one mulch mass and one shrub-residue mass per year, not per plot, so it is not joined.
# No coordinates are in the open files. Georef is not among the open files.
# write_files: missing geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer.
# unknown variables: shrub_density_ (shrub_density?), straw_yield_ (yield?), pod_yield_ (yield?). invalid terms: crop: piliostigma. invalid terms: land_prep_method: zai.

## NOT PROCESSED
# Carnet and PMG workbooks are not in the open download.
# Shrub coppice dates and the calculated daily mean temperature.
# Crop-Shrub-Operations_2018-2022.txt: same table as the file of this name that is read.
# Rendement-Niebe_2018-2023.txt: not read. columns: Le fichier "Rendement-Niebe_2018-2022.xlsx" rapporte sur la production et le rendement du niébé de 2018 à 2022. Pour chacune des 24 parcelles élémentaires des blocs et pour la parcelle 26 hors blocs. Le fichier donne le poids (kg),  après séchage au soleil,  des gousses et des grains produits sur la parcelle élémentaire et,  les rendements correspondant en kg/ha (séchage soleil). Ces résultats sont donnés en face de la densité des arbustes (NS=pas d'arbustes; LSD=faible densité d'arbustes; MSD=densité moyenne d'arbustes et; HSD=haute densité d'arbustes) et du travail du sol (Z=zaï et Nt=sans travail du sol) par parcelle et par bloc.. Observation records are missing from d until this file is standardized.
# Rendement-Sorgho_2018-2023.txt: not read. columns: Le fichier "Rendement-Sorgho_2018-2022.xlsx" rapporte les rendements grain et paille du sorgho obtenus annuellement,  de 2018 à 2022,  dans chacune des parcelles élémentaires du dispositif et dans la parcelle 26. . Observation records are missing from d until this file is standardized.
# Shrub-Aboveground-Biomass_2018-2022.txt: same table as the file of this name that is read.


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
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;shrub_density_",
		response_vars = "yield",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 50,
		carob_effort = 0.55
	)

	excel_date <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("/", "", "NA", "na")] <- NA_character_
		as.character(as.Date(as.numeric(x), origin="1899-12-30"))
	}
	prep <- function(code) {
		out <- rep(NA_character_, length(code))
		out[code == "Nt"] <- "none"
		out[code == "Z"] <- "zai"
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

	f1 <- ff[basename(ff) == "Crop-Shrub-Operations_2018-2022.xlsx"]
	sorg_op <- read_sheet(f1, "Sorghum")
	cow_op <- read_sheet(f1, "Cowpea")
	sow_s <- excel_date(sorg_op[3:nrow(sorg_op), 4])
	harv_s <- excel_date(sorg_op[3:nrow(sorg_op), 12])
	names(sow_s) <- as.character(sorg_op[3:nrow(sorg_op), 1])
	names(harv_s) <- names(sow_s)
	sow_c <- excel_date(cow_op[3:nrow(cow_op), 4])
	harv_c <- excel_date(cow_op[3:nrow(cow_op), 12])
	names(sow_c) <- as.character(cow_op[3:nrow(cow_op), 1])
	names(harv_c) <- names(sow_c)

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
		one <- data.frame(
			trial_id = paste("Kamboinse", yrg[i]),
			rep = block_id(id$block),
			plot_id = trimws(as.character(id$plot)),
			crop = "sorghum",
			intercrops = "cowpea",
			intercropped = TRUE,
			land_prep_method = prep(id$tillage),
			shrub_density_ = trimws(as.character(id$shrub)),
			treatment = paste(id$tillage, id$shrub, sep="-"),
			yield = gy[, i] * 1000,
			yield_part = "grain",
			straw_yield_ = straw * 1000,
			planting_date = unname(sow_s[as.character(yrg[i])]),
			harvest_date = unname(harv_s[as.character(yrg[i])])
		)
		ds <- rbind(ds, one)
	}

	f3 <- ff[basename(ff) == "Rendement-niebe_2018-2022.xlsx"]
	rc <- read_sheet(f3, "2018-22")
	idc <- rc[3:nrow(rc), 1:4]
	names(idc) <- c("block", "plot", "tillage", "shrub")
	# Four measures per year: pod mass, pod yield, grain mass, grain yield. Years start at column 5.
	dc <- NULL
	for (k in 0:4) {
		year <- 2018 + k
		base <- 5 + k * 4
		one <- data.frame(
			trial_id = paste("Kamboinse", year),
			rep = block_id(idc$block),
			plot_id = trimws(as.character(idc$plot)),
			crop = "cowpea",
			intercrops = "sorghum",
			intercropped = TRUE,
			land_prep_method = prep(idc$tillage),
			shrub_density_ = trimws(as.character(idc$shrub)),
			treatment = paste(idc$tillage, idc$shrub, sep="-"),
			yield = as.numeric(unlist(rc[3:nrow(rc), base + 3])),
			yield_part = "seed",
			pod_yield_ = as.numeric(unlist(rc[3:nrow(rc), base + 1])),
			planting_date = unname(sow_c[as.character(year)]),
			harvest_date = unname(harv_c[as.character(year)]),
			straw_yield_ = NA_real_
		)
		dc <- rbind(dc, one)
	}

	f4 <- ff[basename(ff) == "Shrub-Aboveground-Biomass_2018-2022.xlsx"]
	rb <- read_sheet(f4, "Annual_Aboveground_Dry-Biomass")
	idb <- rb[3:nrow(rb), 1:4]
	names(idb) <- c("block", "plot", "tillage", "shrub")
	bm <- apply(rb[3:nrow(rb), 5:9], 2, as.numeric)
	yrb <- as.integer(as.numeric(unlist(rb[2, 5:9])))
	db <- NULL
	for (i in seq_along(yrb)) {
		one <- data.frame(
			trial_id = paste("Kamboinse", yrb[i]),
			rep = block_id(idb$block),
			plot_id = trimws(as.character(idb$plot)),
			crop = "piliostigma",
			intercrops = "sorghum;cowpea",
			intercropped = TRUE,
			land_prep_method = prep(idb$tillage),
			shrub_density_ = trimws(as.character(idb$shrub)),
			treatment = paste(idb$tillage, idb$shrub, sep="-"),
			yield = bm[, i] * 1000,
			yield_part = "aboveground biomass",
			yield_moisture = 0,
			planting_date = unname(sow_s[as.character(yrb[i])]),
			harvest_date = NA_character_,
			straw_yield_ = NA_real_,
			pod_yield_ = NA_real_
		)
		db <- rbind(db, one)
	}

	ds$yield_moisture <- NA_real_
	ds$pod_yield_ <- NA_real_
	dc$yield_moisture <- NA_real_
	d <- rbind(ds, dc, db)
	d$country <- "Burkina Faso"
	d$location <- "Kamboinse"
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	f5 <- ff[basename(ff) == "Climat_Daily_2018-2022.xlsx"]
	r <- carobiner::read.excel(f5, sheet="donn jour", na=c("na", "NA"))
	wth <- data.frame(
		station_id = "Kamboinse",
		station_name = "Kamboinse",
		country = "Burkina Faso",
		date = as.character(as.Date(r[[1]], tz="UTC")),
		tmin = as.numeric(r$TMin),
		tmax = as.numeric(r$Tmax),
		rhmn = as.numeric(r[["Hum Min"]]),
		rhmx = as.numeric(r[["Hum max"]]),
		prec = as.numeric(r[["pluie (mm)"]])
	)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
