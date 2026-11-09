# R script for "carob"
# license: GPL (>=3)

## NOTES
# Maize and cotton in parklands of northern Cameroon. Country is Cameroon and adm1 is North, from geographicCoverage. The tar has both data_raw and raw_data; data_raw is used.
# Faidherbia folders have no tree column. The README names that parkland Faidherbia albida. The mixed folder has no locality column. The README and site_description place it at Bamé.
# Coordinates are degrees and minutes in site_description.csv (Langui 9°08'N 13°06'E, Djiddé 9°21'N 13°39'E, Bamé 9°04'N 13°35'E).
# Yield columns are already kg/ha. total_biomass_kg_ha is about 5 next to grain yields of about 2500, so it is not treated as kg/ha.
# fertilisation_levels contains site names, not a nutrient rate.
# Dictionary SPAD_90 text says 60 days and SPAD_60 says 90 days. The column name is used.
# Microclimate t_mean is the dictionary's mean temperature in C. Rain in that file is present or absent, not millimetres, so it is not precipitation. Solar radiation has no unit in the dictionary.

## ISSUES
# missing variables: yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer
# unknown variables: tree_species_, plot_position_, ginning_pct_
# invalid terms: yield_part: bolls
# NA detected: yield, latitude, longitude
# unknown variable: spad_ (SPAD?)

## NOT PROCESSED
# raw_data copy of the workbooks.
# fiber_quality workbooks.
# soil_nutrient and soil_water workbooks.
# trees_characteristics workbooks.
# total_biomass_kg_ha, leaf area, SLA, leaf dry weight, and cotton density. Units for density and biomass do not match the yield scale.
# Solar radiation sheet. No unit.
# Microclimate rain flag (A or P).


## NEW VARIABLES
# tree_species_: parkland tree. Faidherbia albida is the folder species when the sheet has no tree column.
# plot_position_: 0.5r, 2.5r, or 5r from the trunk.
# spad_: SPAD reading at the DAP of that long row.
# ginning_pct_: cotton ginning percentage, as stored.


carob_script <- function(path) {

"
Assessing the impact of agroforestry trees species on microclimate, fiber quality, maize and cotton productivity in the north Cameroon

This dataset describes how different tree species (monospecific vs. mixed) influence microclimate regulation, soil properties, crop productivity, and fiber quality. It includes microclimatic, agronomic, and fiber quality data collected under and outside the tree canopy across different parkland models. This dataset is part of the [DeSIRA INNOVACC](https://capacity4dev.europa.eu/info/innovacc-cameroun_en) Project.
"

	uri <- "doi:10.18167/DVN1/PD3KQB"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "DeSIRA INNOVACC",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "README.txt"]
	dest <- file.path(tempdir(), "PD3KQB")
	dir.create(dest, FALSE, TRUE)
	untar(file.path(dirname(f1), "agroforestry_data.tar"), exdir=dest)
	root <- file.path(dest, "agroforestry_data", "data_raw")

	# site_description.csv, degrees and minutes, no seconds.
	site <- data.frame(
		key = c("langui", "djidde", "bame"),
		location = c("Langui", "Djiddé", "Bamé"),
		latitude = c(9 + 8/60, 9 + 21/60, 9 + 4/60),
		longitude = c(13 + 6/60, 13 + 39/60, 13 + 35/60),
		elevation = c(190, 190, 252)
	)
	skey <- function(x) chartr("é", "e", tolower(trimws(as.character(r1))))
	to_date <- function(x) {
		if (is.null(r1)) return(NA_character_)
		if (inherits(r1, "POSIXt") || inherits(r1, "Date")) return(as.character(as.Date(r1)))
		r1 <- trimws(as.character(r1))
		r1[r1 %in% c("", "NA")] <- NA_character_
		out <- rep(NA_character_, length(r1))
		dash <- !is.na(r1) & grepl("-", r1, fixed=TRUE)
		slash <- !is.na(r1) & grepl("/", r1, fixed=TRUE)
		if (any(dash)) out[dash] <- as.character(as.Date(r1[dash]))
		if (any(slash)) out[slash] <- as.character(as.Date(r1[slash], "%d/%m/%Y"))
		# Mixed-parkland cotton stores flowering and maturity as Excel serials.
		serial <- !is.na(r1) & !dash & !slash
		if (any(serial)) out[serial] <- as.character(as.Date(as.numeric(r1[serial]), origin="1899-12-30"))
		out
	}
	num <- function(x) {
		r1 <- trimws(as.character(r1))
		r1[tolower(r1) %in% c("", "na", "#n/a")] <- NA_character_
		as.numeric(r1)
	}

	read_crop <- function(f, crop, part) {
		r <- carobiner::read.excel(f, sheet=2)
		names(r) <- trimws(names(r))
		park <- basename(dirname(f))
		if ("locality" %in% names(r)) {
			loc <- trimws(as.character(r$locality))
		} else {
			# README: mixed species parkland is the Bamé site.
			loc <- rep("Bame", nrow(r))
		}
		loc[is.na(loc) | loc == ""] <- NA_character_
		if (grepl("Mixed", park) & all(is.na(loc))) loc <- rep("Bame", nrow(r))
		if ("tree_species" %in% names(r)) {
			tree <- trimws(as.character(r$tree_species))
		} else {
			tree <- rep("Faidherbia albida", nrow(r))
		}
		plant <- if ("sowing_date" %in% names(r)) to_date(r$sowing_date) else rep(NA_character_, nrow(r))
		flower <- if ("flowering_date" %in% names(r)) to_date(r$flowering_date) else rep(NA_character_, nrow(r))
		harvest <- if ("maturity_date" %in% names(r)) to_date(r$maturity_date) else rep(NA_character_, nrow(r))
		yr <- substr(plant, 1, 4)
		if ("year" %in% names(r)) {
			yy <- as.character(r$year)
			yr[!is.na(yy) & nzchar(yy)] <- yy[!is.na(yy) & nzchar(yy)]
		}
		h60 <- if ("height_60days_cm" %in% names(r)) num(r$height_60days_cm) else rep(NA_real_, nrow(r))
		h90col <- if ("height_90days_cm" %in% names(r)) "height_90days_cm" else if ("height_cm_90_cotton" %in% names(r)) "height_cm_90_cotton" else NA
		h90 <- if (!is.na(h90col)) num(r[[h90col]]) else rep(NA_real_, nrow(r))
		s60 <- if ("SPAD_60" %in% names(r)) num(r$SPAD_60) else rep(NA_real_, nrow(r))
		s90 <- if ("SPAD_90" %in% names(r)) num(r$SPAD_90) else rep(NA_real_, nrow(r))
		gin <- NA_real_
		gcol <- names(r)[grepl("Ginning", names(r))]
		if (length(gcol) == 1) gin <- num(r[[gcol]])
		seed <- if ("weight_1000_grains_g" %in% names(r)) num(r$weight_1000_grains_g) else rep(NA_real_, nrow(r))
		pos <- if ("plot_position" %in% names(r)) trimws(as.character(r$plot_position)) else rep(NA_character_, nrow(r))
		id <- paste(crop, park, seq_len(nrow(r)), sep="_")
		d <- data.frame(
			plot_id = id,
			loc_key = skey(loc),
			tree_species_ = tree,
			plot_position_ = pos,
			crop = crop,
			yield_part = part,
			yield = num(r$yield_kg_ha),
			seed_weight = seed,
			ginning_pct_ = gin,
			planting_date = plant,
			flowering_date = flower,
			harvest_date = harvest,
			yr = yr
		)
		h1 <- data.frame(plot_id=id, DAP=60L, plant_height=h60, spad_=s60)
		h2 <- data.frame(plot_id=id, DAP=90L, plant_height=h90, spad_=s90)
		h <- rbind(h1, h2)
		h <- h[!is.na(h$plant_height) | !is.na(h$spad_), ]
		list(d=d, h=h)
	}

	files_m <- list.files(root, pattern="data_maize.xlsx", recursive=TRUE, full.names=TRUE)
	files_c <- list.files(root, pattern="data_cotton.xlsx", recursive=TRUE, full.names=TRUE)
	parts <- c(lapply(files_m, read_crop, crop="maize", part="grain"), lapply(files_c, read_crop, crop="cotton", part="bolls"))
	d <- do.call(rbind, lapply(parts, function(z) z$d))
	dlong <- do.call(rbind, lapply(parts, function(z) z$h))
	d <- merge(d, site, by.x="loc_key", by.y="key", all.x=TRUE)
	d$country <- "Cameroon"
	d$adm1 <- "North"
	d$trial_id <- paste(ifelse(is.na(d$location), "unknown", d$location), d$crop, d$yr)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- !is.na(d$latitude)
	d$loc_key <- NULL
	d$yr <- NULL
	dlong <- merge(dlong, d[, c("plot_id", "trial_id")], by="plot_id", all.x=TRUE)
	dlong <- dlong[!is.na(dlong$trial_id), ]

	clims <- list.files(root, pattern="microclimate.xlsx", recursive=TRUE, full.names=TRUE)
	wth <- NULL
	for (f in clims) {
		r1 <- carobiner::read.excel(f, sheet="temperature_and_humidity")
		names(r1) <- trimws(names(r1))
		# Mixed parkland sheet has no locality column. README places that site at Bamé.
		loc <- if ("locality" %in% names(r1)) trimws(as.character(r1$locality)) else rep("Bame", nrow(r1))
		station <- skey(loc)
		for (nm in c("tree_number", "plot_position", "orientation", "repetition")) {
			if (nm %in% names(r1)) station <- paste(station, trimws(as.character(r1[[nm]])), sep="_")
		}
		w <- data.frame(
			station_id = station,
			loc_key = skey(loc),
			date = as.character(as.Date(r1$date)),
			tmin = num(r1$t_min),
			tmax = num(r1$t_max),
			temp = num(r1$t_mean),
			rhmn = num(r1$h_min),
			rhmx = num(r1$h_max),
			rhum = num(r1$h_mean)
		)
		wth <- rbind(wth, w)
	}
	wth <- merge(wth, site[, c("key", "latitude", "longitude")], by.x="loc_key", by.y="key", all.x=TRUE)
	wth$country <- "Cameroon"
	wth$geo_from_source <- !is.na(wth$latitude)
	wth$loc_key <- NULL
	wth <- wth[!duplicated(wth), ]

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
