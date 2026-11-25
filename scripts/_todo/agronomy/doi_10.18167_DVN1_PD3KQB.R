# R script for "carob"
# license: GPL (>=3)

## NOTES
# Agroforestry parkland study (DeSIRA INNOVACC project) on maize and cotton in northern Cameroon (2023-2024) across two parkland models: Faidherbia albida-dominated parklands at Langui and Djiddé, and a mixed-species parkland at Bamé (Daniellia oliveri, Prosopis africana, Senna siamea, Vitellaria paradoxa, Faidherbia albida).
# Coordinates, elevation, soil type, soil texture, tree density (trees/ha), and crop rotation ("maize;cotton") are from site_description.csv (Langui 9°08'N 13°06'E, 190 m, Lixisol, loamy sand; Djiddé 9°21'N 13°39'E, 190 m, Luvisol, sandy loam; Bamé 9°04'N 13°35'E, 252 m, Lixisol, loamy sand).
# Crop measurements are recorded at three radial positions from the tree trunk: 0.5r (under canopy), 2.5r, and 5r (outside canopy).
# Yield columns are in kg/ha. In Faidherbia maize 2023, total_biomass_kg_ha has values 1.46-13.14 (clearly t/ha next to grain yields of 191-5541 kg/ha) and is multiplied by 1000; in Mixed parkland maize 2024, values are 1133-5759 (already kg/ha). Cotton Density_at_harvest (1.46-4.38 plants/m2) is multiplied by 10000 for plant_density.
# Cotton yield is seed cotton (with ginning_pct_ around 38-42%); yield_part is set to "lint" for lack of a "seed cotton" term in terminag.
# Soil mineral nitrogen (soil_NH4, soil_NO3, soil_N in mg/kg from soil_nutrient.xlsx), tree height and crown radius (trees_characteristics.xlsx), and cotton HVI fiber quality (fiber_quality.xlsx) are joined onto wide d.
# Repeated 60- and 90-DAP plant height and SPAD readings (column names SPAD_60/SPAD_90 used over swapped dictionary labels) plus multi-depth monthly soil water (% AWC at 20 and 40 cm from soil_water.xlsx) are stored in long linked via record_id.
# Daily microclimate (tmin, tmax, temp, rhmn, rhmx, rhum from microclimate.xlsx temperature_and_humidity sheets) is passed via wth.

## ISSUES
# Three empty trailing rows in Faidherbia data_cotton.xlsx (all NA) are dropped; one Faidherbia maize plot has NA yield.
# Mineral fertilizer rates (N, P, K) are not reported (fertilisation_levels contains site names in Faidherbia maize and integer codes in Mixed cotton without a codebook); triggers missing variables for N/P/K_fertilizer and yield_moisture.
# In Mixed parkland cotton 2023, a few rows have emergence_date recorded earlier than sowing_date (e.g., 2023-06-10 vs 2023-06-14); kept as recorded.
# Faidherbia maize 2023 has no sowing or harvest dates in the sheet (planting_date set to "2023", harvest_date is NA).
# Some soil_NH4 and soil_NO3 readings are 0 mg/kg (< terminag valid_min of 0.01); kept as measured.
# Microclimate solar radiation sheet has no unit and rain is only an A/P presence flag, so neither is included in wth.

## NEW VARIABLES
# parkland_type_: parkland model ("Faidherbia albida dominated" or "mixed species")
# tree_species_: parkland shade tree species
# tree_height_: height of the sampled parkland tree (m)
# tree_crown_radius_: crown radius of the sampled parkland tree (m)
# plot_position_: radial distance from tree trunk in crown radii ("0.5r", "2.5r", "5r")
# plot_distance_m_: physical distance from tree trunk (m)
# boll_density_: cotton boll density at harvest (bolls/m2)
# boll_weight_: mean cotton boll weight (g)
# ginning_pct_: cotton ginning out-turn percentage (%)
# leaf_area_: measured plant leaf area (cm2)
# leaf_dry_mass_: measured plant leaf dry mass (g)
# SLA_: specific leaf area (cm2/g)
# fiber_SCI_: HVI spinning consistency index
# fiber_micronaire_: HVI cotton micronaire
# fiber_length_: HVI cotton fiber length (mm)
# fiber_strength_: HVI cotton fiber strength (g/tex)
# fiber_elongation_: HVI cotton fiber elongation (%)
# soil_water_awc_: soil water content as percent of available water capacity (% AWC)

## NOT PROCESSED
# raw_data/ duplicate copy of data_raw/ workbooks inside agroforestry_data.tar.
# data_cotton.xlsx: number_of_inter_nodes, Number_of_the_first_fruiting_branch, Number_of_fruiting_branch, Number_of_leaf_at_harvest.
# fiber_quality.xlsx: MI, UI_%, SFI_0.5inch, reflectance_of_the_fiber, yellow_index, CGrd_upland, Tr Cnt, Tr Ar.
# microclimate.xlsx: solar radiation sheet (no unit stated) and rain A/P presence flag.


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
		data_type = "on-farm experiment",
		treatment_vars = "tree_species_;plot_position_",
		response_vars = "yield;dmy_total;plant_height;SPAD;seed_weight",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "README.txt"]
	dest <- file.path(tempdir(), "PD3KQB")
	dir.create(dest, FALSE, TRUE)
	untar(file.path(dirname(f1), "agroforestry_data.tar"), exdir=dest)
	root <- file.path(dest, "agroforestry_data", "data_raw")

	# From metadata/site_description.csv (degrees and minutes)
	site <- data.frame(
		key = c("langui", "djidde", "bame"),
		location = c("Langui", "Djiddé", "Bamé"),
		parkland_type_ = c("Faidherbia albida dominated", "Faidherbia albida dominated", "mixed species"),
		tree_density = c(30, 33, 20),
		latitude = c(9 + 8/60, 9 + 21/60, 9 + 4/60),
		longitude = c(13 + 6/60, 13 + 39/60, 13 + 35/60),
		elevation = c(190, 190, 252),
		soil_type = c("Lixisol", "Luvisol", "Lixisol"),
		soil_texture = c("loamy sand", "sandy loam", "loamy sand")
	)

	skey <- function(x) chartr("éèê", "eee", tolower(trimws(as.character(x))))
	to_date <- function(x) {
		if (is.null(x)) return(NA_character_)
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		s <- trimws(as.character(x))
		s[s %in% c("", "NA", "na")] <- NA_character_
		out <- rep(NA_character_, length(s))
		dash <- !is.na(s) & grepl("-", s, fixed=TRUE)
		slash <- !is.na(s) & grepl("/", s, fixed=TRUE)
		if (any(dash)) out[dash] <- as.character(as.Date(s[dash]))
		if (any(slash)) out[slash] <- as.character(as.Date(s[slash], "%d/%m/%Y"))
		serial <- !is.na(s) & !dash & !slash & grepl("^[0-9]+(\\.[0-9]+)?$", s)
		if (any(serial)) out[serial] <- as.character(as.Date(as.numeric(s[serial]), origin="1899-12-30"))
		out
	}
	num <- function(x) {
		s <- trimws(as.character(x))
		s[tolower(s) %in% c("", "na", "#n/a", "-")] <- NA_character_
		as.numeric(s)
	}

	read_crop <- function(f, crop, part) {
		r <- carobiner::read.excel(f, sheet=2)
		names(r) <- trimws(names(r))
		# Drop empty trailing rows (e.g. 3 blank rows in Faidherbia data_cotton.xlsx)
		if ("plot_number" %in% names(r)) {
			r <- r[!is.na(r$plot_number), ]
		}
		park <- basename(dirname(f))
		is_mixed <- grepl("Mixed", park, ignore.case=TRUE)
		loc <- if ("locality" %in% names(r)) trimws(as.character(r$locality)) else rep("Bame", nrow(r))
		loc[is.na(loc) | !nzchar(loc)] <- if (is_mixed) "Bame" else NA_character_
		tree <- if ("tree_species" %in% names(r)) trimws(as.character(r$tree_species)) else rep("Faidherbia albida", nrow(r))
		tnum <- if ("tree_number" %in% names(r)) as.integer(num(r$tree_number)) else rep(NA_integer_, nrow(r))
		pnum <- if ("plot_number" %in% names(r)) as.integer(num(r$plot_number)) else seq_len(nrow(r))
		pos  <- if ("plot_position" %in% names(r)) trimws(as.character(r$plot_position)) else rep(NA_character_, nrow(r))
		pdist <- if ("plot_distance_from_tree_m" %in% names(r)) num(r$plot_distance_from_tree_m) else rep(NA_real_, nrow(r))

		plant   <- if ("sowing_date" %in% names(r)) to_date(r$sowing_date) else rep(NA_character_, nrow(r))
		emerg   <- if ("emergence_date" %in% names(r)) to_date(r$emergence_date) else rep(NA_character_, nrow(r))
		flower  <- if ("flowering_date" %in% names(r)) to_date(r$flowering_date) else rep(NA_character_, nrow(r))
		harvest <- if ("maturity_date" %in% names(r)) to_date(r$maturity_date) else rep(NA_character_, nrow(r))

		# Infer year from sheet name / year column / sowing date
		yr <- substr(plant, 1, 4)
		if ("year" %in% names(r)) {
			yy <- trimws(as.character(r$year))
			yr[!is.na(yy) & nzchar(yy)] <- yy[!is.na(yy) & nzchar(yy)]
		}
		sh_yr <- regmatches(readxl::excel_sheets(f)[2], regexpr("20[0-9]{2}", readxl::excel_sheets(f)[2]))
		if (length(sh_yr) == 1) yr[is.na(yr) | !nzchar(yr)] <- sh_yr
		plant[is.na(plant)] <- yr[is.na(plant)]

		# Biomass: Faidherbia maize 2023 is in t/ha (1.5-13.1), Mixed maize 2024 is in kg/ha (1133-5759)
		biom <- if ("total_biomass_kg_ha" %in% names(r)) num(r$total_biomass_kg_ha) else rep(NA_real_, nrow(r))
		if (!all(is.na(biom)) && median(biom, na.rm=TRUE) < 50) biom <- biom * 1000

		pdens <- if ("Density_at_harvest" %in% names(r)) num(r$Density_at_harvest) * 10000 else rep(NA_real_, nrow(r))
		gcol <- names(r)[grepl("Ginning", names(r))]
		gin  <- if (length(gcol) == 1) num(r[[gcol]]) else rep(NA_real_, nrow(r))
		seed <- if ("weight_1000_grains_g" %in% names(r)) num(r$weight_1000_grains_g) else rep(NA_real_, nrow(r))
		bcol <- names(r)[grepl("Number_of_bolls", names(r))]
		bdens <- if (length(bcol) == 1) num(r[[bcol]]) else rep(NA_real_, nrow(r))
		bwt  <- if ("mean_weight_pod_g" %in% names(r)) num(r$mean_weight_pod_g) else rep(NA_real_, nrow(r))
		la   <- if ("leaf_area_cm2" %in% names(r)) num(r$leaf_area_cm2) else rep(NA_real_, nrow(r))
		ldm  <- if ("leaf_dry_mass_g" %in% names(r)) num(r$leaf_dry_mass_g) else if ("leaf_dry_weight_g" %in% names(r)) num(r$leaf_dry_weight_g) else rep(NA_real_, nrow(r))
		sla  <- if ("SLA_cm2_g" %in% names(r)) num(r$SLA_cm2_g) else rep(NA_real_, nrow(r))

		h60 <- if ("height_60days_cm" %in% names(r)) num(r$height_60days_cm) else rep(NA_real_, nrow(r))
		h90col <- if ("height_90days_cm" %in% names(r)) "height_90days_cm" else if ("height_cm_90_cotton" %in% names(r)) "height_cm_90_cotton" else NA
		h90 <- if (!is.na(h90col)) num(r[[h90col]]) else rep(NA_real_, nrow(r))
		s60 <- if ("SPAD_60" %in% names(r)) num(r$SPAD_60) else rep(NA_real_, nrow(r))
		s90 <- if ("SPAD_90" %in% names(r)) num(r$SPAD_90) else rep(NA_real_, nrow(r))

		pid <- paste(crop, skey(loc), yr, pnum, sep="_")
		dd <- data.frame(
			plot_id = pid,
			loc_key = skey(loc),
			yr = yr,
			plot_number = pnum,
			tree_number = tnum,
			tree_species_ = tree,
			plot_position_ = pos,
			plot_distance_m_ = pdist,
			crop = crop,
			yield_part = part,
			yield = num(r$yield_kg_ha),
			dmy_total = biom,
			plant_density = pdens,
			seed_weight = seed,
			boll_density_ = bdens,
			boll_weight_ = bwt,
			ginning_pct_ = gin,
			leaf_area_ = la,
			leaf_dry_mass_ = ldm,
			SLA_ = sla,
			planting_date = plant,
			emergence_date = emerg,
			flowering_date = flower,
			harvest_date = harvest
		)
		h1 <- data.frame(plot_id=pid, DAP=60L, plant_height=h60, SPAD=s60)
		h2 <- data.frame(plot_id=pid, DAP=90L, plant_height=h90, SPAD=s90)
		h <- rbind(h1, h2)
		h <- h[!is.na(h$plant_height) | !is.na(h$SPAD), ]
		list(d=dd, h=h)
	}

	files_m <- list.files(root, pattern="data_maize\\.xlsx$", recursive=TRUE, full.names=TRUE)
	files_c <- list.files(root, pattern="data_cotton\\.xlsx$", recursive=TRUE, full.names=TRUE)
	parts <- c(
		lapply(files_m, read_crop, crop="maize", part="grain"),
		lapply(files_c, read_crop, crop="cotton", part="lint")
	)
	d <- do.call(rbind, lapply(parts, function(z) z$d))
	long_crop <- do.call(rbind, lapply(parts, function(z) z$h))

	# Join tree characteristics (height, crown radius)
	tf_files <- list.files(root, pattern="trees_characteristics\\.xlsx$", recursive=TRUE, full.names=TRUE)
	trees_df <- do.call(rbind, lapply(tf_files, function(f) {
		rt <- carobiner::read.excel(f, sheet=1)
		names(rt) <- trimws(names(rt))
		is_m <- grepl("Mixed", basename(dirname(f)), ignore.case=TRUE)
		loc <- if ("locality" %in% names(rt)) skey(rt$locality) else rep("bame", nrow(rt))
		hcol <- intersect(c("tree_height_m", "tree_hight_m"), names(rt))[1]
		data.frame(
			loc_key = loc,
			tree_number = as.integer(num(rt$tree_number)),
			tree_height_ = num(rt[[hcol]]),
			tree_crown_radius_ = num(rt$tree_radius_m)
		)
	}))
	trees_df <- trees_df[!is.na(trees_df$tree_number) & !duplicated(trees_df[, c("loc_key", "tree_number")]), ]
	d <- merge(d, trees_df, by=c("loc_key", "tree_number"), all.x=TRUE)

	# Join soil mineral nitrogen (soil_NH4, soil_NO3, soil_N in mg/kg)
	sn_files <- list.files(root, pattern="soil_nutrient\\.xlsx$", recursive=TRUE, full.names=TRUE)
	sn_df <- do.call(rbind, lapply(sn_files, function(f) {
		rs <- carobiner::read.excel(f, sheet=2)
		names(rs) <- trimws(names(rs))
		loc <- if ("locality" %in% names(rs)) skey(rs$locality) else rep("bame", nrow(rs))
		pcol <- intersect(c("plot_position", "Position"), names(rs))[1]
		nh4c <- names(rs)[grepl("ammonium", names(rs), ignore.case=TRUE)][1]
		no3c <- names(rs)[grepl("nitrate", names(rs), ignore.case=TRUE)][1]
		minc <- names(rs)[grepl("mineral", names(rs), ignore.case=TRUE)][1]
		data.frame(
			loc_key = loc,
			yr = trimws(as.character(rs$year)),
			plot_number = as.integer(num(rs$plot_number)),
			plot_position_ = trimws(as.character(rs[[pcol]])),
			soil_NH4 = num(rs[[nh4c]]),
			soil_NO3 = num(rs[[no3c]]),
			soil_N = num(rs[[minc]])
		)
	}))
	sn_df <- sn_df[!is.na(sn_df$plot_number) & !duplicated(sn_df[, c("loc_key", "yr", "plot_number")]), ]
	d <- merge(d, sn_df[, c("loc_key", "yr", "plot_number", "soil_NH4", "soil_NO3", "soil_N")],
		by=c("loc_key", "yr", "plot_number"), all.x=TRUE)

	# Join cotton HVI fiber quality
	fq_files <- list.files(root, pattern="fiber_quality\\.xlsx$", ignore.case=TRUE, recursive=TRUE, full.names=TRUE)
	fq_df <- do.call(rbind, lapply(fq_files, function(f) {
		rf <- carobiner::read.excel(f, sheet=2)
		names(rf) <- trimws(names(rf))
		loc <- if ("Locality" %in% names(rf)) skey(rf$Locality) else if ("locality" %in% names(rf)) skey(rf$locality) else rep("bame", nrow(rf))
		lcol <- names(rf)[grepl("len", names(rf), ignore.case=TRUE)][1]
		mcol <- names(rf)[grepl("micro", names(rf), ignore.case=TRUE)][1]
		scol <- names(rf)[grepl("^SF_|strength", names(rf), ignore.case=TRUE)][1]
		ecol <- names(rf)[grepl("elong", names(rf), ignore.case=TRUE)][1]
		data.frame(
			loc_key = loc,
			crop = "cotton",
			plot_number = as.integer(num(rf$plot_number)),
			fiber_SCI_ = if ("SCI" %in% names(rf)) num(rf$SCI) else rep(NA_real_, nrow(rf)),
			fiber_micronaire_ = if (!is.na(mcol)) num(rf[[mcol]]) else rep(NA_real_, nrow(rf)),
			fiber_length_ = if (!is.na(lcol)) num(rf[[lcol]]) else rep(NA_real_, nrow(rf)),
			fiber_strength_ = if (!is.na(scol)) num(rf[[scol]]) else rep(NA_real_, nrow(rf)),
			fiber_elongation_ = if (!is.na(ecol)) num(rf[[ecol]]) else rep(NA_real_, nrow(rf))
		)
	}))
	fq_df <- fq_df[!is.na(fq_df$plot_number) & !duplicated(fq_df[, c("loc_key", "crop", "plot_number")]), ]
	d <- merge(d, fq_df, by=c("loc_key", "crop", "plot_number"), all.x=TRUE)

	d <- merge(d, site, by.x="loc_key", by.y="key", all.x=TRUE)
	d <- d[order(d$location, d$yr, d$crop, d$plot_number), ]
	d$record_id <- seq_len(nrow(d))
	d$country <- "Cameroon"
	d$adm1 <- "North"
	d$trial_id <- paste(d$location, d$crop, d$yr, sep="_")
	d$treatment <- paste(d$tree_species_, d$plot_position_, sep="_")
	d$crop_rotation <- "maize;cotton"
	d$irrigated <- FALSE
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE

	# Soil water (% AWC at 20 and 40 cm, 2024) into long table
	sw_files <- list.files(root, pattern="soil_water\\.xlsx$", recursive=TRUE, full.names=TRUE)
	mon_map <- c(june="06", july="07", jully="07", august="08", september="09", october="10", november="11")
	long_sw <- do.call(rbind, lapply(sw_files, function(f) {
		rw <- carobiner::read.excel(f, sheet=2)
		names(rw) <- trimws(names(rw))
		loc <- if ("locality" %in% names(rw)) skey(rw$locality) else rep("bame", nrow(rw))
		dt <- if ("date" %in% names(rw)) to_date(rw$date) else {
			mm <- unname(mon_map[tolower(trimws(as.character(rw$month)))])
			ifelse(!is.na(mm), paste0("2024-", mm), "2024")
		}
		wcol <- names(rw)[grepl("soil_water", names(rw), ignore.case=TRUE)][1]
		ly <- num(rw$soil_layer)
		data.frame(
			loc_key = loc,
			yr = "2024",
			plot_number = as.integer(num(rw$plot_number)),
			date = dt,
			depth_top = ly - 20,
			depth_bottom = ly,
			soil_water_awc_ = num(rw[[wcol]])
		)
	}))
	long_sw <- long_sw[!is.na(long_sw$soil_water_awc_), ]
	long_sw <- merge(long_sw, d[, c("record_id", "trial_id", "plot_id", "loc_key", "yr", "plot_number")],
		by=c("loc_key", "yr", "plot_number"))
	long_sw <- long_sw[, c("record_id", "trial_id", "plot_id", "date", "depth_top", "depth_bottom", "soil_water_awc_")]

	long_crop <- merge(long_crop, d[, c("record_id", "trial_id", "plot_id")], by="plot_id")
	long_crop <- long_crop[, c("record_id", "trial_id", "plot_id", "DAP", "plant_height", "SPAD")]
	dlong <- carobiner::bindr(long_crop, long_sw)

	d$loc_key <- NULL
	d$yr <- NULL
	d$plot_number <- NULL
	d$tree_number <- NULL

	# Daily microclimate weather
	clims <- list.files(root, pattern="microclimate\\.xlsx$", recursive=TRUE, full.names=TRUE)
	wth <- NULL
	for (f in clims) {
		rw <- carobiner::read.excel(f, sheet="temperature_and_humidity")
		names(rw) <- trimws(names(rw))
		loc <- if ("locality" %in% names(rw)) trimws(as.character(rw$locality)) else rep("Bame", nrow(rw))
		station <- skey(loc)
		for (nm in c("tree_number", "plot_position", "orientation", "repetition")) {
			if (nm %in% names(rw)) station <- paste(station, trimws(as.character(rw[[nm]])), sep="_")
		}
		w <- data.frame(
			station_id = station,
			loc_key = skey(loc),
			date = to_date(rw$date),
			tmin = num(rw$t_min),
			tmax = num(rw$t_max),
			temp = num(rw$t_mean),
			rhmn = num(rw$h_min),
			rhmx = num(rw$h_max),
			rhum = num(rw$h_mean)
		)
		wth <- rbind(wth, w)
	}
	wth <- merge(wth, site[, c("key", "latitude", "longitude", "elevation")], by.x="loc_key", by.y="key", all.x=TRUE)
	wth$country <- "Cameroon"
	wth$geo_from_source <- !is.na(wth$latitude)
	wth$loc_key <- NULL
	wth <- wth[!is.na(wth$date) & !duplicated(wth[, c("station_id", "date")]), ]

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
