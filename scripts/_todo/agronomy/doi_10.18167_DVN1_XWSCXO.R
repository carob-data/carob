# R script for "carob"
# license: GPL (>=3)

## NOTES
# Neufchâteau, Capesterre-Belle-Eau, Guadeloupe. CIRAD 925 on andosol, 2021-2022. Country is geographicCoverage. The description states a randomized complete block with four replicates.
# Treatments from the TREATMENT sheet: BS none plus mineral 14-4-26 at 210 kg N/ha/year; ML Arachis repens plus the same mineral rate; MG Brachiaria mutica plus the same mineral rate; OL Arachis repens plus organic 7-5-7 at 315 kg N/ha/year; OG Brachiaria mutica plus that organic rate. P and K are the elemental amounts from those oxide ratios. The rate is per year, repeated on each cycle row.
# The same sheet says 1250 plant/m2. That population is not used. Bunch and fruit weights stay in g per plant.
# Flowering height is cm (about 150-250). Weather is the daily station sheet: rain mm, minimum and maximum C, wind m/s, radiation MJ/m2 converted to kJ/m2. There is no mean temperature.
# Soil depth 30 and 60 are the 0-30 and 30-60 cm layers named in the description. Cover-crop biomass is already kg/ha, including negative values.
# Cycle 3 has flowering and growth sheets and no harvest sheet. That flowering sheet has no height column. Soil NH4 includes measured zeros.

## ISSUES
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated
# unknown variables: cycle_, bunch_fresh_g_, fruit_fresh_g_
# out of bounds: K_fertilizer (261.389, 323.625)
# invalid terms: cover_crop: Arachis repens, Brachiaria mutica
# datespan: 108 harvest_date more than 366 days after planting_date
# unknown variable: leaf_n_ (leaf_N?)
# unknown variable: soil_position_ (soil_P?)
# unknown variables: banana_id_, banana_cycle_, pseudostem_circ10_, cover_part_, cover_biomass_
# out of bounds: soil_NH4 (0, 61.5)

## NOT PROCESSED
# farming_practices operation log (mowing, tarp, weeding). Planting and fertilizer rates used here come from the plant sheets and the TREATMENT sheet.
# DM_ratio sheet. It repeats fresh and dry weights as ratios.
# WAP. The long table uses the calendar date.
# 2.management.xlsx: not read. sheet TREATMENT (of 2): Treatments, Cover crop species name, Type of N fertilizer, Amount of N fertilizer, Amount of fertilizer per application, Plant population. Observation records are missing from d until this file is standardized.
# 3.banana_cycle_1.xlsx: not read. sheet READ_ME (of 5): Sheet ' C10'. Observation records are missing from d until this file is standardized.
# 4.banana_cycle_2.xlsx: not read. sheet READ_ME (of 4): Sheet ' C10'. Observation records are missing from d until this file is standardized.
# 5.banana_cycle_3.xlsx: not read. sheet READ_ME (of 3): Sheet ' C10'. Observation records are missing from d until this file is standardized.


## NEW VARIABLES
# cycle_: crop cycle 1, 2, or 3.
# fruit_fresh_g_: fruit fresh weight, g per plant.
# bunch_fresh_g_: bunch fresh weight, g per plant.
# banana_id_: plant id in the cycle file.
# banana_cycle_: cycle 1, 2, or 3 on the long rows.
# pseudostem_circ10_: Circ10 from the growth sheet.
# leaf_n_: leaf nitrogen as stored. The sheet does not give the unit.
# cover_part_: cover-crop fraction (green, senescent, weeds, roots).
# cover_biomass_: that fraction, kg/ha.
# soil_position_: row or inter-row, as stored.


carob_script <- function(path) {

"
Growth and nitrogen dynamics dataset from a three-Cycle Banana (CIRAD 925) experiment comparing cover crops and fertilization strategies on andosol in Guadeloupe (2021–2022)

This dataset was generated from a field experiment conducted at the Neufchâteau research station in Guadeloupe between 2021 and 2022 under banana cultivation on an Andosol. The experiment evaluated the effects of different cover crop species and fertilization strategies on banana growth, cover crop development and soil mineral nitrogen dynamics. The banana hybrid 'CIRAD 925' was grown under five management treatments combining two cover crops (Arachis repens, a legume, and Brachiaria mutica, a grass), two fertilization strategies (organic fertilization suitable for organic farming and mineral fertilization), and a control treatment without cover crop receiving mineral fertilization. The experiment followed a randomized complete block design with four replicates.

The dataset includes three crop cycles and contains measurements of banana growth (plant circumference, leaf nitrogen concentration, flowering parameters, plant height, number of leaves and bunch weight), cover crop biomass production and nitrogen concentration, including separate measurements of green and senescent biomass for Brachiaria mutica. Soil mineral nitrogen dynamics (NO3 and NH4) were monitored monthly at two soil depths (0–30 and 30–60 cm) and at two sampling positions (banana row and inter-row), using composite soil samples for each plot.

A tropical cyclone occurring in September 2022 affected the experiment, resulting in missing end-of-cycle banana growth measurements for some plots.
"

	uri <- "doi:10.18167/DVN1/XWSCXO"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.eja.2025.127670",
		project = NA,
		design = "randomized complete block",
		data_type = "experiment",
		treatment_vars = "cover_crop;OM_used;N_fertilizer",
		response_vars = "plant_height;LAI;fruit_fresh_g_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.9
	)

	as_num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "NaN", "-", "nd", "ND")] <- NA_character_
		as.numeric(x)
	}
	as_day <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		out <- rep(NA_character_, length(x))
		dash <- !is.na(x) & grepl("-", x, fixed=TRUE)
		slash <- !is.na(x) & grepl("/", x, fixed=TRUE)
		if (any(dash)) out[dash] <- as.character(as.Date(x[dash]))
		if (any(slash)) out[slash] <- as.character(as.Date(x[slash], "%d/%m/%Y"))
		# Harvest dates in these workbooks are Excel serials stored as text.
		serial <- !is.na(x) & !dash & !slash
		if (any(serial)) out[serial] <- as.character(as.Date(as.numeric(x[serial]), origin="1899-12-30"))
		out
	}
	pick <- function(dir, bn) dir[basename(dir) == bn]

	plants <- NULL
	growth <- NULL
	for (cy in 1:3) {
		f <- pick(ff, paste0(cy + 2, ".banana_cycle_", cy, ".xlsx"))
		r1 <- carobiner::read.excel(f, sheet="FLOWERING")
		r2 <- carobiner::read.excel(f, sheet="C10")
		# Cycle 3 has no HARVEST sheet.
		hv <- tryCatch(carobiner::read.excel(f, sheet="HARVEST"), error=function(e) NULL)
		names(r1) <- trimws(names(r1))
		names(r2) <- trimws(names(r2))
		r1$Banana_ID <- trimws(as.character(r1$Banana_ID))
		a <- data.frame(
			banana_id = r1$Banana_ID,
			treatment = trimws(as.character(r1$Treatment)),
			block = r1$Block,
			planting_date = as_day(r1$Planting_date),
			flowering_date = as_day(r1$Flowering_date),
			# Cycle 3 flowering sheet has no ht column.
			plant_height = if ("ht" %in% names(r1)) as_num(r1$ht) else rep(NA_real_, nrow(r1))
		)
		if (is.null(hv)) {
			b <- data.frame(
				banana_id = r1$Banana_ID,
				treatment_h = NA_character_,
				block_h = NA,
				planting_h = NA_character_,
				harvest_date = NA_character_,
				bunch_fresh_g_ = NA_real_,
				fruit_fresh_g_ = NA_real_
			)
		} else {
			names(hv) <- trimws(names(hv))
			hv$Banana_ID <- trimws(as.character(hv$Banana_ID))
			b <- data.frame(
				banana_id = hv$Banana_ID,
				treatment_h = trimws(as.character(hv$Treatment)),
				block_h = hv$Block,
				planting_h = as_day(hv$Planting_date),
				harvest_date = as_day(hv$Harvest_date),
				bunch_fresh_g_ = as_num(hv$Bunch_FM_g),
				fruit_fresh_g_ = as_num(hv$Fruit_FM_g)
			)
		}
		m <- merge(a, b, by="banana_id", all=TRUE)
		m$treatment <- ifelse(is.na(m$treatment), m$treatment_h, m$treatment)
		m$block <- ifelse(is.na(m$block), m$block_h, m$block)
		m$planting_date <- ifelse(is.na(m$planting_date), m$planting_h, m$planting_date)
		m$cycle_ <- cy
		m <- m[, c("banana_id", "treatment", "block", "planting_date", "flowering_date", "harvest_date", "plant_height", "bunch_fresh_g_", "fruit_fresh_g_", "cycle_")]
		plants <- rbind(plants, m)
		growth <- rbind(growth, data.frame(
			trial_id = "Neufchateau",
			date = as_day(r2$Date),
			banana_id_ = trimws(as.character(r2$Banana_ID)),
			banana_cycle_ = cy,
			LAI = as_num(r2$LAI),
			pseudostem_circ10_ = as_num(r2$Circ10),
			leaf_n_ = as_num(r2$Leaf_N_content)
		))
	}

	org <- plants$treatment %in% c("OL", "OG")
	mineral <- plants$treatment %in% c("BS", "ML", "MG")
	d <- data.frame(
		trial_id = "Neufchateau",
		plot_id = paste(plants$cycle_, plants$banana_id, sep="_"),
		rep = as.integer(as_num(plants$block)),
		cycle_ = as.integer(plants$cycle_),
		country = "Guadeloupe",
		location = "Neufchateau",
		crop = "banana",
		variety = "CIRAD 925",
		treatment = plants$treatment,
		cover_crop = ifelse(plants$treatment == "BS", "none",
			ifelse(plants$treatment %in% c("OL", "ML"), "Arachis repens",
			ifelse(plants$treatment %in% c("OG", "MG"), "Brachiaria mutica", NA_character_))),
		OM_used = org,
		N_fertilizer = ifelse(org, 315, ifelse(mineral, 210, NA_real_)),
		planting_date = plants$planting_date,
		flowering_date = plants$flowering_date,
		harvest_date = plants$harvest_date,
		plant_height = plants$plant_height,
		bunch_fresh_g_ = plants$bunch_fresh_g_,
		fruit_fresh_g_ = plants$fruit_fresh_g_,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	# P2O5/N and K2O/N from the product grade, then elemental P and K.
	p_ox <- ifelse(org, d$N_fertilizer * (5/7), ifelse(mineral, d$N_fertilizer * (4/14), NA_real_))
	k_ox <- ifelse(org, d$N_fertilizer * (7/7), ifelse(mineral, d$N_fertilizer * (26/14), NA_real_))
	d$P_fertilizer <- p_ox / 2.29
	d$K_fertilizer <- k_ox / 1.2051
	d <- d[d$treatment %in% c("BS", "OL", "ML", "MG", "OG"), ]

	r3 <- carobiner::read.excel(pick(ff, "6.cover_crop.xlsx"), sheet="cover_crop")
	names(r3) <- trimws(names(r3))
	cover <- data.frame(
		trial_id = "Neufchateau",
		date = as_day(r3$Date),
		cover_part_ = trimws(as.character(r3$Species_type)),
		cover_biomass_ = as_num(r3$Biomass_kg_ha)
	)
	r4 <- carobiner::read.excel(pick(ff, "7.Soil_N.xlsx"), sheet="Soil_N")
	names(r4) <- trimws(names(r4))
	depth <- as_num(r4$Soil_Depth)
	soil <- data.frame(
		trial_id = "Neufchateau",
		date = as_day(r4$Date),
		depth_top = ifelse(depth == 30, 0, ifelse(depth == 60, 30, NA_real_)),
		depth_bottom = depth,
		soil_NO3 = as_num(r4$NO3_mg_kg),
		soil_NH4 = as_num(r4$NH4_mg_kg),
		soil_position_ = trimws(as.character(r4$Location))
	)

	bind_rows <- function(lst) {
		cols <- unique(unlist(lapply(lst, names)))
		out <- lapply(lst, function(x) {
			for (nm in cols) {
				if (!nm %in% names(x)) x[[nm]] <- NA
			}
			x[, cols]
		})
		do.call(rbind, out)
	}
	dlong <- bind_rows(list(growth, cover, soil))

	r5 <- carobiner::read.excel(pick(ff, "1.Climate.xlsx"), sheet="NCT_2021_2022")
	names(r5) <- trimws(names(r5))
	wth <- data.frame(
		station_id = "Neufchateau",
		station_name = "Neufchateau",
		country = "Guadeloupe",
		date = as_day(r5$Date),
		prec = as_num(r5$Rain),
		tmin = as_num(r5$Min_Temp),
		tmax = as_num(r5$Max_Temp),
		wspd = as_num(r5$Wind),
		# READ_ME: MJ/m2. Weather srad is kJ/m2.
		srad = as_num(r5$Global_Radiation) * 1000
	)

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
