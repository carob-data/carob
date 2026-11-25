# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field experiment at CIRAD Neufchâteau research station, Capesterre-Belle-Eau, Guadeloupe (2021-2022) on an Andosol, RCBD with 4 blocks (20 plots; 10 banana mats monitored per plot across 3 cycles of hybrid CIRAD 925).
# Five treatments from 2.management.xlsx (TREATMENT sheet): BS (no cover crop + mineral NPK 14-4-26 at 210 kg N/ha/yr), ML (Arachis repens + mineral NPK 14-4-26 at 210 kg N/ha/yr), MG (Brachiaria mutica + mineral NPK 14-4-26 at 210 kg N/ha/yr), OL (Arachis repens + organic AB'Flor 7-5-7 at 315 kg N/ha/yr), OG (Brachiaria mutica + organic AB'Flor 7-5-7 at 315 kg N/ha/yr).
# Annual fertilizer rates are repeated on each crop cycle row because ratoon cycles overlap in time and share monthly plantation-level applications (19 applications from 2021-06-11 to 2022-12-12 in farming_practices). Mineral N, P, K (elemental, from N-P2O5-K2O) go in N/P/K_fertilizer (0 for organic plots); organic N, P, K go in N/P/K_organic (0 for mineral plots).
# 2.management.xlsx states plant population as "1250 plant/m2", which is a unit typo for 1250 plants/ha (confirmed by application math: 100 g/plant * 14% N * 12 applications/yr * 1250 plants/ha = 210 kg N/ha/yr; 300 g/plant * 7% N * 12 * 1250 = 315 kg N/ha/yr). Fresh and dry fruit/bunch yields (kg/ha) are normalized from g/plant using plant_density = 1250 plants/ha.
# Per cycle READ_ME, only Bunch_FM_g was directly measured on all harvested plants; Fruit_FM_g and dry weights were derived using partitioning and DM ratios from a 20-plant Cycle 1 subsample (DM_ratio sheet, mean fruit/bunch ratio 0.819, fruit DM ~24.9%). yield_moisture is computed from Fruit_DM_g / Fruit_FM_g.
# Brachiaria mutica is mapped to accepted crop term "brachiaria". Cover-crop sampling dates on WAP 3, 17, 27 are stored as fractional Excel serial numbers and floored to calendar dates.
# In C10 sheets, LAI is allometrically estimated from Circ10 (LAI = 0.0773 * exp(0.0389 * Circ10), per READ_ME, except 14 Cycle 2 rows on 2022-09-15). Leaf_N_content and Leaf_C_content (%) are converted to mg/g (* 10).
# In 7.Soil_N.xlsx, Soil_Depth 30 and 60 correspond to 0-30 and 30-60 cm layers; Total_N_Content (%) is converted to mg/kg (* 10000); implied bulk density from N_kg_ha / N_mg_kg is 0.8 g/cm3.
# Weather (wth) is from 1.Climate.xlsx (NCT_2021_2022); Global_Radiation (MJ/m2) is converted to kJ/m2 (* 1000).

## ISSUES
# Associated paper (doi:10.1016/j.eja.2025.127670) is behind a paywall; irrigated is omitted because irrigation is not explicitly stated in the raw files (annual rainfall at Neufchâteau is ~2900 mm and farming_practices lists no irrigation, so likely rainfed — verify from paper).
# Coordinates (16.0808, -61.6029) are estimated for CIRAD Neufchâteau station (geo_from_source = FALSE); verify against paper.
# NA detected in yield: 3 of 200 plants in Cycle 1, 99 of 200 in Cycle 2, and all 200 in Cycle 3 (no HARVEST sheet) lack harvest weight, largely due to Tropical Storm / Hurricane Fiona in September 2022; all records are kept.
# datespan: 2 Cycle 1 and 106 Cycle 2 harvest_date records exceed 366 days after initial planting_date (2021-05-21), expected for perennial multi-cycle banana (ratoon Cycle 2).
# out of bounds: K_fertilizer (323.625 kg/ha/yr > 300) and fertilizer_amount (1500 kg/ha/yr > 1000) reflect real annual mineral NPK rates for banana; soil_NH4 has 9 measured zeros (< 0.01); kept as reported.
# invalid terms: cover_crop "Arachis repens" and OM_type "AB'Flor" are not yet in terminag; kept as meaningful values.
# Raw data anomalies kept as reported: Cycle 1 FLOWERING plant OL2-8 has Inflorescence_Emergence_date 2021-01-06 (before planting 2021-05-21, likely typo for 2022-01-06); 6.cover_crop.xlsx plot ML2 Weeds on 2022-08-02 has negative DM_content (-867.69%) and Biomass_kg_ha (-593.5 kg/ha); farming_practices row 25 for BS manual weeding has date 2021-01-01 between 2021-10-20 and 2021-11-08.

## NOT PROCESSED
# 2.management.xlsx (farming_practices): non-fertilizer/non-weeding operations (rotary mowing, tarp installation/removal, cover crop planting date 2021-03-08, mowing dates, sucker removal, successor sucker selection 2021-10-20) have no standard columns.
# 3.banana_cycle_1.xlsx, 4.banana_cycle_2.xlsx, 5.banana_cycle_3.xlsx (C10): BAN_DM_g (allometric estimate 129 * exp(0.044 * Circ10), with 154 zeros on harvested Cycle 2 plants) dropped as modeled/derived from Circ10; WAP redundant with date and DAP.
# 3.banana_cycle_1.xlsx, 4.banana_cycle_2.xlsx (HARVEST): secondary reproductive organ estimates (Reproductive_FM_g, Bunck_stalk_FM_g, Floral_Stalk_FM_g, Male_Flower_FM_g and their DM_g counterparts) derived from Bunch_FM_g via fixed partitioning ratios.
# 3.banana_cycle_1.xlsx (DM_ratio): 20-plant subsample calibration table used to derive the partitioning and dry-matter ratios applied in HARVEST.
# 6.cover_crop.xlsx (cover_crop): Biomass_g_m2 redundant with Biomass_kg_ha (= 10 * Biomass_g_m2); WAP redundant with date and DAP.
# 7.Soil_N.xlsx (Soil_N): NO3_kg_ha and NH4_kg_ha redundant constant multiples (= 2.4 * mg/kg using bulk density 0.8 g/cm3 over 30 cm); WAP redundant with date and DAP.

## NEW VARIABLES
# plant_id_: individual banana mat/plant identifier within the experiment (e.g. OL1-1), character.
# cycle_: banana crop cycle (1 = plant crop, 2 = first ratoon, 3 = second ratoon), integer.
# inflorescence_emergence_date_: date banana inflorescence first became visible from pseudostem, date (YYYY-MM-DD).
# bunch_emergence_date_: date banana bunch fully emerged from pseudostem, date (YYYY-MM-DD).
# hands_per_bunch_: number of hands per banana bunch at flowering, count (integer).
# fingers_per_bunch_: number of fingers (fruits) per banana bunch at flowering, count (integer).
# pseudostem_circ10_flowering_: pseudostem circumference 10 cm above soil surface at flowering, cm.
# pseudostem_circ100_flowering_: pseudostem circumference 100 cm above soil surface at flowering, cm.
# functional_leaves_: number of functional leaves at flowering, count (integer).
# bunch_fwy_: fresh harvested banana bunch weight per hectare (directly measured Bunch_FM_g normalized by plant_density 1250 plants/ha), kg/ha.
# bunch_dmy_: dry harvested banana bunch weight per hectare (Bunch_DM_g normalized by plant_density 1250 plants/ha), kg/ha.
# banana_id_: individual banana mat/plant identifier on long growth records, character.
# banana_cycle_: banana crop cycle (1, 2, or 3) on long growth records, integer.
# pseudostem_circ10_: repeated pseudostem circumference measured 10 cm above soil surface, cm.
# leaf_C_: carbon concentration in banana leaves (Cycle 3), mg/g.
# cover_part_: cover-crop or undergrowth compartment sampled (Arachis repens, Green/Senescent Brachiaria m., Senescent Arachis repens, Weeds, Roots), character.
# cover_biomass_: dry biomass of cover_part_, kg/ha.
# cover_dm_: dry matter content of cover_part_, %.
# cover_N_: nitrogen concentration of cover_part_, mg/g.
# cover_C_: carbon concentration of cover_part_, mg/g.
# soil_position_: soil sampling position relative to banana rows ("banana row" or "inter-row"), character.


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
		treatment_vars = "cover_crop;OM_used;N_fertilizer;P_fertilizer;K_fertilizer;N_organic;P_organic;K_organic",
		response_vars = "yield;plant_height;pseudostem_circ10_;leaf_N;cover_biomass_;soil_NO3;soil_NH4;soil_N_min_total",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 88,
		carob_effort = 0.4
	)

	as_day <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		out <- rep(NA_character_, length(x))
		dash <- !is.na(x) & grepl("-", x, fixed=TRUE)
		slash <- !is.na(x) & grepl("/", x, fixed=TRUE)
		if (any(dash)) out[dash] <- as.character(as.Date(x[dash]))
		if (any(slash)) out[slash] <- as.character(as.Date(x[slash], "%d/%m/%Y"))
		serial <- !is.na(x) & !dash & !slash
		if (any(serial)) out[serial] <- as.character(as.Date(floor(as.numeric(x[serial])), origin="1899-12-30"))
		out
	}

	norm_plot <- function(x) gsub("\\s+", "", trimws(as.character(x)))
	plant_density <- 1250 # plants/ha (2.management.xlsx "1250 plant/m2" is a unit typo for plants/ha)
	plant_day0 <- as.Date("2021-05-21")

	## Management practices (2.management.xlsx: farming_practices)
	fm <- ff[basename(ff) == "2.management.xlsx"]
	rm_ops <- carobiner::read.excel(fm, sheet="farming_practices", na=c("", "NA"), col_types="text")
	op_trts <- trimws(as.character(unlist(rm_ops[1, -1])))
	op_dates <- as_day(rm_ops$Date[-1])
	op_body <- rm_ops[-1, -1, drop=FALSE]
	names(op_body) <- op_trts

	fert_dates <- vapply(op_trts, function(tr) {
		i <- grepl("fertilization", op_body[[tr]], ignore.case=TRUE)
		paste(op_dates[i], collapse="; ")
	}, character(1))
	weed_dates <- vapply(op_trts, function(tr) {
		i <- grepl("manual weeding", op_body[[tr]], ignore.case=TRUE)
		if (!any(i)) return(NA_character_)
		paste(op_dates[i], collapse="; ")
	}, character(1))
	weed_times <- vapply(op_trts, function(tr) {
		sum(grepl("manual weeding", op_body[[tr]], ignore.case=TRUE))
	}, integer(1))
	herb_dates <- vapply(op_trts, function(tr) {
		i <- grepl("glyphosate", op_body[[tr]], ignore.case=TRUE)
		if (!any(i)) return(NA_character_)
		paste(op_dates[i], collapse="; ")
	}, character(1))
	herb_times <- vapply(op_trts, function(tr) {
		sum(grepl("glyphosate", op_body[[tr]], ignore.case=TRUE))
	}, integer(1))

	## Banana cycles 1-3: FLOWERING, HARVEST, and C10 growth/leaf N-C
	plants <- NULL
	growth <- NULL
	for (cy in 1:3) {
		f <- ff[basename(ff) == paste0(cy + 2, ".banana_cycle_", cy, ".xlsx")]
		r_fl <- carobiner::read.excel(f, sheet="FLOWERING", na=c("", "NA"), col_types="text")
		r_c10 <- carobiner::read.excel(f, sheet="C10", na=c("", "NA"), col_types="text")
		names(r_fl) <- trimws(names(r_fl))
		names(r_c10) <- trimws(names(r_c10))

		fl <- data.frame(
			plant_id_ = trimws(as.character(r_fl$Banana_ID)),
			plot_id = norm_plot(r_fl$Plot),
			treatment = trimws(as.character(r_fl$Treatment)),
			rep = as.integer(r_fl$Block),
			planting_date = as_day(r_fl$Planting_date),
			inflorescence_emergence_date_ = as_day(r_fl$Inflorescence_Emergence_date),
			bunch_emergence_date_ = as_day(r_fl$Bunch_Emergence_date),
			flowering_date = as_day(r_fl$Flowering_date),
			hands_per_bunch_ = as.integer(r_fl$NDM),
			fingers_per_bunch_ = as.integer(r_fl$NDD),
			pseudostem_circ100_flowering_ = as.numeric(r_fl$C100),
			pseudostem_circ10_flowering_ = as.numeric(r_fl$C10),
			# Cycle 3 flowering sheet has no ht column; Cycle 2 ht is all NA
			plant_height = if ("ht" %in% names(r_fl)) as.numeric(r_fl$ht) else rep(NA_real_, nrow(r_fl)),
			functional_leaves_ = as.integer(r_fl$NF)
		)

		if (cy < 3) {
			r_hv <- carobiner::read.excel(f, sheet="HARVEST", na=c("", "NA"), col_types="text")
			names(r_hv) <- trimws(names(r_hv))
			fruit_fm <- as.numeric(r_hv$Fruit_FM_g)
			fruit_dm <- as.numeric(r_hv$Fruit_DM_g)
			bunch_fm <- as.numeric(r_hv$Bunch_FM_g)
			bunch_dm <- as.numeric(r_hv$Bunch_DM_g)
			hv <- data.frame(
				plant_id_ = trimws(as.character(r_hv$Banana_ID)),
				harvest_date = as_day(r_hv$Harvest_date),
				# g/plant -> kg/ha using plant_density = 1250 plants/ha
				yield = fruit_fm * plant_density / 1000,
				yield_dry = fruit_dm * plant_density / 1000,
				yield_moisture = 100 * (1 - fruit_dm / fruit_fm),
				bunch_fwy_ = bunch_fm * plant_density / 1000,
				bunch_dmy_ = bunch_dm * plant_density / 1000
			)
			m <- merge(fl, hv, by="plant_id_", all=TRUE)
		} else {
			# Cycle 3 has no HARVEST sheet (experiment disrupted by Hurricane Fiona in Sep 2022)
			m <- fl
			m$harvest_date <- NA_character_
			m$yield <- NA_real_
			m$yield_dry <- NA_real_
			m$yield_moisture <- NA_real_
			m$bunch_fwy_ <- NA_real_
			m$bunch_dmy_ <- NA_real_
		}
		m$cycle_ <- as.integer(cy)
		plants <- rbind(plants, m)

		c10_date <- as_day(r_c10$Date)
		g <- data.frame(
			trial_id = "Neufchateau",
			plot_id = norm_plot(r_c10$Plot),
			date = c10_date,
			DAP = as.integer(as.Date(c10_date) - plant_day0),
			banana_id_ = trimws(as.character(r_c10$Banana_ID)),
			banana_cycle_ = as.integer(cy),
			pseudostem_circ10_ = as.numeric(r_c10$Circ10),
			# LAI estimated from Circ10 in raw sheet (READ_ME)
			LAI = as.numeric(r_c10$LAI),
			# % -> mg/g
			leaf_N = as.numeric(r_c10$Leaf_N_content) * 10,
			leaf_C_ = if ("Leaf_C_content" %in% names(r_c10)) as.numeric(r_c10$Leaf_C_content) * 10 else rep(NA_real_, nrow(r_c10))
		)
		growth <- rbind(growth, g)
	}

	org <- plants$treatment %in% c("OL", "OG")
	mineral <- plants$treatment %in% c("BS", "ML", "MG")
	# Annual rates from 2.management.xlsx (TREATMENT): mineral 14-4-26 at 210 kg N/ha/yr (1500 kg/ha/yr);
	# organic AB'Flor 7-5-7 at 315 kg N/ha/yr (4500 kg/ha/yr). P and K converted from P2O5 and K2O.
	d <- data.frame(
		trial_id = "Neufchateau",
		plot_id = plants$plot_id,
		plant_id_ = plants$plant_id_,
		cycle_ = plants$cycle_,
		rep = plants$rep,
		country = "Guadeloupe",
		adm1 = "Basse-Terre",
		adm2 = "Capesterre-Belle-Eau",
		location = "Neufchâteau",
		# CIRAD Neufchâteau research station, Capesterre-Belle-Eau (estimated; not in raw files)
		longitude = -61.6029,
		latitude = 16.0808,
		geo_from_source = FALSE,
		geo_uncertainty = 500,
		geo_source = "CIRAD Neufchâteau research station",
		soil_type = "Andosol",
		crop = "banana",
		variety = "CIRAD 925",
		plant_density = plant_density,
		treatment = plants$treatment,
		cover_crop_used = plants$treatment != "BS",
		cover_crop = ifelse(plants$treatment == "BS", "none",
			ifelse(plants$treatment %in% c("OL", "ML"), "Arachis repens",
			ifelse(plants$treatment %in% c("OG", "MG"), "brachiaria", NA_character_))),
		fertilizer_used = mineral,
		fertilizer_type = ifelse(mineral, "NPK", "none"),
		fertilizer_amount = ifelse(mineral, 1500, 0),
		fertilizer_date = ifelse(mineral, unname(fert_dates[plants$treatment]), NA_character_),
		N_fertilizer = ifelse(mineral, 210, 0),
		P_fertilizer = ifelse(mineral, 210 * (4 / 14) / 2.29, 0),
		K_fertilizer = ifelse(mineral, 210 * (26 / 14) / 1.2051, 0),
		OM_used = org,
		OM_type = ifelse(org, "AB'Flor", "none"),
		OM_amount = ifelse(org, 4500, 0),
		N_organic = ifelse(org, 315, 0),
		P_organic = ifelse(org, 315 * (5 / 7) / 2.29, 0),
		K_organic = ifelse(org, 315 * (7 / 7) / 1.2051, 0),
		herbicide_used = unname(herb_times[plants$treatment]) > 0L,
		herbicide_product = ifelse(unname(herb_times[plants$treatment]) > 0L, "glyphosate", "none"),
		herbicide_times = unname(herb_times[plants$treatment]),
		herbicide_dates = unname(herb_dates[plants$treatment]),
		weeding_done = unname(weed_times[plants$treatment]) > 0L,
		weeding_method = ifelse(unname(weed_times[plants$treatment]) > 0L, "manual", NA_character_),
		weeding_times = unname(weed_times[plants$treatment]),
		weeding_dates = unname(weed_dates[plants$treatment]),
		planting_date = plants$planting_date,
		inflorescence_emergence_date_ = plants$inflorescence_emergence_date_,
		bunch_emergence_date_ = plants$bunch_emergence_date_,
		flowering_date = plants$flowering_date,
		harvest_date = plants$harvest_date,
		plant_height = plants$plant_height,
		hands_per_bunch_ = plants$hands_per_bunch_,
		fingers_per_bunch_ = plants$fingers_per_bunch_,
		pseudostem_circ10_flowering_ = plants$pseudostem_circ10_flowering_,
		pseudostem_circ100_flowering_ = plants$pseudostem_circ100_flowering_,
		functional_leaves_ = plants$functional_leaves_,
		yield_part = "fruit",
		yield_isfresh = TRUE,
		yield = plants$yield,
		yield_dry = plants$yield_dry,
		yield_moisture = plants$yield_moisture,
		bunch_fwy_ = plants$bunch_fwy_,
		bunch_dmy_ = plants$bunch_dmy_,
		on_farm = FALSE,
		is_survey = FALSE
	)

	## Cover crop biomass and C/N (6.cover_crop.xlsx)
	r3 <- carobiner::read.excel(ff[basename(ff) == "6.cover_crop.xlsx"], sheet="cover_crop", na=c("", "NA"), col_types="text")
	names(r3) <- trimws(names(r3))
	cc_date <- as_day(r3$Date)
	cover <- data.frame(
		trial_id = "Neufchateau",
		plot_id = norm_plot(r3$Plot),
		date = cc_date,
		DAP = as.integer(as.Date(cc_date) - plant_day0),
		cover_part_ = trimws(as.character(r3$Species_type)),
		cover_biomass_ = as.numeric(r3$Biomass_kg_ha),
		cover_dm_ = as.numeric(r3$DM_content),
		# % -> mg/g
		cover_N_ = as.numeric(r3$N_content) * 10,
		cover_C_ = as.numeric(r3$C_content) * 10
	)

	## Soil mineral N, moisture, and total C/N (7.Soil_N.xlsx)
	r4 <- carobiner::read.excel(ff[basename(ff) == "7.Soil_N.xlsx"], sheet="Soil_N", na=c("", "NA"), col_types="text")
	names(r4) <- trimws(names(r4))
	s_date <- as_day(r4$Date)
	depth <- as.numeric(r4$Soil_Depth)
	soil <- data.frame(
		trial_id = "Neufchateau",
		plot_id = norm_plot(r4$Plot),
		date = s_date,
		DAP = as.integer(as.Date(s_date) - plant_day0),
		depth_top = ifelse(depth == 30, 0, ifelse(depth == 60, 30, NA_real_)),
		depth_bottom = depth,
		soil_position_ = trimws(as.character(r4$Location)),
		soil_NO3 = as.numeric(r4$NO3_mg_kg),
		soil_NH4 = as.numeric(r4$NH4_mg_kg),
		soil_N = as.numeric(r4$N_mg_kg),
		soil_N_min_total = as.numeric(r4$N_kg_ha),
		soil_GWC = as.numeric(r4$Soil_Water_Content),
		soil_SWC = as.numeric(r4$Soil_Water_Storage_mm),
		# % dry soil -> mg/kg
		soil_N_total = as.numeric(r4$Total_N_Content) * 10000,
		soil_C_total = as.numeric(r4$Total_C_Content)
	)

	dlong <- carobiner::bindr(growth, cover, soil)

	## Daily weather (1.Climate.xlsx: NCT_2021_2022)
	r5 <- carobiner::read.excel(ff[basename(ff) == "1.Climate.xlsx"], sheet="NCT_2021_2022", na=c("", "NA"), col_types="text")
	names(r5) <- trimws(names(r5))
	wth <- data.frame(
		station_id = "Neufchateau",
		station_name = "Neufchâteau",
		country = "Guadeloupe",
		longitude = -61.6029,
		latitude = 16.0808,
		geo_from_source = FALSE,
		date = as_day(r5$Date),
		prec = as.numeric(r5$Rain),
		tmin = as.numeric(r5$Min_Temp),
		tmax = as.numeric(r5$Max_Temp),
		wspd = as.numeric(r5$Wind),
		# READ_ME: MJ/m2 -> kJ/m2
		srad = as.numeric(r5$Global_Radiation) * 1000
	)

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
