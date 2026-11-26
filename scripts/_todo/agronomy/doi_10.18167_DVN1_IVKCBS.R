# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rainfed upland rice (Nerica 4) rotation x fertilization trial at Ivory station, Vakinankaratra, Malagasy highlands over two seasons (2016-2017 and 2017-2018; Ripoche et al., 2021, doi:10.1016/j.agee.2021.107576).
# Observations are measured treatment means with standard errors (used for STICS calibration/evaluation, not simulated outputs).
# Treatments: crop rotation RMC (rice after Mucuna pruriens + Crotalaria mixture) vs RR (continuous rice monocropping); fertilization F1 (farmyard manure only: 7.5 t DM/ha in 2016-2017 and 5.0 t DM/ha in 2017-2018 per Fertilization sheet) vs F2 (manure + 400 kg/ha NPK 11-22-16 basal + 200 kg/ha urea in 2 splits of 1/3 and 2/3). Oxide P2O5 and K2O are converted to elemental P and K; N/P/K_organic are computed from manure DM amount and nutrient percentages in the Fertilization sheet.
# Grain N concentration (grain_N, mg/g) is computed from grain N uptake (kg/ha) / grain yield (kg/ha) * 1000. Previous crop residue amount and C/N chemistry before sowing are joined from DataResidues.
# Coordinates and elevation (19°33'18.90" S, 46°24'53.83" E, 930 m a.s.l.) are from the dataset description. Sowing occurred over two days each season (23-24 Nov 2016, 24-25 Nov 2017), so planting_date is recorded at year-month precision.
# Long table (linked by record_id) preserves: (1) periodic rice vegetative biomass and vegetative N uptake (with SEs), (2) periodic profile soil water content and mineral N (total, NH4, NO3 with SEs), and (3) pre-sowing multi-depth soil moisture (0-80 cm) and mineral NO3/NH4 (0-150 cm) initialization profiles.

## ISSUES
# Records are treatment means (with standard errors) rather than individual replicate plots.
# Grain moisture basis (fresh vs dry) is not explicitly stated in the workbook; yield_moisture is omitted.
# Cropping practices notes "7.5 t/ha manure" in both seasons, while the Fertilization sheet gives 7.5 t DM/ha in 2016-2017 and 5.0 t DM/ha in 2017-2018 (with full chemical analysis); the Fertilization sheet values are used.

## NEW VARIABLES
# yield_se_: standard error of treatment-mean grain yield (kg/ha)
# grain_N_uptake_: treatment-mean nitrogen uptake in rice grain (kg N/ha)
# grain_N_uptake_se_: standard error of nitrogen uptake in rice grain (kg N/ha)
# residue_prevcrop_amount_: dry/fresh amount of previous crop residue present before sowing (kg/ha)
# residue_prevcrop_DM_pct_: dry matter fraction of previous crop residue (0-1)
# residue_prevcrop_N_pct_: nitrogen concentration of previous crop residue (% DM)
# residue_prevcrop_C_pct_: carbon concentration of previous crop residue (% DM)
# residue_prevcrop_CN_: C/N ratio of previous crop residue
# vegetative_biomass_: treatment-mean rice vegetative biomass on observation date (kg/ha)
# vegetative_biomass_se_: standard error of rice vegetative biomass (kg/ha)
# vegetative_N_uptake_: treatment-mean nitrogen uptake in rice vegetative biomass (kg N/ha)
# vegetative_N_uptake_se_: standard error of nitrogen uptake in rice vegetative biomass (kg N/ha)
# soil_water_: treatment-mean soil profile water content on observation date (mm)
# soil_water_se_: standard error of soil profile water content (mm)
# soil_N_min_se_: standard error of soil mineral N (kg N/ha)
# soil_NH4_kg_ha_: soil ammonium-N stock (kg N/ha)
# soil_NH4_se_: standard error of soil ammonium-N stock (kg N/ha)
# soil_NO3_kg_ha_: soil nitrate-N stock (kg N/ha)
# soil_NO3_se_: standard error of soil nitrate-N stock (kg N/ha)
# soil_moisture_init_: pre-sowing gravimetric/relative soil moisture by depth layer (%)

## NOT PROCESSED
# Fertilization: manure Ca (%), Mg (%), C (%), OM (%), and %DM columns (N/P/K_organic already extracted).


carob_script <- function(path) {

"
Dataset for modelling agronomic and environmental performances of rainfed rice based cropping systems with or without legume in Magadascar

This dataset provide data for calibration and evaluation of the model Stics in the specific context of malagasy higlands with rice cultivar Nerica 4. Data include soil moisture and N initialization, fertilization applied (quality and quantity), cropping practices, data on rice biomass and grain, N in biomass and grain for two cropping seasons (2016/217 and 2017/2018) at Ivory station (19°33’18.90’’ lat. S, 46°24'53.83’’ long. E, 930 m a‧s‧l.).
"

	uri <- "doi:10.18167/DVN1/IVKCBS"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.agee.2021.107576",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "crop_rotation;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield;grain_N;vegetative_biomass_;vegetative_N_uptake_;soil_N_min_total",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 95,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "DatasetModellingRiceLegume.xlsx"]
	r_bio  <- carobiner::read.excel(f, sheet = "Rice Biomass & Grains")
	r_nbio <- carobiner::read.excel(f, sheet = "Rice N Biomass & N Grains")
	r_fert <- carobiner::read.excel(f, sheet = "Fertilization")
	r_res  <- carobiner::read.excel(f, sheet = "DataResidues")
	r_swn  <- carobiner::read.excel(f, sheet = "Soil Water and N content")
	r_mi   <- carobiner::read.excel(f, sheet = "Moisture initialization")
	r_ni   <- carobiner::read.excel(f, sheet = "Soil N initialization")

	# Fertilization sheet: mineral NPK + urea and organic farmyard manure nutrients
	om_col <- grep("fumier", names(r_fert), ignore.case = TRUE, value = TRUE)[1]
	n_col  <- grep("^N", names(r_fert), value = TRUE)[1]
	p_col  <- grep("^P", names(r_fert), value = TRUE)[1]
	k_col  <- grep("^K", names(r_fert), value = TRUE)[1]
	om_kg  <- as.numeric(r_fert[[om_col]]) * 1000
	fert_df <- data.frame(
		Year = trimws(as.character(r_fert$Year)),
		Fertilization = trimws(as.character(r_fert$Fertilization)),
		OM_amount = om_kg,
		N_organic = om_kg * as.numeric(r_fert[[n_col]]) / 100,
		P_organic = om_kg * as.numeric(r_fert[[p_col]]) / 100,
		K_organic = om_kg * as.numeric(r_fert[[k_col]]) / 100,
		fertilizer_used = r_fert$Fertilization == "F2",
		N_splits = ifelse(r_fert$Fertilization == "F2", 3L, 0L),
		# F2: 400 kg/ha NPK 11-22-16 + 200 kg/ha urea (46% N)
		N_fertilizer = ifelse(r_fert$Fertilization == "F1", 0, 400 * 0.11 + 200 * 0.46),
		P_fertilizer = ifelse(r_fert$Fertilization == "F1", 0, 400 * 0.22 / 2.29),
		K_fertilizer = ifelse(r_fert$Fertilization == "F1", 0, 400 * 0.16 / 1.2051),
		fertilizer_type = ifelse(r_fert$Fertilization == "F1", "none", "NPK;urea")
	)

	# Previous crop residues before sowing
	res_df <- data.frame(
		Year = ifelse(as.character(r_res$Year) == "1617", "2016-2017", "2017-2018"),
		Fertilization = trimws(as.character(r_res$Fertilization)),
		Treatment = ifelse(grepl("Mucuna", r_res[["Type of residues"]]), "RMC", "RR"),
		residue_prevcrop_amount_ = as.numeric(r_res[["Amount before sowing (t/ha)"]]) * 1000,
		residue_prevcrop_DM_pct_ = as.numeric(r_res[["% DM"]]),
		residue_prevcrop_N_pct_ = as.numeric(r_res[["%Nsec"]]),
		residue_prevcrop_C_pct_ = as.numeric(r_res[["%Csec"]]),
		residue_prevcrop_CN_ = as.numeric(r_res[["C/N"]])
	)

	# Wide table: Grain yield + grain N uptake + grain N concentration
	g_bio <- r_bio[r_bio[["Type of biomass"]] == "Grain", ]
	g_nbio <- r_nbio[r_nbio[["Type of biomass"]] == "Grain", ]
	g_nbio <- data.frame(
		Year = trimws(as.character(g_nbio$Year)),
		Treatment = trimws(as.character(g_nbio$Treatment)),
		Fertilization = trimws(as.character(g_nbio$Fertilization)),
		grain_N_uptake_ = as.numeric(g_nbio[["Mean amount (kg/ha)"]]),
		grain_N_uptake_se_ = as.numeric(g_nbio[["Standard error"]])
	)
	g_bio$Year <- trimws(as.character(g_bio$Year))
	g_bio$Treatment <- trimws(as.character(g_bio$Treatment))
	g_bio$Fertilization <- trimws(as.character(g_bio$Fertilization))

	m <- merge(g_bio, fert_df, by = c("Year", "Fertilization"), all.x = TRUE)
	m <- merge(m, g_nbio, by = c("Year", "Treatment", "Fertilization"), all.x = TRUE)
	m <- merge(m, res_df, by = c("Year", "Treatment", "Fertilization"), all.x = TRUE)
	m <- m[order(m$Year, m$Treatment, m$Fertilization), ]

	# Description: 19°33'18.90" S, 46°24'53.83" E, 930 m a.s.l.
	lat <- -(19 + 33/60 + 18.90/3600)
	lon <- 46 + 24/60 + 53.83/3600
	yld_kg <- as.numeric(m[["Mean amount (t/ha)"]]) * 1000

	# Cropping practices dates: urea splits from Excel serials 42744/42772 (2017-01-09, 2017-02-06) and 43095/43131 (2017-12-26, 2018-01-31)
	f_date <- ifelse(!m$fertilizer_used, NA_character_,
		ifelse(m$Year == "2016-2017", "2016-11;2017-01-09;2017-02-06", "2017-11;2017-12-26;2018-01-31"))

	d <- data.frame(
		record_id = seq_len(nrow(m)),
		trial_id = m$Year,
		plot_id = paste(m$Year, m$Treatment, m$Fertilization, sep = "_"),
		crop = "rice",
		variety = "Nerica 4",
		country = "Madagascar",
		adm1 = "Vakinankaratra",
		location = "Ivory",
		latitude = lat,
		longitude = lon,
		elevation = 930,
		geo_from_source = TRUE,
		land_prep_method = "conventional",
		land_prep_date = ifelse(m$Year == "2016-2017", "2016-11", "2017-10"),
		planting_date = ifelse(m$Year == "2016-2017", "2016-11", "2017-11"),
		harvest_date = as.character(as.Date(m[["Date of Observation"]])),
		crop_rotation = ifelse(m$Treatment == "RMC", "velvet bean_crotalaria;rice", "rice;rice"),
		treatment = paste(m$Treatment, m$Fertilization),
		fertilizer_used = m$fertilizer_used,
		fertilizer_type = m$fertilizer_type,
		N_splits = m$N_splits,
		fertilizer_date = f_date,
		N_fertilizer = m$N_fertilizer,
		P_fertilizer = m$P_fertilizer,
		K_fertilizer = m$K_fertilizer,
		OM_used = TRUE,
		OM_type = "farmyard manure",
		OM_amount = m$OM_amount,
		N_organic = m$N_organic,
		P_organic = m$P_organic,
		K_organic = m$K_organic,
		residue_prevcrop_amount_ = m$residue_prevcrop_amount_,
		residue_prevcrop_DM_pct_ = m$residue_prevcrop_DM_pct_,
		residue_prevcrop_N_pct_ = m$residue_prevcrop_N_pct_,
		residue_prevcrop_C_pct_ = m$residue_prevcrop_C_pct_,
		residue_prevcrop_CN_ = m$residue_prevcrop_CN_,
		yield = yld_kg,
		yield_se_ = as.numeric(m[["Standard error"]]) * 1000,
		yield_part = "grain",
		# Grain N uptake (kg/ha) / grain yield (kg/ha) * 1000 -> mg/g
		grain_N = m$grain_N_uptake_ / yld_kg * 1000,
		grain_N_uptake_ = m$grain_N_uptake_,
		grain_N_uptake_se_ = m$grain_N_uptake_se_,
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	key_d <- data.frame(
		record_id = d$record_id,
		trial_id = d$trial_id,
		plot_id = d$plot_id,
		Year = m$Year,
		Treatment = m$Treatment,
		Fertilization = m$Fertilization
	)

	# Long 1: periodic vegetative biomass + vegetative N uptake
	vb <- r_bio[r_bio[["Type of biomass"]] == "Vegetative biomass", ]
	vn <- r_nbio[r_nbio[["Type of biomass"]] == "Vegetative biomass", ]
	vb_df <- data.frame(
		Year = trimws(as.character(vb$Year)),
		Treatment = trimws(as.character(vb$Treatment)),
		Fertilization = trimws(as.character(vb$Fertilization)),
		date = as.character(as.Date(vb[["Date of Observation"]])),
		vegetative_biomass_ = as.numeric(vb[["Mean amount (t/ha)"]]) * 1000,
		vegetative_biomass_se_ = as.numeric(vb[["Standard error"]]) * 1000
	)
	vn_df <- data.frame(
		Year = trimws(as.character(vn$Year)),
		Treatment = trimws(as.character(vn$Treatment)),
		Fertilization = trimws(as.character(vn$Fertilization)),
		date = as.character(as.Date(vn[["Date of observation"]])),
		vegetative_N_uptake_ = as.numeric(vn[["Mean amount (kg/ha)"]]),
		vegetative_N_uptake_se_ = as.numeric(vn[["Standard error"]])
	)
	long_veg <- merge(vb_df, vn_df, by = c("Year", "Treatment", "Fertilization", "date"), all = TRUE)
	long_veg <- merge(long_veg, key_d, by = c("Year", "Treatment", "Fertilization"), all.x = TRUE)
	long_veg <- long_veg[!is.na(long_veg$record_id),
		c("record_id", "trial_id", "plot_id", "date", "vegetative_biomass_", "vegetative_biomass_se_",
		  "vegetative_N_uptake_", "vegetative_N_uptake_se_")]

	# Long 2: periodic soil water content and mineral N
	swn_df <- data.frame(
		Year = trimws(as.character(r_swn$Year)),
		Treatment = trimws(as.character(r_swn$Treatment)),
		Fertilization = trimws(as.character(r_swn$Fertilization)),
		date = as.character(as.Date(r_swn[["Date of observation"]])),
		soil_water_ = as.numeric(r_swn[["Mean Soil water content (mm)"]]),
		soil_water_se_ = as.numeric(r_swn[["Standard Error Soil water content"]]),
		soil_N_min_total = as.numeric(r_swn[["Mean Soil N (kgN/ha)"]]),
		soil_N_min_se_ = as.numeric(r_swn[["Standard Error Soil N"]]),
		soil_NH4_kg_ha_ = as.numeric(r_swn[["Mean Soil NH4 (kgN/ha)"]]),
		soil_NH4_se_ = as.numeric(r_swn[["Standard Error  Soil NH4"]]),
		soil_NO3_kg_ha_ = as.numeric(r_swn[["Mean Soil NO3 (kgN/ha)"]]),
		soil_NO3_se_ = as.numeric(r_swn[["Standard Error  Soil NO3"]])
	)
	swn_df <- merge(swn_df, key_d, by = c("Year", "Treatment", "Fertilization"), all.x = TRUE)
	long_swn <- swn_df[!is.na(swn_df$record_id),
		c("record_id", "trial_id", "plot_id", "date", "soil_water_", "soil_water_se_",
		  "soil_N_min_total", "soil_N_min_se_", "soil_NH4_kg_ha_", "soil_NH4_se_", "soil_NO3_kg_ha_", "soil_NO3_se_")]

	# Long 3: initial multi-depth soil moisture and NO3/NH4 profiles
	parse_depth <- function(x) {
		p <- strsplit(gsub("-", "_", trimws(as.character(x))), "_")
		data.frame(
			depth_top = as.numeric(vapply(p, `[`, character(1), 1)),
			depth_bottom = as.numeric(vapply(p, `[`, character(1), 2))
		)
	}
	d_mi <- parse_depth(r_mi$Depth)
	mi_df <- data.frame(
		Year = trimws(as.character(r_mi$Year)),
		Treatment = trimws(as.character(r_mi$Treatment)),
		Fertilization = trimws(as.character(r_mi$Fertilization)),
		depth_top = d_mi$depth_top,
		depth_bottom = d_mi$depth_bottom,
		soil_moisture_init_ = as.numeric(r_mi[["Relative moisture (%)"]])
	)
	d_ni <- parse_depth(r_ni$Depth)
	ni_df <- data.frame(
		Year = trimws(as.character(r_ni$Year)),
		Treatment = trimws(as.character(r_ni$Treatment)),
		Fertilization = trimws(as.character(r_ni$Fertilization)),
		depth_top = d_ni$depth_top,
		depth_bottom = d_ni$depth_bottom,
		soil_NO3_kg_ha_ = as.numeric(r_ni[["NO3 kg/ha"]]),
		soil_NH4_kg_ha_ = as.numeric(r_ni[["NH4 kg/ha"]])
	)
	init_df <- merge(mi_df, ni_df, by = c("Year", "Treatment", "Fertilization", "depth_top", "depth_bottom"), all = TRUE)
	init_df <- merge(init_df, key_d, by = c("Year", "Treatment", "Fertilization"), all.x = TRUE)
	long_init <- init_df[!is.na(init_df$record_id),
		c("record_id", "trial_id", "plot_id", "depth_top", "depth_bottom",
		  "soil_moisture_init_", "soil_NO3_kg_ha_", "soil_NH4_kg_ha_")]

	dlong <- carobiner::bindr(long_veg, long_swn, long_init)

	carobiner::write_files(path, meta, d, long = dlong)
}
