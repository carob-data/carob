# R script for "carob"
# license: GPL (>=3)

## NOTES
# Long-term organic waste fertilization trial (SOERE PRO-Senegal, 2016-2025, 10 cycles) at the ISRA research station in Sangalkam, Rufisque, Dakar region, Senegal (Vermeire et al. 2026, Data in Brief 65:112456, doi:10.1016/j.dib.2026.112456).
# Randomized complete block design (RCBD, 3 replicates) testing 7 treatments on a 3-crop annual market-garden rotation (tomato cv. Mongal, lettuce cv. Eden, carrot cv. Pamela +): synthetic fertilizer (SF100) and three organic waste products (sewage sludge SS, poultry litter PL, anaerobic digestate D) at 100% and 200% of crop P requirement, with urea and potassium sulfate complementation.
# Coordinates (14°47'34"N, 17°13'30"W; elevation 10 m) and RCBD design are from the associated publication / SOERE PRO Sangalkam site description.
# Carrot and lettuce harvest weights are fresh kg per 8 m2 cropping bed (* 10000 / 8 -> kg/ha), with plant_density from harvested plant/root counts per 8 m2 bed.
# Tomato is harvested weekly over 5 picks (H1-H5) per 48 m2 cropped plot (6 beds x 8 m2). Wide table d reports the seasonal total tomato yield and marketable (Cat1) yield summed over H1-H5 with final harvest_date and mean fruit moisture; individual weekly picks (H1-H5) are preserved in the long table.
# Planting dates are sowing dates (Date_sowing); for transplanted crops (tomato and lettuce), transplanting_date is recorded from Date_transplanting_or_thinning.
# Empty C1 placeholder rows for D100 and D200 (digestate treatments had not started in cycle C1; all fertilizer amounts 0 and all harvest observations NA) are omitted.
# Mineral fertilizer: NPK 10-10-20 (% N, P2O5, K2O -> P = P2O5/2.29, K = K2O/1.2051), urea (46% N), and potassium sulfate (SOP, 41.5% K). Organic waste products (t raw matter/ha -> kg/ha) are joined with batch chemical analyses (data_CHEM_PROPERTIES) to compute N_organic, P_organic, and K_organic where measured (missing in C4 and C5).
# Annual multi-depth soil physico-chemical observations (1_DATA_SOIL_SOERE-PRO_SENEGAL.xlsx, 718 rows across 0-10, 10-20, 0-20, 20-40, 40-60 cm) are stored in the long table (C0 baseline sampled Feb 2016 is linked to Sangalkam_C1).
# Note: in data_LETTUCE_harvest, cycle C10 has Year = 2024 (typo for 2025 in data_AMOUNT and data_MANAGEMENT); tables are joined by Cycle + Crop (+ Treatment) so C10 lettuce matches its 2025 fertilization and management records.

## ISSUES
# Irrigation is used on these dry-season/market-garden plots in Sangalkam, so irrigated is set to TRUE, though exact irrigation method and amounts are not in the data files.
# "digestate" is not yet in the terminag OM vocabulary; kept as "digestate".
# Potassium rates reach 405 kg K/ha on 200%-dose treatments (expressed per cropped bed area of 48 m2), exceeding terminag's upper bound of 300 kg/ha.
# Fresh vegetable moisture reaches 98.9% (carrot/lettuce/tomato), exceeding terminag's upper bound of 90%; seasonal tomato fresh yield reaches 123.3 t/ha; some beds have 0 harvested plants (plant_density = 0).
# Measured soil fine sand (up to 62%), zero N_NO3/N_NH4, total P (up to 5070 mg/kg), and Olsen P (up to 203 mg/kg) exceed current terminag bounds on heavily OWP-amended sandy soils.
# Bare-soil reference plots (BS, BS_*) in 1_DATA_SOIL are kept in the long table even though they have no crop rows in d (causing the plot_id mismatch message between long and wide).

## NEW VARIABLES
# harvest_number_: weekly tomato harvest pick code (H1-H5) in the long table
# harvest_yield_: fresh tomato yield of a single weekly harvest pick (kg/ha)
# harvest_yield_marketable_: marketable (Cat1) fresh tomato yield of a single weekly pick (kg/ha)
# harvest_yield_moisture_: fruit moisture content of a single weekly tomato pick (%)

## NOT PROCESSED
# 4_DATA_CROP_CARROT / 5_DATA_CROP_LETTUCE / 6_DATA_CROP_TOMATO observations sheets: per-plant morphological measurements (root length/diameter, leaf height, lettuce diameter, tomato firmness/leaves per plant) and binary pest/disease presence scores on 6-8 sampled plants.
# Size-class sub-breakdowns of carrot (diam >2cm, <=2cm, deformed, damaged), lettuce (headed vs non-headed counts), and tomato (Cat1 diameter classes, Cat2-Cat5 individual weights/counts).
# 2_DATA_FERTILIZATION data_CHEM_PROPERTIES: batch pH, C_tot, Ca_tot, Mg_tot, Na_tot, N_Kjeld of OWP batches (DM, N_tot, P_tot, K_tot are used to compute N/P/K_organic).


carob_script <- function(path) {

"
Replication Data for: A dataset of soil properties and crop observations from a long-term organic fertilization trial in Sub-Sahelian market gardening

Recycling the growing stock of organic waste products (OWP) from cities, factories, and farms is a key challenge for sustainable agriculture. However, it must be done with awareness of potential long-term environmental and health risks. The SOERE PRO observatory was created in this context, including the trial in Sangalkam, Dakar region (Senegal), where these data were collected. Since 2016, four fertiliser types - one mineral (synthetic) and three organic - were applied on three crops a year (tomato, lettuce, carrot). A multi-variable dataset was consistently documented, including soil physico-chemical parameters measured at three depth once a year, OWP characterization, crop parameters, and management activities. All soil, OWP and vegetables samples are stored in a sample bank in Dakar, also described in the dataset, and available for additional analyses. The dataset has been shared in a Dataverse repository for free (re)use in meta-analyses, process modelling and other environmental studies or new projects (the trial is still running), in particular for understanding tropical ecosystem dynamics (nutrients, contaminants, biodiversity), and guiding sustainable land management practices.
"

	uri <- "doi:10.18167/DVN1/CHZ81W"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.dib.2026.112456",
		project = "SOERE PRO",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "treatment;OM_type;OM_amount;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield;yield_marketable;fwy_residue;plant_density",
		carob_contributor = NA,
		carob_LLM = "Gemini 4 Neon",
		carob_date = "202?-??-??",
		carob_completion = 85,
		carob_effort = 0.8
	)

	na_vals <- c("", "NA", "na", "<QL", "no_thinning")
	as_iso <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) {
			return(as.character(as.Date(x)))
		}
		x <- trimws(as.character(x))
		out <- rep(NA_character_, length(x))
		is_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}", x)
		out[is_iso] <- substr(x[is_iso], 1, 10)
		is_num <- !is_iso & !is.na(x) & grepl("^\\d+(\\.\\d+)?$", x)
		if (any(is_num)) {
			out[is_num] <- as.character(as.Date(as.numeric(x[is_num]), origin="1899-12-30"))
		}
		out
	}

	f_soil <- ff[basename(ff) == "1_DATA_SOIL_SOERE-PRO_SENEGAL.xlsx"]
	f_fert <- ff[basename(ff) == "2_DATA_FERTILIZATION_SOERE-PRO_SENEGAL.xlsx"]
	f_mgmt <- ff[basename(ff) == "3_DATA_MANAGEMENT_SOERE-PRO_SENEGAL.xlsx"]
	f_car  <- ff[basename(ff) == "4_DATA_CROP_CARROT_SOERE-PRO_SENEGAL.xlsx"]
	f_let  <- ff[basename(ff) == "5_DATA_CROP_LETTUCE_SOERE-PRO_SENEGAL.xlsx"]
	f_tom  <- ff[basename(ff) == "6_DATA_CROP_TOMATO_SOERE-PRO_SENEGAL.xlsx"]

	r_amt  <- carobiner::read.excel(f_fert, sheet="data_AMOUNT", na=na_vals)
	r_chem <- carobiner::read.excel(f_fert, sheet="data_CHEM_PROPERTIES", na=na_vals)
	r_mgmt <- carobiner::read.excel(f_mgmt, sheet="data_MANAGEMENT", na=na_vals)
	r_soil <- carobiner::read.excel(f_soil, sheet="data_SOIL", na=na_vals)

	# Fertilization amounts + OWP batch chemical properties
	om_map <- c(
		sewage_sludge = "sludge",
		poultry_litter = "poultry manure",
		digestate = "digestate",
		synthetic_fertilizer = "none"
	)
	chem_key <- tolower(trimws(as.character(r_chem$Fertilizer_type)))
	chem_df <- data.frame(
		Cycle = r_chem$Cycle,
		Crop = r_chem$Crop,
		ft_key = chem_key,
		DM = as.numeric(r_chem$DM),
		# g/kg DM -> fraction (* 1e-3); mg/kg DM -> fraction (* 1e-6)
		N_frac = as.numeric(r_chem$N_tot) / 1000,
		P_frac = as.numeric(r_chem$P_tot) / 1e6,
		K_frac = as.numeric(r_chem$K_tot) / 1e6
	)
	chem_df <- chem_df[chem_df$ft_key %in% c("sewage_sludge", "poultry_litter", "digestate"), ]
	chem_agg <- aggregate(cbind(DM, N_frac, P_frac, K_frac) ~ Cycle + Crop + ft_key, chem_df,
		function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm=TRUE), na.action=na.pass)

	ft_amt_key <- tolower(trimws(as.character(r_amt$Fertilizer_type)))
	npk <- as.numeric(r_amt$Amount_applied_chem_10_10_20)
	urea <- as.numeric(r_amt$Amount_applied_chem_urea)
	sop <- as.numeric(r_amt$Amount_applied_chem_K2SO4)
	om_t <- unname(om_map[ft_amt_key])
	om_amt <- ifelse(om_t == "none", 0, as.numeric(r_amt$Amount_applied_OWP) * 1000)
	om_t[om_amt == 0] <- "none"

	ft_str <- mapply(function(a, b, c) {
		v <- c(if (!is.na(a) && a > 0) "NPK", if (!is.na(b) && b > 0) "urea", if (!is.na(c) && c > 0) "SOP")
		if (length(v) == 0) "none" else paste(v, collapse="; ")
	}, npk, urea, sop, USE.NAMES=FALSE)

	d_fert <- data.frame(
		Year = as.integer(r_amt$Year),
		Cycle = as.character(r_amt$Cycle),
		Crop = as.character(r_amt$Crop),
		Treatment = as.character(r_amt$Treatment),
		ft_key = ft_amt_key,
		OM_type = om_t,
		OM_amount = om_amt,
		OM_used = om_amt > 0,
		N_fertilizer = npk * 0.10 + urea * 0.46,
		P_fertilizer = npk * 0.10 / 2.29,
		K_fertilizer = npk * 0.20 / 1.2051 + sop * 0.415,
		fertilizer_used = (npk + urea + sop) > 0,
		fertilizer_type = ft_str
	)
	d_fert <- merge(d_fert, chem_agg, by=c("Cycle", "Crop", "ft_key"), all.x=TRUE)
	om_dm_kg <- d_fert$OM_amount * (d_fert$DM / 100)
	d_fert$N_organic <- ifelse(d_fert$OM_used, om_dm_kg * d_fert$N_frac, 0)
	d_fert$P_organic <- ifelse(d_fert$OM_used, om_dm_kg * d_fert$P_frac, 0)
	d_fert$K_organic <- ifelse(d_fert$OM_used, om_dm_kg * d_fert$K_frac, 0)
	d_fert <- d_fert[, c("Cycle", "Crop", "Treatment", "OM_type", "OM_amount", "OM_used",
		"N_organic", "P_organic", "K_organic", "N_fertilizer", "P_fertilizer", "K_fertilizer",
		"fertilizer_used", "fertilizer_type")]

	# Management dates and cultivars (3_DATA_MANAGEMENT README)
	var_map <- c(Tomato = "Mongal", Lettuce = "Eden", Carrot = "Pamela +")
	d_mgmt <- data.frame(
		Cycle = as.character(r_mgmt$Cycle),
		Crop = as.character(r_mgmt$Crop),
		variety = unname(var_map[as.character(r_mgmt$Crop)]),
		planting_date = as_iso(r_mgmt$Date_sowing),
		transplanting_date = ifelse(r_mgmt$Crop == "Carrot", NA_character_, as_iso(r_mgmt$Date_transplanting_or_thinning)),
		harvest_date = as_iso(r_mgmt$Harvest_date),
		land_prep_method = "tillage",
		land_prep_date = as_iso(r_mgmt$Tillage_start_date),
		fertilizer_date = ifelse(
			!is.na(as_iso(r_mgmt$SF_application_date)) & !is.na(as_iso(r_mgmt$Date_chemical_complementation)),
			paste(as_iso(r_mgmt$SF_application_date), as_iso(r_mgmt$Date_chemical_complementation), sep="; "),
			as_iso(r_mgmt$SF_application_date)
		)
	)

	# 1) Carrot (per 8 m2 cropping bed)
	r_car <- carobiner::read.excel(f_car, sheet="data_CARROT_harvest", na=na_vals)
	r_cdw <- carobiner::read.excel(f_car, sheet="data_CARROT_dry_weight", na=na_vals)
	# Exclude C1 D100/D200 placeholder rows (digestate started in C2; all observations NA)
	r_car <- r_car[!(r_car$Cycle == "C1" & r_car$Treatment %in% c("D100", "D200")), ]
	d_car <- data.frame(
		Year = as.integer(r_car$Year),
		Cycle = as.character(r_car$Cycle),
		Crop = "Carrot",
		Treatment = as.character(r_car$Treatment),
		Plot = as.character(r_car$Plot),
		plot_id = paste(r_car$Plot, r_car$Cropping_bed, sep="_"),
		plot_area = 8,
		crop = "carrot",
		yield = as.numeric(r_car$Tot_weight_carrots) / 8 * 10000,
		yield_part = "roots",
		yield_marketable = as.numeric(r_car$Weight_diam_sup_2cm) / 8 * 10000,
		fwy_residue = as.numeric(r_car$Total_weight_leaves) / 8 * 10000,
		plant_density = as.numeric(r_car$Tot_nb_carrots) / 8 * 10000,
		harvest_date_obs = NA_character_
	)
	d_cdw <- data.frame(
		Cycle = as.character(r_cdw$Cycle),
		Plot = as.character(r_cdw$Plot),
		yield_moisture = as.numeric(r_cdw$Perc_hum_carrot_root)
	)
	d_car <- merge(d_car, d_cdw, by=c("Cycle", "Plot"), all.x=TRUE)

	# 2) Lettuce (per 8 m2 cropping bed)
	r_let <- carobiner::read.excel(f_let, sheet="data_LETTUCE_harvest ", na=na_vals)
	r_ldw <- carobiner::read.excel(f_let, sheet="data_LETTUCE_dry_weight", na=na_vals)
	r_let <- r_let[!(r_let$Cycle == "C1" & r_let$Treatment %in% c("D100", "D200")), ]
	d_let <- data.frame(
		Year = as.integer(r_let$Year),
		Cycle = as.character(r_let$Cycle),
		Crop = "Lettuce",
		Treatment = as.character(r_let$Treatment),
		Plot = as.character(r_let$Plot),
		plot_id = paste(r_let$Plot, r_let$Cropping_bed, sep="_"),
		plot_area = 8,
		crop = "lettuce",
		yield = as.numeric(r_let$Tot_weight_lettuce) / 8 * 10000,
		yield_part = "leaves",
		yield_marketable = as.numeric(r_let$Weight_headed) / 8 * 10000,
		fwy_residue = NA_real_,
		plant_density = as.numeric(r_let$Tot_nb_lettuce) / 8 * 10000,
		harvest_date_obs = NA_character_
	)
	d_ldw <- data.frame(
		Cycle = as.character(r_ldw$Cycle),
		Plot = as.character(r_ldw$Plot),
		yield_moisture = as.numeric(r_ldw$Perc_hum_lettuce_leaves)
	)
	d_let <- merge(d_let, d_ldw, by=c("Cycle", "Plot"), all.x=TRUE)

	# 3) Tomato (5 weekly harvests H1-H5 per 48 m2 cropped plot -> seasonal sum in d, weekly picks in long)
	r_tom <- carobiner::read.excel(f_tom, sheet="data_TOMATO_harvest", na=na_vals)
	r_tdw <- carobiner::read.excel(f_tom, sheet="data_TOMATO_dry_weight", na=na_vals)
	tom_tot_kg <- as.numeric(r_tom$Cat1_tot_weight) + as.numeric(r_tom$Cat2_weight) +
		as.numeric(r_tom$Cat3_weight) + as.numeric(r_tom$Cat4_weight) + as.numeric(r_tom$Cat5_weight)
	tom_pick <- data.frame(
		Year = as.integer(r_tom$Year),
		Cycle = as.character(r_tom$Cycle),
		Treatment = as.character(r_tom$Treatment),
		Plot = as.character(r_tom$Plot),
		harvest_number_ = as.character(r_tom$Harvest_nb),
		date = as_iso(r_tom$Date_harvest),
		yield = tom_tot_kg / 48 * 10000,
		yield_marketable = as.numeric(r_tom$Cat1_tot_weight) / 48 * 10000,
		plant_density = as.numeric(r_tom$Nb_plants) / 48 * 10000
	)
	tom_dw_pick <- data.frame(
		Year = as.integer(r_tdw$Year),
		Cycle = as.character(r_tdw$Cycle),
		Plot = as.character(r_tdw$Plot),
		harvest_number_ = as.character(r_tdw$Harvest_nb),
		yield_moisture = as.numeric(r_tdw$Perc_hum_tomato_fruit)
	)
	tom_dw_plot <- aggregate(yield_moisture ~ Year + Cycle + Plot, tom_dw_pick,
		function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm=TRUE), na.action=na.pass)

	tom_sum <- aggregate(cbind(yield, yield_marketable) ~ Year + Cycle + Treatment + Plot, tom_pick, sum, na.rm=TRUE)
	tom_pd  <- aggregate(plant_density ~ Year + Cycle + Treatment + Plot, tom_pick,
		function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm=TRUE), na.action=na.pass)
	tom_hd  <- aggregate(date ~ Year + Cycle + Treatment + Plot, tom_pick,
		function(x) if (all(is.na(x))) NA_character_ else max(x, na.rm=TRUE), na.action=na.pass)
	names(tom_hd)[5] <- "harvest_date_obs"

	d_tom <- merge(tom_sum, tom_pd, by=c("Year", "Cycle", "Treatment", "Plot"), all.x=TRUE)
	d_tom <- merge(d_tom, tom_hd, by=c("Year", "Cycle", "Treatment", "Plot"), all.x=TRUE)
	d_tom <- merge(d_tom, tom_dw_plot, by=c("Year", "Cycle", "Plot"), all.x=TRUE)
	d_tom$Crop <- "Tomato"
	d_tom$plot_id <- d_tom$Plot
	d_tom$plot_area <- 48
	d_tom$crop <- "tomato"
	d_tom$yield_part <- "fruit"
	d_tom$fwy_residue <- NA_real_

	cols_union <- c("Cycle", "Crop", "Treatment", "Plot", "plot_id", "plot_area", "crop",
		"yield", "yield_part", "yield_marketable", "yield_moisture", "fwy_residue",
		"plant_density", "harvest_date_obs")
	d <- rbind(d_car[, cols_union], d_let[, cols_union], d_tom[, cols_union])
	d <- merge(d, d_fert, by=c("Cycle", "Crop", "Treatment"), all.x=TRUE)
	d <- merge(d, d_mgmt, by=c("Cycle", "Crop"), all.x=TRUE)
	d$harvest_date[!is.na(d$harvest_date_obs)] <- d$harvest_date_obs[!is.na(d$harvest_date_obs)]
	d$harvest_date_obs <- NULL

	d$trial_id <- paste("Sangalkam", d$Cycle, sep="_")
	d$treatment <- d$Treatment
	d$rep <- as.integer(sub(".*_", "", d$Plot))
	d$yield_isfresh <- TRUE
	d$crop_rotation <- "tomato; lettuce; carrot"
	d$country <- "Senegal"
	d$adm1 <- "Dakar"
	d$adm2 <- "Rufisque"
	d$location <- "Sangalkam"
	# ISRA Sangalkam SOERE PRO site (14°47'34"N, 17°13'30"W, 10 m; Vermeire et al. 2026)
	d$longitude <- -17.2250
	d$latitude <- 14.7928
	d$elevation <- 10
	d$geo_from_source <- TRUE
	# Market gardening plots are irrigated year-round in Sangalkam
	d$irrigated <- TRUE
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	d$Cycle <- d$Crop <- d$Treatment <- d$Plot <- NULL

	# Long table 1: weekly tomato picks (H1-H5)
	tom_long <- merge(tom_pick, tom_dw_pick[tom_dw_pick$harvest_number_ != "H1-5", ],
		by=c("Year", "Cycle", "Plot", "harvest_number_"), all.x=TRUE)
	long_tom <- data.frame(
		trial_id = paste("Sangalkam", tom_long$Cycle, sep="_"),
		plot_id = tom_long$Plot,
		date = tom_long$date,
		harvest_number_ = tom_long$harvest_number_,
		harvest_yield_ = tom_long$yield,
		harvest_yield_marketable_ = tom_long$yield_marketable,
		harvest_yield_moisture_ = tom_long$yield_moisture
	)

	# Long table 2: annual multi-depth soil measurements (1_DATA_SOIL_SOERE-PRO_SENEGAL.xlsx)
	dp <- strsplit(as.character(r_soil$Depth_soil), "_")
	dtop <- as.numeric(vapply(dp, `[`, character(1), 1))
	dbot <- as.numeric(vapply(dp, `[`, character(1), 2))
	# C0 baseline soil (Feb 2016) is linked to Sangalkam_C1 so trial_id matches wide records
	soil_cyc <- ifelse(as.character(r_soil$Cycle) == "C0", "C1", as.character(r_soil$Cycle))
	long_soil <- data.frame(
		trial_id = paste("Sangalkam", soil_cyc, sep="_"),
		plot_id = as.character(r_soil$Plot),
		date = as_iso(r_soil$Date_sampling),
		depth_top = dtop,
		depth_bottom = dbot,
		soil_clay = as.numeric(r_soil$Clay),
		soil_silt_fine = as.numeric(r_soil$Fine_silt),
		soil_silt_coarse = as.numeric(r_soil$Coarse_silt),
		soil_silt = as.numeric(r_soil$Fine_silt) + as.numeric(r_soil$Coarse_silt),
		soil_sand_fine = as.numeric(r_soil$Fine_sand),
		soil_sand_coarse = as.numeric(r_soil$Coarse_sand),
		soil_sand = as.numeric(r_soil$Fine_sand) + as.numeric(r_soil$Coarse_sand),
		soil_pH = as.numeric(r_soil$pH_H2O),
		soil_pH_KCl = as.numeric(r_soil$pH_KCl),
		soil_NO3 = as.numeric(r_soil$N_NO3),
		soil_NH4 = as.numeric(r_soil$N_NH4),
		# g N/kg -> mg N/kg
		soil_N_total = as.numeric(r_soil$N_tot) * 1000,
		# g C/kg -> %
		soil_C_total = as.numeric(r_soil$C_tot) / 10,
		soil_SOC = as.numeric(r_soil$C_org) / 10,
		soil_P_total = as.numeric(r_soil$P_tot),
		soil_P = as.numeric(r_soil$P_avail),
		soil_Ca_exch = as.numeric(r_soil$Ca_exch),
		soil_Mg_exch = as.numeric(r_soil$Mg_exch),
		soil_Na_exch = as.numeric(r_soil$Na_exch),
		soil_K_exch = as.numeric(r_soil$K_exch),
		soil_CEC = as.numeric(r_soil$CEC)
	)
	long_soil$soil_P_method <- ifelse(!is.na(long_soil$soil_P), "Olsen", NA_character_)

	long <- carobiner::bindr(long_tom, long_soil)
	carobiner::write_files(path, meta, d, long=long)
}
