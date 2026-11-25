# R script for "carob"
# license: GPL (>=3)

## NOTES
# Irrigated ratoon-1 (R1) sugarcane weed-control trial (P12, 2018-2019, variety R587) at the CIRAD La Mare experimental station, Réunion (Projet AgriecoH).
# Cycle start is harvesting_date_ini (2018-10-02) and harvest is harvesting_date (2019-09-18).
# Treatments (Factor_Trial joined via Factor_Unit) compare inter-row weeding methods on plots P1-P6 (234 m2 each) plus weedy control TE (121 m2, no yield row): M1 (broyage/mowing in inter-row), Tref (P2 & P5, chemical weeding), M2 (P3, black cowpea Vigna unguiculata cover crop sown in inter-row), M3 (P4, Weedseeker spot-spraying), M4 (P6, patte d'oie shallow cultivation), and TE (weeds left in inter-row).
# On P3, the cowpea intercrop (VIGSI_B, sown 2018-11-19 at 22 kg/ha mechanically, not harvested) is given its own row sharing plot_id = "P3" so its planting date, seed rate, and ground cover attach cleanly.
# Mineral fertilizer rates in ITK.xlsx are 120-80-192 kg/ha (800 kg/ha of 15-10-24) on 2018-10-23 plus 82.5-44-121 kg/ha (550 kg/ha of 15-8-22) on 2018-12-03 (plus 41.4 kg N/ha on 2019-01-23 for TE). Although column headers say P and K, the exact 15-10-24 and 15-8-22 ratios show these are P2O5 and K2O, so P and K are converted to elemental P (/ 2.29) and K (/ 1.2051).
# Herbicide products in ITK.xlsx are mapped to active ingredients: Merlin (isoxaflutole), Prowl (pendimethalin), Camix (s-metolachlor; mesotrione), Roundup (glyphosate), 2,4-D (2,4-D), Sencoral (metribuzin), Callisto (mesotrione).
# Baseline trial-level soil properties (0-30 cm, sampled 2017-09-18 on P12_2018_2019_Soil) are attached to all plots: soil_organic_C (g/kg -> %), soil_organic_N (g/kg -> mg/kg).
# Long table captures repeated plot-level weed cover (*_Weed), per-species weed cover (*_<EPPO> mapped to scientific names via Accession.xlsx Taxo), and cowpea cover-crop ground cover (P3_*_CC). Redundant *_Weeding rows (= Weed + CC) and non-experimental Bordure/Chemins units are excluded.
# Daily weather data (Weather.xlsx, P12_Interpol_smartis) are passed via wth; global radiation (3-31 MJ/m2/day) is converted to kJ/m2 (* 1000).

## ISSUES
# P2 and P5 are both the chemical-weeding treatment (Tref); no explicit replicate column is given. Individual herbicide application rows for P2 and P5 are missing in ITK.xlsx even though Observation.xlsx reports IFTH_tot_cycle = 5.6 for P2 and P5, so herbicide_used is TRUE while herbicide_product is NA for P2 and P5.
# Cane yield moisture is not stated in the dataset.
# P3 cowpea intercrop and TE (weedy control) have NA yield because they were not harvested.
# Experimental_design.xlsx Design sheet column "profondeur" has text "0-30cm" on the last row only, causing a readxl type-coercion message if read without col_types.

## NEW VARIABLES
# observation_zone_: spatial zone within the plot where cover was observed (plot, row, inter-row)

## NOT PROCESSED
# Observation.xlsx: IFTH_tot_cycle (herbicide treatment frequency index, 5.6 on P2/P5, 2.7 on P3); Weeding_manual_number_tot_cycle / Weeding_manual_number_tot_cyle (all 0 on P2, P3, P5); *_Weeding ground cover (redundant sum of Weed + CC); Bordure and Chemins border/path units; soil_AWC_mm (100 mm) and soil_CN_dm_ratio_g.g-1 (11.83).
# ITK.xlsx: individual commercial herbicide product volumes (L/ha or kg product/ha without formulation ai concentration); sowing_nb_row_in_IR (4 rows of cowpea in inter-row).


carob_script <- function(path) {

"
Experimental dataset for the evaluation of mechanical weed control methods with and without tillage (2018 - 2019) - P12 trial - Projet AgriecoH

The AgriecoH (Agro-equipment and low Herbicide innovation) project, an Ecophyto experimentation project with innovative micromechanisation techniques to reduce the use of herbicides in cane. This project is based on trials conducted in experimental stations or in co-design with growers in Réunion. This is an experimental dataset from an irrigated sugarcane trial (R1) set up in 2018 and harvested in 2019, at the La Mare experimental site.
"

	uri <- "doi:10.18167/DVN1/0LKVJU"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIRAD",
		publication = NA,
		project = "AgriecoH",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;weeding_method;intercrops;herbicide_used",
		response_vars = "yield;sugar_content;weed_cover;ground_cover",
		carob_contributor = NA,
		carob_LLM = "Gemini 4 Neon",
		carob_date = "202?-??-??",
		carob_completion = 85,
		carob_effort = 0.7
	)

	f_acc <- ff[basename(ff) == "Accession.xlsx"]
	f_des <- ff[basename(ff) == "Experimental_design.xlsx"]
	f_itk <- ff[basename(ff) == "ITK.xlsx"]
	f_obs <- ff[basename(ff) == "Observation.xlsx"]
	f_wth <- ff[basename(ff) == "Weather.xlsx"]

	r_tax <- carobiner::read.excel(f_acc, sheet="Taxo")
	r_fu  <- carobiner::read.excel(f_des, sheet="Factor_Unit")
	r_ft  <- carobiner::read.excel(f_des, sheet="Factor_Trial")
	r_itk <- carobiner::read.excel(f_itk, sheet="Itk")
	r_obs <- carobiner::read.excel(f_obs, sheet="PlotPlant_data")
	r_wth <- carobiner::read.excel(f_wth, sheet="Weather_day")

	plots <- c(paste0("P", 1:6), "TE")

	# Treatments from Factor_Unit + Factor_Trial
	fu <- r_fu[r_fu$unit_code %in% plots, ]
	desc <- r_ft$description[match(fu$factor_level, r_ft$factor_level)]
	wm_map <- c(
		M1 = "mowing",
		Tref = "chemical",
		M2 = "cover crop; mechanical; chemical",
		M3 = "spot spraying",
		M4 = "mechanical; chemical",
		TE = "none"
	)
	trt <- data.frame(
		plot_id = fu$unit_code,
		treatment = trimws(gsub("\\s+", " ", desc)),
		weeding_method = unname(wm_map[fu$factor_level]),
		level = fu$factor_level
	)

	# Plot areas from Experimental_design.xlsx Design sheet (P1-P6 = 234 m2, TE = 121 m2)
	area_map <- c(P1=234, P2=234, P3=234, P4=234, P5=234, P6=234, TE=121)

	# Mineral fertilizer from ITK.xlsx (NPK 15-10-24 and 15-8-22 -> P2O5/2.29 and K2O/1.2051)
	fert_vars <- c("ferti_N_app_rate_kg.ha-1", "ferti_P_app_rate_kg.ha-1", "ferti_K_app_rate_kg.ha-1")
	fv <- r_itk[r_itk$itk_variable %in% fert_vars, ]
	fv_plot <- sub("_R$", "", fv$unit_code)
	fv <- fv[fv_plot %in% plots, ]
	fv_plot <- fv_plot[fv_plot %in% plots]
	fv_rate <- as.numeric(fv$itk_value)
	fv_df <- data.frame(plot_id = fv_plot, var = fv$itk_variable, rate = fv_rate, date = as.character(as.Date(fv$itk_date, "%d/%m/%Y")))

	fn <- aggregate(rate ~ plot_id, fv_df[fv_df$var == fert_vars[1], ], sum)
	fp <- aggregate(rate ~ plot_id, fv_df[fv_df$var == fert_vars[2], ], sum)
	fk <- aggregate(rate ~ plot_id, fv_df[fv_df$var == fert_vars[3], ], sum)
	nspl <- aggregate(date ~ plot_id, fv_df[fv_df$var == fert_vars[1], ], function(x) length(unique(x)))
	fdat <- aggregate(date ~ plot_id, fv_df, function(x) paste(sort(unique(x)), collapse="; "))
	names(fn)[2] <- "N_fertilizer"
	names(fp)[2] <- "P_fertilizer"
	names(fk)[2] <- "K_fertilizer"
	names(nspl)[2] <- "N_splits"
	names(fdat)[2] <- "fertilizer_date"
	fp$P_fertilizer <- fp$P_fertilizer / 2.29
	fk$K_fertilizer <- fk$K_fertilizer / 1.2051

	# Herbicides from ITK.xlsx
	hb_map <- c(
		"herbicide_merlin_L.ha-1" = "isoxaflutole",
		"herbicide_prowl_L.ha-1" = "pendimethalin",
		"herbicide_camix_L.ha-1" = "s-metolachlor; mesotrione",
		"herbicide_roundup_L.ha-1" = "glyphosate",
		"herbicide_2.4D_L.ha-1" = "2,4-D",
		"herbicide_sencoral_kg.ha-1" = "metribuzin",
		"herbicide_callisto_L.ha-1" = "mesotrione"
	)
	hb <- r_itk[grepl("^herbicide_", r_itk$itk_variable), ]
	hb_plot <- sub("_.*$", "", hb$unit_code)
	hb_df <- data.frame(
		plot_id = hb_plot,
		date = as.character(as.Date(hb$itk_date, "%d/%m/%Y")),
		ai = unname(hb_map[hb$itk_variable])
	)
	hb_df <- hb_df[hb_df$plot_id %in% plots, ]
	hb_prod <- aggregate(ai ~ plot_id, hb_df[!is.na(hb_df$ai), ], function(x) {
		paste(unique(trimws(unlist(strsplit(x, ";")))), collapse="; ")
	})
	hb_dates <- aggregate(date ~ plot_id, hb_df, function(x) paste(sort(unique(x)), collapse="; "))
	hb_times <- aggregate(date ~ plot_id, hb_df, function(x) length(unique(x)))
	names(hb_prod)[2] <- "herbicide_product"
	names(hb_dates)[2] <- "herbicide_dates"
	names(hb_times)[2] <- "herbicide_times"

	# Mechanical / manual weeding events from ITK.xlsx
	wd <- r_itk[grepl("^Weeding_", r_itk$itk_variable), ]
	wd_df <- data.frame(
		plot_id = sub("_.*$", "", wd$unit_code),
		date = as.character(as.Date(wd$itk_date, "%d/%m/%Y"))
	)
	wd_df <- wd_df[wd_df$plot_id %in% plots, ]
	wd_dates <- aggregate(date ~ plot_id, wd_df, function(x) paste(sort(unique(x)), collapse="; "))
	wd_times <- aggregate(date ~ plot_id, wd_df, function(x) length(unique(x)))
	names(wd_dates)[2] <- "weeding_dates"
	names(wd_times)[2] <- "weeding_times"

	# Yield and sugar content from Observation.xlsx
	y_obs <- r_obs[r_obs$OBS_VARIABLE == "Yield_CAS_tha", ]
	s_obs <- r_obs[r_obs$OBS_VARIABLE == "Richness_Sugar_CAS", ]
	y_df <- data.frame(
		plot_id = sub("_Cane$", "", y_obs$UNIT_CODE),
		yield = as.numeric(y_obs$VALEUR_QUANTITATIVE) * 1000
	)
	s_df <- data.frame(
		plot_id = sub("_Cane$", "", s_obs$UNIT_CODE),
		sugar_content = as.numeric(s_obs$VALEUR_QUANTITATIVE)
	)

	# Baseline trial soil observation (P12_2018_2019_Soil, 0-30 cm, 2017-09-18)
	so <- r_obs[r_obs$UNIT_CODE == "P12_2018_2019_Soil", ]
	sval <- function(nm) so$VALEUR_QUANTITATIVE[so$OBS_VARIABLE == nm][1]

	d_cane <- data.frame(
		rec_key = paste0(plots, "_cane"),
		trial_id = "P12_2018_2019",
		plot_id = plots,
		plot_area = unname(area_map[plots]),
		crop = "sugarcane",
		variety = "R587",
		planting_date = "2018-10-02",
		harvest_date = "2019-09-18",
		yield_part = "stems",
		country = "Réunion",
		location = "La Mare",
		longitude = 55.53255556,
		latitude = -20.9036528,
		elevation = 70,
		geo_from_source = TRUE,
		irrigated = TRUE,
		on_farm = FALSE,
		is_survey = FALSE,
		fertilizer_used = TRUE,
		fertilizer_type = "NPK",
		soil_sample_date = "2017-09-18",
		depth_top = 0,
		depth_bottom = 30,
		soil_depth = as.numeric(sval("soil_depth_cm")),
		soil_type = as.character(sval("soil_type_WRB")),
		soil_pH = as.numeric(sval("soil_pH")),
		# g/kg -> mg/kg
		soil_N_total = as.numeric(sval("soil_organic_N_g.kg-1")) * 1000,
		# g/kg -> %
		soil_SOC = as.numeric(sval("soil_organic_C_g.kg-1")) / 10,
		soil_P = as.numeric(sval("soil_P_mg.kg-1")),
		soil_K_exch = as.numeric(sval("soil_exchange_K_cmol.kg-1")),
		soil_Ca_exch = as.numeric(sval("soil_exchange_Ca_cmol.kg-1")),
		soil_Mg_exch = as.numeric(sval("soil_exchange_Mg_cmol.kg-1")),
		soil_Na_exch = as.numeric(sval("soil_exchange_Na_cmol.kg-1")),
		soil_CEC = as.numeric(sval("soil_CEC_dm_cmol.kg-1"))
	)

	d_cane <- merge(d_cane, trt, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, y_df, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, s_df, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, fn, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, fp, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, fk, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, nspl, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, fdat, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, hb_prod, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, hb_dates, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, hb_times, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, wd_dates, by="plot_id", all.x=TRUE)
	d_cane <- merge(d_cane, wd_times, by="plot_id", all.x=TRUE)

	# P2 and P5 are chemical weeding (IFTH_tot_cycle = 5.6); TE had no herbicide
	d_cane$herbicide_used <- d_cane$level != "TE"
	d_cane$weeding_done <- d_cane$level != "TE"
	d_cane$weeding_times[is.na(d_cane$weeding_times)] <- 0L
	d_cane$intercropped <- d_cane$level == "M2"
	d_cane$intercrops <- ifelse(d_cane$level == "M2", "cowpea", "none")
	d_cane$cover_crop_used <- d_cane$level == "M2"
	d_cane$cover_crop <- ifelse(d_cane$level == "M2", "cowpea", "none")
	d_cane$seed_rate <- NA_real_
	d_cane$planting_method <- NA_character_
	d_cane$level <- NULL

	# Separate row for the cowpea intercrop on P3 (sown 2018-11-19, 22 kg/ha, not harvested)
	d_cc <- d_cane[d_cane$plot_id == "P3", ]
	d_cc$rec_key <- "P3_cowpea"
	d_cc$crop <- "cowpea"
	d_cc$variety <- NA_character_
	d_cc$intercrops <- "sugarcane"
	d_cc$planting_date <- "2018-11-19"
	d_cc$seed_rate <- 22
	d_cc$planting_method <- "mechanized"
	d_cc$yield <- NA_real_
	d_cc$yield_part <- "aboveground biomass"
	d_cc$sugar_content <- NA_real_
	d_cc$N_fertilizer <- 0
	d_cc$P_fertilizer <- 0
	d_cc$K_fertilizer <- 0
	d_cc$N_splits <- 0L
	d_cc$fertilizer_used <- FALSE
	d_cc$fertilizer_type <- "none"
	d_cc$fertilizer_date <- NA_character_

	d <- rbind(d_cane, d_cc)
	d$record_id <- seq_len(nrow(d))

	# Long table: weed cover (*_Weed), weed species cover (*_<EPPO>), and cowpea cover crop (*_CC)
	gc <- r_obs[r_obs$OBS_VARIABLE == "plant_ground_cover_%", ]
	gc_plot <- sub("_.*$", "", gc$UNIT_CODE)
	gc_suf <- sub("^[^_]+_", "", gc$UNIT_CODE)
	keep_gc <- gc_plot %in% plots & !grepl("Weeding$", gc_suf)
	gc <- gc[keep_gc, ]
	gc_plot <- gc_plot[keep_gc]
	gc_suf <- gc_suf[keep_gc]

	is_cc <- grepl("CC$", gc_suf)
	is_tot_weed <- grepl("Weed$", gc_suf)
	is_sp <- !(is_cc | is_tot_weed)

	zone <- rep("plot", length(gc_suf))
	zone[grepl("^IR_", gc_suf)] <- "inter-row"
	zone[grepl("^R_", gc_suf)] <- "row"

	val <- as.numeric(gc$VALEUR_QUANTITATIVE)
	dt <- as.character(as.Date(gc$OBS_DATE, "%d/%m/%Y"))
	sp_name <- ifelse(is_sp, r_tax$taxo_name[match(gc_suf, r_tax$taxo_code)], NA_character_)
	gc_key <- ifelse(is_cc, paste0(gc_plot, "_cowpea"), paste0(gc_plot, "_cane"))

	long <- data.frame(
		record_id = d$record_id[match(gc_key, d$rec_key)],
		trial_id = "P12_2018_2019",
		plot_id = gc_plot,
		date = dt,
		observation_zone_ = zone,
		ground_cover = ifelse(is_cc, val, NA_real_),
		weed_cover = ifelse(is_tot_weed, val, NA_real_),
		weed_species = sp_name,
		weed_species_cover = ifelse(is_sp, val, NA_real_)
	)
	long <- long[!is.na(val), ]
	d$rec_key <- NULL

	# Daily weather table
	wth <- data.frame(
		station_name = r_wth$nom_station,
		country = "Réunion",
		location = "La Mare",
		longitude = 55.53255556,
		latitude = -20.9036528,
		elevation = 70,
		geo_from_source = TRUE,
		date = as.character(as.Date(r_wth$date, "%d/%m/%Y")),
		prec = as.numeric(r_wth$rain_day_interpol),
		# MJ/m2/day -> kJ/m2/day
		srad = as.numeric(r_wth$global_radiation_interpol) * 1000,
		tmin = as.numeric(r_wth$air_temp_min_day_interpol),
		temp = as.numeric(r_wth$air_temp_mean_day_interpol),
		tmax = as.numeric(r_wth$air_temp_max_day_interpol),
		ETP = as.numeric(r_wth$etp_interpol)
	)

	carobiner::write_files(path, meta, d, long=long, wth=wth)
}
