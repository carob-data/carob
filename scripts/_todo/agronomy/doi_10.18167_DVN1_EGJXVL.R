# R script for "carob"
# license: GPL (>=3)

## NOTES
# Measured grain yields are Trials_yield_observation.csv in the calibration folder. The definition file gives the unit as t dry matter per ha, so yield is times 1000 and yield_moisture is 0. Model outputs and the virtual-experiment files are not used.
# The yield file is one quoted field per line. It is unquoted before reading. id_treatment +C+N is the fertilizer code C1N1, and the same for the other three combinations.
# fe_n, fe_p, and fe_k are kg/ha of the elements, summed over the applications on that row. Blank cells on a matched fertilizer row are treated as 0. Organic amount is om_amount_DW in t/ha, times 1000.
# Countries are the ones on the yield rows: Côte d'Ivoire, Kenya, and Zimbabwe. site_general_info spells Ivory Cost for the Gagnoa site. Coordinates and elevation are that site table.

## ISSUES
# Some yield cells are empty and are kept.
# write_files: missing irrigated, harvest_date. invalid terms: fertilizer_type: ammonium nitrate, CaNH4NO2, CaNH4NO3, PK fertiliser, super-phosphate. NA detected: yield.

## NOT PROCESSED
# Trials_models_yield_simulations.csv, Trials_models_SOC_simulations.csv, and Models_ouputs_simulation_scenarios.csv.
# Virtual_experiments_models_input_outputs.
# Soil-layer files, residue_initial, soil_preparation, and the DSSAT weather files.
# time_from_start on the yield file.
# obs_phenology.csv: not read. columns: id_season;id_tratnt;pdate;pdate_doy;emergence_date;emergence_date_doy;anthesis_date;anthesis_date_doy;physio_maturity_date;physio_maturity_date_DOY;harvest_date;harvest_date_doy. Observation records are missing from d until this file is standardized.
# phenology_info.csv: not read. columns: id_site;crop;cultivar;cul_note;gdd_emerg_anthesis;gdd_anthesis_maturity. Observation records are missing from d until this file is standardized.
# soil_general_info.csv: not read. columns: id_site;id_soil;texture;slope;soil_type;max_root_depth;cropping history. Observation records are missing from d until this file is standardized.
# soil_layers_characterisctics_corrected.xlsx: not read. sheet Feuil1 (of 2): id_soil, layer_base_depth, layer_thickness, lower_limit, drained_upper_limit, saturation, Or, alpha, n, bulk_density, clay, silt, .... Observation records are missing from d until this file is standardized.
# soil_layers_initial.csv: not read. columns: soil_id;id_treatment;layer_base_depth;ini_Nmin_conc;ini_Nmin_amount;ini_soc_conc;ini_soc_conc_sd;ini_soc_amount;ini_soc_amount_sd;ini_son_conc;ini_son conc_sd;ini_son_amount;ini_son_amount_sd. Observation records are missing from d until this file is standardized.
# ICGA - Copie.txt: not read. columns: Station #: ICGA, , , , , , . Observation records are missing from d until this file is standardized.
# ICGA.txt: not read. columns: Station #: ICGA, , , , , , . Observation records are missing from d until this file is standardized.
# KEEM.txt: not read. columns: Station #: KEEM. Observation records are missing from d until this file is standardized.
# KEMA.txt: not read. columns: Station #: KEMA, , , , , , . Observation records are missing from d until this file is standardized.
# ZIMU.txt: not read. columns: Station #: ZIMU, , , , , , . Observation records are missing from d until this file is standardized.
# Trials_SOC_observations.csv: not read. columns: "id_site", "Date", "id_treatment", "SOC", "time_from_start". Observation records are missing from d until this file is standardized.
# READ.ME.txt: documentation or a codebook, not an observation table.
# summary_data_definition.xlsx: documentation or a codebook, not an observation table.
# crop_sequence_SA.csv: not read. columns: id_site, id_season, planting_year, season, crop. Observation records are missing from d until this file is standardized.
# mineral_fertilisation_SA.csv: not read. columns: id_site;season;Ninput;fedate_doy1;fe_type1;fe_applic_note1;fe_depth1;fe_n1;fe_p1;fe_k1;fedate_doy2;fe_type2;fe_applic_note2;fe_depth2;fe_n2;fe_p2;fe_k2;fedate_doy3;fe_type3;fe_applic_note3;fe_depth3;fe_n3;fe_p3;fe_k3. Observation records are missing from d until this file is standardized.
# organic_amendment_SA.csv: not read. columns: id_site, season, Cinput, omdate_doy, om_type, om_note, om_amount_DW, om_ms_content, om_carbon_conc_DW, om_n_conc, om_p_conc, om_k_conc. Observation records are missing from d until this file is standardized.
# planting_SA.csv: not read. columns: id_site;season;crop;cultivar;cul_note;pdate_doy;plant_population;row_spacing;planting_depth. Observation records are missing from d until this file is standardized.
# soil_layers_initial_SA.csv: not read. columns: soil_id;layer_base_depth;ini_Nmin_amount;ini_soc_conc;ini_soc_amount;ini_son_conc;ini_son_amount. Observation records are missing from d until this file is standardized.
# soil_preparation_SA.csv: not read. columns: id_site, season, sprep_doy, sprep_type, sprep_implement, sprep_depth, sprep_doy2, sprep_implement2, sprep_type2, sprep_depth2. Observation records are missing from d until this file is standardized.
# ICGA_SA.WTH: not read. .wth file. Observation records are missing from d until this file is standardized.
# KEEM_SA.WTH: not read. .wth file. Observation records are missing from d until this file is standardized.
# KEMA_SA.WTH: not read. .wth file. Observation records are missing from d until this file is standardized.
# ZIMU_SA.WTH: not read. .wth file. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Data for 'Beyond climate change: the role of integrated soil fertility management for sustaining future maize yield in sub-Saharan Africa'

The dataset contains 15 soil crop model i) input and outputs on long-term experimental data on maize growth and soil organic carbon and ii) climate change and integrated soil fertility management simulation outputs at four sites in sub-Saharan Africa (2026)
"

	uri <- "doi:10.18167/DVN1/EGJXVL"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;OM_amount",
		response_vars = "yield",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 45,
		carob_effort = 0.6
	)

	read_semi <- function(f) {
		read.csv(f, sep=";", check.names=FALSE, na.strings=c("", "NA", "na"))
	}
	read_yield <- function(f) {
		raw <- readChar(f, file.info(f)$size, useBytes=TRUE)
		raw <- iconv(raw, from="latin1", to="UTF-8")
		lines <- strsplit(raw, "\r\n|\n")[[1]]
		lines <- lines[nzchar(lines)]
		lines <- sub("^\"", "", lines)
		lines <- sub("\"$", "", lines)
		lines <- gsub("\"\"", "\"", lines, fixed=TRUE)
		read.csv(text=paste(lines, collapse="\n"), check.names=FALSE, na.strings=c("", "NA", "na"))
	}
	sum3 <- function(a, b, c) {
		a <- as.numeric(a)
		b <- as.numeric(b)
		c <- as.numeric(c)
		a[is.na(a)] <- 0
		b[is.na(b)] <- 0
		c[is.na(c)] <- 0
		a + b + c
	}

	f1 <- ff[basename(ff) == "Trials_yield_observation.csv"]
	y <- read_yield(f1)
	y$site_code <- substr(y$id_season, 1, 4)
	y$fert_code <- y$id_treatment
	y$fert_code <- gsub("+C", "C1", y$fert_code, fixed=TRUE)
	y$fert_code <- gsub("-C", "C0", y$fert_code, fixed=TRUE)
	y$fert_code <- gsub("+N", "N1", y$fert_code, fixed=TRUE)
	y$fert_code <- gsub("-N", "N0", y$fert_code, fixed=TRUE)

	f2 <- ff[basename(ff) == "planting.csv"]
	plant <- read_semi(f2)
	f3 <- ff[basename(ff) == "crop_sequence.csv"]
	r1 <- read.csv(f3, check.names=FALSE)
	f4 <- ff[basename(ff) == "site_general_info.csv"]
	r2 <- read.csv(f4, check.names=FALSE)
	f5 <- ff[basename(ff) == "mineral_fertilisation.csv"]
	fert <- read_semi(f5)
	f6 <- ff[basename(ff) == "organic_amendment.csv"]
	om <- read_semi(f6)

	fert$N_fertilizer <- sum3(fert$fe_n1, fert$fe_n2, fert$fe_n3)
	fert$P_fertilizer <- sum3(fert$fe_p1, fert$fe_p2, fert$fe_p3)
	fert$K_fertilizer <- sum3(fert$fe_k1, fert$fe_k2, fert$fe_k3)
	pick_type <- function(a, b, c) {
		mapply(function(x, y, z) {
			v <- unique(trimws(c(x, y, z)))
			v <- v[!is.na(v) & nzchar(v)]
			if (length(v) == 0) "none" else paste(v, collapse=";")
		}, as.character(a), as.character(b), as.character(c), USE.NAMES=FALSE)
	}
	fert$fertilizer_type <- pick_type(fert$fe_type1, fert$fe_type2, fert$fe_type3)
	fert <- fert[, c("id_season", "id_treatment", "N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_type")]
	names(fert)[names(fert) == "id_treatment"] <- "fert_code"

	om$OM_amount <- as.numeric(om$om_amount_DW) * 1000
	om$OM_amount[is.na(om$OM_amount)] <- 0
	om <- om[, c("id_season", "id_treatment", "OM_amount")]
	names(om)[names(om) == "id_treatment"] <- "fert_code"

	crop <- r1[, c("id_season", "crop")]
	crop$crop[crop$crop == "fallow"] <- "none"
	plant <- plant[, c("id_season", "pdate", "plant_population", "row_spacing")]

	d <- merge(y, crop, by="id_season", all.x=TRUE)
	d <- merge(d, plant, by="id_season", all.x=TRUE)
	d <- merge(d, fert, by=c("id_season", "fert_code"), all.x=TRUE)
	d <- merge(d, om, by=c("id_season", "fert_code"), all.x=TRUE)
	d <- merge(d, r2[, c("id_site", "site_name", "latitude", "longitude", "elevation")], by.x="site_code", by.y="id_site", all.x=TRUE)

	country <- c(ICGA="Côte d'Ivoire", KEEM="Kenya", KEMA="Kenya", ZIMU="Zimbabwe")
	part <- c(maize="grain", soybean="seed", none="none")
	when <- as.Date(d$pdate, "%m/%d/%Y")

	d <- data.frame(
		trial_id = d$id_season,
		country = unname(country[d$site_code]),
		location = as.character(d$site_name),
		latitude = as.numeric(d$latitude),
		longitude = as.numeric(d$longitude),
		elevation = as.numeric(d$elevation),
		geo_from_source = TRUE,
		crop = d$crop,
		treatment = d$id_treatment,
		yield = as.numeric(d$Yield) * 1000,
		yield_part = unname(part[d$crop]),
		yield_moisture = 0,
		planting_date = as.character(when),
		N_fertilizer = d$N_fertilizer,
		P_fertilizer = d$P_fertilizer,
		K_fertilizer = d$K_fertilizer,
		fertilizer_type = d$fertilizer_type,
		OM_amount = d$OM_amount,
		OM_used = d$OM_amount > 0,
		OM_type = ifelse(d$OM_amount > 0, "compost", "none"),
		plant_density = as.numeric(d$plant_population) * 10000,
		row_spacing = as.numeric(d$row_spacing),
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$OM_type[is.na(d$OM_amount)] <- NA_character_
	d$OM_used[is.na(d$OM_amount)] <- NA
	d$fertilizer_type[is.na(d$fertilizer_type)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
