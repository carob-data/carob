# R script for "carob"
# license: GPL (>=3)

## NOTES
# Ten cover crop collection and weed-control field trials across 3 CIRAD stations in Réunion (La Mare, Bassin Plat, Colimaçons) from 2016 to 2018 (Tardy/Dorel/Christina et al., doi:10.1016/j.eja.2021.126316).
# Trial metadata (site, coordinates, elevation, soil type, plot area, sowing date, fertilization, manual weed control) is read from Trial and species description.xlsx (tab-separated text with merged cells forward-filled). Pre-sowing herbicide was applied in all trials; all sites except Colimaçons were irrigated (Readme.txt).
# NPK rates (e.g. 36-82-60) are treated as kg/ha of N, P2O5, and K2O (converted to elemental P and K); "no" is 0.
# Wide table d covers all 12 trial units (1, 2, 3, 4a, 4b, 4c, 5, 6, 7, 8, 9, 10) from the union of biomass, phenology, plant cover, and height/leaf files, so all raw observations link cleanly to d via record_id. Mixture plots in weed-control trials have one row per component crop sharing plot_id (with intercrops set to the partner species).
# Aboveground fresh and dry biomass (t/ha -> kg/ha) from DATARAW_aboveground_biomass.xlsx: for cover crop rows, yield = ABV_FM * 1000, dmy_total = ABV_DM * 1000, yield_part = "aboveground biomass"; for adjacent natural weed control plots (Type = "Weed"), yield = NA, weed_biomass = ABV_FM * 1000, and weed_dmy_ = ABV_DM * 1000.
# Phenology (emergence_days, flowering_days, fructification_days_) is joined from DATARAW_phenology.xlsx. Repeated ground cover, weed cover, bare soil cover, plant height, and leaf number observations are stored in the long table linked via record_id.
# Daily meteorological data (Meteorological_data.xlsx) is passed via wth (Rg in MJ/m2 converted * 1000 -> kJ/m2 for srad).

## ISSUES
# Trials without biomass harvest (collection trials and weed-control trials 7-8) and natural weed control plots have yield = NA (triggers NA detected: yield).
# Fresh aboveground biomass of cowpea (up to 11.8 t/ha) and pearl millet (up to 47.0 t/ha) exceeds terminag's grain-yield max_yield check (high crop yield), and early cover-crop plant heights include values below 10 cm; both are kept as measured.
# Several tropical cover-crop species (Canavalia gladiata, Centrosema pascuorum, Lens nigricans, Lotus corniculatus, Macrotyloma axillare, Neonotonia wightii, Pueraria phaseoloides, Arachis pintoi) are not yet in terminag's crop list and are retained as Latin binomials.
# Trial and species description.xlsx leaves Fertilization blank for trials 3, 4b, 4c, 6, 8, and 10 due to merged cells; forward-filled within each site/trial-type block.

## NEW VARIABLES
# trial_type_: experimental trial type ("collection" or "weed control")
# mixture_id_: plot/mixture identifier code in weed control trials
# weed_dmy_: dry weight biomass of weeds in natural weed control plots (kg/ha)
# fructification_days_: days from sowing to fructification (days)
# bare_soil_cover_: percent bare soil cover recorded in weed-control plots (%)
# leaf_number_: number of leaves per plant

## NOT PROCESSED
# DATA_calculated_collection_trials.xlsx and DATA_calculated_weed_control_trials.xlsx: derived summary rates/means/WCE (used only as a French-to-Latin species dictionary).
# DATARAW_phenology.xlsx: Emerg_SDD, Flo_SDD, Fru_SDD (derived thermal time sums in degree-days).
# Readme.txt: study documentation, not an observation table.


carob_script <- function(path) {

"
Cover plant and weed control experimental data from field trials in Reunion (2016-2018)

<p>The dataset included experimental data from 10 trials with cover crops performed in Reunion from 2016 to 2018.</p>
<p>The files in the dataset included:</p>
<ul>
<li>\"Trial and species description\" : description of the trials and cover plant species</li>
<li>\"DATARAW\" files: raw measurements of plant coverage, height, number of leaves, phenology and biomass performed in the different trials.</li>
<li>\"DATA_calculated: provides trait variables calculated from the raw measurements per species and trials.</li>
</ul>
"

	uri <- "doi:10.18167/DVN1/HA0V2G"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.eja.2021.126316",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "crop;intercrops;trial_type_;weeding_done",
		response_vars = "yield;dmy_total;weed_biomass;ground_cover;weed_cover;plant_height;emergence_days;flowering_days",
		carob_LLM = "Gemini 4 Neon",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_completion = 90,
		carob_effort = 0
	)

	num <- function(x) {
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x <- gsub(",", ".", x, fixed=TRUE)
		x[x %in% c("", "NA", "na")] <- NA_character_
		as.numeric(x)
	}
	readx <- function(f, sheet) {
		carobiner::read.excel(f, sheet=sheet, col_types="text")
	}
	as_day <- function(x) {
		# Excel dates read as text are serial day numbers.
		as.character(as.Date(num(x), origin="1899-12-30"))
	}

	latin_crop <- c(
		"Avena sativa" = "oats",
		"Avena strigosa" = "black oats",
		"Pennisetum glaucum" = "pearl millet",
		"Canavalia ensiformis" = "jack bean",
		"Raphanus sativus" = "radish",
		"Guizotia abyssinica" = "niger",
		"Vicia villosa" = "vetch",
		"Vigna unguiculata" = "cowpea",
		"Sorghum bicolor" = "sorghum",
		"Stylosanthes guianensis" = "stylosanthes",
		"Eleusine coracana" = "finger millet",
		"Crotalaria juncea" = "sunn hemp",
		"Lablab purpureus" = "lablab",
		"Cajanus cajan" = "pigeon pea",
		"Mucuna pruriens" = "velvet bean",
		"Vigna radiata" = "mung bean",
		"Phaseolus lunatus" = "lima bean",
		"Arachis hypogaea" = "groundnut",
		"Fagopyrum esculentum" = "buckwheat",
		"Lathyrus sativus" = "grass pea",
		"Medicago sativa" = "lucerne",
		"Phalaris canariensis" = "canary grass",
		"Setaria italica" = "foxtail millet",
		"Trigonella foenum-graecum" = "fenugreek",
		"Vigna subterranea" = "bambara groundnut",
		"Cichorium endivia" = "endive",
		"Phacelia tanacetifolia" = "phacelia",
		"Brachiaria decumbens" = "brachiaria",
		"Crotalaria retusa" = "crotalaria",
		"Crotalaria spectabilis" = "crotalaria",
		"Crotalaria trichotoma" = "crotalaria",
		"Brassica carinata" = "carinata",
		"Desmodium heterocarpon" = "desmodium",
		"Desmodium uncinatum" = "desmodium",
		"Lupinus albus" = "white lupin",
		"Pisum sativum" = "pea",
		"Sinapis alba" = "white mustard",
		"Trifolium hybridum" = "clover",
		"Trifolium incarnatum" = "crimson clover",
		"Trifolium pratense" = "red clover",
		"Trifolium repens" = "white clover",
		"Zea mays" = "maize"
	)

	dict <- data.frame(common=character(), latin=character())
	for (bn in c("DATA_calculated_collection_trials.xlsx", "DATA_calculated_weed_control_trials.xlsx")) {
		rr <- readx(ff[basename(ff) == bn], sheet="DATA")
		nm <- names(rr)
		c1 <- nm[nm %in% c("Common french name", "Common name 1")]
		l1 <- nm[nm %in% c("Latin name", "Latin name 1")]
		if (length(c1) == 1 && length(l1) == 1) {
			dict <- rbind(dict, data.frame(common=trimws(as.character(rr[[c1]])), latin=trimws(as.character(rr[[l1]]))))
		}
		c2 <- nm[nm == "Common name 2"]
		l2 <- nm[nm == "Latin name 2"]
		if (length(c2) == 1) {
			dict <- rbind(dict, data.frame(common=trimws(as.character(rr[[c2]])), latin=trimws(as.character(rr[[l2]]))))
		}
	}
	dict <- dict[!is.na(dict$common) & nzchar(dict$common) & !is.na(dict$latin) & nzchar(dict$latin) & dict$latin != "NA", ]
	dict <- dict[!duplicated(dict$common), ]
	# Extra spelling variants in raw files
	extra_cn <- c(
		"Mi" = "pearl millet",
		"Millet" = "pearl millet",
		"Millet Perlé" = "pearl millet",
		"Voème noir" = "cowpea",
		"Weed" = "none",
		"weed" = "none"
	)
	to_crop <- function(common) {
		cn <- trimws(as.character(common))
		lat <- dict$latin[match(cn, dict$common)]
		out <- unname(latin_crop[lat])
		miss <- is.na(out) | !nzchar(out)
		out[miss] <- lat[miss]
		ex <- cn %in% names(extra_cn)
		out[ex] <- unname(extra_cn[cn[ex]])
		out[is.na(out)] <- cn[is.na(out)]
		trimws(out)
	}

	# Trial metadata (tab-separated text with .xlsx extension; merged cells forward-filled)
	f1 <- ff[basename(ff) == "Trial and species description.xlsx"]
	r1 <- read.delim(f1, skip=1, check.names=FALSE, quote="\"", fileEncoding="latin1")
	names(r1) <- trimws(names(r1))
	idcol <- names(r1)[names(r1) == "Trial ID"]
	r1 <- r1[!is.na(r1[[idcol]]) & nzchar(trimws(r1[[idcol]])), ]
	fill_cols <- c("Trial type", "Site", "Latitude", "Longitude", "Altitude", "Soil type",
		"Plot area", "Fertilization", "Manual weed control")
	for (col in fill_cols) {
		for (i in seq_len(nrow(r1))[-1]) {
			if (is.na(r1[[col]][i]) || !nzchar(trimws(as.character(r1[[col]][i])))) {
				r1[[col]][i] <- r1[[col]][i - 1]
			}
		}
	}
	r1$Site <- trimws(as.character(r1$Site))
	r1$Site[grepl("Colima", r1$Site)] <- "Colimaçons"
	pa_num <- as.numeric(sub("^([0-9]+).*", "\\1", trimws(as.character(r1[["Plot area"]]))))

	site <- data.frame(
		trial_id = trimws(as.character(r1[[idcol]])),
		trial_type_ = tolower(trimws(as.character(r1[["Trial type"]]))),
		location = r1$Site,
		latitude = num(r1$Latitude),
		longitude = num(r1$Longitude),
		elevation = num(sub(" m", "", r1$Altitude)),
		soil_type = trimws(as.character(r1[["Soil type"]])),
		plot_area = pa_num,
		planting_date = as.character(as.Date(num(r1[["Sowing date (dd/mm/yy)"]]), origin="1899-12-30")),
		weeding_done = tolower(trimws(as.character(r1[["Manual weed control"]]))) == "yes",
		fert_raw = trimws(as.character(r1$Fertilization))
	)
	site$fert_raw[site$fert_raw %in% c("", "NA")] <- NA_character_
	npk <- regmatches(site$fert_raw, regexec("([0-9]+)-([0-9]+)-([0-9]+)", site$fert_raw))
	site$N_fertilizer <- NA_real_
	site$P_fertilizer <- NA_real_
	site$K_fertilizer <- NA_real_
	site$fertilizer_used <- NA
	site$fertilizer_type <- NA_character_
	for (i in seq_len(nrow(site))) {
		if (length(npk[[i]]) == 4) {
			site$N_fertilizer[i] <- as.numeric(npk[[i]][2])
			site$P_fertilizer[i] <- as.numeric(npk[[i]][3]) / 2.29
			site$K_fertilizer[i] <- as.numeric(npk[[i]][4]) / 1.2051
			site$fertilizer_used[i] <- TRUE
			site$fertilizer_type[i] <- "NPK"
		} else if (!is.na(site$fert_raw[i]) && tolower(site$fert_raw[i]) == "no") {
			site$N_fertilizer[i] <- 0
			site$P_fertilizer[i] <- 0
			site$K_fertilizer[i] <- 0
			site$fertilizer_used[i] <- FALSE
			site$fertilizer_type[i] <- "none"
		}
	}

	# Read all 4 raw observation files
	b  <- readx(ff[basename(ff) == "DATARAW_aboveground_biomass.xlsx"], sheet="Weed control trial data")
	p  <- readx(ff[basename(ff) == "DATARAW_phenology.xlsx"], sheet="Collection trial data")
	c1 <- readx(ff[basename(ff) == "DATARAW_plant_cover.xlsx"], sheet="Collection trial data")
	c2 <- readx(ff[basename(ff) == "DATARAW_plant_cover.xlsx"], sheet="Weed control trial data")
	h1 <- readx(ff[basename(ff) == "DATARAW_height_number_of_leaves.xlsx"], sheet="Collection trials data")
	h2 <- readx(ff[basename(ff) == "DATARAW_height_number_of_leaves.xlsx"], sheet="Weed control trials data")

	# Standardize common names across files
	clean_cn <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("Mi", "Millet", "Millet Perlé")] <- "Millet perlé"
		x
	}
	b[["Common name"]]  <- clean_cn(b[["Common name"]])
	p[["Common name"]]  <- clean_cn(p[["Common name"]])
	c1[["Common name"]] <- clean_cn(c1[["Common name"]])
	c2[["Common name"]] <- clean_cn(c2[["Common name"]])
	h1[["Common name"]] <- clean_cn(h1[["Common name"]])
	h2[["Common name"]] <- clean_cn(h2[["Common name"]])

	# Build master table of experimental plots x component species across ALL trials
	# 1) Collection trials: one row per Trial ID x Common name
	coll_keys <- unique(rbind(
		data.frame(trial_id = trimws(p[["Trial ID"]]), common = p[["Common name"]]),
		data.frame(trial_id = trimws(c1[["Trial ID"]]), common = c1[["Common name"]]),
		data.frame(trial_id = trimws(h1[["Trial ID"]]), common = h1[["Common name"]])
	))
	coll_keys <- coll_keys[!is.na(coll_keys$trial_id) & !is.na(coll_keys$common), ]
	coll_base <- data.frame(
		trial_id = coll_keys$trial_id,
		rep = NA_integer_,
		wc_type = "Collection",
		mixture_id_ = NA_character_,
		common = coll_keys$common,
		plot_id = paste(coll_keys$trial_id, coll_keys$common, sep="_")
	)

	# Join phenology onto collection rows
	phen <- data.frame(
		trial_id = trimws(p[["Trial ID"]]),
		common = p[["Common name"]],
		emergence_days = num(p$Emergence),
		flowering_days = num(p$Flowering),
		fructification_days_ = num(p$Fructification)
	)
	phen <- phen[!duplicated(phen[, c("trial_id", "common")]), ]
	coll_base <- merge(coll_base, phen, by=c("trial_id", "common"), all.x=TRUE)
	coll_base$harvest_date <- NA_character_
	coll_base$yield <- NA_real_
	coll_base$dmy_total <- NA_real_
	coll_base$yield_moisture <- NA_real_
	coll_base$weed_biomass <- NA_real_
	coll_base$weed_dmy_ <- NA_real_

	# 2) Weed control trials:
	# Note: in c2, Weed and Soil cover rows share plot_id with the cover crop (Mixture ID != "Weed") OR belong to the adjacent Weed control plot (Type == "Weed")
	wc_crop_keys <- unique(rbind(
		data.frame(trial_id = trimws(b[["Trial ID"]]), rep = as.integer(b$Repetition),
			wc_type = trimws(b$Type), mixture_id_ = trimws(b[["Mixture ID"]]), common = b[["Common name"]]),
		data.frame(trial_id = trimws(c2[["Trial ID"]]), rep = as.integer(c2$Repetition),
			wc_type = trimws(c2$Type), mixture_id_ = trimws(c2[["Mixture ID"]]), common = c2[["Common name"]]),
		data.frame(trial_id = trimws(h2[["Trial ID"]]), rep = as.integer(h2$Repetition),
			wc_type = trimws(h2$Type), mixture_id_ = trimws(h2[["Mixture ID"]]), common = h2[["Common name"]])
	))
	# Drop Soil rows and Weed sub-observations from Pure/Mixture plots (they attach as weed_cover / bare_soil_cover_ in long)
	wc_crop_keys <- wc_crop_keys[wc_crop_keys$common != "Soil" & !(wc_crop_keys$wc_type != "Weed" & wc_crop_keys$common == "Weed"), ]
	wc_crop_keys <- wc_crop_keys[!is.na(wc_crop_keys$trial_id) & !is.na(wc_crop_keys$common), ]
	wc_crop_keys$plot_id <- paste(wc_crop_keys$trial_id, wc_crop_keys$rep, wc_crop_keys$mixture_id_, sep="_")

	# Biomass in weed control trials: aggregate if duplicate weed control subsamples exist per plot
	b_df <- data.frame(
		trial_id = trimws(b[["Trial ID"]]),
		rep = as.integer(b$Repetition),
		wc_type = trimws(b$Type),
		mixture_id_ = trimws(b[["Mixture ID"]]),
		common = b[["Common name"]],
		harvest_date = as_day(b$Date),
		fm = num(b$ABV_FM) * 1000,
		dm = num(b$ABV_DM) * 1000
	)
	b_df$is_weed <- b_df$wc_type == "Weed"
	# Keep all weed biomass replicate samples as separate rows in d (do not aggregate away)
	b_weed <- b_df[b_df$is_weed, ]
	b_crop <- b_df[!b_df$is_weed, ]
	b_crop <- b_crop[!duplicated(b_crop[, c("trial_id", "rep", "wc_type", "mixture_id_", "common")]), ]

	wc_crop <- wc_crop_keys[wc_crop_keys$wc_type != "Weed", ]
	wc_crop <- merge(wc_crop, b_crop[, c("trial_id", "rep", "wc_type", "mixture_id_", "common", "harvest_date", "fm", "dm")],
		by=c("trial_id", "rep", "wc_type", "mixture_id_", "common"), all.x=TRUE)
	wc_crop$yield <- wc_crop$fm
	wc_crop$dmy_total <- wc_crop$dm
	wc_crop$yield_moisture <- ifelse(!is.na(wc_crop$fm) & wc_crop$fm > 0, (1 - wc_crop$dm / wc_crop$fm) * 100, NA_real_)
	wc_crop$weed_biomass <- NA_real_
	wc_crop$weed_dmy_ <- NA_real_
	wc_crop$fm <- NULL
	wc_crop$dm <- NULL

	# Weed control plots: trials 9-10 have biomass rows; trials 7-8 have cover-only weed rows
	wc_weed_cov <- wc_crop_keys[wc_crop_keys$wc_type == "Weed" & !(paste(wc_crop_keys$trial_id, wc_crop_keys$rep) %in% paste(b_weed$trial_id, b_weed$rep)), ]
	wc_weed_cov$harvest_date <- NA_character_
	wc_weed_cov$yield <- NA_real_
	wc_weed_cov$dmy_total <- NA_real_
	wc_weed_cov$yield_moisture <- NA_real_
	wc_weed_cov$weed_biomass <- NA_real_
	wc_weed_cov$weed_dmy_ <- NA_real_

	wc_weed_bio <- data.frame(
		trial_id = b_weed$trial_id,
		rep = b_weed$rep,
		wc_type = b_weed$wc_type,
		mixture_id_ = b_weed$mixture_id_,
		common = b_weed$common,
		plot_id = paste(b_weed$trial_id, b_weed$rep, b_weed$mixture_id_, sep="_"),
		harvest_date = b_weed$harvest_date,
		yield = NA_real_,
		dmy_total = NA_real_,
		yield_moisture = NA_real_,
		weed_biomass = b_weed$fm,
		weed_dmy_ = b_weed$dm
	)
	wc_all <- rbind(wc_crop, wc_weed_bio, wc_weed_cov)
	wc_all$emergence_days <- NA_real_
	wc_all$flowering_days <- NA_real_
	wc_all$fructification_days_ <- NA_real_

	all_d <- rbind(coll_base, wc_all)
	all_d$crop <- to_crop(all_d$common)

	# Determine intercrop partner for Mixture plots sharing plot_id
	all_d$intercropped <- all_d$wc_type == "Mixture"
	all_d$intercrops <- "none"
	mix_pids <- unique(all_d$plot_id[all_d$intercropped])
	for (pid in mix_pids) {
		idx <- which(all_d$plot_id == pid & all_d$intercropped)
		crops_p <- unique(all_d$crop[idx])
		if (length(crops_p) >= 2) {
			for (k in idx) {
				other <- setdiff(crops_p, all_d$crop[k])
				if (length(other) > 0) all_d$intercrops[k] <- paste(other, collapse=";")
			}
		}
	}

	all_d <- merge(all_d, site[, c("trial_id", "trial_type_", "location", "latitude", "longitude", "elevation",
		"soil_type", "plot_area", "planting_date", "weeding_done", "fertilizer_used", "fertilizer_type",
		"N_fertilizer", "P_fertilizer", "K_fertilizer")], by="trial_id", all.x=TRUE)
	all_d <- all_d[order(all_d$trial_id, all_d$plot_id, all_d$common), ]

	d <- data.frame(
		record_id = seq_len(nrow(all_d)),
		trial_id = all_d$trial_id,
		plot_id = all_d$plot_id,
		rep = all_d$rep,
		trial_type_ = all_d$trial_type_,
		mixture_id_ = all_d$mixture_id_,
		treatment = ifelse(all_d$wc_type == "Collection", paste0("collection_", all_d$common),
			paste(all_d$wc_type, all_d$mixture_id_, sep="_")),
		country = "Réunion",
		location = all_d$location,
		latitude = all_d$latitude,
		longitude = all_d$longitude,
		elevation = all_d$elevation,
		geo_from_source = TRUE,
		soil_type = all_d$soil_type,
		plot_area = all_d$plot_area,
		crop = all_d$crop,
		variety = ifelse(all_d$common == "Weed", NA_character_, all_d$common),
		intercropped = all_d$intercropped,
		intercrops = all_d$intercrops,
		planting_date = all_d$planting_date,
		harvest_date = all_d$harvest_date,
		emergence_days = all_d$emergence_days,
		flowering_days = all_d$flowering_days,
		fructification_days_ = all_d$fructification_days_,
		weeding_done = all_d$weeding_done,
		herbicide_used = TRUE,
		fertilizer_used = all_d$fertilizer_used,
		fertilizer_type = all_d$fertilizer_type,
		N_fertilizer = all_d$N_fertilizer,
		P_fertilizer = all_d$P_fertilizer,
		K_fertilizer = all_d$K_fertilizer,
		yield = all_d$yield,
		dmy_total = all_d$dmy_total,
		yield_part = "aboveground biomass",
		yield_moisture = all_d$yield_moisture,
		weed_biomass = all_d$weed_biomass,
		weed_dmy_ = all_d$weed_dmy_,
		# All sites irrigated except Colimaçons (Readme.txt)
		irrigated = all_d$location != "Colimaçons",
		on_farm = FALSE,
		is_survey = FALSE
	)

	# Long table: ground cover, weed cover, bare soil cover, plant height, leaf number linked via plot_id (and record_id where component-specific)
	# Key for matching (plot_id, common) -> first matching record_id
	key_rc <- data.frame(
		record_id = d$record_id,
		trial_id = d$trial_id,
		plot_id = d$plot_id,
		common = all_d$common
	)
	key_rc <- key_rc[!duplicated(key_rc[, c("plot_id", "common")]), ]
	key_plot <- key_rc[!duplicated(key_rc$plot_id), c("record_id", "trial_id", "plot_id")]

	# Collection cover
	lc1 <- data.frame(
		plot_id = paste(trimws(c1[["Trial ID"]]), c1[["Common name"]], sep="_"),
		common = c1[["Common name"]],
		date = as_day(c1$Date),
		DAP = as.integer(num(c1$DAS)),
		ground_cover = num(c1$Cov)
	)
	lc1 <- merge(lc1, key_rc, by=c("plot_id", "common"), all.x=TRUE)
	lc1 <- lc1[!is.na(lc1$record_id) & !is.na(lc1$ground_cover), c("record_id", "trial_id", "plot_id", "date", "DAP", "ground_cover")]

	# Weed control cover (split cover crop vs Weed vs Soil)
	c2_pid <- paste(trimws(c2[["Trial ID"]]), as.integer(c2$Repetition), trimws(c2[["Mixture ID"]]), sep="_")
	c2_cn <- c2[["Common name"]]
	c2_dt <- as_day(c2$Date)
	c2_val <- num(c2$Cov)

	is_c2_weed <- c2_cn == "Weed"
	is_c2_soil <- c2_cn == "Soil"
	is_c2_crop <- !is_c2_weed & !is_c2_soil

	lc2_crop <- data.frame(plot_id = c2_pid[is_c2_crop], common = c2_cn[is_c2_crop], date = c2_dt[is_c2_crop], ground_cover = c2_val[is_c2_crop])
	lc2_crop <- merge(lc2_crop, key_rc, by=c("plot_id", "common"), all.x=TRUE)
	lc2_crop <- lc2_crop[!is.na(lc2_crop$record_id) & !is.na(lc2_crop$ground_cover), c("record_id", "trial_id", "plot_id", "date", "ground_cover")]

	lc2_weed <- data.frame(plot_id = c2_pid[is_c2_weed], date = c2_dt[is_c2_weed], weed_cover = c2_val[is_c2_weed])
	lc2_weed <- merge(lc2_weed, key_plot, by="plot_id", all.x=TRUE)
	lc2_weed <- lc2_weed[!is.na(lc2_weed$record_id) & !is.na(lc2_weed$weed_cover), c("record_id", "trial_id", "plot_id", "date", "weed_cover")]

	lc2_soil <- data.frame(plot_id = c2_pid[is_c2_soil], date = c2_dt[is_c2_soil], bare_soil_cover_ = c2_val[is_c2_soil])
	lc2_soil <- merge(lc2_soil, key_plot, by="plot_id", all.x=TRUE)
	lc2_soil <- lc2_soil[!is.na(lc2_soil$record_id) & !is.na(lc2_soil$bare_soil_cover_), c("record_id", "trial_id", "plot_id", "date", "bare_soil_cover_")]

	# Collection & weed-control height and leaf number
	lh1 <- data.frame(
		plot_id = paste(trimws(h1[["Trial ID"]]), h1[["Common name"]], sep="_"),
		common = h1[["Common name"]],
		date = as_day(h1$Date),
		plant_height = num(h1$Height),
		leaf_number_ = num(h1[["Number of leaves"]])
	)
	lh1 <- merge(lh1, key_rc, by=c("plot_id", "common"), all.x=TRUE)
	lh1 <- lh1[!is.na(lh1$record_id) & !(is.na(lh1$plant_height) & is.na(lh1$leaf_number_)),
		c("record_id", "trial_id", "plot_id", "date", "plant_height", "leaf_number_")]

	lh2 <- data.frame(
		plot_id = paste(trimws(h2[["Trial ID"]]), as.integer(h2$Repetition), trimws(h2[["Mixture ID"]]), sep="_"),
		common = h2[["Common name"]],
		date = as_day(h2$Date),
		plant_height = num(h2$Height),
		leaf_number_ = num(h2[["Number of leaves"]])
	)
	lh2 <- merge(lh2, key_rc, by=c("plot_id", "common"), all.x=TRUE)
	lh2 <- lh2[!is.na(lh2$record_id) & !(is.na(lh2$plant_height) & is.na(lh2$leaf_number_)),
		c("record_id", "trial_id", "plot_id", "date", "plant_height", "leaf_number_")]

	dlong <- carobiner::bindr(lc1, lc2_crop, lc2_weed, lc2_soil, lh1, lh2)

	# Daily weather
	mw <- readx(ff[basename(ff) == "Meteorological_data.xlsx"], sheet="Meteorological_data")
	wth <- data.frame(
		station_name = trimws(as.character(mw$Station)),
		date = as_day(mw$Date),
		prec = num(mw$PPT),
		# Rg is in MJ/m2 -> * 1000 for kJ/m2
		srad = num(mw$Rg) * 1000,
		tmin = num(mw$Tmin),
		temp = num(mw$Tmean),
		tmax = num(mw$Tmax),
		ETP = num(mw$ETP)
	)
	wgeo <- unique(site[, c("location", "latitude", "longitude", "elevation")])
	wgeo$station_name <- wgeo$location
	wgeo$station_name[wgeo$location == "Colimaçons"] <- "Colimaçon"
	wth <- merge(wth, wgeo[, c("station_name", "latitude", "longitude", "elevation")], by="station_name", all.x=TRUE)
	wth$country <- "Réunion"

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
