# R script for "carob"
# license: GPL (>=3)

## NOTES
# Paper: Kwenda et al. 2025 Field Crops Research (doi:10.1016/j.fcr.2025.109890); its rainfall and yield ranges match the data.
# On-farm, Mutoko (Zimbabwe): 12 fields in 2021-22, 11 in 2022-23 (23 trials); 9 unreplicated plots per field.
# 276 records (207 plots; intercrop plots have one record per crop); yields from sheet pLER_LER, management from other sheets.
# Long table: 1157 LAI, soil water and N records; wth: daily station (KWERE) weather and rain gauge rainfall.

## ISSUES
# Grain yield and biomass column labels are swapped in pLER_LER (checked with pLER columns and paper); swapped back.
# No yield in the data for the unfertilised control and late-sown sole cowpea (46 records), and 16 others; NA.
# Season 1 intercropped maize has one ammonium nitrate split (46.5 kg N/ha) in the data; paper says 80. Kept data.
# Intercropped cowpea records get 0 N, P, K: fertilizer was applied on the maize rows. Keep?
# Compound D at 200 kg/ha (data note) would give 14 N, 12.2 P, 11.6 K, not the 12-10-10 in the data. Data used.
# SCHI 2021-22 has two "ZS500_CPA maize" rows; the one with cowpea sowing date and depth taken as SC403_CPA cowpea.
# Year typos corrected: 5 harvest dates (CHIU, SMAN) and 2 BMAN top dressing dates in 2021-22.
# BMAN and SCHI 2022-23: maize sown 2022-12-20 but basal fertilizer (at sowing) dated 2022-12-31. Kept.
# soil pH is labelled pH_water in the data but the paper says CaCl2; stored as soil_pH. Check.
# Treatment descriptions in sheet Treatments contradict the codes for several farmers; codes used.
# Rain gauge sheets repeat identical temperature, radiation, wind and humidity series; only their rainfall is used.
# Rainfall missing for HOND, SAJP, SCHI, CHIU and PSMA 2021-22; KANB 2021-22 stops on 2021-12-31.
# 48 long rows share plot, crop and date with another row (9 with conflicting LAI or N uptake); kept, numbered by order.
# irrigated set FALSE: rainfed implied by the water stress analysis, not stated explicitly. Check.

## NEW VARIABLES
# Ndfa: N in cowpea aboveground biomass derived from the atmosphere (15N natural abundance), % (checked: N_fixation = kg/ha, crop_N = kg/ha)

## NOT PROCESSED
# Soil_results: layers 20-120 cm (only 0-20 cm stored; no per-field soil table in carob).
# pLER_LER: simulated (STICS) water and N stress factors; pLER and LER (computed from the yields); planting day numbers (repeat Crop_management).
# Climate_data at rain gauges: srad, tmax, tmin, wind speed, humidity (identical across gauges, not from the station).
# Treatments: fertiliser flags and descriptions (repeat Fertilization); Fertilization: application depth (5 or 3 cm).
# Paper: insecticides (carbaryl, malathion) and fortnightly hand weeding were advised; application left to farmers, not recorded.
# Paper: plots kept in the same positions in both seasons (previous crop not set).


carob_script <- function(path) {

"
Data for 'Intercrop overyielding is maintained under estimated water and nitrogen stress in maize-cowpea on-farm trials in semi-arid Zimbabwe'

These are the raw data of the paper 'Intercrop overyielding is maintained under estimated water and nitrogen stress in maize-cowpea on-farm trials in semi-arid Zimbabwe' authored by Illiana W. Kwenda, Gatien N. Falconnier, Rémi Cardinael, François Affholder, Antoine Couëdel, Frederic Baudron, Angelinus C. Franke, Isaiah Nyagumbo, Stanford Mabasa, Mathilde de Freitas, Valentin Pret, Souleymane Diop, Eleanor F. Magwaza, Regis Chikowo
"

	uri <- "doi:10.18167/DVN1/G5TYUZ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "UZIM; CIRAD; CIMMYT; UFS; UPS",
		publication = "doi:10.1016/j.fcr.2025.109890",
		project = NA,
		design = "one unreplicated plot per treatment per field",
		data_type = "on-farm experiment",
		treatment_vars = "variety;intercrops;planting_date;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield;dmy_total;N_fixation;Ndfa;crop_N;LAI;soil_SWC",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-24",
		carob_completion = 85,
		carob_effort = 0.3
	)

	f1 <- ff[basename(ff) == "Kwenda_et_al_data_vf.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Legend")                      # codebook
	r1b <- carobiner::read.excel(f1, sheet="Sites")                       # coordinates of fields and weather station
	r1c <- carobiner::read.excel(f1, sheet="Treatments")                  # treatment codes per field, soil water tube depth
	r1d <- carobiner::read.excel(f1, sheet="Soil_results")                # soil profile per field (Oct 2021)
	r1e <- carobiner::read.excel(f1, sheet="Climate_data", na="NA")       # daily weather, station and rain gauges
	r1f <- carobiner::read.excel(f1, sheet="Fertilization")               # fertilizer applications per field, season, treatment
	r1g <- carobiner::read.excel(f1, sheet="Crop_management", na="NA")    # sowing, harvest, density per plot and crop
	r1h <- carobiner::read.excel(f1, sheet="N2-fixation", na="NA")        # N uptake and N fixation per plot, crop and date
	r1i <- carobiner::read.excel(f1, sheet="Soil_water_LAI", na="NA")     # LAI and soil water per plot, crop and date
	r1j <- carobiner::read.excel(f1, sheet="pLER_LER", na="NA")           # yields, simulated stresses, LER per field x variety

## d1b from r1b: coordinates
	d1b <- data.frame(
		location = r1b$id_site,                      # farm code
		longitude = r1b$longitude,
		latitude = r1b$latitude,
		elevation = r1b$elevation
	)
	d1b$location[d1b$location == "KWRE"] <- "KWERE"  # weather station code in Climate_data

## d1c from r1c: depth of the soil water access tubes per field
	d1c <- unique(data.frame(
		location = r1c$id_soil,
		depth_bottom = r1c[["max_depth_for_soil_water_measurements per each farmer"]]   # cm
	))
	d1c <- d1c[!is.na(d1c$depth_bottom), ]
	# HOND and RUMU give two depths (160, 180 cm) but have no soil water data
	d1c <- d1c[!d1c$location %in% d1c$location[duplicated(d1c$location)], ]

## d1d from r1d: topsoil (0-20 cm), sampled October 2021
	top <- r1d$layer_base_depth == 20
	d1d <- data.frame(
		location = r1d$farmer[top],
		soil_clay = r1d[["clay (%)"]][top],
		soil_silt = r1d[["silt (%)"]][top],
		soil_sand = r1d[["sand (%)"]][top],
		soil_pH = r1d$pH_water[top],                                     # paper: CaCl2 method
		soil_N_total = r1d[["Ntotal_g_kg of soil"]][top] * 1000,         # g/kg -> mg/kg, micro-Kjeldahl
		soil_P = r1d[["Available_P_mg_Kg of soil"]][top],                # resin extractable (paper)
		soil_K = r1d[["Available _K_mg_kg_soil"]][top],                  # ammonium acetate (paper)
		soil_Ca = r1d[["Total Ca__mg_kg of soil"]][top],                 # "total" in codebook, exchangeable (EDTA) in paper
		soil_Mg = r1d[["Total Mg_mg_kg of soil"]][top],
		soil_SOC = r1d[["organic_Carbon_g_kg_soil"]][top] / 10           # g/kg -> %, Walkley-Black
	)
	d1d$soil_P_method <- "resin"

## d1e from r1e: daily weather
	d1e <- data.frame(
		location = r1e$id_wst,
		date = as.character(as.Date(r1e$Date)),
		srad = r1e[["solar_radiation (J/m²)"]] * 1000,   # values are MJ/m2/day, not J/m2 (codebook) -> kJ/m2
		tmax = r1e[["max_temperature (°C)"]],
		tmin = r1e[["min_temperture (°C)"]],
		prec = r1e[["rainfall (mm)"]],
		wspd = r1e[["wind_speed (m/s)"]],
		rhum = r1e[["relative_humidity (%)"]]
	)
	# paper: only rainfall was measured at the fields; other variables at the central station KWERE
	# (November 2021 at KWERE is NASA POWER gridded data, paper 2.3.1)
	gauge <- d1e$location != "KWERE"
	d1e[gauge, c("srad", "tmax", "tmin", "wspd", "rhum")] <- NA

## d1f from r1f: fertilizer per field, season and treatment (no season column; taken from the basal date)
	fd1 <- as.Date(r1f[["fertiliser application_date_ymd1"]])
	fd2 <- as.Date(r1f[["fertiliser_application_date_ymd2"]])
	fd3 <- as.Date(r1f[["fertiliser_application_date_ymd3"]])
	# year typos: BMAN 2021-22 QPM and SC403 top dressings dated Dec 2022-Jan 2023 (ZS500 there: 2021-12-24, 2022-01-07)
	late2 <- which(fd2 - fd1 > 300)
	fd2[late2] <- fd2[late2] - 365
	late3 <- which(fd3 - fd1 > 300)
	fd3[late3] <- fd3[late3] - 365
	nf <- r1f[, c("fertiliser_Nitrogen content in first application_1", "fertiliser nitrogen amount_N2", "fertiliser_application_N amount_3")]
	d1f <- data.frame(
		location = r1f$farmer,
		season = ifelse(fd1 < as.Date("2022-07-01"), "Season 1", "Season 2"),
		treatment = r1f$treatment,
		N_fertilizer = rowSums(nf, na.rm=TRUE),        # kg/ha N: 12 basal (compound D) + 34.5 per ammonium nitrate split
		P_fertilizer = r1f[["fertiliser_phosphorous content in first application_1"]],   # kg/ha P (elemental; paper 10 kg P/ha)
		K_fertilizer = r1f[["fertiliser_ potassium content in first application_K1"]],   # kg/ha K
		N_splits = as.integer(rowSums(!is.na(nf))),
		fertilizer_type = ifelse(is.na(r1f$fertiliser_type2), "D-compound", "D-compound;AN"),   # compound D 7-14-7, ammonium nitrate
		fertilization_method = ifelse(is.na(r1f$fertiliser_type2), "basal", "basal;top dressing")
	)
	d1f$fertilizer_date <- gsub(";NA", "", paste(fd1, fd2, fd3, sep=";"))
	d1f$crop <- ifelse(grepl("^CPA", d1f$treatment), "cowpea", "maize")
	# control plots have no fertilizer and no date (season unknown); set to 0 below
	d1f <- d1f[!is.na(fd1), ]

## d1g from r1g: plot and crop records
	hd <- r1g$harvest_date
	xl <- grepl("^[0-9]{5}$", hd)                     # 7 harvest dates stored as Excel day numbers
	hd[xl] <- as.character(as.Date(as.numeric(hd[xl]), origin="1899-12-30"))
	d1g <- data.frame(
		location = r1g$farmer,
		season = r1g$Growing_season,
		treatment = r1g$treatment,
		crop = r1g$plant,
		variety = r1g$cultivar,
		variety_type = r1g$cultivar_note,
		planting_date = as.character(as.Date(r1g$planting_date)),
		harvest_date = hd,
		plant_density = r1g$plant_population * 10000,   # plants/m2 -> plants/ha; maize 2 plants per station
		row_spacing = r1g$interrow_spacing,
		plant_spacing = r1g$inrow_spacing,              # between planting stations
		planting_depth = r1g$planting_depth
	)
	# SCHI 2021-22: the second "ZS500_CPA maize" row has the cowpea sowing date and depth; it is the missing SC403_CPA cowpea row
	schi <- d1g$location == "SCHI" & d1g$season == "Season 1" & d1g$treatment == "ZS500_CPA" & d1g$crop == "maize" & d1g$planting_depth == 3
	d1g$treatment[schi] <- "SC403_CPA"
	d1g$crop[schi] <- "cowpea"
	d1g$variety[schi] <- "Nyadawa"
	d1g$variety_type[schi] <- "landrace"
	d1g$plant_spacing[schi] <- 30                    # intercropped cowpea 0.9 x 0.3 m (paper)
	# year typos: 5 harvest dates of 2021-22 plots (CHIU, SMAN) are in 2023 (paper: harvest by 25 May 2022)
	s1late <- d1g$season == "Season 1" & d1g$harvest_date > "2023-01-01"
	d1g$harvest_date[s1late] <- sub("^2023", "2022", d1g$harvest_date[s1late])
	d1g$variety[d1g$variety %in% c("QPM", "QPM 623")] <- "QPM623"
	d1g$variety[d1g$variety == "CONTR"] <- "SC403"   # control is unfertilised SC403

## d1h from r1h: N uptake and N fixation (long)
	d1h <- data.frame(
		row = seq_len(nrow(r1h)),
		location = r1h$farmer,
		season = r1h$growing_season,
		treatment = r1h$treatment,
		crop = tolower(r1h$Plant),
		date = r1h$Date,
		Ndfa = r1h[["%Ndfa"]],                          # cowpea at peak flowering
		N_fixation = r1h$N2_fixation,                   # kg/ha, cowpea aboveground biomass at peak flowering
		crop_N = r1h$N_uptake                           # kg/ha, aboveground N uptake (flowering or maturity)
	)

## d1i from r1i: LAI and soil water (long); same rows as r1h
	d1i <- data.frame(
		row = seq_len(nrow(r1i)),
		location = r1i$farmer,
		season = r1i$growing_season,
		treatment = r1i$treatment,
		crop = tolower(r1i$Plant),
		date = r1i$Date,
		LAI = r1i$leaf_area_index,                      # mean of 4 transects; maize + cowpea canopy in intercrops (paper)
		soil_SWC = r1i$cumulative_soil_moisture_mm_per_each_site   # mm, whole profile at the plot access tube
	)

## d1j from r1j: grain yield and aboveground biomass per field x maize variety
	# columns labelled "aboveground biomass" hold grain yield and vice versa (pLER columns and paper ranges)
	n1j <- nrow(r1j)
	mv <- r1j[["maize variety"]]
	d1j <- data.frame(
		location = rep(r1j$farmer, 4),
		season = rep(sub("season", "Season", r1j[["growing season"]]), 4),
		treatment = c(mv, paste0(mv, "_CPA"), paste0(mv, "_CPA"), rep("CPA_PD1", n1j)),
		crop = rep(c("maize", "maize", "cowpea", "cowpea"), each=n1j),
		yield = 1000 * c(r1j[["Maize aboveground biomass for sole cropping"]], r1j[["Maize aboveground biomass for intercropping"]],
			r1j[["cowpea aboveground biomass for intercropping"]], r1j[["cowpea aboveground biomass for sole cropping"]]),       # Mg DM/ha -> kg/ha
		dmy_total = 1000 * c(r1j[["maize grain yield for sole cropping"]], r1j[["Maize grain yield for intercropping"]],
			r1j[["cowpea grain yield for intercropping"]], r1j[["cowpea grain yield for sole cropping"]])                 # Mg DM/ha -> kg/ha
	)
	# sole cowpea (sown with maize, D1) is repeated for each maize variety
	d1j <- unique(d1j)

## build d
	d <- merge(d1g, d1j, by=c("location", "season", "treatment", "crop"), all.x=TRUE)
	d <- merge(d, d1f, by=c("location", "season", "treatment", "crop"), all.x=TRUE)
	d <- merge(d, d1b, by="location", all.x=TRUE)
	d <- merge(d, d1d, by="location", all.x=TRUE)

	ctrl <- d$treatment == "CONTR"
	icc <- grepl("_CPA", d$treatment) & d$crop == "cowpea"
	d$N_fertilizer[ctrl | icc] <- 0                  # control unfertilised; intercropped cowpea: fertilizer on maize rows only (paper)
	d$P_fertilizer[ctrl | icc] <- 0
	d$K_fertilizer[ctrl | icc] <- 0
	d$N_splits[ctrl | icc] <- 0L
	d$fertilizer_type[ctrl | icc] <- "none"

	d$intercropped <- grepl("_CPA", d$treatment)
	d$intercrops <- ifelse(d$intercropped, ifelse(d$crop == "maize", "cowpea", "maize"), "none")
	d$intercrop_type <- ifelse(d$intercropped, "row", "none")   # one cowpea row between two maize rows
	d$variety_traits <- c(QPM623="quality protein maize", ZS500="provitamin A biofortified", SC403="drought tolerant", Nyadawa="dual purpose (grain and fodder)")[d$variety]
	d$planting_method <- ifelse(d$crop == "maize", "dibbling", NA)   # planting holes made with hand hoes (paper)

	d$trial_id <- paste(d$location, ifelse(d$season == "Season 1", "2021-22", "2022-23"), sep="_")
	d$plot_id <- paste(d$trial_id, d$treatment, sep="_")
	d$season <- NULL
	d$record_id <- seq_len(nrow(d))
	d$country <- "Zimbabwe"
	d$adm1 <- "Mashonaland East"
	d$adm2 <- "Mutoko"
	d$geo_from_source <- TRUE
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE                             # rainfed; water stress from rainfall (paper)
	d$plot_area <- 30                                # 6 x 5 m
	d$yield_part <- ifelse(d$crop == "maize", "grain", "seed")
	d$yield_moisture <- 0                            # oven dried at 65 C (paper)
	d$yield_isfresh <- FALSE

## long table: LAI, soil water, N uptake and N fixation
	dlong <- merge(d1i, d1h, by=c("row", "location", "season", "treatment", "crop", "date"))
	vars <- c("LAI", "soil_SWC", "Ndfa", "N_fixation", "crop_N")
	dlong <- dlong[rowSums(!is.na(dlong[, vars])) > 0, ]
	dlong$trial_id <- paste(dlong$location, ifelse(dlong$season == "Season 1", "2021-22", "2022-23"), sep="_")
	dlong$plot_id <- paste(dlong$trial_id, dlong$treatment, sep="_")
	dlong <- merge(dlong, d[, c("plot_id", "crop", "record_id")], by=c("plot_id", "crop"), all.x=TRUE)
	dlong <- merge(dlong, d1c, by="location", all.x=TRUE)
	dlong$depth_bottom[is.na(dlong$soil_SWC)] <- NA
	dlong$depth_top <- ifelse(is.na(dlong$soil_SWC), NA, 0)
	dlong <- dlong[order(dlong$record_id, dlong$date, dlong$row), ]
	dlong$order <- ave(dlong$record_id, dlong$record_id, dlong$date, FUN=seq_along)   # > 1 only for duplicated rows
	dlong <- dlong[, c("record_id", "date", "order", vars, "depth_top", "depth_bottom")]

## weather table
	wth <- merge(d1e, d1b, by="location", all.x=TRUE)
	wth$country <- "Zimbabwe"

	carobiner::write_files(path, meta, d, long=dlong, wth=wth)
}
