# R script for "carob"
# license: GPL (>=3)

## NOTES
# Long-term organic cropping system experiment ("Bodemkwaliteit op zand", BKZ) at WUR-proeflocatie Vredepeel, Netherlands (2000-2016).
# Two systems are compared: standard organic (BIO, 2000-2016) and organic with extra compost (BIOcompost, 2011-2016 on plots 34.1a and 34.2b).
# Fresh main-product yield (Hoofdproduct Vers opbrengst, kg/ha) is stored as yield, dry main-product yield as yield_dry, and dry-matter fraction is converted to yield_moisture = 100 * (1 - DM).
# Marktbare opbrengst is stored as yield_marketable except for silage maize, lucerne, and grass-clover, where the source reports dry matter in that column.
# Organic manure (MEST), irrigation and rainfall (NEERSLAGOVERSCHOT, 1 Mar-1 Mar hydrological year), and soil chemistry (BODEM) are joined by system, year, and plot. Four plot-years (2000-2003) grew sequential pea and green bean on the same plot, so plot-year manure, irrigation, and soil values apply to both crops.
# Synthetic fertilizer (NPK kunstmest) is 0 across all plots. Manure volumes (m3/ha) are approximated as 1000 kg/m3 for OM_amount; organic P2O5 and K2O are converted to elemental P and K.
# Coordinates are for WUR Proeflocatie Vredepeel (51.5429 N, 5.8493 E; not given in the deposit).

## ISSUES
# Exact planting and harvest dates are not given in the workbook; year is recorded for planting_date and harvest_date.
# Astilbe and Boshaag (woody hedgerow nursery stock; not harvested in year 1) are not in the terminag crop vocabulary; they are kept as "astilbe" and "hedgerow".
# Grasklaver (grass-clover) is mapped to crop = "clover" with intercropped = TRUE (grass species not named).
# Vinassekali (VK, potassium vinasse) is not in the terminag OM vocabulary; kept as "vinasse".
# Fresh vegetables have yield_moisture above 90% (up to 92.4%); forage maize fresh yield reaches 62.4 t/ha; K_organic reaches 512.8 kg/ha (liquid cattle manure + vinassekali).
# BIOcompost 2011-2013 rows (5 rows) and Boshaag jr1 (2 rows) have NA yield because yields were not recorded / not harvested.
# Soil clay values "<1" / "< 1" and soil Na values "< 1" / "<6" in BODEM are set to half the detection limit (0.5 and 3; soil_Na = 0.5 is below terminag valid_min of 1).

## NEW VARIABLES
# None.

## NOT PROCESSED
# GEWAS: quality parameters (suiker%, vocht%, WIN, TM-getal, owg, tarra%), fresh-weight nutrient concentrations (g N, P2O5, K2O per kg), calculated NPK removal and N fixation formulas, EOS supply calculations, and N-nawerking columns.
# MEST: per-application manure composition, application months/methods, NH3 volatilization, and working/humification coefficients (totals are processed).
# BODEM: seasonal N-min stocks (0-30, 30-60, 60-90 cm), Pw / P-AL, CEC cation saturation percentages, soil S/Ca stocks, and soil B (mixed mg/kg and ug/kg units in source).
# NEMATODEN: plant-parasitic nematode counts per 100 ml soil (2000-2016).
# NAWERKING: calculated nitrogen mineralization after-effects.
# NO3 GRONDWATER: groundwater nitrate concentrations (mg N-NO3/l).
# GRONDWATERSTAND: winter groundwater table depth (2015-2017).
# OS 1998-2016: historical soil organic matter time series starting in 1988.
# OVERZICHT / OVERZICHT COMPOST: summary pivot tables derived from the detail sheets.
# Data rapport _csv_/*.csv: CSV exports of the same Excel sheets (per Readme.txt, formatting/notes are lost in CSV).


carob_script <- function(path) {

"
Biologische teelt op een zuidelijke zandgrond: opbrengst, bemesting, bodemkwaliteit en stikstofverliezen

In deze dataset en het bijbehorende rapport worden de resultaten van het biologische bedrijfssysteem op WUR-proeflocatie Vredepeel in de periode 2001-2016 gepresenteerd met focus op gewasopbrengst, bodemkwaliteit, bemesting en stikstofuitspoeling.
"

	uri <- "doi:10.17026/dans-x3n-64rp"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "WUR",
		publication = "doi:10.18174/440225",
		project = "Bodemkwaliteit op zand",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;OM_type;OM_amount",
		response_vars = "yield;dmy_total;dmy_residue",
		carob_contributor = NA,
		carob_LLM = "Gemini 4 Neon",
		carob_date = "202?-??-??",
		carob_completion = 70,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Data rapport Bodemkwaliteit op zand 2000-2016_Biologisch_Definitief.xlsx"]

	r1 <- carobiner::read.excel(f, sheet="GEWAS", skip=7, col_names=FALSE)
	r1 <- r1[!is.na(r1[[2]]), ]

	crop_map <- c(
		"Aardappel" = "potato",
		"Waspeen" = "carrot",
		"Peen" = "carrot",
		"Suikerbiet" = "sugar beet",
		"Snijmaïs" = "forage maize",
		"Korrelmais" = "maize",
		"Triticale" = "triticale",
		"Conservenerwt" = "pea",
		"Zomergerst" = "barley",
		"Prei" = "leek",
		"Broccoli" = "broccoli",
		"Stamslaboon" = "green bean",
		"Luzerne" = "lucerne",
		"Grasklaver" = "clover",
		"Astilbe" = "astilbe",
		"Boshaag jr1" = "hedgerow",
		"Boshaag jr2" = "hedgerow"
	)
	part_map <- c(
		potato = "tubers",
		carrot = "roots",
		"sugar beet" = "roots",
		"forage maize" = "aboveground biomass",
		maize = "grain",
		triticale = "grain",
		pea = "seed",
		barley = "grain",
		leek = "aboveground biomass",
		broccoli = "flowers",
		"green bean" = "pod",
		lucerne = "aboveground biomass",
		clover = "aboveground biomass",
		astilbe = "aboveground biomass",
		hedgerow = "aboveground biomass"
	)
	cc_map <- c(
		"Gras-klaver" = "clover",
		"Grasklaver" = "clover",
		"Japanse haver" = "oats",
		"Klaver" = "clover",
		"Luzerne" = "lucerne",
		"Triticale" = "triticale",
		"Zomergerst" = "barley",
		"Zomergerst erwt mengsel" = "barley; pea"
	)

	raw_crop <- trimws(as.character(r1[[5]]))
	cn <- unname(crop_map[raw_crop])
	raw_cc <- trimws(as.character(r1[[30]]))
	# Marktbare opbrengst is dry weight for forage maize, lucerne, and grass-clover; keep fresh-weight marketable yield only
	mkt <- as.numeric(r1[[7]])
	mkt[raw_crop %in% c("Snijmaïs", "Luzerne", "Grasklaver")] <- NA_real_

	d <- data.frame(
		treatment = trimws(as.character(r1[[1]])),
		year = as.integer(r1[[2]]),
		plot_id = trimws(as.character(r1[[3]])),
		trial_id = paste0("Vredepeel_", as.integer(r1[[2]])),
		LTE_name = "Bodemkwaliteit op zand",
		country = "Netherlands",
		adm1 = "Limburg",
		adm2 = "Venray",
		location = "Vredepeel",
		# WUR Proeflocatie Vredepeel experimental farm coordinates
		longitude = 5.8493,
		latitude = 51.5429,
		geo_from_source = FALSE,
		crop = cn,
		intercropped = raw_crop == "Grasklaver",
		cover_crop = unname(cc_map[raw_cc]),
		cover_crop_used = !is.na(raw_cc),
		planting_date = as.character(as.integer(r1[[2]])),
		harvest_date = as.character(as.integer(r1[[2]])),
		yield = as.numeric(r1[[13]]),
		yield_isfresh = TRUE,
		yield_moisture = (1 - as.numeric(r1[[14]])) * 100,
		yield_dry = as.numeric(r1[[42]]),
		yield_marketable = mkt,
		yield_part = unname(part_map[cn]),
		fwy_residue = as.numeric(r1[[18]]),
		dmy_residue = as.numeric(r1[[43]]),
		dmy_total = as.numeric(r1[[41]]),
		land_prep_method = ifelse(trimws(as.character(r1[[6]])) == "ploegen", "ploughing", NA_character_),
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$cover_crop[is.na(d$cover_crop)] <- "none"

	# Organic manure and synthetic fertilizer (sheet MEST)
	r2 <- carobiner::read.excel(f, sheet="MEST", skip=7, col_names=FALSE)
	r2 <- r2[r2[[1]] %in% c("BIO", "BIOcompost"), ]
	om_map <- c(RDM = "liquid manure", VRM = "farmyard manure", compost = "compost", VK = "vinasse")
	om_types <- mapply(function(a, b, c) {
		v <- unique(unname(om_map[c(a, b, c)]))
		v <- v[!is.na(v)]
		if (length(v) == 0) "none" else paste(v, collapse="; ")
	}, trimws(as.character(r2[[6]])), trimws(as.character(r2[[21]])), trimws(as.character(r2[[36]])), USE.NAMES=FALSE)

	# Manure gifts 1 & 2 are in m3/ha (approximated as 1000 kg/m3); compost is in ton/ha (* 1000 kg/t)
	g1 <- as.numeric(r2[[7]]); g1[is.na(g1)] <- 0
	g2 <- as.numeric(r2[[22]]); g2[is.na(g2)] <- 0
	gc <- as.numeric(r2[[37]]); gc[is.na(gc)] <- 0
	om_amt <- (g1 + g2 + gc) * 1000

	d_mest <- data.frame(
		treatment = trimws(as.character(r2[[1]])),
		year = as.integer(r2[[2]]),
		plot_id = trimws(as.character(r2[[3]])),
		N_fertilizer = as.numeric(r2[[59]]),
		P_fertilizer = as.numeric(r2[[60]]),
		K_fertilizer = as.numeric(r2[[61]]),
		fertilizer_used = FALSE,
		fertilizer_type = "none",
		N_organic = as.numeric(r2[[51]]),
		P_organic = as.numeric(r2[[52]]) / 2.29,
		K_organic = as.numeric(r2[[53]]) / 1.2051,
		OM_type = om_types,
		OM_amount = om_amt,
		OM_used = om_amt > 0
	)
	d <- merge(d, d_mest, by=c("treatment", "year", "plot_id"), all.x=TRUE)

	# Rainfall (1 Mar - 1 Mar hydrological year) and irrigation (sheet NEERSLAGOVERSCHOT)
	r3 <- carobiner::read.excel(f, sheet="NEERSLAGOVERSCHOT", skip=6, col_names=FALSE)
	r3 <- r3[r3[[1]] %in% c("BIO", "BIOcompost"), ]
	d_irr <- data.frame(
		treatment = trimws(as.character(r3[[1]])),
		year = as.integer(r3[[2]]),
		plot_id = trimws(as.character(r3[[3]])),
		rain = as.numeric(r3[[5]]),
		irrigation_amount = as.numeric(r3[[6]]),
		irrigated = as.numeric(r3[[6]]) > 0
	)
	d <- merge(d, d_irr, by=c("treatment", "year", "plot_id"), all.x=TRUE)

	# Soil fertility measurements (sheet BODEM)
	r4 <- carobiner::read.excel(f, sheet="BODEM", skip=6, col_names=FALSE)
	r4 <- r4[r4[[1]] %in% c("BIO", "BIOcompost"), ]
	clay_chr <- trimws(as.character(r4[[47]]))
	clay_chr[clay_chr %in% c("<1", "< 1")] <- "0.5"
	na_chr <- trimws(as.character(r4[[41]]))
	na_chr[na_chr == "< 1"] <- "0.5"
	na_chr[na_chr == "<6"] <- "3"
	d_soil <- data.frame(
		treatment = trimws(as.character(r4[[1]])),
		year = as.integer(r4[[2]]),
		plot_id = trimws(as.character(r4[[3]])),
		soil_SOM = as.numeric(r4[[16]]),
		soil_N_total = as.numeric(r4[[17]]),
		# g/kg -> %
		soil_C_total = as.numeric(r4[[20]]) / 10,
		soil_P = as.numeric(r4[[25]]),
		soil_K = as.numeric(r4[[27]]),
		# mmol+/kg -> cmol(+)/kg
		soil_CEC = as.numeric(r4[[30]]) / 10,
		soil_pH_KCl = as.numeric(r4[[36]]),
		soil_Mg = as.numeric(r4[[40]]),
		soil_Na = as.numeric(na_chr),
		soil_clay = as.numeric(clay_chr),
		soil_silt = as.numeric(r4[[48]]),
		soil_sand = as.numeric(r4[[49]])
	)
	d <- merge(d, d_soil, by=c("treatment", "year", "plot_id"), all.x=TRUE)

	d$year <- NULL
	carobiner::write_files(path, meta, d)
}
