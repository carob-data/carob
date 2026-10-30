# R script for "carob"
# license: GPL (>=3)

## NOTES
# Regional Striga-resistant and drought-tolerant hybrid trials, about 2012-2019 (Menkir; DTMA and STMA). Four files.
# m28fulli_irrigation-data.csv is padded with about 337000 blank rows; those rows were dropped. The unnamed column between REP and PLOT (integers 1-42) is stored as block_id because it occupies the same place as BLK in the multiloc file. Confirm that reading.
# Grain yield is kg/ha (the irrigation and multiloc headers say so; the other two files use the same scale).
# Columns named "Silking date" and "Antheis Day" are days from planting, not calendar dates (values sit next to anthesis, about 45-90).
# m28striga-data.csv has infested and non-infested measurements on one row. They are split into two rows. Striga ratings and counts stay on the infested row only.
# m28multiloc-data-ii.csv (Kadawa, 2017) is the only file with variety names and grain moisture (MOI, %).
# Elsewhere the variety is the entry number. Entry numbers are not a stable variety id across years or files, and were not joined to the Kadawa names.
# Maize streak virus (MSV) and Curvularia leaf spot in the multiloc file go to the long pest/disease table.
# Country and location names were filled only where the site code is a known place. No coordinates were added. Metadata lists five unlabelled coordinate pairs; they were not used.

## ISSUES
# Irrigation file has no location, so those trials cannot be placed. The unnamed column may be a block or a site index.
# Many multiloc codes have no country: AGWAN_MALAMI, CARI_14, CARI_15, CARI_16, CARI_18, CARI_20, DALABANI, DUSU, INA, JANGEME, KALENDA, KASHILE, KUDU, MAGADAS, NALWEYO, PANDA, SABONGARI, TANIINA, YAMTAKE. INA may be Ina (Benin) or Baba Ina (Nigeria).
# Countries outside the metadata list (Nigeria, Ghana, Mali, Benin) were assigned from the place name and need confirmation: Beni Suef (Egypt), Manga (Burkina Faso), Gambia, Gandajika (Democratic Republic of the Congo), Kilissi (Guinea), Gbedin 19 (Liberia). Kita, Lere, and Zamfara (a state name, not a town) also need confirmation. Gambia is stored as both location and country because the file gives no finer site.
# Numbered codes (Bagauda 2, Fumesua 1, Kadawa 1, and so on) were kept separate from the base name. IFE and ILE-IFE were both read as Ile-Ife.
# No fertilizer rates and no day of planting or harvest. Irrigation is set only for the irrigation file.
# Variety names are missing except in m28multiloc-data-ii.csv.
# Blank cells and "." in the multiloc file are missing values (notably husk cover).
# Left as recorded, outside usual ranges: anthesis and silking of 0, plant and ear height of 0, plant aspect from -9 to 29, ear aspect of 0, husk cover of 0 and up to 30, MSV up to 14.
# m28multiloc-data.csv has yield missing on some rows. Those rows were kept.
# NP, PL, and SD in the Kadawa file are undefined abbreviations.
# Non-infested Striga-file rows have no anthesis-silking interval, ear height is only on the non-infested side, and ears per plant and anthesis are not paired for every side. See the split.
# On-farm status is not in the files (regional trials may include farmer fields). Recorded as station trials (on_farm FALSE).
# Expect NA country, missing longitude/latitude/geo_from_source, missing N/P/K and harvest_date, NA yield, out-of-bounds messages, and unknown-variable warnings.

## NOT PROCESSED
# m28fulli_irrigation-data.csv, blank trailing rows: empty export rows.
# m28multiloc-data.csv, trailing empty column: no header and no values.
# m28multiloc-data-ii.csv, PS, MSV, HM, CUR, ER: present but entirely missing.
# m28multiloc-data-ii.csv, NP, PL, SD: undefined abbreviations (NP and PL about 28-36, SD about 60-74). Not assigned to density or silking.
# m28striga-data.csv, ENV: used only inside trial_id.
# No data dictionary is in the download.

## NEW VARIABLES
# ears_per_plant_: ears per plant as recorded (dimensionless).
# senescence_1_: first senescence score in the Kadawa file (scale not defined).
# senescence_2_: second senescence score in the Kadawa file (scale not defined).


carob_script <- function(path) {

"
Regional trials of Striga resistant and drought tolerant maize hybrids conducted for eight years

Breeding for stress resilient maize varieties and hybrids
"

	uri <- "doi:10.25502/4xv7-bc93/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Drought Tolerant Maize for Africa (DTMA); Stress Tolerant Maize for Africa (STMA)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;anthesis_days;silking_days;asi;plant_height;ear_height;aspect;ear_aspect",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 58,
		carob_effort = 1.2,
		notes = NA
	)

	geo <- data.frame(
		loc_code = c(
			"AKURE", "ASKIRA", "BAGAUDA", "BAGAUDA_2", "BIRNI_KUDU", "DANSADAU", "DUTSIMA", "EJIBA", "ERUWA",
			"IFE", "IFE_1", "IFE_2", "IKENNE", "ILE-IFE", "ILORIN", "KADAWA", "KADAWA_1", "KADAWA_2", "KANO",
			"KISHI", "LAPAI", "LERE", "MOKWA", "MOKWA_2", "OMU-ARAN", "SAMARU", "SAMINAKA", "SHEDA", "ZAMFARA",
			"ZARIA", "ZARIA_2",
			"DAMONGO", "EJURA", "EJURA_2", "FUMESUA", "FUMESUA_1", "FUMESUA_2", "KPEVE", "NYANKPALA", "WA", "YENDI",
			"ANGARADEBOU", "BAGOU",
			"BOUGOUMI", "KITA", "SAMANKO", "SOTUBA",
			"BANI_SUEF", "MANGA", "GAMBIA", "GANDAJIKA", "KILISSI", "GBEDIN_19",
			"ABJ", "ABUJA", "MOK"
		),
		location = c(
			"Akure", "Askira", "Bagauda", "Bagauda 2", "Birnin Kudu", "Dansadau", "Dutsin-Ma", "Ejiba", "Eruwa",
			"Ile-Ife", "Ile-Ife 1", "Ile-Ife 2", "Ikenne", "Ile-Ife", "Ilorin", "Kadawa", "Kadawa 1", "Kadawa 2", "Kano",
			"Kishi", "Lapai", "Lere", "Mokwa", "Mokwa 2", "Omu-Aran", "Samaru", "Saminaka", "Sheda", "Zamfara",
			"Zaria", "Zaria 2",
			"Damongo", "Ejura", "Ejura 2", "Fumesua", "Fumesua 1", "Fumesua 2", "Kpeve", "Nyankpala", "Wa", "Yendi",
			"Angaradebou", "Bagou",
			"Bougouni", "Kita", "Samanko", "Sotuba",
			"Beni Suef", "Manga", "Gambia", "Gandajika", "Kilissi", "Gbedin 19",
			"Abuja", "Abuja", "Mokwa"
		),
		country = c(
			rep("Nigeria", 31),
			rep("Ghana", 10),
			rep("Benin", 2),
			rep("Mali", 4),
			"Egypt", "Burkina Faso", "Gambia", "Democratic Republic of the Congo", "Guinea", "Liberia",
			"Nigeria", "Nigeria", "Nigeria"
		),
		stringsAsFactors = FALSE
	)

	place <- function(loc_code) {
		loc_code <- trimws(as.character(loc_code))
		m <- match(loc_code, geo$loc_code)
		location <- loc_code
		country <- rep(NA_character_, length(loc_code))
		hit <- !is.na(m)
		location[hit] <- geo$location[m[hit]]
		country[hit] <- geo$country[m[hit]]
		# unmatched codes were all capitals; keep them readable
		up <- !is.na(location) & location == toupper(location) & grepl("[A-Z]", location)
		location[up] <- carobiner::fix_name(location[up], "title")
		data.frame(location = location, country = country, stringsAsFactors = FALSE)
	}

	chr_num <- function(x) {
		ifelse(is.na(x), NA_character_, as.character(x))
	}

	f1 <- ff[basename(ff) == "m28fulli_irrigation-data.csv"]
	r1 <- read.csv(f1, na.strings = c("NA", "", "."))
	# trailing blank rows from the export
	r1 <- r1[!is.na(r1$YEAR), ]
	v1 <- as.character(r1$ENTRY)
	d1 <- data.frame(
		planting_date = as.character(r1$YEAR),
		rep = as.integer(r1$REP),
		block_id = as.character(r1$X),
		plot_id = as.character(r1$PLOT),
		variety_code = v1,
		variety = v1,
		treatment = v1,
		yield = as.numeric(r1$YIELD..kg.ha.),
		anthesis_days = as.numeric(r1$Antheis.Day),
		silking_days = as.numeric(r1$Silking.date),
		asi = as.numeric(r1$ASI),
		plant_height = as.numeric(r1$Plant.height),
		ear_height = as.numeric(r1$Eear.height),
		ears_per_plant_ = as.numeric(r1$Ears.per.plant),
		husk = chr_num(r1$Husk.cover),
		ear_aspect = as.numeric(r1$Ear.aspect),
		aspect = as.numeric(r1$Plant.aspect),
		irrigated = TRUE,
		stringsAsFactors = FALSE
	)
	d1$trial_id <- paste("irrigation", d1$planting_date, sep = "_")

	f2 <- ff[basename(ff) == "m28multiloc-data.csv"]
	r2 <- read.csv(f2, na.strings = c("NA", "", "."))
	p2 <- place(r2$LOC)
	v2 <- as.character(r2$ENTRY)
	d2 <- data.frame(
		planting_date = as.character(r2$YEAR),
		location = p2$location,
		country = p2$country,
		rep = as.integer(r2$REP),
		block_id = as.character(r2$BLK),
		plot_id = as.character(r2$PLOT),
		variety_code = v2,
		variety = v2,
		treatment = v2,
		yield = as.numeric(r2$YIELD..kg.ha.),
		anthesis_days = as.numeric(r2$Antheis.Day),
		silking_days = as.numeric(r2$Silking.date),
		asi = as.numeric(r2$ASI),
		plant_height = as.numeric(r2$Plant.height),
		ear_height = as.numeric(r2$Eear.height),
		ears_per_plant_ = as.numeric(r2$Ears.per.plant),
		husk = chr_num(r2$Husk.cover),
		ear_aspect = as.numeric(r2$Ear.aspect),
		aspect = as.numeric(r2$Plant.aspect),
		streak = as.numeric(r2$MSV),
		curv = as.numeric(r2$Curvularia.leaf.spot),
		stringsAsFactors = FALSE
	)
	d2$trial_id <- paste(d2$location, d2$planting_date, r2$ENV, sep = "_")

	f3 <- ff[basename(ff) == "m28multiloc-data-ii.csv"]
	r3 <- read.csv(f3, na.strings = c("NA", "", "."))
	p3 <- place(r3$LOC)
	v3 <- trimws(as.character(r3$NAME))
	d3 <- data.frame(
		planting_date = as.character(r3$YEAR),
		location = p3$location,
		country = p3$country,
		rep = as.integer(r3$REP),
		block_id = as.character(r3$BLK),
		plot_id = as.character(r3$PLOT),
		variety_code = as.character(r3$ENTRY),
		variety = v3,
		treatment = v3,
		yield = as.numeric(r3$YIELD),
		anthesis_days = as.numeric(r3$AD),
		asi = as.numeric(r3$ASI),
		plant_height = as.numeric(r3$PH),
		ear_height = as.numeric(r3$EH),
		root_lodging = chr_num(r3$RL),
		stalk_lodging = chr_num(r3$SL),
		ears_per_plant_ = as.numeric(r3$EPP),
		husk = chr_num(r3$HC),
		ear_aspect = as.numeric(r3$EA),
		aspect = as.numeric(r3$PA),
		yield_moisture = as.numeric(r3$MOI),
		senescence_1_ = as.numeric(r3$SENES1),
		senescence_2_ = as.numeric(r3$SENES2),
		stringsAsFactors = FALSE
	)
	d3$trial_id <- paste(d3$location, d3$planting_date, r3$ENV, sep = "_")

	f4 <- ff[basename(ff) == "m28striga-data.csv"]
	r4 <- read.csv(f4, na.strings = c("NA", "", "."))
	p4 <- place(r4$LOC)
	v4 <- as.character(r4$ENTRY)
	d4i <- data.frame(
		planting_date = as.character(r4$YEAR),
		location = p4$location,
		country = p4$country,
		rep = as.integer(r4$REP),
		plot_id = as.character(r4$PLOT),
		variety_code = v4,
		variety = v4,
		treatment = v4,
		yield = as.numeric(r4$Yield.infested),
		silking_days = as.numeric(r4$Silking.days.infested),
		anthesis_days = as.numeric(r4$Anthesis.days.infested),
		plant_height = as.numeric(r4$Plant.height.infested),
		ear_aspect = as.numeric(r4$Eear.aspect.infested),
		asi = as.numeric(r4$ASI.Infested),
		ears_per_plant_ = as.numeric(r4$Ears.per.plant.infested),
		str_rat1 = as.numeric(r4$Striga.rating.8.WAP),
		str_rat2 = as.numeric(r4$Striga.rating.10.WAP),
		str_co1 = as.numeric(r4$Striga.count.8.WAP),
		str_co2 = as.numeric(r4$Striga.counet.10.WAP),
		stress = "striga",
		striga_trial = TRUE,
		striga_infected = TRUE,
		stringsAsFactors = FALSE
	)
	d4i$trial_id <- paste(d4i$location, d4i$planting_date, "infested", r4$ENV, sep = "_")
	d4u <- data.frame(
		planting_date = as.character(r4$YEAR),
		location = p4$location,
		country = p4$country,
		rep = as.integer(r4$REP),
		plot_id = as.character(r4$PLOT),
		variety_code = v4,
		variety = v4,
		treatment = v4,
		yield = as.numeric(r4$Yield.non.infested),
		silking_days = as.numeric(r4$Silking.days.non.infested),
		anthesis_days = as.numeric(r4$Anthesis.days.non.infested),
		plant_height = as.numeric(r4$Plant.height.non.infested),
		ear_height = as.numeric(r4$Ear.height.non.infested),
		aspect = as.numeric(r4$Plant.aspect.non.infested),
		husk = chr_num(r4$Husk.cover.non.infested),
		ear_aspect = as.numeric(r4$Eear.aspect.non.infested),
		stress = "none",
		striga_trial = TRUE,
		striga_infected = FALSE,
		stringsAsFactors = FALSE
	)
	d4u$trial_id <- paste(d4u$location, d4u$planting_date, "noninfested", r4$ENV, sep = "_")

	d <- carobiner::bindr(d1, d2, d3, d4i, d4u)
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$variety_type <- "hybrid"
	d$borer_trial <- FALSE
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$striga_trial[is.na(d$striga_trial)] <- FALSE
	d$striga_infected[is.na(d$striga_infected)] <- FALSE
	d$husk <- trimws(d$husk)
	d$husk[d$husk %in% c("", ".", "NA", "na")] <- NA_character_
	d$record_id <- as.integer(seq_len(nrow(d)))
	mz_pd_long <- carobiner::get_function("mz_pd_long", path, group)
	long <- mz_pd_long(d)
	d[intersect(c("e_rot", "streak", "curv", "str_co1", "str_co2", "str_rat1", "str_rat2"), names(d))] <- NULL
	carobiner::write_files(path, meta, d, long = long)
}
