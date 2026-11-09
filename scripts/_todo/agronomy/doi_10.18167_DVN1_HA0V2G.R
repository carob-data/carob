# R script for "carob"
# license: GPL (>=3)

## NOTES
# Raw sheets are used for biomass, cover, height and phenology. DATA_calculated_* trait columns (rates, means, WCE) are not used.
# Latin names in DATA_calculated_collection_trials and DATA_calculated_weed_control_trials are used only as a species dictionary for the French common names.
# Aboveground fresh and dry mass are t/ha. Weed-plot rows (Type Weed) use that fresh mass as weed_biomass; cover-plant rows use it as yield.
# NPK (36-82-60) in the trial description is treated as kg/ha of N, P2O5 and K2O. "no" is 0. Blank fertilization is left unknown.
# The description file is tab-separated text with an xlsx name. Sowing cells are Excel serials even though the header says dd/mm/yy.
# Coordinates are in that file. Country is geographicCoverage Réunion. Colimaçons spelling follows the readme.
# All trials were irrigated except Colimaçons (readme). Weather Rg is labeled MJ/m2 and stored as kJ/m2.
# Publication DOI is 10.1016/j.eja.2021.126316.

## ISSUES
# Collection rows have phenology but no biomass, so yield is missing there. Cover and height for trials that have neither biomass nor phenology are left out of the long table.
# Guizotia is stored as "niger " so it matches the crop vocabulary, which flags the trailing space. Species with no vocabulary name keep the Latin binomial.
# Fresh biomass of cowpea and pearl millet is high for a grain-yield limit. Plant height includes values below 10 cm.
# write_files: untrimmed characters crop; invalid terms crop Avena strigosa, Canavalia gladiata, Centrosema pascuorum, Lens nigricans, Lotus corniculatus, Lupinus albus, Macrotyloma axillare, Neonotonia wightii, Pueraria phaseoloides; high crop yield cowpea 11790, pearl millet 46958.33333; NA detected yield; unknown variable leaf_number_; unknown variables measured_on_; out of bounds plant_height (0, 175.6).

## NEW VARIABLES
# leaf_number_: number of leaves on the measured plant
# measured_on_: trial, replicate, mixture and species label for a cover or height row

## NOT PROCESSED
# DATA_calculated_collection_trials trait columns (Cov_mean, Cov_rate, H_rate, Leaf_rate, and the averaged weather)
# DATA_calculated_weed_control_trials trait columns (Cov_end_*, Cov_rate_*, WCE_*)
# Thermal time (Emerg_SDD, Flo_SDD, Fru_SDD)
# Readme.txt: documentation or a codebook, not an observation table.

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
		treatment_vars = "treatment",
		response_vars = "yield;ground_cover;weed_biomass",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 50,
		carob_effort = 1.1
	)

	num <- function(x) {
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x <- gsub(",", ".", x, fixed=TRUE)
		x[x %in% c("", "NA", "na")] <- NA
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
		"Pennisetum glaucum" = "pearl millet",
		"Canavalia ensiformis" = "jack bean",
		"Raphanus sativus" = "radish",
		"Guizotia abyssinica" = "niger ",
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
		"Crotalaria trichotoma" = "crotalaria"
	)
	# "niger " keeps the trailing space used in the crop vocabulary.
	dict <- data.frame(common=character(), latin=character())
	for (bn in c("DATA_calculated_collection_trials.xlsx", "DATA_calculated_weed_control_trials.xlsx")) {
		r <- readx(ff[basename(ff) == bn], sheet="DATA")
		nm <- names(r)
		c1 <- nm[nm %in% c("Common french name", "Common name 1")]
		l1 <- nm[nm %in% c("Latin name", "Latin name 1")]
		if (length(c1) == 1 && length(l1) == 1) {
			dict <- rbind(dict, data.frame(common=as.character(r[[c1]]), latin=as.character(r[[l1]])))
		}
		c2 <- nm[nm == "Common name 2"]
		l2 <- nm[nm == "Latin name 2"]
		if (length(c2) == 1) {
			dict <- rbind(dict, data.frame(common=as.character(r[[c2]]), latin=as.character(r[[l2]])))
		}
	}
	dict <- dict[!is.na(dict$common) & dict$common != "" & !is.na(dict$latin) & dict$latin != "", ]
	dict <- dict[!duplicated(dict$common), ]
	to_crop <- function(common) {
		lat <- dict$latin[match(common, dict$common)]
		out <- latin_crop[lat]
		out <- unname(out)
		miss <- is.na(out) | out == ""
		out[miss] <- lat[miss]
		out[is.na(out)] <- common[is.na(out)]
		out[common %in% c("Weed", "weed")] <- "none"
		out
	}

	f1 <- ff[basename(ff) == "Trial and species description.xlsx"]
	r1 <- read.delim(f1, skip=1, check.names=FALSE, quote="\"", fileEncoding="latin1")
	idcol <- names(r1)[names(r1) == "Trial ID"]
	r1 <- r1[!is.na(r1[[idcol]]) & r1[[idcol]] != "", ]
	for (col in c("Site", "Latitude", "Longitude", "Altitude", "Soil type")) {
		for (i in seq_len(nrow(r1))[-1]) {
			if (is.na(r1[[col]][i]) || r1[[col]][i] == "") r1[[col]][i] <- r1[[col]][i - 1]
		}
	}
	r1$Site <- as.character(r1$Site)
	r1$Site[grepl("Colima", r1$Site)] <- "Colimaçons"
	site <- data.frame(
		trial_id = as.character(r1[[idcol]]),
		location = r1$Site,
		latitude = num(r1$Latitude),
		longitude = num(r1$Longitude),
		elevation = num(sub(" m", "", r1$Altitude)),
		soil_type = r1[["Soil type"]],
		planting_date = as.character(as.Date(num(r1[["Sowing date (dd/mm/yy)"]]), origin="1899-12-30")),
		fert_raw = as.character(r1$Fertilization)
	)
	site$fert_raw[site$fert_raw %in% c("", "NA")] <- NA
	npk <- regmatches(site$fert_raw, regexec("([0-9]+)-([0-9]+)-([0-9]+)", site$fert_raw))
	site$N_fertilizer <- NA_real_
	site$P_fertilizer <- NA_real_
	site$K_fertilizer <- NA_real_
	for (i in seq_len(nrow(site))) {
		if (length(npk[[i]]) == 4) {
			site$N_fertilizer[i] <- as.numeric(npk[[i]][2])
			site$P_fertilizer[i] <- as.numeric(npk[[i]][3]) / 2.29
			site$K_fertilizer[i] <- as.numeric(npk[[i]][4]) / 1.2051
		} else if (!is.na(site$fert_raw[i]) && tolower(site$fert_raw[i]) == "no") {
			site$N_fertilizer[i] <- 0
			site$P_fertilizer[i] <- 0
			site$K_fertilizer[i] <- 0
		}
	}

	f2 <- ff[basename(ff) == "DATARAW_aboveground_biomass.xlsx"]
	b <- readx(f2, sheet="Weed control trial data")
	d1 <- data.frame(
		trial_id = as.character(b[["Trial ID"]]),
		rep = as.integer(b$Repetition),
		treatment = as.character(b$Type),
		plot_id = paste(b[["Trial ID"]], b$Repetition, b[["Mixture ID"]], b[["Common name"]], sep="_"),
		crop = to_crop(as.character(b[["Common name"]])),
		variety = as.character(b[["Common name"]]),
		date = as_day(b$Date),
		yield = num(b$ABV_FM) * 1000,
		dmy_total = num(b$ABV_DM) * 1000,
		yield_part = "aboveground biomass"
	)
	d1$yield_moisture <- (1 - d1$dmy_total / d1$yield) * 100
	d1$yield_moisture[d1$yield == 0] <- NA
	d1$weed_biomass <- ifelse(d1$treatment == "Weed", d1$yield, NA_real_)
	d1$flowering_days <- NA_real_

	f3 <- ff[basename(ff) == "DATARAW_phenology.xlsx"]
	p <- readx(f3, sheet="Collection trial data")
	d2 <- data.frame(
		trial_id = as.character(p[["Trial ID"]]),
		rep = NA_integer_,
		treatment = "collection",
		plot_id = paste(p[["Trial ID"]], p[["Common name"]], sep="_"),
		crop = to_crop(as.character(p[["Common name"]])),
		variety = as.character(p[["Common name"]]),
		date = NA_character_,
		yield = NA_real_,
		dmy_total = NA_real_,
		yield_part = "aboveground biomass",
		yield_moisture = NA_real_,
		weed_biomass = NA_real_,
		flowering_days = num(p$Flowering)
	)

	d <- rbind(d1, d2)
	d <- merge(d, site[, c("trial_id", "location", "latitude", "longitude", "elevation", "soil_type", "planting_date", "N_fertilizer", "P_fertilizer", "K_fertilizer")], by="trial_id", all.x=TRUE)
	d$country <- "Réunion"
	d$geo_from_source <- TRUE
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- d$location != "Colimaçons"
	d$harvest_date <- d$date
	d$date <- NULL
	# country from geographicCoverage. irrigated from the readme (all sites except Colimaçons).

	f4 <- ff[basename(ff) == "DATARAW_plant_cover.xlsx"]
	c1 <- readx(f4, sheet="Collection trial data")
	c2 <- readx(f4, sheet="Weed control trial data")
	long_c <- rbind(
		data.frame(
			trial_id = as.character(c1[["Trial ID"]]),
			measured_on_ = paste(c1[["Trial ID"]], c1[["Common name"]], sep="_"),
			date = as_day(c1$Date),
			DAP = as.integer(c1$DAS),
			ground_cover = num(c1$Cov),
			plant_height = NA_real_,
			leaf_number_ = NA_real_
		),
		data.frame(
			trial_id = as.character(c2[["Trial ID"]]),
			measured_on_ = paste(c2[["Trial ID"]], c2$Repetition, c2[["Mixture ID"]], c2[["Common name"]], sep="_"),
			date = as_day(c2$Date),
			DAP = NA_integer_,
			ground_cover = num(c2$Cov),
			plant_height = NA_real_,
			leaf_number_ = NA_real_
		)
	)

	f5 <- ff[basename(ff) == "DATARAW_height_number_of_leaves.xlsx"]
	h1 <- readx(f5, sheet="Collection trials data")
	h2 <- readx(f5, sheet="Weed control trials data")
	long_h <- rbind(
		data.frame(
			trial_id = as.character(h1[["Trial ID"]]),
			measured_on_ = paste(h1[["Trial ID"]], h1[["Common name"]], sep="_"),
			date = as_day(h1$Date),
			DAP = NA_integer_,
			ground_cover = NA_real_,
			plant_height = num(h1$Height),
			leaf_number_ = num(h1[["Number of leaves"]])
		),
		data.frame(
			trial_id = as.character(h2[["Trial ID"]]),
			measured_on_ = paste(h2[["Trial ID"]], h2$Repetition, h2[["Mixture ID"]], h2[["Common name"]], sep="_"),
			date = as_day(h2$Date),
			DAP = NA_integer_,
			ground_cover = NA_real_,
			plant_height = num(h2$Height),
			leaf_number_ = num(h2[["Number of leaves"]])
		)
	)
	long <- rbind(long_c, long_h)
	long <- long[!(is.na(long$ground_cover) & is.na(long$plant_height) & is.na(long$leaf_number_)), ]
	# Keep only trials that also have a biomass or phenology row, so trial_id matches.
	long <- long[long$trial_id %in% d$trial_id, ]

	f6 <- ff[basename(ff) == "Meteorological_data.xlsx"]
	mw <- readx(f6, sheet="Meteorological_data")
	wth <- data.frame(
		station_name = as.character(mw$Station),
		date = as_day(mw$Date),
		prec = num(mw$PPT),
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

	carobiner::write_files(path, meta, d, long=long, wth=wth)
}
