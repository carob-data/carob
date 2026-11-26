# R script for "carob"
# license: GPL (>=3)

## NOTES
# Early-maturing maize hybrids, West Africa (Nigeria, Benin, Ghana), 2017-2019 (STMA Phase I). Two files: genetic-gains-in-grain-yield-early-maturing-maize-hybrids.csv (stress locations) and genetic-gains-in-early-maturing-maize-hybrids-optimum-env.csv (optimum locations). YIELD is kg/ha in the dictionary.
# Location codes combine IITA West Africa station codes with management suffixes (AB = Abuja, AN = Angaradebou, BG = Bagauda, EJ = Ejura, IA/INA = Ina, IFE = Ile-Ife, IK = Ikenne, MO = Mokwa, NY = Nyankpala, ZA = Zaria); expanded to place names with country and station coordinates (geo_from_source = FALSE), while keeping raw code in trial_id. MXOPT is unexpanded (stored as Mxopt with country/coords NA).
# Stress on the stress file was read from the code: STR = striga (those rows are the only ones with RAT/CO), LN = nitrogen deficiency, IKDS = drought. The optimum file is stress = none. Confirm this reading.
# Striga damage (RAT1, RAT2) and emerged-plant counts (CO1, CO2) are at 8 and 10 weeks after planting. Days after planting used here are 56 and 70 (weeks x 7). Square-root counts (STRC0_1, STRC0_2) were not stored.
# RL and SL have no unit in the dictionary. Values go above 9, so they are not a 1-9 score; stored as recorded.
# Dev_Year is the year the hybrid was developed, not the trial year and not a release year. Trial year is YEAR.

## ISSUES
# Station coordinates assigned from standard IITA West Africa research stations (geo_from_source = FALSE); CKAN point (coverage_x 9.60004, coverage_y 7.99997) was not used. Confirm IA_STR/INAOPT/INAUN = Ina (Benin) and identify MXOPT (currently unmapped, country/coords NA).
# IKDS = drought and LN = nitrogen deficiency are inferred from the codes. Only the STR reading is backed by Striga columns being filled on those rows alone.
# No planting day, harvest date, fertilizer, irrigation, or yield moisture.
# Ear aspect is 16 on one IA_STR 2017 row and 0 on one MOHN 2017 row. Left as recorded.
# Ear rot (E_ROT) reaches 44, so it is not a 1-9 score. Stored as the recorded number in the long disease table.
# No paper DOI in the dataset metadata.
# write_files: missing yield_moisture, harvest_date, N/P/K, irrigated; out of bounds ear_aspect 0 and 16; NA country/lat/lon for MXOPT. Unknown underscore variables.

## NOT PROCESSED
# Both phenotype files, ENV: environment number. Location and year already define the trial.
# Both phenotype files, STRC0_1 and STRC0_2: transformed Striga counts. CO1 and CO2 were used.
# Optimum file, RAT1, RAT2, CO1, CO2, STGR: empty in that file.
# data_dictionary.csv: documentation or a codebook, not an observation table.

## NEW VARIABLES
# breeding_period_: period in which the hybrid was developed (Period, values 1-3). Unit: none.
# variety_development_year_: year the hybrid was developed (Dev_Year). Unit: year.
# ears_per_plant_: ears per plant (EPP). Unit: ears/plant.
# stay_green_: stay-green score (STGR). Empty in the optimum file. Unit: score.


carob_script <- function(path) {

"
Genetic Gains in Grain Yield of Early-maturing Maize Hybrids under Contrasting Environments

Stress Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/8r04-qa29/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Stress Tolerant Maize for Africa (STMA) Phase I",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "yield;plant_height;anthesis_days;silking_days",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 76,
		carob_effort = 0
	)

	as_num <- function(x) {
		if (is.numeric(x)) return(as.numeric(x))
		if (is.logical(x)) return(rep(NA_real_, length(x)))
		z <- trimws(as.character(x))
		z[z %in% c("", ".", "NA", "na")] <- NA_character_
		as.numeric(z)
	}
	as_count <- function(x) {
		x <- as_num(x)
		out <- rep(NA_character_, length(x))
		i <- !is.na(x)
		out[i] <- as.character(x[i])
		out
	}
	stress_from_loc <- function(loc) {
		out <- rep(NA_character_, length(loc))
		out[grepl("STR", loc)] <- "striga"
		out[grepl("LN", loc)] <- "nitrogen deficiency"
		out[loc == "IKDS"] <- "drought"
		out
	}

	loc_map <- data.frame(
		code = c("AB_STR", "ABOPT", "ANOPT", "BGOPT", "EJHN", "IA_STR", "INAOPT", "INAUN",
			"IFE_LN", "IFELN", "IFE_HN", "IFEHN", "IKDS", "IKOPT",
			"MO_STR", "MOSTR", "MOLN", "MOHN", "MOOPT", "NY_STR", "NYOPT", "ZAOPT"),
		location = c("Abuja", "Abuja", "Angaradebou", "Bagauda", "Ejura", "Ina", "Ina", "Ina",
			"Ile-Ife", "Ile-Ife", "Ile-Ife", "Ile-Ife", "Ikenne", "Ikenne",
			"Mokwa", "Mokwa", "Mokwa", "Mokwa", "Mokwa", "Nyankpala", "Nyankpala", "Zaria"),
		country = c("Nigeria", "Nigeria", "Benin", "Nigeria", "Ghana", "Benin", "Benin", "Benin",
			"Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria",
			"Nigeria", "Nigeria", "Nigeria", "Nigeria", "Nigeria", "Ghana", "Ghana", "Nigeria"),
		latitude = c(9.1538, 9.1538, 11.323, 11.570, 7.383, 9.967, 9.967, 9.967,
			7.483, 7.483, 7.483, 7.483, 6.901, 6.901,
			9.296, 9.296, 9.296, 9.296, 9.296, 9.400, 9.400, 11.025),
		longitude = c(7.3220, 7.3220, 3.041, 8.385, -1.367, 2.733, 2.733, 2.733,
			4.560, 4.560, 4.560, 4.560, 3.698, 3.698,
			5.054, 5.054, 5.054, 5.054, 5.054, -0.981, -0.981, 7.652)
	)

	read_piece <- function(fname, optimal) {
		f <- ff[basename(ff) == fname]
		r <- read.csv(f)
		loc <- trimws(r$LOC)
		if (optimal) {
			stress <- rep("none", nrow(r))
		} else {
			stress <- stress_from_loc(loc)
		}
		m <- match(loc, loc_map$code)
		place <- ifelse(!is.na(m), loc_map$location[m], carobiner::fix_name(loc, "title"))
		country <- ifelse(!is.na(m), loc_map$country[m], NA_character_)
		lat <- ifelse(!is.na(m), loc_map$latitude[m], NA_real_)
		lon <- ifelse(!is.na(m), loc_map$longitude[m], NA_real_)
		variety <- trimws(r$Pedigree)
		data.frame(
			trial_id = paste(loc, r$YEAR),
			location = place,
			country = country,
			latitude = lat,
			longitude = lon,
			geo_from_source = ifelse(!is.na(lat) & !is.na(lon), FALSE, NA),
			planting_date = as.character(r$YEAR),
			plot_id = as.character(r$PLOT),
			rep = as.integer(r$Rep),
			block_id = as.character(r$Block),
			variety = variety,
			variety_pedigree = variety,
			variety_code = as.character(r$Entry),
			variety_type = "early-maturing hybrid",
			treatment = variety,
			stress = stress,
			breeding_period_ = as.integer(r$Period),
			variety_development_year_ = as.integer(r$Dev_Year),
			yield = r$YIELD,
			anthesis_days = r$POLLEN,
			silking_days = r$DYSK,
			asi = r$ASI,
			plant_height = r$PLHT,
			ear_height = r$EHT,
			root_lodging = as_count(r$RL),
			stalk_lodging = as_count(r$SL),
			husk = as.character(as_num(r$HUSK)),
			aspect = as_num(r$PLASP),
			ear_aspect = as_num(r$EASP),
			ears_per_plant_ = r$EPP,
			stay_green_ = as_num(r$STGR),
			crop = "maize",
			yield_part = "grain",
			on_farm = FALSE,
			is_survey = FALSE,
			e_rot = as_num(r$E_ROT),
			str_rat1 = as_num(r$RAT1),
			str_rat2 = as_num(r$RAT2),
			str_co1 = as_num(r$CO1),
			str_co2 = as_num(r$CO2)
		)
	}

	d1 <- read_piece("genetic-gains-in-grain-yield-early-maturing-maize-hybrids.csv", FALSE)
	d2 <- read_piece("genetic-gains-in-early-maturing-maize-hybrids-optimum-env.csv", TRUE)
	d <- rbind(d1, d2)
	d$record_id <- as.integer(seq_len(nrow(d)))

	long <- NULL
	add <- function(keep, disease = NA_character_, disease_severity = NA_character_,
		pest_species = NA_character_, pest_severity = NA_character_,
		pest_incidence = NA_integer_, DAP = NA_integer_, ord = NA_integer_) {
		if (!any(keep, na.rm = TRUE)) return(invisible(NULL))
		n <- sum(keep)
		chr1 <- function(x) {
			if (length(x) == 1L) {
				if (is.na(x)) return(rep(NA_character_, n))
				return(rep(as.character(x), n))
			}
			as.character(x[keep])
		}
		int1 <- function(x) {
			if (length(x) == 1L) {
				if (is.na(x)) return(rep(NA_integer_, n))
				return(rep(as.integer(x), n))
			}
			as.integer(x[keep])
		}
		x <- data.frame(
			record_id = d$record_id[keep],
			disease = chr1(disease),
			disease_severity = chr1(disease_severity),
			disease_incidence = rep(NA_character_, n),
			pest_species = chr1(pest_species),
			pest_severity = chr1(pest_severity),
			pest_incidence = int1(pest_incidence),
			DAP = int1(DAP),
			order = int1(ord)
		)
		if (is.null(long)) long <<- x else long <<- rbind(long, x)
	}

	add(!is.na(d$e_rot), disease = "ear rot", disease_severity = d$e_rot)
	# dictionary: 8 WAP and 10 WAP. 8*7 = 56, 10*7 = 70
	k1 <- !is.na(d$str_rat1) | !is.na(d$str_co1)
	add(k1, pest_species = "Striga", pest_severity = d$str_rat1, pest_incidence = d$str_co1, DAP = 56L, ord = 1L)
	k2 <- !is.na(d$str_rat2) | !is.na(d$str_co2)
	add(k2, pest_species = "Striga", pest_severity = d$str_rat2, pest_incidence = d$str_co2, DAP = 70L, ord = 2L)

	d[c("e_rot", "str_rat1", "str_rat2", "str_co1", "str_co2")] <- NULL

	carobiner::write_files(path, meta, d, long = long)
}
