# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two hundred and five testcrosses of early-maturing yellow maize inbreds plus five hybrid checks evaluated across 17 environments (Striga, low soil nitrogen, drought, and optimal conditions) in Nigeria in 2015-2016 (STMA / DTMA).
# Data file is ba15188-across.csv (7,140 plot records). YIELD is grain yield in kg/ha, MOIST is grain moisture at harvest (%).
# Location codes combine IITA Nigeria station prefixes with year and management condition:
#   AB = Abuja, IF = Ile-Ife, IK = Ikenne, KDW = Kadawa, MI = Minjibir, MK = Mokwa;
#   15/16 = trial year; DT = drought, LN = low nitrogen, HN = high nitrogen (optimal), OPT = optimal, STR = Striga-infested.
# Root and stalk lodging are percentages (RLPERC, SLPERC). Ear rot (EROT) and Striga damage ratings/counts at 8 and 10 WAP (RAT1, RAT2, CO1, CO2) go to the long pest/disease table via mz_pd_long.
# Associated publication: doi:10.1038/s41598-019-50345-3 (Badu-Apraku et al., 2019, Scientific Reports).

## ISSUES
# No coordinates in the data file; assigned standard IITA Nigeria research station coordinates for Abuja, Ile-Ife, Ikenne, Kadawa, Minjibir, and Mokwa (geo_from_source = FALSE).
# No exact planting/harvest day, fertilizer rates (LN vs HN/OPT N rates not in the CSV), or irrigation flag.
# Yield moisture has some 0 values and missing values; left as recorded. Some EASP/PASP/HC values are 0 or NA; left as recorded.
# Expect missing harvest_date, N/P/K fertilizer, irrigated, and unknown-variable warnings for custom underscore columns.

## NOT PROCESSED
# ba15188-across.csv, ID: row number.
# ba15188-across.csv, RESCON, Site: numeric condition and environment indices (encoded in LOC).
# ba15188-across.csv, FWT: field weight of ears per plot; plot area is not provided so not converted per ha.
# metadata.csv: column dictionary, used to verify variable mappings.

## NEW VARIABLES
# mating_group_: heterotic/mating group code (Group). Unit: none.
# plant_stand_: plants per plot (PLST). Unit: plants/plot.
# plants_harvested_: plants harvested per plot (PHARV). Unit: plants/plot.
# ears_harvested_: ears harvested per plot (EHARV). Unit: ears/plot.
# ears_per_plant_: ears per plant (EPP). Unit: ears/plant.
# stay_green_: stay-green score (STGR, 1-9 scale). Unit: score.


carob_script <- function(path) {

"
Availability of multiple-stress tolerant maize is critical for improvement in maize production in West and Central Africa. Two hundred and five testcrosses of early maturing yellow maize inbreds were grown with five hybrid checks under Striga, low soil nitrogen, drought and optimal conditions in Nigeria in 2015 and 2016.
"

	uri <- "doi:10.25502/K7Q1-BW26/D"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = "doi:10.1038/s41598-019-50345-3",
		project = "Drought Tolerant Maize for Africa (DTMA); Stress Tolerant Maize for Africa (STMA)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "yield;anthesis_days;silking_days;asi;plant_height;ear_height;aspect;ear_aspect",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 78,
		carob_effort = 0,
		notes = NA
	)

	f <- ff[basename(ff) == "ba15188-across.csv"]
	r <- read.csv(f, na.strings = c("", ".", "NA", "na"))

	num <- function(x) {
		z <- trimws(as.character(x))
		z[z %in% c("", ".", "NA", "na")] <- NA_character_
		as.numeric(z)
	}
	pct <- function(x) {
		v <- num(x)
		out <- rep(NA_character_, length(v))
		i <- !is.na(v)
		out[i] <- paste0(v[i], "%")
		out
	}

	loc_code <- trimws(as.character(r$LOC))
	location <- rep(NA_character_, length(loc_code))
	location[grepl("^AB", loc_code)] <- "Abuja"
	location[grepl("^IF", loc_code)] <- "Ile-Ife"
	location[grepl("^IK", loc_code)] <- "Ikenne"
	location[grepl("^KDW", loc_code)] <- "Kadawa"
	location[grepl("^MI", loc_code)] <- "Minjibir"
	location[grepl("^MK", loc_code)] <- "Mokwa"

	geo <- data.frame(
		location = c("Abuja", "Ile-Ife", "Ikenne", "Kadawa", "Minjibir", "Mokwa"),
		latitude = c(9.1538, 7.483, 6.901, 11.645, 12.177, 9.296),
		longitude = c(7.3220, 4.560, 3.698, 8.433, 8.659, 5.054)
	)
	m <- match(location, geo$location)

	year <- ifelse(grepl("16", loc_code), "2016", "2015")
	# Dry-season managed drought trials harvested in 2015/2016 were planted in late 2014/2015
	year[loc_code %in% c("IK15DT", "MI15DT")] <- "2014"
	year[loc_code == "KDW16DT"] <- "2015"

	stress <- rep("none", length(loc_code))
	stress[grepl("DT$", loc_code)] <- "drought"
	stress[grepl("LN$", loc_code)] <- "nitrogen deficiency"
	stress[grepl("STR$", loc_code)] <- "striga"
	striga <- grepl("STR$", loc_code)

	variety <- trimws(as.character(r$Pedigree))
	d <- data.frame(
		trial_id = loc_code,
		location = location,
		country = "Nigeria",
		latitude = geo$latitude[m],
		longitude = geo$longitude[m],
		geo_from_source = FALSE,
		planting_date = year,
		plot_id = as.character(r$PLOT),
		rep = as.integer(r$Rep),
		block_id = as.character(r$Blk),
		variety_code = as.character(r$Entry),
		variety = variety,
		variety_pedigree = variety,
		variety_type = "early-maturing yellow hybrid",
		treatment = variety,
		female_parent = trimws(as.character(r$Line)),
		male_parent = trimws(as.character(r$Tester)),
		mating_group_ = trimws(as.character(r$Group)),
		stress = stress,
		yield = num(r$YIELD),
		yield_moisture = num(r$MOIST),
		anthesis_days = num(r$POLLEN),
		silking_days = num(r$DYSK),
		asi = num(r$ASI),
		plant_height = num(r$PLHT),
		ear_height = num(r$EHT),
		root_lodging = pct(r$RLPERC),
		stalk_lodging = pct(r$SLPERC),
		husk = ifelse(is.na(num(r$HC)), NA_character_, as.character(num(r$HC))),
		aspect = num(r$PASP),
		ear_aspect = num(r$EASP),
		plant_stand_ = num(r$PLST),
		plants_harvested_ = num(r$PHARV),
		ears_harvested_ = num(r$EHARV),
		ears_per_plant_ = num(r$EPP),
		stay_green_ = num(r$STGR),
		e_rot = num(r$EROT),
		str_rat1 = num(r$RAT1),
		str_rat2 = num(r$RAT2),
		str_co1 = num(r$CO1),
		str_co2 = num(r$CO2),
		crop = "maize",
		yield_part = "grain",
		borer_trial = FALSE,
		striga_trial = striga,
		striga_infected = striga,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d$record_id <- as.integer(seq_len(nrow(d)))
	mz_pd_long <- carobiner::get_function("mz_pd_long", path, group)
	long <- mz_pd_long(d)
	d[intersect(c("e_rot", "str_co1", "str_co2", "str_rat1", "str_rat2"), names(d))] <- NULL

	carobiner::write_files(path, meta, d, long = long)
}
