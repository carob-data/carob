# R script for "carob"
# license: GPL (>=3)

## NOTES
# Okra germplasm evaluated for aphids and other insects at Buea, Evodoula, Foumbot and Maroua, Cameroon.
# Individual plant records (Plant_Number 1-5) are preserved without aggregation. There is no yield. The two Camponotus columns stay separate via tag_.
# Observation dates are day/month/year (14/05/2014 is unambiguous). Many ObsDate values are missing; ObsDay (1-7) is kept separately.
# Season 1/2 mapped to terminag first/second. Country is Cameroon from the dictionary.
# Aphid columns are described as counts. The values only reach 0-6, so they may be a score rather than a raw count. Stored as pest_incidence because the dictionary says "number".
# Leaf length and width by stratum have no unit (values about 0-48).

## ISSUES
# No yield, plot area, fertilizer, or irrigation. Coordinates are approximate site locations (geo_from_source = FALSE).
# on_farm is not stated.
# Two unlabeled Num/Dam column pairs sit in front of the named natural-enemy columns. The dictionary only says "Number" and "Damages", so they were not mapped.
# Aphid "counts" of 0-6 need a human check against the field sheet (score vs count).
# Dataset metadata names CORAF/WECARD with IITA. That string is not in the organization vocabulary, so data_organization is IITA only.
# Crop pests (aphid, whitefly, mealybug, leaf beetle) are stored in pest_species; natural enemies and ants are stored in insect_species/insect_abundance.

## NOT PROCESSED
# Num, Dam, Num.1, Dam.1: unlabeled number and damage columns.
# ID: row number.
# data dictionary: column definitions used above, not a data table.
# data_dictionary_okra_performance_and_effect_of_agroecology.csv: documentation or a codebook, not an observation table.

## NEW VARIABLES
# plant_number_: individual plant number within plot (1-5).
# stratum_: bottom, middle, top or apex of the plant for insect counts and leaf size.
# obs_day_: ObsDay, 1-7, not a calendar date.
# bot_leaf_length_, bot_leaf_width_, mid_leaf_length_, mid_leaf_width_, top_leaf_length_, top_leaf_width_: leaf size, unit not stated.


carob_script <- function(path) {

"
Okra performance and effect of agroecology on resistance of various germplasms to aphids

IPM of aphid pests of vegetables
"

	uri <- "doi:10.25502/9qkm-qf24/d"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "pest_incidence;insect_abundance",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 60,
		carob_effort = 0.7,
		notes = NA
	)

	f <- ff[basename(ff) == "okra_performance_and_effect_of_agroecology.csv"]
	r <- read.csv(f, na.strings=c("", "NA"), check.names=TRUE)

	d <- data.frame(
		country = "Cameroon",
		location = r$Loc,
		trial_id = paste(r$Loc, r$Season, sep="_"),
		season = c("Season 1"="first", "Season 2"="second")[r$Season],
		date = as.character(as.Date(r$ObsDate, "%d/%m/%Y")),
		obs_day_ = as.integer(r$ObsDay),
		rep = as.integer(r$Rep),
		plot_id = as.character(r$Plot_No),
		plant_number_ = as.integer(r$Plant_Number),
		variety = r$Accession,
		treatment = r$Accession,
		bot_leaf_length_ = r$BotLength,
		bot_leaf_width_ = r$BotWidth,
		mid_leaf_length_ = r$MidLength,
		mid_leaf_width_ = r$MidWidth,
		top_leaf_length_ = r$TopLength,
		top_leaf_width_ = r$TopWidth,
		crop = "okra",
		yield_part = "fruit",
		on_farm = NA,
		is_survey = FALSE,
		geo_from_source = FALSE,
		record_id = seq_len(nrow(r))
	)

	add_pest <- function(value, species, stratum, severity=NULL, tag=1L) {
		out <- data.frame(
			record_id = d$record_id,
			pest_species = species,
			insect_species = NA_character_,
			stratum_ = stratum,
			pest_incidence = as.integer(round(value)),
			insect_abundance = NA_character_,
			pest_severity = if (is.null(severity)) NA_character_ else as.character(severity),
			order = tag
		)
		out[!is.na(value), ]
	}

	add_insect <- function(value, species, tag=1L) {
		out <- data.frame(
			record_id = d$record_id,
			pest_species = NA_character_,
			insect_species = species,
			stratum_ = NA_character_,
			pest_incidence = NA_integer_,
			insect_abundance = as.character(value),
			pest_severity = NA_character_,
			order = tag
		)
		out[!is.na(value), ]
	}

	long <- rbind(
		add_pest(r$AphidCountAtBot, "aphid", "bottom"),
		add_pest(r$AphidCountAtMid, "aphid", "middle"),
		add_pest(r$AphidCountAtTop, "aphid", "top"),
		add_pest(r$AphidCountAtApex, "aphid", "apex"),
		add_pest(r$WhiteflyAtBot, "whitefly", "bottom"),
		add_pest(r$WhiteflyAtMid, "whitefly", "middle"),
		add_pest(r$WhiteflyAtTop, "whitefly", "top"),
		add_pest(r$LeafBeetleNum, "leaf beetle", NA, r$LeafBeetleDam),
		add_pest(r$Mil, "mealybug", NA),
		add_insect(r$Syrph, "syrphid"),
		add_insect(r$Lbb, "ladybird beetle"),
		add_insect(r$Par, "parasitoid"),
		add_insect(r$Mum, "aphid mummy"),
		add_insect(r$Ph, "Pheidole"),
		add_insect(r$Camp1, "Camponotus", tag=1L),
		add_insect(r$Camp2, "Camponotus", tag=2L),
		add_insect(r$Myr, "Myrmicaria"),
		add_insect(r$SmallRed, "small red ant")
	)
	# Aphid damage is one score per plant, not per stratum.
	aph <- data.frame(
		record_id = d$record_id,
		pest_species = "aphid",
		insect_species = NA_character_,
		stratum_ = NA_character_,
		pest_incidence = NA_integer_,
		insect_abundance = NA_character_,
		pest_severity = as.character(r$AphidDamSev),
		order = 1L
	)
	aph <- aph[!is.na(r$AphidDamSev), ]
	long <- rbind(long, aph)
	long$date <- d$date[match(long$record_id, d$record_id)]

	geo <- data.frame(
		location = c("Buea", "Evodoula", "Foumbot", "Maroua"),
		latitude = c(4.156, 4.083, 5.508, 10.591),
		longitude = c(9.232, 11.200, 10.632, 14.316)
	)
	d <- merge(d, geo, by="location", all.x=TRUE)
	d$date <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
