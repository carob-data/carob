# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer participatory cassava cutting treatments in Tanzania, 10 farmers (codes BL-1 to MS-10).
# pest-and-disease-data.csv is the long table: whitefly adults and nymphs, CMD, CBSD, and cassava green mite severity at 1, 3, 6, and 10 months after planting.
# yield-data.csv is one row per plant (grams of roots, dictionary unit g). It is not converted to kg/ha because area and plant density are absent.
# Stem height is cm (dictionary). CBSD_Incidence_infected is the percent of roots infected and is stored as root_infection.
# parasitism-data.csv is kept separate. It has block 1-10 and abbreviated treatments (MandiLeca, ImidaLeca) and no farmer code.
# Yield file "Imidacloprid+ Lecatech" is aligned to "Imidacloprid + Lecatech".
# CKAN coverage_x -6.52471 and coverage_y 35.78784 are one point for the whole dataset (latitude, longitude), not farm GPS.
# Whitefly is stored as Bemisia tabaci because that is the species named in the dataset title. Cassava green mite is stored under that common name.

## ISSUES
# Root weight is grams per plant, not kg/ha. One Stem_Height is 2 cm (below the usual minimum). One Total_Infected_Roots is 180, which is larger than Total_Root_Number on that row.
# CMD severity is 1 on every pest-file row. Severity scales are not defined in this dictionary (the companion oil trial uses 1-5, 1 = no symptoms; that scale is not copied here).
# Parasitism treatments MandiLeca and ImidaLeca are probably Mandiplus + Lecatech and Imidacloprid + Lecatech. They are not renamed, and they are not joined to farmer codes. Their hhid is block_plant, not a farmer code.
# Parasitism Month is the constant 3. It is not a calendar date.
# Mandiplus and Lecatech are not mapped to an active ingredient. No product rates.
# 10 months after planting does not fit inside the CKAN window 2024-01-01 to 2024-09-01. Planting date is not set.
# No village names, variety, fertilizer, or irrigation.
# write_files: missing yield, yield_moisture, N/P/K, irrigated, planting_date, harvest_date; pest names; possible out-of-bounds plant_height; carob_contributor empty.

## NOT PROCESSED
# All three data files: ID. Row number.

## NEW VARIABLES
# plant_number_: plant number within the farmer or block.
# months_after_planting_: 1, 3, 6, or 10 from the Period code (pest file only).
# stage_: adult or nymph for whitefly counts in the long table.
# root_weight_g_: fresh root weight per plant, grams.
# root_number_: total roots per plant.
# root_healthy_: roots classed as healthy.
# root_infected_: roots classed as CBSD infected. Includes the value 180.
# stem_number_: stems per plant.
# nymphs_healthy_: whitefly nymphs without parasitoids (parasitism file).
# nymphs_parasitized_: whitefly nymphs with parasitoids (parasitism file).
# sample_month_: the value 3 from the parasitism file.


carob_script <- function(path) {

"
IPM of cassava whiteflies and whitefly-transmitted viruses through farmer participatory trials using insecticidal cutting

RTB crops are of strategic importance to the future food security of Africa under predicted climate change scenarios. Cassava is the most important RTB crop in Africa, and its production and consumption are projected to increase due to its adaptability to climate change. Currently cassava is threatened by two viral diseases, cassava mosaic (CMD) and cassava brown streak (CBSD) disease. To boost and sustain cassava production there is a need to develop and deploy strategies that will halt/slow the spread of pandemics of these viruses. Approaches to achieve this include participatory surveillance based on digital tools as well as the promotion of IPM solutions such as use of clean planting materials and control of virus vectors. This project will seek to scale out the use of digital tools for diagnosis of pests and diseases (PlantVillage NURU), access to modern agronomic practices (AKILIMO) and access to clean seed planting material (SeedTracker). This study will strengthen surveillance methods for monitoring virus disease spread and develop early warning strategies that will facilitate tracking of disease fronts. IPM packages for control of cassava whiteflies will be developed through the evaluation and validation of insecticidal cutting dip treatments and spraying with entomopathogens.
"

	uri <- "doi:10.25502/nqeb-q165/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "on-farm; 10 farmers; cutting dip treatments",
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "pest_incidence;disease_severity;root_infection",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "pest-and-disease-data.csv"]
	f2 <- ff[basename(ff) == "yield-data.csv"]
	f3 <- ff[basename(ff) == "parasitism-data.csv"]
	r1 <- read.csv(f1, check.names=FALSE, stringsAsFactors=FALSE)
	r2 <- read.csv(f2, check.names=FALSE, stringsAsFactors=FALSE)
	r3 <- read.csv(f3, check.names=FALSE, stringsAsFactors=FALSE)

	fix_trt <- function(x) {
		x <- trimws(x)
		x[x == "Imidacloprid+ Lecatech"] <- "Imidacloprid + Lecatech"
		x
	}
	r1$Treatment <- fix_trt(r1$Treatment)
	r2$Treatment <- fix_trt(r2$Treatment)

	id1 <- unique(data.frame(
		hhid = trimws(r1$Farmer),
		treatment = r1$Treatment,
		plant_number_ = as.integer(r1$Plant),
		stringsAsFactors = FALSE
	))
	y <- data.frame(
		hhid = trimws(r2$Farmer),
		treatment = r2$Treatment,
		plant_number_ = as.integer(r2$Plant),
		root_weight_g_ = as.numeric(r2$Total_Yield),
		root_number_ = as.integer(r2$Total_Root_Number),
		root_healthy_ = as.integer(r2$Total_Healthy_Roots),
		root_infected_ = as.integer(r2$Total_Infected_Roots),
		root_infection = as.numeric(r2$CBSD_Incidence_infected),
		plant_height = as.numeric(r2$Stem_Height),
		stem_number_ = as.integer(r2$Stem_Numbers),
		stringsAsFactors = FALSE
	)
	d1 <- merge(id1, y, by=c("hhid", "treatment", "plant_number_"), all=TRUE)
	d1 <- d1[order(d1$hhid, d1$treatment, d1$plant_number_), ]
	d1$record_id <- seq_len(nrow(d1))
	d1$crop <- "cassava"
	d1$yield_part <- "roots"
	d1$trial_id <- d1$hhid
	d1$insecticide_used <- d1$treatment != "Control"
	d1$block_id <- NA_character_
	d1$pest_species <- NA_character_
	d1$pest_incidence <- NA_integer_
	d1$nymphs_healthy_ <- NA_integer_
	d1$nymphs_parasitized_ <- NA_integer_
	d1$sample_month_ <- NA_integer_

	r1$months_after_planting_ <- as.integer(sub("MAP", "", r1$Period))
	r1$key <- paste(trimws(r1$Farmer), r1$Treatment, as.integer(r1$Plant), sep="|")
	d1$key <- paste(d1$hhid, d1$treatment, d1$plant_number_, sep="|")
	r1$record_id <- d1$record_id[match(r1$key, d1$key)]
	stopifnot(!any(is.na(r1$record_id)))

	one <- function(species, stage, incidence, severity, disease, dsev) {
		data.frame(
			record_id = r1$record_id,
			months_after_planting_ = r1$months_after_planting_,
			pest_species = species,
			stage_ = stage,
			pest_incidence = incidence,
			pest_severity = severity,
			disease = disease,
			disease_severity = dsev,
			stringsAsFactors = FALSE
		)
	}
	dd <- rbind(
		one("Bemisia tabaci", "adult", as.integer(r1$Whitefly_adults), NA_character_, NA_character_, NA_character_),
		one("Bemisia tabaci", "nymph", as.integer(r1$Whitefly_nymphs), NA_character_, NA_character_, NA_character_),
		one("cassava green mite", NA_character_, NA_integer_, as.character(r1$Cassava_green_mite_severity), NA_character_, NA_character_),
		one(NA_character_, NA_character_, NA_integer_, NA_character_, "cassava mosaic", as.character(r1$Cassava_mosaic_disease_severity)),
		one(NA_character_, NA_character_, NA_integer_, NA_character_, "cassava brown streak", as.character(r1$Cassava_brown_streak_severity))
	)

	d3 <- data.frame(
		hhid = paste(r3$Block, r3$Plant, sep="_"),
		treatment = trimws(r3$Treatment),
		plant_number_ = as.integer(r3$Plant),
		root_weight_g_ = NA_real_,
		root_number_ = NA_integer_,
		root_healthy_ = NA_integer_,
		root_infected_ = NA_integer_,
		root_infection = NA_real_,
		plant_height = NA_real_,
		stem_number_ = NA_integer_,
		record_id = seq_len(nrow(r3)) + nrow(d1),
		crop = "cassava",
		yield_part = "roots",
		trial_id = paste0("parasitism_", r3$Block),
		insecticide_used = trimws(r3$Treatment) != "Control",
		block_id = as.character(r3$Block),
		pest_species = "Bemisia tabaci",
		pest_incidence = as.integer(r3$Total_nymphs),
		nymphs_healthy_ = as.integer(r3$Healthy_nymphs),
		nymphs_parasitized_ = as.integer(r3$Parasitized_nymphs),
		sample_month_ = as.integer(r3$Month),
		stringsAsFactors = FALSE
	)

	d <- carobiner::bindr(d1, d3)
	d$key <- NULL
	d$country <- "Tanzania"
	d$latitude <- -6.52471
	d$longitude <- 35.78784
	d$geo_from_source <- TRUE
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$stress <- "pests;disease"

	carobiner::write_files(path, meta, d, dd)
}
