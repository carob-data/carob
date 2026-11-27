# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. No plot area is reported, so sampled fruit weight (fruit_weight_, g) cannot be converted to yield (kg/ha). Neem seed oil application rate, concentration, and spray dates are not given.
# 2. Both Block (1-4) and Replication (1 or NA) are present; Block is mapped to block_id and Replication to rep. Confirm whether Block should also be used as rep.
# 3. 42 of 112 rows have NA for fruit count, fruit weight, sieving, replication, and all insect counts (no fruits sampled on that date); all rows are retained.
# 4. Fruit flies and parasitoids are only identified to genus (Bactrocera sp., Dacus sp., Fopius sp.; dictionary spells "Bractocera").
# 5. Coordinates (3.84803 N, 11.50208 E) come from the dataset coverage metadata (central Yaounde, ~5 km from IITA Nkolbisson station); confirm exact station coordinates.

## NEW
# fruit_number_: number of fruits in the incubated sample (count)
# fruit_weight_: weight of sampled fruits (g)
# incubation_: incubation batch number (integer, 1-7)
# sieving_: evaluation/sieving number (integer; dictionary: N° d'évaluation)
# insect_sex_: sex of the counted insect ("male" or "female"; NA when unsexed)
# life_stage_: developmental stage of the counted insect ("adult", "unhatched", "pupa", "empty pupa")
# pest_species: "Bactrocera", "Dacus" (not yet in values_pest.csv)

## MISSING
# yield, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date

## NOTES
# Field trial at IITA (Yaounde, Cameroon), 13-30 July 2016, comparing neem seed oil ("traité", mapped to insecticide_product = "margosa oil") against untreated ("non traité") cucumber (Concombre) and squash (Courgette) across 4 blocks.
# Wide table d has one row per fruit sample (112 rows); long table dd retains all non-missing insect counts by genus/organism, sex, and stage (Bactrocera and Dacus in pest_species/pest_incidence; Fopius parasitoids, total pupae, and empty puparia in insect_species/insect_abundance).

## NOT PROCESSED
# fruit-fly-incidnce-n-abundanc-on-cucmber-n-squash.csv: Remarks (entirely empty).

carob_script <- function(path) {

"
Effect of neem seed oil incidence on fruit fly incidence and abondance on cucumber and squash

Preliminary testing of plant extracts as insecticides in organic cultivation
"

	uri <- "doi:10.25502/y7q6-rn92/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Enhancing horticultural productivity, incomes and livelihoods through integrated management of aphid pests on vegetables in sub-Saharan Africa",
		design = "Neem seed oil versus untreated cucumber and squash in 4 blocks; fruit samples incubated for fruit flies",
		data_type = "experiment",
		treatment_vars = "crop;insecticide_used;insecticide_product",
		response_vars = "pest_incidence",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "fruit-fly-incidnce-n-abundanc-on-cucmber-n-squash.csv"]
	r <- read.csv(f, check.names = FALSE, fileEncoding = "latin1")

	mon <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
		jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")
	parts <- strsplit(trimws(r$Date), "-")
	dday <- sprintf("%02d", as.integer(vapply(parts, `[`, "", 1)))
	dmon <- unname(mon[tolower(vapply(parts, `[`, "", 2))])
	dyy <- paste0("20", vapply(parts, `[`, "", 3))
	date <- paste(dyy, dmon, dday, sep = "-")

	tr <- tolower(trimws(r$Traitement))
	treatment <- ifelse(grepl("^non", tr), "untreated", ifelse(grepl("^trait", tr), "neem seed oil", tr))
	fruit <- trimws(r$Fruit)
	crop <- ifelse(fruit == "Concombre", "cucumber", ifelse(fruit == "Courgette", "squash", tolower(fruit)))

	d <- data.frame(
		country = "Cameroon",
		adm1 = "Centre",
		adm2 = "Mfoundi",
		location = "Yaoundé",
		site = trimws(r$site),
		latitude = 3.84803,
		longitude = 11.50208,
		geo_from_source = TRUE,
		geo_uncertainty = 5000,
		crop = crop,
		treatment = treatment,
		insecticide_used = treatment == "neem seed oil",
		insecticide_product = ifelse(treatment == "neem seed oil", "margosa oil", "none"),
		block_id = as.character(r$Block),
		rep = as.integer(r$Replication),
		date = date,
		fruit_number_ = as.integer(r$Nbre_des_fruits),
		fruit_weight_ = as.numeric(r$Weight_g),
		incubation_ = as.integer(r$Inccubation),
		sieving_ = as.integer(r$Sieving),
		stress = "pests",
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		yield_part = "fruit"
	)
	d$record_id <- seq_len(nrow(d))

	stack_pest <- function(count, pest, sex_value, stage) {
		keep <- !is.na(count)
		n <- sum(keep)
		data.frame(
			record_id = d$record_id[keep],
			date = d$date[keep],
			pest_species = rep(pest, n),
			pest_incidence = as.integer(count[keep]),
			insect_species = rep(NA_character_, n),
			insect_abundance = rep(NA_character_, n),
			insect_sex_ = rep(sex_value, n),
			life_stage_ = rep(stage, n)
		)
	}

	stack_insect <- function(count, species, sex_value, stage) {
		keep <- !is.na(count)
		n <- sum(keep)
		data.frame(
			record_id = d$record_id[keep],
			date = d$date[keep],
			pest_species = rep(NA_character_, n),
			pest_incidence = rep(NA_integer_, n),
			insect_species = rep(species, n),
			insect_abundance = as.character(as.integer(count[keep])),
			insect_sex_ = rep(sex_value, n),
			life_stage_ = rep(stage, n)
		)
	}

	dd <- rbind(
		stack_pest(r$BD_male, "Bactrocera", "male", "adult"),
		stack_pest(r$BD_female, "Bactrocera", "female", "adult"),
		stack_pest(r$DS_male, "Dacus", "male", "adult"),
		stack_pest(r$DS_female, "Dacus", "female", "adult"),
		stack_pest(r$UN_BD, "Bactrocera", NA_character_, "unhatched"),
		stack_pest(r$UN_DS, "Dacus", NA_character_, "unhatched"),
		stack_insect(r$Fo_male, "Fopius", "male", "adult"),
		stack_insect(r$Fo_female, "Fopius", "female", "adult"),
		stack_insect(r$UN_Fo, "Fopius", NA_character_, "unhatched"),
		stack_insect(r$Pupe, "fruit fly pupae", NA_character_, "pupa"),
		stack_insect(r$Empty, "empty pupae", NA_character_, "empty pupa")
	)

	carobiner::write_files(path, meta, d, long = dd)
}
