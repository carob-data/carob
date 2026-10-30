# R script for "carob"
# license: GPL (>=3)

## NOTES
# Fruit samples from cucumber and squash at site "IITA", 13-30 July 2016, neem seed oil
#   ("traité") versus untreated. Dates in the file are like 13-Jul-16; parsed without
#   relying on the locale. Coverage coordinates 3.84803 N, 11.50208 E.
# d is one fruit sample (a row of the csv). pest_incidence on d is the sum of adult
#   Bactrocera (columns BD_*) and Dacus (columns DS_*) males and females. It is NA when
#   all four counts are NA, and 0 when the flies were counted as zero.
# dd splits every non-missing count (organism_count_): Bactrocera, Dacus, their unhatched
#   pupae, Fopius (a parasitoid), total pupae, and empty pupae. pest_incidence is not
#   repeated on dd.
# The dictionary spells the genus "Bractocera"; stored as Bactrocera.
# Weight_g is the weight of the sampled fruits, not a plot yield. Several rows have
#   incubation/sieving flags and no fruit count.

## ISSUES
# 1. No plot area, so fruit weight is not a yield in kg/ha. No fertilizer or neem rate.
# 2. Block and Replication are both present. Replication is 1 or blank; Block is 1-4.
#    rep is Replication only (not copied from Block).
# 3. BD/DS/Fo are not identified to species (dictionary: Bractocera sp, Dacus sp, Fopius sp).
# 4. write_files: missing yield, yield_moisture, irrigated, N/P/K, planting_date, harvest_date.
#    "neem seed oil", "Bactrocera" and "Dacus" are not in the vocabularies. Coordinates are the
#    dataset coverage point, not a GPS column.

## NOT PROCESSED
# SN: row number.
# Remarks: empty.
# fruit-fly dictionary: definitions used above.

## NEW VARIABLES
# fruit_number_: fruits in the sample
# fruit_weight_g_: sample fruit weight, grams
# incubation_: incubation number
# sieving_: evaluation number (dictionary: N° d'évaluation)
# sex: male or female of the counted insect (not the respondent); NA when the count is not sexed
# stage_: adult, pupa, unhatched or empty pupa
# organism_: name used for parasitoids and for pupae that are not identified to genus
# organism_count_: count of organism_ (pest_incidence is used for Bactrocera and Dacus)

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
		design = "Neem seed oil versus untreated cucumber and squash; fruit samples incubated for fruit flies",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "pest_incidence",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 66,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "fruit-fly-incidnce-n-abundanc-on-cucmber-n-squash.csv"]
	r <- read.csv(f, check.names = FALSE, fileEncoding = "latin1", stringsAsFactors = FALSE)

	mon <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
		jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")
	x <- trimws(r$Date)
	parts <- strsplit(x, "-")
	dday <- sprintf("%02d", as.integer(vapply(parts, `[`, "", 1)))
	dmon <- mon[tolower(vapply(parts, `[`, "", 2))]
	dyy <- paste0("20", vapply(parts, `[`, "", 3))
	date <- paste(dyy, dmon, dday, sep = "-")

	tr <- tolower(trimws(r$Traitement))
	treatment <- ifelse(grepl("^non", tr), "untreated", ifelse(grepl("^trait", tr), "neem seed oil", tr))
	fruit <- trimws(r$Fruit)
	crop <- ifelse(fruit == "Concombre", "cucumber", ifelse(fruit == "Courgette", "squash", tolower(fruit)))

	flies <- cbind(r$BD_male, r$BD_female, r$DS_male, r$DS_female)
	allna <- apply(flies, 1, function(z) all(is.na(z)))
	pest_n <- as.integer(rowSums(flies, na.rm = TRUE))
	pest_n[allna] <- NA_integer_

	d <- data.frame(
		country = "Cameroon",
		location = "IITA",
		latitude = 3.84803,
		longitude = 11.50208,
		geo_from_source = TRUE,
		crop = crop,
		treatment = treatment,
		insecticide_used = treatment == "neem seed oil",
		insecticide_product = ifelse(treatment == "neem seed oil", "neem seed oil", NA_character_),
		block_id = as.character(r$Block),
		rep = as.integer(r$Replication),
		date = date,
		fruit_number_ = r$Nbre_des_fruits,
		fruit_weight_g_ = r$Weight_g,
		incubation_ = r$Inccubation,
		sieving_ = r$Sieving,
		pest_incidence = pest_n,
		stress = "pests",
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		yield_part = "fruit",
		stringsAsFactors = FALSE
	)
	d$record_id <- seq_len(nrow(d))

	stack_count <- function(count, pest, sex_value, stage, organism) {
		keep <- !is.na(count)
		n <- sum(keep)
		data.frame(
			record_id = d$record_id[keep],
			pest_species = if (is.na(pest)) rep(NA_character_, n) else rep(pest, n),
			sex = if (is.na(sex_value)) rep(NA_character_, n) else rep(sex_value, n),
			stage_ = rep(stage, n),
			organism_ = if (is.na(organism)) rep(NA_character_, n) else rep(organism, n),
			organism_count_ = as.integer(count[keep]),
			stringsAsFactors = FALSE
		)
	}

	dd <- rbind(
		stack_count(r$BD_male, "Bactrocera", "male", "adult", NA),
		stack_count(r$BD_female, "Bactrocera", "female", "adult", NA),
		stack_count(r$DS_male, "Dacus", "male", "adult", NA),
		stack_count(r$DS_female, "Dacus", "female", "adult", NA),
		stack_count(r$UN_BD, "Bactrocera", NA, "unhatched", NA),
		stack_count(r$UN_DS, "Dacus", NA, "unhatched", NA),
		stack_count(r$Fo_male, NA, "male", "adult", "Fopius"),
		stack_count(r$Fo_female, NA, "female", "adult", "Fopius"),
		stack_count(r$UN_Fo, NA, NA, "unhatched", "Fopius"),
		stack_count(r$Pupe, NA, NA, "pupa", "fruit fly pupae"),
		stack_count(r$Empty, NA, NA, "empty pupa", "empty pupae")
	)

	carobiner::write_files(path, meta, d, long = dd)
}
