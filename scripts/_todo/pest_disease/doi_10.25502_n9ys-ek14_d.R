# R script for "carob"
# license: GPL (>=3)

## ISSUES
# - Coordinates (4.61255, 13.15358) are a single country-level coverage point from dataset metadata, not actual field coordinates; no trial locality or site name is reported.
# - Confirm the crop ("plantain" vs "banana"; the abstract refers to "banana-plantain production") and verify that counted aphids are Pentalonia nigronervosa (banana aphid).
# - Spore concentration exponent was corrupted in the source CSV ("3.2x 10?"); related lab bioassays by the same group (Enoh et al. 2024, PLOS ONE) used 3.2 x 10^6 conidia/ml. In the source CSV this string was also filled down on Control and INSECTICIDE rows (set to NA here for those treatments).
# - Active ingredient and rate of the chemical INSECTICIDE treatment are not named.

## NEW
# formulation_: biopesticide carrier formulation ("water" or "oil")
# isolate_: entomopathogenic fungal isolate code (MIITAC6.2.2, BIITAC6.2.2, BIITAC10.3.3)
# concentration_: fungal spore concentration text as recorded in the source file
# week_: weeks after treatment (0 = initial pre-treatment count, 1-4 = weeks after treatment)
# pest_species: Pentalonia nigronervosa
# insecticide_product: beauveria bassiana

## MISSING
# yield, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date

## NOTES
# Field efficacy trial in Cameroon (2024, dry and wet seasons) evaluating 3 entomopathogenic fungal isolates (Metarhizium anisopliae MIITAC6.2.2; Beauveria bassiana BIITAC6.2.2, BIITAC10.3.3) in water or oil formulations against an untreated control and an unnamed chemical insecticide (20 replications each; 320 plots/plants).
# Repeated weekly aphid counts per plant (initial count through week 4) are stored in the long table linked by record_id and order/week_.
# Year (2024) is the evaluation year (stored as date in the long table), not planting_date.

## NOT PROCESSED
# field-assessment-2024.csv: ID (row number).
# data_dictionary.csv: metadata/codebook only.


carob_script <- function(path) {

"
Performance of Beauveria bassiana and Metarhizium anisopliae based biopesticide formulations under field conditions (2024)

The project aims to develop environmentally friendly management strategies to enable continued banana-plantain production in areas affected by banana bunchy top disease, by targeting the banana aphid which is the primary vector.
"

	uri <- "doi:10.25502/n9ys-ek14/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Plant Health Initiative",
		design = "3 fungal isolates x 2 formulations (water, oil) plus untreated control and chemical insecticide; 20 replications; dry and wet seasons",
		data_type = "experiment",
		treatment_vars = "insecticide_used;insecticide_product",
		response_vars = "pest_incidence",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "field-assessment-2024.csv"]
	r <- read.csv(f, check.names = FALSE, na.strings = "")

	form_raw <- trimws(r$Formulation)
	form <- ifelse(form_raw == "Water formulations", "water",
		ifelse(form_raw == "Oil formulation", "oil", form_raw))
	iso_raw <- trimws(r$isolate)
	is_fungal <- !(iso_raw %in% c("Control", "INSECTICIDE"))

	prod <- ifelse(iso_raw == "Control", "none",
		ifelse(iso_raw == "INSECTICIDE", "unknown",
		ifelse(iso_raw == "MIITAC6.2.2", "metarhizium anisopliae", "beauveria bassiana")))

	seas <- trimws(r$season)
	season <- ifelse(seas == "Dry", "dry", ifelse(seas == "Raining", "wet", tolower(seas)))
	treatment <- ifelse(is.na(form), iso_raw, paste(iso_raw, form, sep = " | "))

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		country = "Cameroon",
		# Single country-level coverage coordinate from dataset metadata (no site name reported)
		latitude = 4.61255,
		longitude = 13.15358,
		geo_from_source = TRUE,
		date = as.character(r$Year),
		season = season,
		trial_id = paste(r$Year, season, sep = "_"),
		rep = as.integer(r$Repetition),
		crop = "plantain",
		yield_part = "fruit",
		treatment = treatment,
		insecticide_used = iso_raw != "Control",
		insecticide_product = prod,
		isolate_ = ifelse(is_fungal, iso_raw, NA_character_),
		formulation_ = form,
		# Source filled "3.2x 10?" down onto Control and INSECTICIDE rows; keep only for fungal treatments
		concentration_ = ifelse(is_fungal, trimws(r$Concentration), NA_character_),
		stress = "pests",
		on_farm = NA,
		is_survey = FALSE
	)

	cols <- c("Initial_count", "week1", "week2", "week3", "week4")
	pieces <- lapply(seq_along(cols), function(i) {
		data.frame(
			record_id = d$record_id,
			date = as.character(r$Year),
			order = as.integer(i),
			week_ = as.integer(i - 1L),
			pest_species = "Pentalonia nigronervosa",
			pest_incidence = as.integer(r[[cols[i]]])
		)
	})
	dd <- do.call(rbind, pieces)

	carobiner::write_files(path, meta, d, long = dd)
}
