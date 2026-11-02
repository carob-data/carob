# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field counts of aphids on 320 plots in 2024 (Dry and Raining seasons). Treatments are
#   a fungal isolate (Beauveria or Metarhizium codes BIITAC/MIITAC), a water or oil
#   formulation, plus Control and INSECTICIDE. Concentration text is kept as read;
#   the exponent did not survive in the csv ("3.2x 10?").
# Initial_count and week1-week4 are aphid counts per plant (dictionary). They are stacked
#   in dd with week_ 0 (initial) to 4. One wide row per plot.
# The crop is not a column. The description is banana and plantain, targeting the banana
#   aphid. crop is set to plantain and pest_species to "banana aphid"; both need confirmation.
# Season "Raining" is stored as wet, "Dry" as dry.
# Coordinates are the dataset coverage point coverage_x 4.61255, coverage_y 13.15358
#   (latitude, longitude; same order as the other IITA records). No place name.
# Rows with a blank formulation are kept; treatment is then the isolate alone
#   (Control and INSECTICIDE).

## ISSUES
# 1. Confirm the crop (banana versus plantain) and that the insects counted were the banana aphid.
# 2. Recover the spore concentration (the power of 10 was lost in the file).
# 3. No site name, yield, planting date, or fertilizer. The coverage point needs a place name.
# 4. The insecticide used in the INSECTICIDE treatment is not named.
# 5. write_files: missing yield, yield_part, yield_moisture, N/P/K, planting_date, harvest_date.
#    formulation_, isolate_ and concentration_ are not in terminag yet.

## NOT PROCESSED
# ID: row number.
# data_dictionary.csv rows Formulated, Unformulated, MIITAC6.2.2, BIITAC6.2.2, BIITAC10.3.3:
#   code lists, already carried in Formulation and isolate.

## NEW VARIABLES
# formulation_: water or oil formulation
# isolate_: fungal isolate, Control, or INSECTICIDE
# concentration_: spore concentration text as in the file
# week_: 0 = initial count, 1-4 = weeks after treatment

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
		design = "Fungal isolates in water or oil formulations, a control and an insecticide; up to 20 repetitions; dry and wet seasons",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "pest_incidence",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "field-assessment-2024.csv"]
	r <- read.csv(f, check.names = FALSE)

	form <- trimws(r$Formulation)
	form[form == ""] <- NA_character_
	iso <- trimws(r$isolate)
	iso[iso == ""] <- NA_character_
	conc <- trimws(r$Concentration)
	conc[conc == ""] <- NA_character_
	seas <- trimws(r$season)
	season <- ifelse(seas == "Dry", "dry", ifelse(seas == "Raining", "wet", seas))
	treatment <- ifelse(is.na(form), iso, paste(form, iso, sep = " | "))

	d <- data.frame(
		country = "Cameroon",
		latitude = 4.61255,
		longitude = 13.15358,
		geo_from_source = TRUE,
		planting_date = as.character(r$Year),
		season = season,
		treatment = treatment,
		formulation_ = form,
		isolate_ = iso,
		concentration_ = conc,
		rep = as.integer(r$Repetition),
		insecticide_used = !is.na(iso) & iso == "INSECTICIDE",
		crop = "plantain",
		stress = "pests",
		trial_id = paste(r$Year, season, sep = "_"),
		on_farm = NA,
		is_survey = FALSE
	)
	d$record_id <- seq_len(nrow(d))

	cols <- c("Initial_count", "week1", "week2", "week3", "week4")
	pieces <- vector("list", length(cols))
	for (i in seq_along(cols)) {
		pieces[[i]] <- data.frame(
			record_id = d$record_id,
			date = as.character(r$Year),
			week_ = i - 1L,
			pest_species = "banana aphid",
			pest_incidence = as.integer(r[[cols[i]]])
		)
	}
	dd <- do.call(rbind, pieces)

	carobiner::write_files(path, meta, d, long = dd)
}
