# R script for "carob"
# license: GPL (>=3)

## NOTES
# Same layout as doi:10.25502/n9ys-ek14/d, for 2023. 320 plots, seasons Raining and Dry.
# Aphid counts per plant at the start (week_ 0) and for four weeks, stacked in dd.
# Concentration is "3.2x 10?" or "x 10?"; the exponent is not readable in the csv.
# crop = plantain and pest_species = "banana aphid" follow the description (banana-plantain,
#   banana aphid as the bunchy-top vector). Neither is a column in the file.
# "Raining" stored as wet. Coordinates are the dataset coverage point 4.61255 N, 13.15358 E.
# Blank formulation (Control, INSECTICIDE) is kept; treatment is then the isolate name.
# planting_date is the year only.

## ISSUES
# 1. Confirm crop (banana versus plantain) and the aphid species that was counted.
# 2. Spore concentration exponent is missing. One level is only "x 10?".
# 3. No site name, yield, fertilizer, or spray rate for INSECTICIDE.
# 4. write_files: missing yield, yield_part, yield_moisture, N/P/K, harvest_date.
#    formulation_, isolate_, concentration_ and week_ are not in terminag.

## NOT PROCESSED
# ID: row number.
# data_dictionary.csv code-list rows (Formulated, Unformulated, isolate names): already in the data columns.

## NEW VARIABLES
# formulation_: water or oil formulation
# isolate_: fungal isolate, Control, or INSECTICIDE
# concentration_: spore concentration text as in the file
# week_: 0 = initial count, 1-4 = weeks after treatment

carob_script <- function(path) {

"
Performance of Beauveria bassiana and Metarhizium anisopliae based biopesticide formulations under field conditions (2023)

The project aims to develop environmentally friendly management strategies to enable continued banana-plantain production in areas affected by banana bunchy top disease, by targeting the banana aphid which is the primary vector.
"

	uri <- "doi:10.25502/yv51-k695/d"
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
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "field-assessment-2023.csv"]
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
