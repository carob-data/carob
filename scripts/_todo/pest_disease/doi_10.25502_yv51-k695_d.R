# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Crop is not a column in the CSV; description refers to "banana-plantain" and "banana aphid" (Pentalonia nigronervosa, BBTV vector). Crop is set to "plantain" and pest_species to "Pentalonia nigronervosa"; confirm both.
# 2. Spore concentration exponents were corrupted to "?" during CSV export ("3.2x 10?" for MIITAC6.2.2; "x 10?" for all other rows, including Control and INSECTICIDE where concentration_ is set to NA).
# 3. No site name is given; coordinates (4.61255 N, 13.15358 E) come only from CKAN dataset coverage metadata (eastern Cameroon, ~55 km west of Bertoua) and may be coarse.
# 4. The active ingredient and rate of the synthetic "INSECTICIDE" treatment are not reported (mapped to insecticide_product = "unknown").

## NEW
# formulation_: biopesticide formulation carrier ("water" or "oil")
# isolate_: entomopathogenic fungal isolate code (MIITAC6.2.2 = Metarhizium anisopliae; BIITAC6.2.2 and BIITAC10.3.3 = Beauveria bassiana), Control, or INSECTICIDE
# concentration_: fungal spore concentration text as recorded (corrupted exponent in source CSV)
# week_: weeks after treatment (0 = initial pre-treatment count, 1-4 = weekly post-treatment counts)
# insecticide_product: "beauveria bassiana" (values_insecticides.csv currently only lists specific B. bassiana strains)
# pest_species: "Pentalonia nigronervosa" (banana aphid; not yet in values_pest.csv)

## MISSING
# yield, yield_part, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date

## NOTES
# Field evaluation in 2023 (dry and wet seasons, 320 plots = 2 seasons x 8 treatments x 20 repetitions) of Metarhizium anisopliae (MIITAC6.2.2) and Beauveria bassiana (BIITAC6.2.2, BIITAC10.3.3) in water and oil formulations versus an untreated control and synthetic insecticide against banana aphid in Cameroon. Companion dataset for 2024 is doi:10.25502/n9ys-ek14/d.
# Year is evaluation year (date = "2023"), not planting_date. Weekly aphid counts per plant (Initial_count, week1-week4) are stored in long table dd with order (1-5) and week_ (0-4).

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
		design = "Fungal biopesticide isolates in water or oil formulations, control, and insecticide; 20 repetitions in dry and wet seasons",
		data_type = "experiment",
		treatment_vars = "treatment;insecticide_used;insecticide_product",
		response_vars = "pest_incidence",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "field-assessment-2023.csv"]
	r <- read.csv(f, check.names = FALSE)

	form <- tolower(trimws(r$Formulation))
	form <- ifelse(grepl("^water", form), "water", ifelse(grepl("^oil", form), "oil", NA_character_))

	iso <- trimws(r$isolate)
	iso[iso == ""] <- NA_character_

	conc <- trimws(r$Concentration)
	conc[conc == "" | iso %in% c("Control", "INSECTICIDE")] <- NA_character_

	seas <- trimws(r$season)
	season <- ifelse(seas == "Dry", "dry", ifelse(seas == "Raining", "wet", tolower(seas)))

	treatment <- ifelse(is.na(form), iso, paste(form, iso, sep = " | "))

	ins_prod <- ifelse(iso == "Control", "none",
		ifelse(iso == "INSECTICIDE", "unknown",
		ifelse(grepl("^MIITAC", iso), "metarhizium anisopliae",
		ifelse(grepl("^BIITAC", iso), "beauveria bassiana", NA_character_))))

	d <- data.frame(
		country = "Cameroon",
		latitude = 4.61255,
		longitude = 13.15358,
		geo_from_source = TRUE,
		geo_uncertainty = 50000,
		date = as.character(r$Year),
		season = season,
		treatment = treatment,
		formulation_ = form,
		isolate_ = iso,
		concentration_ = conc,
		rep = as.integer(r$Repetition),
		insecticide_used = iso != "Control",
		insecticide_product = ins_prod,
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
			order = as.integer(i),
			week_ = as.integer(i - 1L),
			pest_species = "Pentalonia nigronervosa",
			pest_incidence = as.integer(r[[cols[i]]])
		)
	}
	dd <- do.call(rbind, pieces)

	carobiner::write_files(path, meta, d, long = dd)
}
