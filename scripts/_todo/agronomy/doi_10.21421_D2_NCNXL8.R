# R script for "carob"
# license: GPL (>=3)

## NOTES
# Peanut genotypes at ICRISAT, Patancheru, India. The deposit has two of the eight environments named in the description: post-rainy 2009-10 irrigated, and post-rainy 2009-10 rainfed.
# Post-rainy is stored as season rabi. planting_date is 2009 from the file names (the season is 2009-10).
# The yield column is pod yield (kg/ha, about 1400 to 6600). yield_part is pod.
# Iron and zinc are about 30 to 90. That matches mg/kg. grain_Fe and grain_Zn are mg/g, so the values are divided by 1000.
# Seed oil is about 44 to 55, which matches percent. oil_content is g/kg, so the values are multiplied by 10. Protein is about 20 to 31 percent and is stored as grain_protein.
# Hundred grain weight (g) times 10 is the thousand-seed weight.
# The rainfed file has a replication number. The irrigated file does not.
# Country is India and the site is Patancheru, both from the description. The files have no coordinates. on_farm is FALSE because the description places the experiments at ICRISAT.

## ISSUES
# The description says eight environments. Only the two 2009-10 workbooks are in this deposit.
# Genotype in the workbooks is a number, not a name. It is stored as character.
# grain_Fe is out of bounds (about 0.03 to 0.07 mg/g). The vocabulary minimum is 1 mg/g. The file values are about 30 to 70, which were treated as mg/kg.
# carob_contributor is unset until someone has read this script.
# write_files: missing yield_moisture, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. out of bounds grain_Fe (0.031, 0.071).

## NOT PROCESSED
# Block. There is no block variable. plot_id is the plot number.

carob_script <- function(path) {

"
The data is on biofortification (delivery of micronutrients via micronutrient-dense crops) and this can be achieved through plant breeding and offers a cost-effective and sustainable approach to fighting micronutrient malnutrition. The present study was conducted to facilitate the initiation of a breeding programme to improve the concentration of iron (Fe) and zinc (Zn) in Peanut (Arachis hypogaea L.) seeds. The experiment was conducted with 64 diverse Peanut genotypes in eight different environments at the International Crops Research Institute for the Semi-Arid Tropics, Patancheru, India to assess the genetic variation for Fe and Zn concentrations in peanut seeds and their heritability and correlations with other traits.
"

	uri <- "doi:10.21421/D2/NCNXL8"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRISAT",
		publication = "doi:10.1017/S0021859614000525",
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety;irrigated",
		response_vars = "yield;grain_Fe;grain_Zn;grain_protein;oil_content",
		carob_effort = 0.4,
		carob_completion = 55,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		notes = "Only two of eight environments are in the deposit. No coordinates in the files. Iron and zinc were treated as mg/kg."
	)

	one <- function(f, trial, irrigated) {
		r <- carobiner::read.excel(f)
		rep <- rep(NA_integer_, nrow(r))
		if ("Replication number" %in% names(r)) {
			rep <- as.integer(r[["Replication number"]])
		}
		data.frame(
			trial_id = trial,
			plot_id = as.character(r[["Plot No"]]),
			country = "India",
			location = "Patancheru",
			crop = "groundnut",
			variety = as.character(r$Genotype),
			treatment = as.character(r$Genotype),
			rep = rep,
			yield = as.numeric(r[["Pod Yield"]]),
			yield_part = "pod",
			grain_Fe = as.numeric(r$Iron) / 1000,
			grain_Zn = as.numeric(r$Zinc) / 1000,
			grain_protein = as.numeric(r[["Seed Protein Content"]]),
			oil_content = as.numeric(r[["Seed Oil Content"]]) * 10,
			shelling_percentage = as.numeric(r[["Shelling outturn"]]),
			seed_weight = as.numeric(r[["Hundred grain weight"]]) * 10,
			planting_date = "2009",
			season = "rabi",
			irrigated = irrigated,
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	d <- rbind(
		one(ff[basename(ff) == "Post rainy 2009-10 Irrigated.xlsx"], "Patancheru_2009_irrigated", TRUE),
		one(ff[basename(ff) == "Post rainy 2009-10 Rainfed.xlsx"], "Patancheru_2009_rainfed", FALSE)
	)
	d$record_id <- as.integer(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
