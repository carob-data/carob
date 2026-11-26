# R script for "carob"
# license: GPL (>=3)

## NOTES
# Groundnut (peanut) biofortification trial of 64 genotypes at ICRISAT Patancheru, India. Two of the eight environments described in the abstract are deposited: post-rainy (rabi) 2009-10 irrigated and rainfed.
# Pod yield is in kg/ha (1848 to 6640 kg/ha); yield_part = "pod".
# Seed Fe (30-60 ppm = mg/kg) and Zn (47-91 ppm = mg/kg) are divided by 1000 to mg/g for terminag grain_Fe and grain_Zn.
# Seed oil (%) is multiplied by 10 to g/kg for terminag oil_content; seed protein (%) is mapped to grain_protein; 100-grain weight (g) * 10 = seed_weight (1000-seed weight, g).
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# Only two of the eight environments described in the abstract are present in this Dataverse deposit.
# Genotypes are numeric entry codes in the workbooks (stored as character).
# Replication number is present in the rainfed workbook only; both workbooks have Block (mapped to block_id).
# No fertilizer rates, yield moisture, or calendar harvest date in the files.
# write_files: missing carob_contributor, yield_moisture, K/N/P_fertilizer, harvest_date.

## NOT PROCESSED
# None.


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
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;irrigated",
		response_vars = "yield;grain_Fe;grain_Zn;grain_protein;oil_content;shelling_percentage;seed_weight",
		carob_effort = 0,
		carob_completion = 90,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		notes = "Only two of eight environments are in the deposit. Fe and Zn converted from mg/kg to mg/g."
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
			block_id = as.character(r$Block),
			country = "India",
			adm1 = "Telangana",
			adm2 = "Sangareddy",
			location = "Patancheru",
			latitude = 17.511,
			longitude = 78.275,
			geo_from_source = FALSE,
			geo_uncertainty = 2000,
			crop = "groundnut",
			variety = as.character(r$Genotype),
			treatment = as.character(r$Genotype),
			rep = rep,
			yield = as.numeric(r[["Pod Yield"]]),
			yield_part = "pod",
			# Iron and Zinc in mg/kg (ppm) -> mg/g
			grain_Fe = as.numeric(r$Iron) / 1000,
			grain_Zn = as.numeric(r$Zinc) / 1000,
			grain_protein = as.numeric(r[["Seed Protein Content"]]),
			# Seed oil in % -> g/kg
			oil_content = as.numeric(r[["Seed Oil Content"]]) * 10,
			shelling_percentage = as.numeric(r[["Shelling outturn"]]),
			seed_weight = as.numeric(r[["Hundred grain weight"]]) * 10,
			planting_date = "2009",
			season = "rabi",
			irrigated = irrigated,
			on_farm = FALSE,
			is_survey = FALSE
		)
	}

	d <- rbind(
		one(ff[basename(ff) == "Post rainy 2009-10 Irrigated.xlsx"], "Patancheru_2009_irrigated", TRUE),
		one(ff[basename(ff) == "Post rainy 2009-10 Rainfed.xlsx"], "Patancheru_2009_rainfed", FALSE)
	)
	d$record_id <- seq_len(nrow(d))

	carobiner::write_files(path, meta, d)
}
