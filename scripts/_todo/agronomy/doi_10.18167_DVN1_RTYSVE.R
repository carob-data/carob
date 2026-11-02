# R script for "carob"
# license: GPL (>=3)

## NOTES
# Country is France from geographicCoverage. The data file Site column is SOERE and does not name Réunion or Guadeloupe. geographicCoverage state is La Réunion and city is La Mare. The description says the trial was on Reunion Island.
# Coordinates are the single-point geographicBoundingBox (55.5297, -20.9031), not a geocode.
# Variety R579 is named in the dataset description.
# F is N fertilization and NF is no N fertilization (root readme). The fertilized rate is not in the files, so N_fertilizer is 0 only for NF.
# Date_harvest in the root file is the start of that cycle (it equals Date_ini for P0, and the previous cycle's last date for R1 and R2). Stalk yield is the aboveground row on the last date of P0 (DAP 350, 2022-09-22) and of R1 (DAP 721, 2023-09-28). R2 has only an early regrowth date and is not treated as a harvest.
# Stalk and aboveground masses are dry Mg/ha. yield_moisture is 0. Fine-root g/m2 is converted to kg/ha in the long table.
# Publication link in the Dataverse record is a HAL id, not a DOI.

## ISSUES
# N rate for treatment F is not in the files, so N_fertilizer is NA on those rows. P and K are not reported. Irrigation is not stated.
# The published point falls in Réunion while geographicCoverage country is France. site is stored as Soere; the file value is SOERE.
# R1 harvest is more than 366 days after the previous harvest. Root length and diameter traits are not mapped.
# write_files: missing irrigated, K_fertilizer, P_fertilizer; out of bounds dmy_stems (9462.761, 35675.757); datespan 6 harvest_date more than 366 days after planting_date; coordinates in wrong country France/Réunion; unknown variable root_necromass_; unknown variables distance_from_row_, root_alive_; out of bounds dmy_roots (0, 5157.878).

## NEW VARIABLES
# root_alive_: living root dry mass in the soil layer (kg/ha), from aRB_gm2
# root_necromass_: root necromass in the soil layer (kg/ha), from RN_gm2
# distance_from_row_: horizontal distance of the core from the cane row (cm)

## NOT PROCESSED
# Root length, diameter, volume, length density, specific root length and specific root area (sRL, fRL, aRL, sRD, fRD, aRD, sRV, fRV, aRV, sRLD, fRLD, aRLD, sSRL, fSRL, aSRL, sSRA, fSRA, aSRA)
# Sample-scale grams (sRB_sample, fRB_sample, aRB_sample, RN_sample) because the g/m2 columns are the areal values

carob_script <- function(path) {

"
Dataset of sugarcane root production and turnover: influence of crop harvest and nitrogen fertilization

Backgrounds and Aims Root production and turnover are key drivers of belowground carbon inputs, but remain poorly quantified in tropical cropping systems such as sugarcane. This study aimed to provide the first quantification of sugarcane root production and turnover across a full crop cycle, including plant crop and first ratoon, and to examine how nitrogen fertilization modulates biomass allocation, fine root traits, and turnover.
Methods A two-year field experiment (plant and ratoon crop) was conducted on Reunion Island under contrasting nitrogen fertilization with the R579 variety. Sequential soil coring was used to monitor living root biomass, necromass, and fine root traits down to 50 cm depth every two months.
"

	uri <- "doi:10.18167/DVN1/RTYSVE"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield;dmy_total;dmy_roots",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 45,
		carob_effort = 0.7
	)

	fa <- ff[basename(ff) == "DATA_Aboveground_biomass.xlsx"]
	a <- carobiner::read.excel(fa, sheet="DATA")
	fr <- ff[basename(ff) == "DATA_Root_biomass_traits.xlsx"]
	r <- carobiner::read.excel(fr, sheet="DATA")

	# Last aboveground date of P0 and of R1 lines up with the next cycle's Date_harvest, i.e. the harvest.
	harvest_dap <- c(P0=350, R1=721)
	a$is_harvest <- a$DAP == harvest_dap[as.character(a$Cycle)]
	h <- a[a$is_harvest %in% TRUE, ]

	d <- data.frame(
		country = "France",
		location = "La Mare",
		site = "Soere",
		longitude = 55.5297,
		latitude = -20.9031,
		geo_from_source = TRUE,
		crop = "sugarcane",
		variety = "R579",
		treatment = ifelse(h$Treatment == "NF", "no N fertilizer", "N fertilized"),
		N_fertilizer = ifelse(h$Treatment == "NF", 0, NA_real_),
		plot_id = h$Plot,
		block_id = h$Block,
		trial_id = paste("SOERE", h$Cycle, sep="_"),
		planting_date = ifelse(h$Cycle == "P0", "2021-10-07", ifelse(h$Cycle == "R1", "2022-09-22", NA_character_)),
		harvest_date = ifelse(h$Cycle == "P0", "2022-09-22", ifelse(h$Cycle == "R1", "2023-09-28", NA_character_)),
		yield = as.numeric(h$SDM) * 1000,
		yield_part = "stems",
		yield_moisture = 0,
		dmy_stems = as.numeric(h$SDM) * 1000,
		dmy_total = as.numeric(h$ADM) * 1000,
		on_farm = FALSE,
		is_survey = FALSE
	)
	# longitude/latitude: geographicBoundingBox west=east and north=south.
	# variety from the dataset description. country from geographicCoverage.

	d$record_id <- as.integer(seq_len(nrow(d)))

	layer <- gsub("\\[|\\]", "", r$Soil_layer)
	parts <- do.call(rbind, strsplit(layer, "-"))
	long <- data.frame(
		trial_id = paste("SOERE", r$Cycle, sep="_"),
		plot_id = r$Plot,
		DAP = as.integer(r$DAP),
		date = as.character(as.Date(r$Date)),
		depth_top = as.numeric(parts[, 1]),
		depth_bottom = as.numeric(parts[, 2]),
		distance_from_row_ = as.numeric(r$Position),
		dmy_roots = as.numeric(r$fRB_gm2) * 10,
		root_alive_ = as.numeric(r$aRB_gm2) * 10,
		root_necromass_ = as.numeric(r$RN_gm2) * 10
	)

	carobiner::write_files(path, meta, d, long=long)
}
