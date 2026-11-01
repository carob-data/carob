# R script for "carob"
# license: GPL (>=3)

## NOTES
# Okra germplasm trial at Foumbot, Buea, Evodoula and Maroua, Cameroon. Season First/Second mapped to terminag first/second.
# Sowing dates are day/month/year. 27/03/2014 and 29/08/2014 fix that order, so 5/4/2014 is 5 April 2014 and 9/9/2014 is 9 September 2014.
# Plot-level means were used for plant height, stem diameter, pod length, pod width and pod weight. The dictionary does not state units. Heights 10-149 and pod lengths 2-23 were treated as cm; pod weights 2-311 were treated as grams. Confirm before publication.
# Block (1-3) is the only replicate column and was stored as both rep and block_id.
# Country is Cameroon from the dictionary coverage field. No coordinates in the files.
# Dataset metadata names CORAF/WECARD with IITA. That organization string is not in the carob organization vocabulary, so data_organization is IITA only.

## ISSUES
# No plot area, so kg/ha yield was not computed. TotWt (0-1553) is kept as total_pod_weight_g_ under the gram assumption above.
# DayCoMat includes 0, which is outside maturity_days (minimum 45). Left as recorded.
# Anthesis and several means are missing on many rows.
# Georeference Foumbot, Buea, Evodoula and Maroua, Cameroon.
# Not stated whether the sites are on-farm. on_farm left NA.
# N/P/K and irrigation are not reported.
# Individual plant and pod columns were not reshaped (see NOT PROCESSED).

## NOT PROCESSED
# PodPerPlt1-14, DamagedPodPerPlt*, PlantHeight1-5, StemDiameter1-5, PodNum1-5, PW1-13, PodL1-12, PodW1-12: individual plant or pod records. Plot means or totals were used instead (AvPlntHt, AvStemDiam, AvPodL, AvPodW, AvPodWt, TotPodHarvest, TotDamagedPod, TotWt, TotPod).
# AvgNoPodsPerPlt: derived from the pod counts.
# ID: row number.

## NEW VARIABLES
# stem_diameter_cm_, pod_length_cm_, pod_width_cm_, pod_weight_g_, total_pod_weight_g_: units inferred from magnitude, not from the dictionary.
# plants_per_plot_: NoPlnts.
# total_pods_: TotPodHarvest.
# damaged_pods_: TotDamagedPod.
# damaged_pod_percent_: %DamagePodPerPlt.


carob_script <- function(path) {

"
Okra adaptation and yield stability of various germplasms under diversed agroecologies

IPM of aphid pests of vegetables
"

	uri <- "doi:10.25502/4faf-mr44/d"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "plant_height;flowering_days;maturity_days",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.7,
		notes = NA
	)

	f <- ff[basename(ff) == "okra_adaptation_and_yield_stability.csv"]
	r <- read.csv(f, na.strings=c("", "NA"), check.names=FALSE)

	sowing <- as.character(as.Date(trimws(r$SowingDate), "%d/%m/%Y"))
	# a few dates are d/m/yyyy with a one-digit month (5/4/2014)
	miss <- is.na(sowing)
	sowing[miss] <- as.character(as.Date(trimws(r$SowingDate[miss]), "%d/%m/%Y"))
	# %d/%m/%Y accepts 5/4/2014 in R. If any remain, try the same format with explicit day.
	still <- is.na(sowing)
	if (any(still)) {
		parts <- strsplit(trimws(r$SowingDate[still]), "/")
		sowing[still] <- vapply(parts, function(z) {
			as.character(as.Date(sprintf("%04d-%02d-%02d", as.integer(z[3]), as.integer(z[2]), as.integer(z[1]))))
		}, "")
	}

	d <- data.frame(
		country = "Cameroon",
		location = r$Location,
		trial_id = paste(r$Location, tolower(r$Season), sep="_"),
		season = c(First="first", Second="second")[r$Season],
		planting_date = sowing,
		rep = as.integer(r$Block),
		block_id = as.character(r$Block),
		variety = r$Accession,
		treatment = r$Accession,
		flowering_days = r$Anthesis,
		maturity_days = r$DayCoMat,
		plant_height = r$AvPlntHt,
		stem_diameter_cm_ = r$AvStemDiam,
		pod_length_cm_ = r$AvPodL,
		pod_width_cm_ = r$AvPodW,
		pod_weight_g_ = r$AvPodWt,
		total_pod_weight_g_ = r$TotWt,
		plants_per_plot_ = r$NoPlnts,
		total_pods_ = r$TotPodHarvest,
		damaged_pods_ = r$TotDamagedPod,
		damaged_pod_percent_ = r[["%DamagePodPerPlt"]],
		crop = "okra",
		yield_part = "fruit",
		on_farm = NA,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
