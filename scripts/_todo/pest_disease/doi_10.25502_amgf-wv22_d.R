# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cucumber (Concombre) and squash (Courgette) at one site, harvested on 13 dates from
#   12 July to 22 August 2016 (dates are day/month/year: 22/08/2016). Four replicates.
#   "traité" / "non traité" is neem seed oil versus untreated, from the dataset title.
#   The file does not name the site. Coverage coordinates are 3.84803 N, 11.50208 E
#   (coverage_x / coverage_y, same order as the other IITA records here).
# Each row is one plot on one harvest date (not a season total). Fruit weight is grams
#   for that harvest (pds_ttl_frt_g). Plot area is not given, so there is no kg/ha yield.
# dd holds per-fruit length, diameter and puncture counts for fruits 1-5 when that fruit
#   exists (index <= NbreFruit) or a measurement is > 0. Units of length and diameter
#   are not stated.
# Read as latin1 so the treatment and replicate column names keep their accents.

## ISSUES
# 1. No plot area or plant spacing, so fruit weight stays in grams per harvest.
# 2. Neem rate, active ingredient, spray dates and fertilizer are not in the file.
# 3. Site name is absent. doi:10.25502/y7q6-rn92/d (same project and coverage point) says IITA.
# 4. Confirm length and diameter units (both often 14-21; cm is plausible for length,
#    large for cucumber diameter).
# 5. write_files: missing yield, yield_moisture, N/P/K, planting_date. harvest_date is the
#    row date because each row is a harvest. Pest names are not attached to the punctures.

## NOT PROCESSED
# SN: row number.
# Dictionary columns L6, D6, P6: not present in the data file (only fruits 1-5).

## NEW VARIABLES
# plants_: number of plants (NbrePlante)
# flowers_: number of flowers (NbreFleur)
# fruit_number_: number of fruits (NbreFruit)
# fruit_damaged_: number of damaged fruits (Fruit_attaq)
# fruit_weight_g_: total fruit weight of the harvest, grams
# fruit_index_: fruit number 1-5 on the plot
# fruit_length_: L, unit not stated
# fruit_diameter_: D, unit not stated
# fruit_punctures_: P, puncture count on that fruit

carob_script <- function(path) {

"
Effect of neem seed oil on insect pests of cucumber and squash and yield

Preliminary testing of plant extracts as insecticides in organic cultivation
"

	uri <- "doi:10.25502/amgf-wv22/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Enhancing horticultural productivity, incomes and livelihoods through integrated management of aphid pests on vegetables in sub-Saharan Africa",
		design = "Neem seed oil versus untreated cucumber and squash; 4 replicates; repeated harvests",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "fruit_weight_g_;fruit_damaged_",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 62,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "yield-data_effects-of-neem-seed-oil-against-pests-and-d.csv"]
	r <- read.csv(f, check.names = FALSE, fileEncoding = "latin1")
	repcol <- grep("tition$", names(r), value = TRUE)

	tr <- tolower(trimws(r$traitement))
	treatment <- ifelse(grepl("^non", tr), "untreated", ifelse(grepl("^trait", tr), "neem seed oil", tr))
	crop <- ifelse(r$Culture == "Concombre", "cucumber", ifelse(r$Culture == "Courgette", "squash", tolower(trimws(r$Culture))))

	d <- data.frame(
		country = "Cameroon",
		latitude = 3.84803,
		longitude = 11.50208,
		geo_from_source = TRUE,
		crop = crop,
		treatment = treatment,
		insecticide_used = treatment == "neem seed oil",
		insecticide_product = ifelse(treatment == "neem seed oil", "neem seed oil", NA_character_),
		rep = as.integer(r[[repcol]]),
		harvest_date = as.character(as.Date(r$Date, "%d/%m/%Y")),
		date = as.character(as.Date(r$Date, "%d/%m/%Y")),
		plants_ = r$NbrePlante,
		flowers_ = r$NbreFleur,
		fruit_number_ = r$NbreFruit,
		fruit_damaged_ = r$Fruit_attaq,
		fruit_weight_g_ = r$pds_ttl_frt_g,
		stress = "pests",
		trial_id = "1",
		on_farm = NA,
		is_survey = FALSE,
		yield_part = "fruit"
	)
	d$record_id <- seq_len(nrow(d))

	pieces <- vector("list", 5)
	for (i in 1:5) {
		L <- r[[paste0("L", i)]]
		D <- r[[paste0("D", i)]]
		P <- r[[paste0("P", i)]]
		keep <- (!is.na(r$NbreFruit) & r$NbreFruit >= i) | (!is.na(L) & L > 0) | (!is.na(D) & D > 0) | (!is.na(P) & P > 0)
		pieces[[i]] <- data.frame(
			record_id = d$record_id[keep],
			fruit_index_ = i,
			fruit_length_ = L[keep],
			fruit_diameter_ = D[keep],
			fruit_punctures_ = P[keep]
		)
	}
	dd <- do.call(rbind, pieces)

	carobiner::write_files(path, meta, d, long = dd)
}
