# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. No plot area or plant spacing is reported, so per-harvest fruit weight (g) cannot be converted to kg/ha yield.
# 2. Neem seed oil concentration/rate, spray schedule, and fertilizer/irrigation management are not in the dataset.
# 3. No site name or plot GPS is in the CSV; coordinates (3.84803 N, 11.50208 E, Yaoundé) come from CKAN coverage metadata (companion dataset doi:10.25502/y7q6-rn92/d names site "IITA").
# 4. Units of fruit length (L1-L5) and diameter (D1-D5) are not stated; D1-D5 values (14-21 for cucumber, 17-25 for squash) nearly equal length, suggesting circumference (cm) rather than diameter.
# 5. Pest species causing fruit punctures (likely tephritid fruit flies, per companion dataset doi:10.25502/y7q6-rn92/d) are not identified in this file.

## NEW
# plant_count_: number of plants in the plot (NbrePlante)
# flower_count_: number of flowers counted per plot on the harvest date (NbreFleur)
# fruit_count_: number of harvested fruits per plot on the harvest date (NbreFruit)
# fruit_damaged_: number of pest-damaged fruits per plot on the harvest date (Fruit_attaq)
# fruit_weight_: total harvested fruit weight per plot on the harvest date, g (pds_ttl_frt_g)
# fruit_length_: individual fruit length, unit not stated (likely cm; L1-L5)
# fruit_diameter_: individual fruit diameter (or circumference), unit not stated (likely cm; D1-D5)
# fruit_punctures_: number of insect punctures on the individual fruit (P1-P5)

## MISSING
# yield, yield_moisture, irrigated, planting_date, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Repeated harvest measurements on cucumber (Concombre) and squash (Courgette) across 14 dates
#   (12 Jul to 22 Aug 2016; 13 harvest dates per crop), 4 replicates, comparing neem seed oil
#   ("traité") against untreated control ("non traité") in Yaoundé, Cameroon.
# Each row of d is one plot on one harvest date; dd (long) holds individual fruit measurements
#   (order 1-5) linked by record_id when fruit exists (order <= NbreFruit) or any measure > 0.
# The dictionary shifts fruit column names by one (L2-L6 for fruits 1-5), while the CSV uses L1-L5.

## NOT PROCESSED
# None (all measurement columns are processed; only row counter SN and codebook file are omitted).


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
		design = "4 replicates; neem seed oil versus untreated control on cucumber and squash; repeated harvests",
		data_type = "experiment",
		treatment_vars = "insecticide_used;insecticide_product",
		response_vars = "fruit_weight_;fruit_damaged_;fruit_punctures_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "yield-data_effects-of-neem-seed-oil-against-pests-and-d.csv"]
	r <- read.csv(f, check.names = FALSE, fileEncoding = "latin1")
	repcol <- grep("tition$", names(r), value = TRUE)

	tr <- tolower(trimws(r$traitement))
	treatment <- ifelse(grepl("^non", tr), "control", ifelse(grepl("^trait", tr), "neem seed oil", tr))
	crop <- ifelse(r$Culture == "Concombre", "cucumber",
		ifelse(r$Culture == "Courgette", "squash", tolower(trimws(r$Culture))))
	hdate <- as.character(as.Date(r$Date, "%d/%m/%Y"))

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = "1",
		plot_id = paste(crop, gsub(" ", "_", treatment), r[[repcol]], sep = "_"),
		country = "Cameroon",
		adm1 = "Centre",
		latitude = 3.84803,
		longitude = 11.50208,
		geo_from_source = TRUE,
		crop = crop,
		treatment = treatment,
		insecticide_used = treatment == "neem seed oil",
		# azadirachtin is the active ingredient of neem seed oil
		insecticide_product = ifelse(treatment == "neem seed oil", "azadirachtin", "none"),
		rep = as.integer(r[[repcol]]),
		date = hdate,
		harvest_date = hdate,
		plant_count_ = as.integer(r$NbrePlante),
		flower_count_ = as.integer(r$NbreFleur),
		fruit_count_ = as.integer(r$NbreFruit),
		fruit_damaged_ = as.integer(r$Fruit_attaq),
		fruit_weight_ = as.numeric(r$pds_ttl_frt_g),
		stress = "pests",
		on_farm = NA,
		is_survey = FALSE,
		yield_part = "fruit"
	)

	pieces <- vector("list", 5)
	for (i in 1:5) {
		L <- as.numeric(r[[paste0("L", i)]])
		D <- as.numeric(r[[paste0("D", i)]])
		P <- as.integer(r[[paste0("P", i)]])
		keep <- (!is.na(r$NbreFruit) & r$NbreFruit >= i) |
			(!is.na(L) & L > 0) | (!is.na(D) & D > 0) | (!is.na(P) & P > 0)
		pieces[[i]] <- data.frame(
			record_id = d$record_id[keep],
			order = as.integer(i),
			fruit_length_ = L[keep],
			fruit_diameter_ = D[keep],
			fruit_punctures_ = P[keep]
		)
	}
	dd <- do.call(rbind, pieces)

	carobiner::write_files(path, meta, d, long = dd)
}
