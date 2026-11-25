# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rothamsted long-term liming experiment, 1962-1996 (Rothamsted Experimental Farm, Harpenden, United Kingdom; silty clay loam soil).
# Plot and sub-plot yields are in crop_data.csv. Factor files give P, K, Mg, Mn, and S rates in kg element/ha, and annual_lime in t CaCO3/ha (multiplied by 1000 to kg/ha for terminag lime).
# Yield in crop_data.csv is t/ha (grains < 10, fresh potatoes up to ~62) -> multiplied by 1000 to kg/ha. Percent dry matter (%_dry_matter) gives yield_moisture = 100 - %_dry_matter (not recorded for potatoes).
# N, basal K, and basal Mg rates are kg element/ha (matching the factor-file units for P, K, Mg, Mn, S).
# Estimated coordinates for Rothamsted Experimental Farm (51.810, -0.376) added with geo_from_source = FALSE and geo_uncertainty = 1000 m.

## ISSUES
# Initial 1962 high-lime plots received 15 t CaCO3/ha (15000 kg/ha), exceeding the terminag valid_max of 10000 kg/ha for lime; kept as recorded.
# Winter lupins sown 1992-10-07 were harvested 1993-10-10 (368 days after sowing).
# Fallow rows have crop = "none" and NA yield; kept as recorded to preserve the rotation sequence.
# Irrigation is not explicitly stated in crop_data.csv (rainfed UK arable experiment; left omitted if unstated).
# write_files: missing carob_contributor, irrigated; out of bounds: lime (0, 15000); datespan > 366 days on winter lupins; NA in yield (fallow/missing plots).

## NOT PROCESSED
# notes_data.csv, crop_types_data.csv, and *_factor_data.csv: metadata/factor description tables used to verify units.
# cumulative_lime and p_rate_cumulative: cumulative totals across years (annual rates are standardized).
# 01-RothLTliming-yield.xlsx: Excel mirror of the standalone CSV files.


carob_script <- function(path) {

"
Rothamsted long-term liming experiment crop yields 1962-1996

The experiment tests the effects of different amounts of lime (and thus soil pH), P and K on the yields of a sequence of arable crops, on a silty clay loam soil at Rothamsted Experimental Farm, from 1962-1996. Mg, Mn and S were tested on split plots at different times. Target soil pH was 4.5, 5.5, 6.5 and 7.5. The dataset contains plot and sub-plot yields for all plots and years, 1962-1996, details of all treatments applied (lime, P, K, Mg, Mn and S), and agronomic details (crop variety, sowing and harvest date, basal fertilizer).
"

	uri <- "doi:10.23637/rcs10-rltlyields-01"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	# Rothamsted download does not include a Dataverse metadata json readable by get_metadata().
	meta <- data.frame(
		uri = uri,
		dataset_id = "doi_10.23637_rcs10-rltlyields-01",
		title = "Rothamsted long-term liming experiment crop yields 1962-1996.",
		authors = "Margaret Glendining; Jonathan Holland; Steve McGrath; Keith Goulding",
		authors_email = NA_character_,
		date_published = "2021",
		publisher = "Electronic Rothamsted Archive, Rothamsted Research",
		version = "1.0",
		license = "CC-BY-4.0",
		description = "The experiment tests the effects of different amounts of lime (and thus soil pH), P and K on the yields of a sequence of arable crops, on a silty clay loam soil at Rothamsted Experimental Farm, from 1962-1996.",
		data_citation = "Margaret Glendining; Jonathan Holland; Steve McGrath; Keith Goulding (2021). Rothamsted long-term liming experiment crop yields 1962-1996. Electronic Rothamsted Archive, Rothamsted Research. Version 1.0. doi:10.23637/rcs10-rltlyields-01",
		carob_group = group,
		data_organization = "ROTH",
		publication = "doi:10.1016/j.eja.2019.02.016",
		project = NA_character_,
		design = NA_character_,
		data_type = "experiment",
		treatment_vars = "lime;P_fertilizer;K_fertilizer;Mg_fertilizer;Mn_fertilizer;S_fertilizer",
		response_vars = "yield",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "crop_data.csv"]
	r <- read.csv(f, check.names=FALSE)
	num <- function(x) {
		x <- trimws(as.character(x))
		x[x == "-"] <- "0"
		x[x %in% c("", "*", "NA")] <- NA_character_
		as.numeric(x)
	}
	add <- function(a, b) ifelse(is.na(a) & is.na(b), NA_real_, ifelse(is.na(a), 0, a) + ifelse(is.na(b), 0, b))

	crop <- c(
		"spring beans" = "faba bean", "spring barley" = "barley", "potatoes" = "potato",
		"fallow" = "none", "spring oats" = "oats", "spring oilseed rape" = "rapeseed",
		"winter triticale" = "triticale", "spring lupins" = "white lupin",
		"spring linseed" = "flax", "winter oilseed rape" = "rapeseed",
		"winter lupins" = "white lupin", "winter wheat" = "wheat"
	)
	cn <- crop[r$crop]
	if (any(is.na(cn))) stop("crop not mapped")
	part <- c(
		"faba bean" = "seed", barley = "grain", potato = "tubers", none = "none",
		oats = "grain", rapeseed = "seed", triticale = "grain", "white lupin" = "seed",
		flax = "seed", wheat = "grain"
	)

	sp <- trimws(as.character(r[["sub-plot"]]))
	pid <- as.character(r$plot)
	use <- !(is.na(sp) | sp %in% c("", "-", "NA"))
	pid[use] <- paste(pid[use], sp[use], sep="-")
	dm <- num(r[["%_dry_matter"]])

	lime_kg <- num(r$annual_lime) * 1000
	ldate <- trimws(as.character(r$lime_date))
	ldate[ldate %in% c("", "*", "-", "NA")] <- NA_character_

	trt <- paste(
		paste0("Lime:", trimws(as.character(r$liming_factor_level))),
		paste0("P:", trimws(as.character(r$p_factor_level))),
		paste0("K:", trimws(as.character(r$k_factor_level))),
		sep=";"
	)

	N_val <- num(r$n_basal_rate)
	P_val <- num(r$p_rate_annual)
	K_val <- add(num(r$k_rate_annual), num(r$k_basal_rate))
	Mg_val <- add(num(r$mg_rate_annual), num(r$mg_basal_rate))
	Mn_val <- num(r$mn_rate_annual)
	S_val <- num(r$s_rate_annual)

	d <- data.frame(
		trial_id = as.character(r$harvest_year),
		LTE_name = "Rothamsted Long-Term Liming Experiment",
		country = "United Kingdom",
		adm1 = "England",
		location = trimws(as.character(r$site)),
		latitude = 51.810,
		longitude = -0.376,
		geo_from_source = FALSE,
		geo_uncertainty = 1000,
		soil_texture = "silty clay loam",
		plot_id = pid,
		rep = as.integer(r$block),
		treatment = trt,
		crop = unname(cn),
		variety = trimws(as.character(r$variety)),
		planting_date = trimws(as.character(r$sow_date)),
		harvest_date = trimws(as.character(r$harvest_date)),
		yield = num(r$yield) * 1000,
		yield_moisture = 100 - dm,
		yield_part = unname(part[cn]),
		lime = lime_kg,
		lime_used = !is.na(lime_kg) & lime_kg > 0,
		lime_date = ldate,
		fertilizer_used = (ifelse(is.na(N_val), 0, N_val) + ifelse(is.na(P_val), 0, P_val) + ifelse(is.na(K_val), 0, K_val) + ifelse(is.na(Mg_val), 0, Mg_val) + ifelse(is.na(Mn_val), 0, Mn_val) + ifelse(is.na(S_val), 0, S_val)) > 0,
		N_fertilizer = N_val,
		P_fertilizer = P_val,
		K_fertilizer = K_val,
		Mg_fertilizer = Mg_val,
		Mn_fertilizer = Mn_val,
		S_fertilizer = S_val,
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$variety[d$variety %in% c("", "*")] <- NA_character_
	d$planting_date[d$planting_date %in% c("", "*")] <- NA_character_
	d$harvest_date[d$harvest_date %in% c("", "*")] <- NA_character_

	carobiner::write_files(path, meta, d)
}
