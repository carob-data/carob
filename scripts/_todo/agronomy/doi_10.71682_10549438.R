# R script for "carob"
# license: GPL (>=3)

## NOTES
# Seven-year Conservation Agriculture experiment (2017-2023) at the San Miguel Tlacamama Research Platform, Oaxaca, Mexico. Factorial: crop diversification (M = maize monoculture vs MB = maize-cowpea intercropping), stubble management (R = removed vs K = kept), and fertilization (OF = organic Supermagro biofertilizer vs OMF = Supermagro + NPK 51-12-15 kg/ha).
# Coordinates are parsed from DMS (unsigned longitude is west -> negated for Oaxaca, Mexico).
# Yield_moist is grain/seed yield adjusted to 14% moisture for maize and 12% for cowpea (kg/ha); Yield_dry is dry-matter yield (kg/ha -> yield_dry).
# "Bean" rows in the raw sheet represent cowpea (Variety = Cowpea; Legend = Maize-Cowpea Bean). Intercropped maize and cowpea rows share plot_id/year and carry intercropped = TRUE with intercrops = "cowpea" / "maize".
# Following the Legend treatment definitions, OF treatments receive organic Supermagro only (fertilizer_used = FALSE, N/P/K_fertilizer = 0) while OMF treatments receive Supermagro + mineral NPK (fertilizer_used = TRUE, N = 51, P = 12, K = 15 kg/ha). The raw sheet repeats 51/12/15 in Fert_N/P/K on OF rows too (see ISSUES).
# Zero tillage is land_prep_method = "none"; Temporal water regime is irrigated = FALSE. Plant heights < 10 are in meters and converted to cm (* 100); raw typo "3.09.8" is corrected to 309.8 cm.

## ISSUES
# Raw sheet columns Fert_N, Fert_P, Fert_K contain 51, 12, 15 on every row including organic-only (OF) treatments, contradicting the Legend ("Supermagro" vs "Supermagro + NPK 51-12-15"). Mineral NPK is set to 0 on OF treatments per Legend and abstract.
# Soil data (24 plots) and Grain Quality Data (12 maize samples) sheets have no sampling year column, so they cannot be unambiguously assigned to a specific year without risk of duplicating across all 7 seasons; documented in NOT PROCESSED.
# write_files: missing carob_contributor; some NA yields/dates/densities on failed or unrecorded rows.

## NOT PROCESSED
# Soil data sheet (24 plots: SP, FC, PWP, HC, BD, SOM, P, K, Ca, Mg, Al, S, NO3, pH, CEC, Na, Fe, Zn, Mn, Cu): sampling year not stated in sheet.
# Grain Quality Data sheet (12 rows: protein, tryptophan, lysine, Fe, Zn, Ca, K, P, ash, starch): harvest year not stated in sheet.
# Yearly operations sheets (2017-2023): qualitative activity logs.
# Cost, Price, Gross, Profit (currency unit in Legend is "$/ha" without MXN vs USD confirmation).
# Flower_days (defined in Legend as days between flowering and maturity, not days to flowering).
# Count (~120, inconsistent with plants/m2), Row_dist and Intrarow (both 1 without units), Precip and Irrig (empty).


carob_script <- function(path) {

"
Results of a 7-Year Conservation Agriculture Experiment at the San Miguel Tlacamama Research Platform, Oaxaca, Mexico (2017–2023)

Rainfed maize production in southern Mexico is constrained by climate variability and soil fertility limitations. This dataset contains seven years of data from a long-term field experiment conducted at the San Miguel Tlacamama Research Platform, Oaxaca, Mexico, evaluating the effects of crop diversification (maize monoculture vs. maize–cowpea intercropping), stubble management (remove vs. keep), and fertilization (organic vs. organic–mineral fertilization).
"

	uri <- "doi:10.71682/10549438"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "intercropped;residue_prevcrop_used;fertilizer_used;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield;yield_dry;plant_height;seed_weight",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 80,
		carob_effort = 0
	)

	dms <- function(x) {
		x <- as.character(x)
		x <- gsub("[\u00b0\u00ba]", " ", x)
		x <- gsub("[\u2019'`]", " ", x)
		x <- gsub("[\u201d\"\u2033]", " ", x)
		sp <- strsplit(trimws(x), "[^0-9.]+")
		out <- rep(NA_real_, length(x))
		for (i in seq_along(sp)) {
			p <- sp[[i]]
			p <- p[nzchar(p)]
			if (!length(p)) next
			out[i] <- as.numeric(p[1])
			if (length(p) >= 2) out[i] <- out[i] + as.numeric(p[2]) / 60
			if (length(p) >= 3) out[i] <- out[i] + as.numeric(p[3]) / 3600
		}
		out
	}
	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & nchar(x) >= 10 & substr(x, 5, 5) == "-"
		out[iso] <- substr(x[iso], 1, 10)
		out
	}
	plain_num <- function(x) {
		if (is.numeric(x)) return(as.numeric(x))
		z <- trimws(as.character(x))
		z[z %in% c("", "NA", "N/A", "n/a", ".", "-")] <- NA_character_
		as.numeric(z)
	}
	height_cm <- function(x) {
		z <- trimws(as.character(x))
		z[z %in% c("", "NA", "N/A", "n/a", ".", "-")] <- NA_character_
		two <- !is.na(z) & grepl("\\..*\\.", z)
		z[two] <- sub("\\.", "", z[two])
		n <- as.numeric(z)
		n[!is.na(n) & n < 10] <- n[!is.na(n) & n < 10] * 100
		n
	}

	f <- ff[basename(ff) == "DAT-SMTlacamamaExperiments.xlsx"]
	r <- carobiner::read.excel(f, sheet="San Miguel Tlacamama_OAX")
	crop <- rep(NA_character_, nrow(r))
	crop[r$Crop == "Maize"] <- "maize"
	crop[r$Crop == "Bean"] <- "cowpea"
	part <- ifelse(crop == "maize", "grain", "seed")
	moist <- ifelse(crop == "maize", 14, 12)
	trt <- trimws(as.character(r$Name_tr))
	is_inter <- grepl("^MB", trt)
	inter_partner <- ifelse(!is_inter, NA_character_, ifelse(crop == "maize", "cowpea", "maize"))
	# Per Legend: OF = organic Supermagro only; OMF = Supermagro + NPK (51-12-15)
	has_npk <- grepl("OMF", trt)

	lon <- -abs(dms(r$Longitud))

	d <- data.frame(
		country = "Mexico",
		adm1 = "Oaxaca",
		location = "San Miguel Tlacamama",
		LTE_name = "San Miguel Tlacamama Research Platform",
		trial_id = paste("San Miguel Tlacamama", r$Year),
		plot_id = as.character(r$Parcela),
		rep = as.integer(r$Num_Rep),
		treatment = trt,
		crop = crop,
		intercropped = is_inter,
		intercrops = inter_partner,
		variety = trimws(as.character(r$Variety)),
		yield = plain_num(r$Yield_moist),
		yield_dry = plain_num(r$Yield_dry),
		yield_moisture = moist,
		yield_part = part,
		seed_weight = plain_num(r$Thousand),
		planting_date = as_date(r$Sowing_date),
		emergence_date = as_date(r$Emergence_date),
		harvest_date = as_date(r$Harvest_date),
		maturity_days = as.numeric(as.Date(as_date(r$Mat_days)) - as.Date(as_date(r$Sowing_date))),
		plant_height = height_cm(r$Height),
		plant_density = plain_num(r$Plants_m2) * 10000,
		seed_density = plain_num(r$Seed_dens),
		plot_area = plain_num(r$Area_plot),
		elevation = plain_num(r$Altitud),
		latitude = dms(r$Latitud),
		longitude = lon,
		OM_used = TRUE,
		fertilizer_used = has_npk,
		N_fertilizer = ifelse(has_npk, plain_num(r$Fert_N), 0),
		P_fertilizer = ifelse(has_npk, plain_num(r$Fert_P), 0),
		K_fertilizer = ifelse(has_npk, plain_num(r$Fert_K), 0),
		land_prep_method = "none",
		residue_prevcrop_used = trimws(as.character(r$Residue)) == "Kept",
		previous_crop_residue_perc = plain_num(r$Res_perc),
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
