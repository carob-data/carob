# R script for "carob"
# license: GPL (>=3)

## NOTES
# N2Africa baseline, Ethiopia, interviews mostly 2014 (16 records dated 2013). Field table is f_crop_production_1.csv: one row per field, crop_1 plus optional crop_2, size_ha, varieties, min_fert_type, harvest_amount_kg.
# Household location from a_b_general_1.csv (gps_latitude_dec / gps_longitude_dec present for 277 of 400 households).
# Yield (kg/ha) = harvest_amount_kg / size_ha when size_ha > 0. Values >= 99999 in harvest_amount_kg are treated as a missing-data sentinel.
# size_timad is kept as area_timad_ and is not converted (a timad is not a fixed hectare fraction in this file).
# bean / haricot / haircot mapped to common bean. chat/chatt mapped to khat. crop_2 is joined with "_" when both crops are present; the harvest is one figure for the field, not per component.
# organic_input_applied and inoculant_applied are Y/N only. Other letters in those columns (l, e, c, d, B) are left as NA.
# Explicit "no" / "non" / "0" fertiliser text is fertilizer_type "none". A bare "N" is left as N (it may mean nitrogen or "no").

## ISSUES
# 123 households have no decimal coordinates. adm2 is the action site (Boricha, Pawe, Sinana, Enemay), which are woredas; confirm GADM spelling of Benishangul-Gumuz and the Southern Nations region (source values include Binishangul, SNNPR, SNNP).
# min_fert_type has no amount, so elemental N/P/K are not calculated. Several strings are DAP+urea combinations.
# harvest_amount_local_unit / weight_unit were not used; the kg column is the one mapped. Local units include quintal and donkey cart.
# size_ha reaches 75. Left as recorded.
# organic_input_applied is not a clean Y/N for every row.
# Livestock, labour, nutrition and legume-utilisation modules are not merged.
# No publication DOI in the deposit.
# write_files: missing latitude/longitude on some rows (NA detected), invalid fertiliser or crop tokens, possible out-of-range yields, missing carob_contributor.

## NEW VARIABLES
# area_timad_: field area in local timad, unconverted

## NOT PROCESSED
# a_b_general_2.csv to a_b_general_4.csv: household composition.
# c_livestock_*.csv: livestock counts and feed scores.
# d_labour_1.csv, d_labour_2.csv: labour.
# e_land_use_1.csv, e_land_use_2.csv: total farm area and fallow (not the field harvest).
# f_crop_production_1.csv columns crop_*_rel_share, walking_distance_min, field_managed_by, harvest_sale_decided_by, harvest_amount_local_unit, weight_unit.
# f_crop_production_2.csv: production constraints (free text).
# g_legume_utilisation_*.csv: use of the legume grain and haulms.
# h_labour_saving_tools_*.csv, i_information_access_*.csv, j_nutrition_*.csv.
# variable_definitions.csv: units column empty for harvest and area.


carob_script <- function(path) {

"N2Africa Baseline Survey - Ethiopia, 2014

'N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. 

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.'
"

	uri <- "doi:10.25502/a1qc-3b87/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; ICRAF; WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	map_token <- function(z) {
		z <- gsub("\\s+", " ", trimws(z))
		z <- trimws(gsub("\\([^)]*\\)", "", z))
		if (!nzchar(z) || z %in% c("na", "n/a", "none", "other", "others", "rented in land", "local", "cropping", "fallow", "fallow land", "fallowland", "grazing", "grazing land", "grass land", "forage grass", "tree", "trees")) return(NA_character_)
		if (grepl("^relay\\b|^solo\\b|^cropping$", z)) return(NA_character_)
		if (grepl(",| and |\\+", z)) {
			parts <- trimws(unlist(strsplit(z, ",| and |\\+")))
			parts <- parts[nzchar(parts)]
			mapped <- unique(unlist(lapply(parts, map_token)))
			mapped <- mapped[!is.na(mapped) & nzchar(mapped)]
			if (length(mapped)) return(paste(mapped, collapse="_"))
			return(NA_character_)
		}
		if (grepl("sweet ?potato|sweat potato", z)) return("sweetpotato")
		if (grepl("irish potato|^potato", z)) return("potato")
		if (grepl("soy|soya", z)) return("soybean")
		if (grepl("ground ?nut|grounnut", z)) return("groundnut")
		if (grepl("cow ?pea", z)) return("cowpea")
		if (grepl("pigeon ?pea", z)) return("pigeon pea")
		if (grepl("common bean|^beans?$|haricot|haircot|haicot|harcot|^hb\\b", z)) return("common bean")
		if (grepl("faba|fava", z)) return("faba bean")
		if (grepl("grass ?pea|grasspe", z)) return("grass pea")
		if (grepl("chick ?pea|chicp", z)) return("chickpea")
		if (grepl("lentil|lentel", z)) return("lentil")
		if (grepl("fieldpea|field pea|^peas?$", z)) return("pea")
		if (grepl("finger millet|finger m|dagusa", z)) return("finger millet")
		if (grepl("^millet$", z)) return("pearl millet")
		if (grepl("^cassava$", z)) return("cassava")
		if (grepl("^maize\\b|mazie", z)) return("maize")
		if (grepl("sorg", z)) return("sorghum")
		if (grepl("^tef|teff", z)) return("teff")
		if (grepl("weat|wheat", z)) return("wheat")
		if (grepl("barl|bariey|barely", z)) return("barley")
		if (grepl("enset", z)) return("enset")
		if (grepl("sesame", z)) return("sesame")
		if (grepl("^coff", z)) return("coffee")
		if (grepl("^k?hat$|^chatt?$|^chat$", z)) return("khat")
		if (grepl("^rice$", z)) return("rice")
		if (grepl("^abish$|^absh$", z)) return("fenugreek")
		if (grepl("niger", z)) return("noug")
		if (grepl("linseed|flax", z)) return("flax")
		if (grepl("peper|pepper|green paper|granpaper", z)) return("chili pepper")
		if (grepl("sugar ?cane|sugare|suger", z)) return("sugarcane")
		z
	}

	map_one <- function(x) {
		x <- tolower(trimws(as.character(x)))
		out <- rep(NA_character_, length(x))
		ok <- !is.na(x) & nzchar(x)
		out[ok] <- vapply(x[ok], map_token, character(1))
		out
	}

	clean_fert <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "-88")] <- NA
		out <- x
		for (i in which(!is.na(x))) {
			parts <- unlist(strsplit(x[i], "\\s*(,|;|/|\\+| and |&)+\\s*"))
			parts <- trimws(parts)
			parts <- parts[nzchar(parts)]
			lab <- character()
			for (s in parts) {
				sl <- tolower(s)
				if (sl %in% c("none", "no", "non", "nil", "0", "n/a")) lab <- c(lab, "none")
				else if (grepl("npk", sl)) lab <- c(lab, "NPK")
				else if (grepl("urea|ureia|^uera$|^uerea$|^ura$", sl)) lab <- c(lab, "urea")
				else if (grepl("dap", sl)) lab <- c(lab, "DAP")
				else if (sl %in% c("yes", "kg") || grepl("^[0-9.]+$", sl)) next
				else lab <- c(lab, trimws(s))
			}
			lab <- unique(lab)
			out[i] <- if (length(lab)) paste(lab, collapse=";") else NA_character_
		}
		out
	}

	yn <- function(x) {
		x <- toupper(trimws(as.character(x)))
		ifelse(x == "Y", TRUE, ifelse(x == "N", FALSE, NA))
	}

	f <- ff[basename(ff) == "f_crop_production_1.csv"]
	r <- read.csv(f, stringsAsFactors=FALSE)
	g <- read.csv(ff[basename(ff) == "a_b_general_1.csv"], stringsAsFactors=FALSE)

	c1 <- map_one(r$crop_1)
	c2 <- map_one(r$crop_2)
	crop <- ifelse(!is.na(c1) & !is.na(c2), paste(c1, c2, sep="_"), ifelse(!is.na(c1), c1, c2))

	size <- r$size_ha
	harv <- r$harvest_amount_kg
	harv[!is.na(harv) & harv >= 99999] <- NA
	yield <- ifelse(!is.na(size) & size > 0, harv / size, NA_real_)

	country <- trimws(g$country)
	country[is.na(country) | country == ""] <- "Ethiopia"

	adm <- tolower(trimws(g$sector_state))
	adm1 <- rep(NA_character_, length(adm))
	adm1[grepl("oromia", adm)] <- "Oromia"
	adm1[grepl("amh|amah", adm)] <- "Amhara"
	adm1[grepl("bin|benish|binsh", adm)] <- "Benishangul-Gumuz"
	adm1[grepl("snn|snp", adm)] <- "Southern Nations, Nationalities and Peoples"

	site <- sub("\\s*\\(.*", "", trimws(g$action_site))
	date <- rep(NA_character_, nrow(g))
	okd <- !is.na(g$date_interview_yyyy) & g$date_interview_yyyy >= 2000 &
		!is.na(g$date_interview_mm) & g$date_interview_mm >= 1 & g$date_interview_mm <= 12 &
		!is.na(g$date_interview_dd) & g$date_interview_dd >= 1 & g$date_interview_dd <= 31
	date[okd] <- sprintf("%04d-%02d-%02d", g$date_interview_yyyy[okd], g$date_interview_mm[okd], g$date_interview_dd[okd])

	loc <- data.frame(
		farm_id = g$farm_id,
		country = country,
		adm1 = adm1,
		adm2 = carobiner::fix_name(site, "title"),
		location = carobiner::fix_name(g$village, "title"),
		longitude = g$gps_longitude_dec,
		latitude = g$gps_latitude_dec,
		elevation = g$gps_altitude_dec,
		date = date,
		stringsAsFactors = FALSE
	)
	loc <- loc[!duplicated(loc$farm_id), ]
	loc$longitude[loc$longitude == 0] <- NA
	loc$latitude[loc$latitude == 0] <- NA
	# -9999 and values above the Ethiopian highlands are not elevations
	loc$elevation[!is.na(loc$elevation) & (loc$elevation < -100 | loc$elevation > 4500)] <- NA

	d <- data.frame(
		hhid = as.character(r$farm_id),
		plot_id = as.character(r$field),
		crop = crop,
		variety = trimws(r$varieties),
		field_size = size,
		area_timad_ = r$size_timad,
		yield = yield,
		fertilizer_type = clean_fert(r$min_fert_type),
		OM_used = yn(r$organic_input_applied),
		inoculated = yn(r$inoculant_applied),
		stringsAsFactors = FALSE
	)
	d$variety[d$variety %in% c("", "NA", "N/A", "no")] <- NA
	d$farm_id <- r$farm_id
	d <- merge(d, loc, by="farm_id", all.x=TRUE)
	d$farm_id <- NULL
	d <- d[!is.na(d$crop) & !is.na(d$hhid), ]

	grain <- c("maize", "wheat", "teff", "sorghum", "finger millet", "barley", "rice")
	seed <- c("soybean", "common bean", "cowpea", "groundnut", "chickpea", "faba bean", "grass pea", "pea", "sesame", "lentil", "fenugreek", "flax", "noug", "coffee")
	part1 <- function(z) {
		if (z %in% grain) "grain"
		else if (z %in% seed) "seed"
		else if (z == "enset") "rhizome"
		else if (z == "khat") "leaves"
		else if (z == "sweetpotato") "roots"
		else if (z == "sugarcane") "stems"
		else if (z == "chili pepper") "fruit"
		else NA_character_
	}
	d$yield_part <- vapply(strsplit(d$crop, "_", fixed=TRUE), function(parts) {
		p <- unique(unlist(lapply(parts, part1)))
		p <- p[!is.na(p)]
		if (length(p) == 1) p else NA_character_
	}, character(1))

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- !is.na(d$longitude) & !is.na(d$latitude)
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
