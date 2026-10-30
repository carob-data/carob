# R script for "carob"
# license: GPL (>=3)

## NOTES
# N2Africa 2013 impact questionnaire, Malawi. Plot table is c_land_holding_management_2.csv (one row per field).
# Location from a_general_1.csv. country is Malawi. action_site is the district (Dedza, Salima, Dowa, Ntcheu), all in the Central Region, so adm1 is Central Region and adm2 is the action site. sector_state is the partner organisation (DAES, World Vision, Concern Universal), not an admin unit.
# gps_dec_latitude / gps_dec_longitude are empty. gps_latitude and gps_longitude are decimal degrees, but latitude is stored unsigned (about 13-15) with longitude about 33-35. The minus sign is restored so latitudes are negative. That correction is still the source coordinate.
# harvest_amount is kilograms where weight_unit is kg/kgs. Yield (kg/ha) = harvest / size_ha when size_ha > 0.
# bean/beans mapped to common bean. Intercrop strings such as "Maize (70%), Beans (30%)" are split and joined with "_".
# min_fert_amount kept as fertilizer_amount_ (dictionary has no unit). Sympal, Chitowe and 23:21:0 are left as recorded. Elemental N/P/K are not calculated.

## ISSUES
# Confirm the latitude sign correction. Values are coarse (often one decimal place).
# Fertilizer product names that are not in the vocabulary (Sympal, Chitowe, 23:21:0+4S) need a terminag decision. Grades were collapsed only for strings that contain "NPK".
# min_fert_amount unit is undocumented.
# crops_grown still has free-text mixtures; unrecognised tokens are left as typed.
# d_crop_production_use.csv is a second farm-level production table and was not merged.
# No publication DOI in the deposit.
# write_files: expect invalid fertilizer terms, possible out-of-range yields, and missing carob_contributor.

## NEW VARIABLES
# fertilizer_amount_: raw min_fert_amount (unit not in the dictionary)

## NOT PROCESSED
# a_general_2.csv to a_general_5.csv: household composition, labour, housing. sector_state in a_general_1 is the partner organisation, not used as adm1. Not plot production.
# b_inputs_1.csv, b_inputs_2.csv: whether N2Africa inputs were received, by season. Not rates on the measured fields.
# c_land_holding_management.csv: farm_size_ha and crop_rotation_* (free text, farm level, not field level); legume_seed_obtained and fertiliser source.
# d_crop_production_use.csv: farm-level total_production_farm, amount_for_sale, amount_for_hh, amount_kept_seed_planting. Parallel to the plot harvest.
# e_changes_*.csv, e_legumes_produced_4_years_ago.csv: recall of area and use four years earlier, haulm and grain processing.
# f_nutrition_1.csv to f_nutrition_4.csv: nutrition module.
# variable_definitions.csv: dictionary (units column is empty for harvest and fertiliser).


carob_script <- function(path) {

"N2Africa impact survey - Malawi, 2013

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/fcwa-d071"
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
		carob_effort = 0.7
	)

	map_token <- function(z) {
		z <- gsub("\\s+", " ", trimws(z))
		if (!nzchar(z) || z %in% c("na", "n/a", "none", "other", "others")) return(NA_character_)
		if (grepl("sweet ?potato", z)) return("sweetpotato")
		if (grepl("irish potato|^potato", z)) return("potato")
		if (grepl("soy|soya", z)) return("soybean")
		if (grepl("ground ?nut", z)) return("groundnut")
		if (grepl("cow ?pea", z)) return("cowpea")
		if (grepl("pigeon ?pea", z)) return("pigeon pea")
		if (grepl("common bean|^beans?$|haricot", z)) return("common bean")
		if (grepl("faba|fava", z)) return("faba bean")
		if (grepl("finger millet", z)) return("finger millet")
		if (grepl("^millet$", z)) return("pearl millet")
		if (grepl("bambara", z)) return("bambara groundnut")
		if (grepl("^cassava$", z)) return("cassava")
		if (grepl("^maize$", z)) return("maize")
		if (grepl("banana", z)) return("banana")
		if (grepl("^cotton$", z)) return("cotton")
		if (grepl("tobacco", z)) return("tobacco")
		if (grepl("sorgh", z)) return("sorghum")
		if (grepl("^tef+$", z)) return("teff")
		if (grepl("wheat", z)) return("wheat")
		if (grepl("^barley$", z)) return("barley")
		if (grepl("^rice$", z)) return("rice")
		if (grepl("coffee|coffea|caffee", z)) return("coffee")
		if (grepl("^k?hat$|^chatt?$", z)) return("khat")
		if (grepl("amaranth", z)) return("amaranth")
		if (grepl("^yam$", z)) return("yam")
		if (grepl("^miaze$|maize", z)) return("maize")
		if (grepl("copwea", z)) return("cowpea")
		if (grepl("grounsnut|groundbeansa", z)) return("groundnut")
		if (grepl("pegion", z)) return("pigeon pea")
		if (grepl("sun ?flower", z)) return("sunflower")
		if (z %in% c("tree", "trees", "eucalyptus", "wood", "fallow")) return(NA_character_)
		z
	}

	map_crops <- function(x) {
		x <- as.character(x)
		out <- rep(NA_character_, length(x))
		ok <- !is.na(x) & trimws(x) != ""
		y <- tolower(trimws(x[ok]))
		y <- gsub("\\([^)]*\\)", " ", y)
		y <- gsub("[0-9.]+\\s*%", " ", y)
		y <- gsub("/", ",", y)
		y <- gsub(";", ",", y)
		y <- gsub("\\+", ",", y)
		y <- gsub(" and ", ",", y)
		out[ok] <- vapply(strsplit(y, ","), function(parts) {
			parts <- trimws(parts)
			mapped <- unique(unlist(lapply(parts, map_token)))
			mapped <- mapped[!is.na(mapped) & nzchar(mapped)]
			if (!length(mapped)) NA_character_ else paste(mapped, collapse="_")
		}, character(1))
		out
	}

	map_fert_one <- function(s) {
		sl <- tolower(trimws(s))
		if (!nzchar(sl)) return(NA_character_)
		if (sl %in% c("none", "no", "non", "nil", "0", "n/a", "na")) return("none")
		if (grepl("23[:.]21", sl)) return("NPS")
		if (grepl("npk", sl)) return("NPK")
		if (grepl("^ur$|^ure$|urea|ureia", sl)) return("urea")
		if (grepl("dap", sl)) return("DAP")
		if (grepl("sympa", sl)) return("sympal")
		if (grepl("d[- .]*comp", sl)) return("D-compound")
		if (grepl("^can$|ammonium nitrate", sl)) return("CAN")
		if (grepl("^tsp$|supetriphosphate|superphosphate", sl)) return(if (grepl("supetri|superphos", sl)) "SSP" else "TSP")
		if (grepl("chitowe", sl)) return("Chitowe")
		trimws(s)
	}

	clean_fert <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "-88")] <- NA
		x[!is.na(x) & grepl("23[:.]21", x, ignore.case=TRUE)] <- "NPS"
		out <- x
		for (i in which(!is.na(x))) {
			parts <- unlist(strsplit(x[i], "\\s*[,+/]| and "))
			parts <- unlist(lapply(parts, map_fert_one))
			parts <- unique(parts[!is.na(parts) & nzchar(parts)])
			out[i] <- if (length(parts)) paste(parts, collapse=";") else NA_character_
		}
		out
	}

	yn <- function(x) {
		x <- toupper(trimws(as.character(x)))
		ifelse(x == "Y", TRUE, ifelse(x == "N", FALSE, NA))
	}

	yield_part_of <- function(crop) {
		grain <- c("maize", "wheat", "teff", "sorghum", "finger millet", "pearl millet", "barley", "rice")
		seed <- c("soybean", "common bean", "cowpea", "groundnut", "chickpea", "faba bean", "grass pea", "pea", "pigeon pea", "sesame", "bambara groundnut", "coffee", "sunflower")
		one <- function(z) {
			if (z %in% grain) return("grain")
			if (z %in% seed) return("seed")
			if (z %in% c("cassava", "sweetpotato")) return("roots")
			if (z %in% c("potato", "yam")) return("tubers")
			if (z %in% c("banana", "tomato", "eggplant")) return("fruit")
			if (z %in% c("tobacco", "khat", "amaranth")) return("leaves")
			if (z == "cotton") return("lint")
			if (z == "enset") return("rhizome")
			NA_character_
		}
		vapply(strsplit(crop, "_", fixed=TRUE), function(parts) {
			p <- unique(unlist(lapply(parts, one)))
			p <- p[!is.na(p)]
			if (length(p) == 1) p else NA_character_
		}, character(1))
	}

	f <- ff[basename(ff) == "c_land_holding_management_2.csv"]
	r <- read.csv(f, stringsAsFactors=FALSE)
	g <- read.csv(ff[basename(ff) == "a_general_1.csv"], stringsAsFactors=FALSE)

	# Plain numbers are kg. "20/70" is the harvest of each crop in a mixture, summed.
	# A piece >= 1900 is a date typed into the harvest field and is left NA.
	# "75 (Groundnuts), 20 (Soy Beans)" is summed the same way when the whole string is those pieces.
	parse_harvest_kg <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "-88", "-99")] <- NA
		out <- rep(NA_real_, length(x))
		plain <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
		out[plain] <- as.numeric(x[plain])
		slash <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?(/[0-9]+(\\.[0-9]+)?)+$", x)
		if (any(slash)) {
			out[slash] <- vapply(strsplit(x[slash], "/", fixed=TRUE), function(p) {
				n <- as.numeric(p)
				if (any(n >= 1900)) NA_real_ else sum(n)
			}, numeric(1))
		}
		paren <- !is.na(x) & is.na(out) & grepl("[0-9]+(\\.[0-9]+)?[[:space:]]*\\([^)]+\\)", x)
		if (any(paren)) {
			out[paren] <- vapply(x[paren], function(s) {
				m <- gregexpr("[0-9]+(\\.[0-9]+)?[[:space:]]*\\([^)]+\\)", s)
				hits <- regmatches(s, m)[[1]]
				if (!length(hits)) return(NA_real_)
				rest <- s
				for (h in hits) rest <- sub(h, "", rest, fixed=TRUE)
				rest <- gsub("[,;[:space:]]", "", rest)
				if (nzchar(rest)) return(NA_real_)
				sum(as.numeric(sub("[[:space:]]*\\(.*", "", hits)))
			}, numeric(1))
		}
		out
	}

	harvest <- parse_harvest_kg(r$harvest_amount)
	fert_amt <- r$min_fert_amount
	fert_amt[fert_amt %in% c(-88, -99)] <- NA
	size <- r$size_ha
	size[size %in% c(-88, -99)] <- NA

	unit <- tolower(trimws(r$weight_unit))
	unit <- sub("s$", "", unit)
	is_kg <- unit == "kg"
	yield <- ifelse(is_kg & !is.na(size) & size > 0, harvest / size, NA_real_)

	country <- trimws(g$country)
	country[country == ""] <- NA

	# gps_dec_* are empty; unsigned gps_latitude is Malawi latitude with the sign dropped (see NOTES)
	lat <- g$gps_latitude
	lon <- g$gps_longitude
	flip <- !is.na(lat) & lat > 0 & !is.na(lon) & lon > 30 & lon < 40
	lat[flip] <- -lat[flip]

	date <- rep(NA_character_, nrow(g))
	okd <- !is.na(g$date_interview_yyyy) & g$date_interview_yyyy >= 2000 &
		!is.na(g$date_interview_mm) & g$date_interview_mm >= 1 & g$date_interview_mm <= 12 &
		!is.na(g$date_interview_dd) & g$date_interview_dd >= 1 & g$date_interview_dd <= 31
	date[okd] <- sprintf("%04d-%02d-%02d", g$date_interview_yyyy[okd], g$date_interview_mm[okd], g$date_interview_dd[okd])

	loc <- data.frame(
		farm_id = g$farm_id,
		country = country,
		adm1 = "Central Region",
		adm2 = carobiner::fix_name(g$action_site, "title"),
		location = carobiner::fix_name(g$village, "title"),
		longitude = lon,
		latitude = lat,
		elevation = g$gps_altitude,
		date = date,
		stringsAsFactors = FALSE
	)
	loc <- loc[!duplicated(loc$farm_id), ]
	loc$elevation[!is.na(loc$elevation) & (loc$elevation < -100 | loc$elevation > 3000)] <- NA

	d <- data.frame(
		hhid = as.character(r$farm_id),
		plot_id = as.character(r$field),
		crop = map_crops(r$crops_grown),
		variety = trimws(r$varieties),
		field_size = size,
		yield = yield,
		fertilizer_type = clean_fert(r$min_fert_type),
		fertilizer_amount_ = fert_amt,
		OM_used = yn(r$organic_input_applied),
		inoculated = yn(r$inoculant_applied),
		stringsAsFactors = FALSE
	)
	d$variety[d$variety %in% c("", "NA", "N/A", "-88")] <- NA
	d$farm_id <- r$farm_id
	d <- merge(d, loc, by="farm_id", all.x=TRUE)
	d$farm_id <- NULL
	d <- d[!is.na(d$crop) & !is.na(d$hhid) & !is.na(d$country), ]
	d$yield_part <- yield_part_of(d$crop)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- !is.na(d$longitude) & !is.na(d$latitude)
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
