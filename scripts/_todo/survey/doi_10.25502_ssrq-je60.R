# R script for "carob"
# license: GPL (>=3)

## NOTES
# N2Africa use survey, households in Ghana, Malawi and three Kenya records. Field table is legumes_area_and_management.csv.
# type_legume_variety is the legume (soybean, groundnut, cowpea, common bean, pigeon pea, bambara groundnut). "Bush beans" and "Climbing beans" are common bean; the growth habit is variety_type. "Jenguma 3kg" is the Jenguma soybean variety (the "3kg" is not kept).
# production_field_kg is empty for every row, so yield is not calculated.
# Column area has no unit in variable_definitions. Values run from 0 to 49000 (median 400), which is not hectares, so it is kept as area_reported_ and not written to field_size.
# amount_fert_kg is kilograms of product on the field (the column name). It is not converted to kg/ha because the area unit is unknown, and elemental N/P/K are not calculated.
# Location from general.csv (district, village, interview year). Recorded gps_latitude / gps_longitude are not used: the pairs do not fall in Ghana or Malawi.

## ISSUES
# Area unit must be confirmed before field_size or yield can be filled. production_field_kg is entirely missing.
# GPS (47-57 non-missing values, latitude about 0.2-34, longitude about 10-14) was not mapped. Human should georeference districts: Ghana Wa East, Bawku West, Karaga, Chereponi, Nadowli; Malawi Dedza, Salima, Dowa, Ntcheu, Lilongwe. Codes K/N/E and KN/E were left as typed.
# season is missing for 304 of 494 households. planting_date is set only where season is 2010 or 2011. Interview year is mostly 2012.
# Fertilizer names Sympal, 23:21:0+4S and "Farmers own source" are not in the fertilizer vocabulary.
# inputs_received.csv is a separate item list (current / last / previous season) and was not joined.
# No publication DOI.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude; unknown area_reported_, fertilizer_product_kg_, ward_; invalid intercrops "chinese"; invalid fertilizer_type MOP, Nitrogen fertilizer, Super D.

## NEW VARIABLES
# area_reported_: raw area from legumes_area_and_management (unit not documented)
# fertilizer_product_kg_: amount_fert_kg, kilograms of product on the field, not per hectare
# ward_: sector_ward (EPA or region name, not used as adm3)

## NOT PROCESSED
# awareness.csv: knowledge of nodules and inoculant storage. Not a production record.
# inputs_received.csv: inputs received by season (item list).
# reasons_adoptation_a.csv, reasons_adoptation_b.csv: reasons for adoption.
# general.csv columns farmer_sex, farmer_age, farm_size_ha, organisation, gps_*: farm_size_ha is the whole farm, not the legume field. GPS not used (see ISSUES).
# legumes_area_and_management.csv: seed_used_kg, perc_legume_versus_non_legume, inoculant_source, other_n2africa_tech_applied, specify_aspects_technology.
# variable_definitions.csv: units blank.


carob_script <- function(path) {

"N2Africa use survey - Ghana, Malawi, 2010 - 2011

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/ssrq-je60"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; ICRAF; WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	map_legume <- function(x) {
		z <- tolower(trimws(as.character(x)))
		out <- rep(NA_character_, length(z))
		out[grepl("jenguma", z)] <- "soybean"
		out[grepl("bush bean|climbing bean|common bean", z)] <- "common bean"
		out[grepl("soy", z) & is.na(out)] <- "soybean"
		out[grepl("groundnut|bambara", z) & grepl("bambara", z)] <- "bambara groundnut"
		out[grepl("groundnut", z) & is.na(out)] <- "groundnut"
		out[grepl("cowpea", z)] <- "cowpea"
		out[grepl("pigeon", z)] <- "pigeon pea"
		out
	}

	clean_fert <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA
		out <- x
		for (i in which(!is.na(x))) {
			parts <- unlist(strsplit(x[i], "\\s*(,|/| and |&)+\\s*"))
			parts <- trimws(parts)
			parts <- parts[nzchar(parts)]
			lab <- character()
			for (s in parts) {
				sl <- tolower(s)
				if (grepl("23[:.]21[:.][0o]", sl)) lab <- c(lab, "NPS")
				else if (grepl("15-15-15|npk|n\\.p\\.k", sl)) lab <- c(lab, "NPK")
				else if (grepl("urea", sl)) lab <- c(lab, "urea")
				else if (grepl("^tsp", sl)) lab <- c(lab, "TSP")
				else if (grepl("^can$", sl)) lab <- c(lab, "CAN")
				else if (grepl("mop", sl)) lab <- c(lab, "MOP")
				else if (grepl("sympal", sl)) lab <- c(lab, "sympal")
				else if (grepl("d[- ]?comp", sl)) lab <- c(lab, "D-compound")
				else if (grepl("sulphate of ammonia|sulfate of ammonia|sulphate ammonia|sulphate of ammonium|sulphate ammonium", sl)) lab <- c(lab, "DAS")
				else if (sl %in% c("farmers own source", "market", "plant spacing (75*5cm)")) next
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

	r <- read.csv(ff[basename(ff) == "legumes_area_and_management.csv"], stringsAsFactors=FALSE)
	g <- read.csv(ff[basename(ff) == "general.csv"], stringsAsFactors=FALSE)

	leg <- tolower(trimws(r$type_legume_variety))
	crop <- map_legume(r$type_legume_variety)
	variety <- rep(NA_character_, nrow(r))
	variety[grepl("jenguma", leg)] <- "Jenguma"
	vtype <- rep(NA_character_, nrow(r))
	vtype[grepl("bush bean", leg)] <- "bush"
	vtype[grepl("climbing bean", leg)] <- "climbing"

	applied <- yn(r$synthetic_fert_applied)
	fert <- clean_fert(r$synthetic_fert_types)
	fert[applied %in% FALSE] <- "none"

	map_inter_token <- function(p) {
		p <- gsub("^with[[:space:]]+", "", trimws(p))
		if (!nzchar(p) || p %in% c("na", "n/a", "n2africa")) return(NA_character_)
		if (grepl("jenguma|anidaso|soybeanm|^soy", p)) return("soybean")
		if (grepl("groundnut", p)) return("groundnut")
		if (grepl("cowpea", p)) return("cowpea")
		if (grepl("pigeon", p)) return("pigeon pea")
		if (grepl("millet", p)) return("pearl millet")
		if (grepl("common bean|commo|commonbeana|commonbean|beans?", p)) return("common bean")
		if (grepl("maize", p)) return("maize")
		if (grepl("tobacco", p)) return("tobacco")
		if (grepl("cotton", p)) return("cotton")
		if (grepl("^peas?$", p)) return("pea")
		p
	}
	other <- tolower(trimws(r$with_which_crops))
	other[other %in% c("", "na")] <- NA
	inter <- rep(NA_character_, length(other))
	ok_i <- !is.na(other) & yn(r$intercropped) %in% TRUE
	inter[ok_i] <- vapply(other[ok_i], function(s) {
		parts <- trimws(unlist(strsplit(s, ",|;| and ")))
		lab <- unique(unlist(lapply(parts, map_inter_token)))
		lab <- lab[!is.na(lab) & nzchar(lab)]
		if (!length(lab)) NA_character_ else paste(lab, collapse=";")
	}, character(1))

	d <- data.frame(
		hhid = as.character(r$farm_id),
		plot_id = as.character(r$field),
		crop = crop,
		variety = variety,
		variety_type = vtype,
		intercrops = inter,
		area_reported_ = r$area,
		fertilizer_type = fert,
		fertilizer_product_kg_ = r$amount_fert_kg,
		OM_used = yn(r$organic_fert_applied),
		inoculated = yn(r$inoculant_applied),
		stringsAsFactors = FALSE
	)

	dist <- trimws(g$district_lga)
	dl <- tolower(dist)
	dist[dl %in% c("wa east", "wast east")] <- "Wa East"
	dist[dl %in% c("bawku west", "b/w")] <- "Bawku West"
	dist[dl == "karaga"] <- "Karaga"
	dist[dl == "chereponi" | dl == "cherepponi"] <- "Chereponi"
	dist[dl == "nadowli"] <- "Nadowli"
	dist[dl == "dowa"] <- "Dowa"

	season <- trimws(as.character(g$season))
	plant <- ifelse(season %in% c("2010", "2011"), season, NA_character_)
	date <- ifelse(!is.na(g$date_survey_yyyy) & g$date_survey_yyyy >= 2000, as.character(g$date_survey_yyyy), NA_character_)

	loc <- data.frame(
		farm_id = g$farm_id,
		country = trimws(g$country),
		adm2 = carobiner::fix_name(dist, "title"),
		location = carobiner::fix_name(g$village, "title"),
		ward_ = trimws(g$sector_ward),
		planting_date = plant,
		date = date,
		stringsAsFactors = FALSE
	)
	loc$ward_[loc$ward_ == ""] <- NA
	loc <- loc[!duplicated(loc$farm_id), ]

	d$farm_id <- r$farm_id
	d <- merge(d, loc, by="farm_id", all.x=TRUE)
	d$farm_id <- NULL
	d <- d[!is.na(d$crop) & !is.na(d$hhid) & !is.na(d$country), ]
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
