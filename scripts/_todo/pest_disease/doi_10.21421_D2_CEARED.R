# R script for "carob"
# license: GPL (>=3)

## ISSUES
# No coordinates in the file or Dataverse metadata; 6 villages were georeferenced to village centroids (2-3 km uncertainty) and 6 hamlets to their mandal centroids (10 km uncertainty).
# Most fertilizer rate entries list alternative products (e.g., "17:17:17, 20:20:0 (OR) DAP 100kg / per acre"), so elemental N/P/K rates could not be computed; only fertilizer_used, fertilizer_amount, and unambiguous fertilizer_type are populated.
# Land holdings in Agricultural information do not sum for hhid 47 (total is NA) and hhid 55 (0 irrigated + 6 rain-fed vs 5 total); values are kept as recorded.
# In Biotic constraints, hhid 123 listed "Helicoverpa" under 1st disease (moved to pest_species) and one storage pest value is truncated ("Groundnut bru", interpreted as "groundnut bruchid").
# In GAP's, hhid 35 reports 90% commercial and 90% self-consumption (kept as recorded for commercial_share_); two rows reported proportions (0.9, 0.95) and were scaled to percentages.
# Currency for Yearly Income is not printed in the header; assumed INR based on Indian comma notation (2,00,000) and Andhra Pradesh location.

## NEW
# rainfed_area_: rain-fed land holding, ha
# soil_tested_: whether farmer tests soil (logical)
# aflatoxin_aware_: whether farmer has heard of aflatoxin (logical; all FALSE)
# kernels_seen_: whether farmer has seen contaminated groundnut kernels/pods (logical)
# kernel_appearance_: visual appearance/color of contaminated kernels as reported
# abiotic_constraint_: major abiotic constraint reported (Drought or Rainfall)
# drought_timing_: crop growth stage most affected in drought years
# drought_management_: farmer drought risk management strategy
# pest_control_: field pest and disease control method
# pest_where_: whether reported pest occurs in field or storage
# drying_place_: location where harvested groundnut pods are dried
# drying_days_: duration of pod drying, days
# storage_days_: duration of groundnut storage after harvest, days
# storage_method_: post-harvest storage structure/container
# storage_pest_control_: storage pest and disease control practices
# commercial_share_: percentage of groundnut production for commercial sale, %
# sale_place_: where groundnuts are sold
# sale_month_: month when groundnuts are normally sold
# pest_species: leaf folder; Helicoverpa; Spodoptera; red hairy caterpillar; jassid; red weevil; groundnut bruchid; red flour beetle
# disease: viral diseases (bud necrosis, stem necrosis); soil-borne diseases (stem rot, pod rot); soil-borne diseases (stem rot, root rot)

## NOTES
# Household survey of 280 groundnut farmers across 12 villages in 4 mandals of Anantapur district, Andhra Pradesh, India (crops recorded for 2011-2013; no aflatoxin lab measurements).
# Personal names and mobile numbers are excluded. Land holdings (acres) and GAP application rates (tons or kg per acre) were converted to ha and kg/ha (1 acre = 0.4046863 ha).
# Regional Telugu crop names were standardized to terminag: chini -> orange (sweet orange), korra -> foxtail millet, kurbuja -> muskmelon, paddy -> rice, vegetables -> vegetable.
# Multi-year cropping history (2011-2013), ranked field/storage pests, diseases, and livestock counts are stored in the long table keyed by hhid and order.

## NOT PROCESSED
# Personal information: Name of the Farmer, Mobile number (PII); No.Children (no age cutoff to map to hh_child_18 and mixes counts with "NoChildren"/"N0"/"Nil").
# GAP's: who-does-this-work gender/labour columns, constant/near-constant questions (crop rotation all No, optimum maturity harvest all Yes, sun drying all Sun drying, manual moisture check all Yes, desire/willingness to invest all Yes), seed selection criteria, pod stripping/segregation details, and 5-year consumption trend reasons.
# Market: who sells and immediate sale flag/reason (all Yes, "Lack of money in hand").

carob_script <- function(path) {

"
Aflatoxin contamination problem in Anantapur district

Aflatoxin contamination problem studies  in Anantapur district of Andhra Pradesh of South India
"

	uri <- "doi:10.21421/D2/CEARED"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 4,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 75,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Aflatoxin problem Anantapur AP India.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Personal information", skip = 1, na = c("NA", ""))
	r2 <- carobiner::read.excel(f, sheet = "Agricultural information", skip = 4, col_names = FALSE, na = c("NA", ""))
	r3 <- carobiner::read.excel(f, sheet = "Biotic constraints", skip = 1, na = c("NA", ""))
	r4 <- carobiner::read.excel(f, sheet = "Abiotic constraints ", skip = 1, na = c("NA", ""))
	r5 <- carobiner::read.excel(f, sheet = "Awareness on aflatoxin conta", skip = 2, na = c("NA", ""))
	r6 <- carobiner::read.excel(f, sheet = "GAP's", skip = 2, na = c("NA", ""), col_types = "text")
	r7 <- carobiner::read.excel(f, sheet = "Market ", skip = 1, na = c("NA", ""))

	hhid <- as.character(r1[["S.No"]])
	align <- function(x) {
		id <- trimws(as.character(x[[1]]))
		ok <- !is.na(id) & grepl("^[0-9]+$", id)
		x <- x[ok, ]
		x[match(hhid, trimws(as.character(x[[1]]))), ]
	}

	acre <- 0.4046863
	r2 <- align(r2)
	r3 <- align(r3)
	r4 <- align(r4)
	r5 <- align(r5)
	r6 <- align(r6)
	r7 <- align(r7)

	yesno <- function(x) {
		z <- tolower(trimws(as.character(x)))
		z[z == "n0"] <- "no"
		out <- rep(NA, length(z))
		out[z == "yes"] <- TRUE
		out[z == "no"] <- FALSE
		out
	}

	parse_fym <- function(x) {
		z <- tolower(trimws(as.character(x)))
		val <- rep(NA_real_, length(z))
		ok <- !is.na(z) & grepl("[0-9]", z)
		num <- as.numeric(sub("^([0-9]+\\.?[0-9]*).*", "\\1", z[ok]))
		is_ton <- grepl("ton", z[ok])
		val[ok] <- ifelse(is_ton, num * 1000, num) / acre
		val
	}

	parse_kg_acre <- function(x) {
		z <- tolower(trimws(as.character(x)))
		val <- rep(NA_real_, length(z))
		m <- regexpr("[0-9]+\\.?[0-9]*(?=\\s*kg)", z, perl = TRUE)
		ok <- !is.na(z) & m > 0
		val[ok] <- as.numeric(regmatches(z, m)) / acre
		val
	}

	clean_edu <- function(x) {
		z <- trimws(as.character(x))
		z <- gsub("([0-9](?:st|nd|rd|th))class", "\\1 class", z, ignore.case = TRUE)
		z[tolower(z) == "nil"] <- "Nil"
		z
	}

	clean_kernel <- function(x) {
		z <- tolower(trimws(gsub("\\s+", " ", as.character(x))))
		out <- rep(NA_character_, length(z))
		out[grepl("yellow\\s*-?\\s*green|green\\s+yellow", z)] <- "yellow-green"
		out[is.na(out) & grepl("yellow", z)] <- "yellow"
		out[is.na(out) & grepl("black", z)] <- "black"
		out[is.na(out) & grepl("green", z)] <- "green"
		out
	}

	inc <- gsub(",", "", as.character(r1[["Yearly Income"]]))
	major <- trimws(as.character(r4[[2]]))
	zinc <- tolower(trimws(as.character(r4[[5]]))) == "zinc"
	zinc[is.na(zinc)] <- FALSE
	stress <- ifelse(tolower(major) == "drought", "drought", NA_character_)
	stress[zinc & !is.na(stress)] <- paste(stress[zinc & !is.na(stress)], "zinc deficiency", sep = ";")
	stress[zinc & is.na(stress)] <- "zinc deficiency"

	om_used <- yesno(r6[[7]])
	om_amt <- parse_fym(r6[[8]])
	om_amt[!is.na(om_used) & !om_used] <- 0

	fert_used <- yesno(r6[[15]])
	fert_amt <- parse_kg_acre(r6[[16]])
	fert_amt[!is.na(fert_used) & !fert_used] <- 0
	fert_raw <- trimws(as.character(r6[[16]]))
	fert_type <- rep(NA_character_, length(fert_raw))
	fert_type[!is.na(fert_used) & !fert_used] <- "none"
	fert_type[grepl("^DAP\\s+[0-9]", fert_raw)] <- "DAP"
	fert_type[grepl("^17:17:17\\s+[0-9]", fert_raw)] <- "NPK"

	amend_used <- yesno(r6[[18]])
	amend_amt <- parse_kg_acre(r6[[19]])
	amend_amt[!is.na(amend_used) & !amend_used] <- 0

	comm <- as.numeric(r6[[41]])
	comm[!is.na(comm) & comm <= 1] <- comm[!is.na(comm) & comm <= 1] * 100

	mandal <- carobiner::fix_name(trimws(r1[["Name of the mandal"]]), "title")
	mandal[mandal == "Kudaeru"] <- "Kuderu"
	mandal[mandal == "Rapthadu"] <- "Raptadu"
	village <- carobiner::fix_name(trimws(r1[["Name of the village"]]), "title")

	d <- data.frame(
		hhid = hhid,
		trial_id = hhid,
		country = "India",
		adm1 = "Andhra Pradesh",
		adm2 = "Anantapur",
		adm3 = mandal,
		location = village,
		sex = tolower(trimws(r1$Gender)),
		age = as.numeric(r1$Age),
		education = clean_edu(r1[["Educational level"]]),
		civil_status = tolower(trimws(r1[["Mariatal status"]])),
		hh_income = as.numeric(inc),
		currency = "INR",
		farmland = as.numeric(r2[[4]]) * acre,
		land_irrigated = as.numeric(r2[[2]]) * acre,
		rainfed_area_ = as.numeric(r2[[3]]) * acre,
		soil_tested_ = yesno(r6[[2]]),
		OM_used = om_used,
		OM_type = ifelse(!is.na(om_used) & om_used, "farmyard manure", ifelse(!is.na(om_used) & !om_used, "none", NA_character_)),
		OM_amount = om_amt,
		residue_prevcrop_used = FALSE,
		fertilizer_used = fert_used,
		fertilizer_type = fert_type,
		fertilizer_amount = fert_amt,
		soil_amendment_type = ifelse(!is.na(amend_used) & amend_used, "gypsum or lime", NA_character_),
		soil_amendment = amend_amt,
		harvest_method = "manual",
		aflatoxin_aware_ = yesno(r5[[2]]),
		kernels_seen_ = yesno(r5[[4]]),
		kernel_appearance_ = clean_kernel(r5[[5]]),
		abiotic_constraint_ = major,
		drought_timing_ = trimws(as.character(r4[[3]])),
		drought_management_ = trimws(as.character(r4[[4]])),
		stress = stress,
		pest_control_ = trimws(as.character(r3[[12]])),
		drying_place_ = trimws(as.character(r6[[31]])),
		drying_days_ = as.numeric(r6[[32]]),
		storage_days_ = as.numeric(r6[[37]]),
		storage_method_ = trimws(as.character(r6[[38]])),
		storage_pest_control_ = trimws(gsub("\\s+", " ", as.character(r6[[40]]))),
		commercial_share_ = comm,
		sale_place_ = trimws(as.character(r7[[2]])),
		sale_month_ = trimws(as.character(r7[[7]])),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	geo <- data.frame(
		location = c(
			"Gangalakunta", "Palabhavi", "Krishnapuram",
			"Ippaeru", "Muddulapuram", "Ramachandrapuram",
			"Chintalapalli", "Uppanaesanapalli", "Vasanthapuram",
			"Pampanur", "Vaddipalli", "Kurlapally"
		),
		latitude = c(
			14.6114, 14.4699, 14.6189,
			14.7688, 14.7617, 14.7322,
			14.5044, 14.4137, 14.4137,
			14.6150, 14.6133, 14.6133
		),
		longitude = c(
			77.6213, 77.5792, 77.6039,
			77.3902, 77.3894, 77.4315,
			77.7039, 77.7126, 77.7126,
			77.3900, 77.4261, 77.4261
		),
		geo_uncertainty = c(
			2000, 2000, 10000,
			2000, 2000, 10000,
			2000, 10000, 10000,
			3000, 10000, 10000
		),
		geo_source = c(
			"Mapcarta village centroid", "Mapcarta village centroid", "Raptadu mandal centroid",
			"Maps of India village centroid", "Mapcarta village centroid", "Kuderu mandal centroid",
			"Mapcarta village centroid", "Dharmavaram mandal centroid", "Dharmavaram mandal centroid",
			"Maps of India village centroid", "Atmakur mandal centroid", "Atmakur mandal centroid"
		)
	)
	gidx <- match(d$location, geo$location)
	d$latitude <- geo$latitude[gidx]
	d$longitude <- geo$longitude[gidx]
	d$geo_uncertainty <- geo$geo_uncertainty[gidx]
	d$geo_source <- geo$geo_source[gidx]

	map_crop <- function(z) {
		z0 <- tolower(trimws(gsub("\\s+", " ", z)))
		out <- z0
		out[z0 %in% c("groundnut", "grpoundnut")] <- "groundnut"
		out[z0 == "paddy"] <- "rice"
		out[grepl("pigeon", z0)] <- "pigeon pea"
		out[z0 %in% c("chilli", "chili")] <- "chili pepper"
		out[z0 == "castor"] <- "castor bean"
		out[z0 %in% c("water melon", "watermelon")] <- "watermelon"
		out[z0 == "chini"] <- "orange"
		out[z0 == "korra"] <- "foxtail millet"
		out[z0 == "kurbuja"] <- "muskmelon"
		out[z0 == "vegetables"] <- "vegetable"
		out
	}

	crop_rows <- function(col, year, season) {
		raw <- gsub("&| and ", ",", as.character(r2[[col]]), ignore.case = TRUE)
		parts <- strsplit(raw, ",")
		h <- rep(hhid, lengths(parts))
		crops <- trimws(unlist(parts))
		keep <- !is.na(crops) & nzchar(crops) & !is.na(h)
		if (!any(keep)) {
			return(data.frame(hhid = character(), crop = character(), season = character(), planting_date = character()))
		}
		data.frame(
			hhid = h[keep],
			crop = map_crop(crops[keep]),
			season = rep(season, sum(keep)),
			planting_date = rep(year, sum(keep))
		)
	}
	crops <- rbind(
		crop_rows(5, "2013", "kharif"),
		crop_rows(6, "2013", "rabi"),
		crop_rows(7, "2012", "kharif"),
		crop_rows(8, "2012", "rabi"),
		crop_rows(9, "2011", "kharif"),
		crop_rows(10, "2011", "rabi")
	)

	clean_pest <- function(x) {
		z <- tolower(trimws(gsub("\\s+", " ", as.character(x))))
		z[!nzchar(z)] <- NA_character_
		out <- z
		out[z %in% c("leaf folder", "leaf floder")] <- "leaf folder"
		out[z %in% c("helicoverpa", "helico verpa")] <- "Helicoverpa"
		out[z == "spodoptera"] <- "Spodoptera"
		out[grepl("red ?hairy|redhiary", z)] <- "red hairy caterpillar"
		out[z == "jassids"] <- "jassid"
		out[z == "redweevils"] <- "red weevil"
		out[z %in% c("groundnut bru", "groundnut bruchid")] <- "groundnut bruchid"
		out[z == "red flour beetle"] <- "red flour beetle"
		out
	}

	pest_rows <- function(x, where) {
		raw <- gsub("&", ";", as.character(x))
		parts <- lapply(strsplit(raw, ";"), function(p) {
			p <- clean_pest(p)
			p[!is.na(p)]
		})
		keep <- lengths(parts) > 0
		if (!any(keep)) {
			return(data.frame(hhid = character(), pest_species = character(), pest_where_ = character()))
		}
		data.frame(
			hhid = rep(hhid[keep], lengths(parts[keep])),
			pest_species = unlist(parts[keep]),
			pest_where_ = rep(where, sum(lengths(parts[keep])))
		)
	}

	# hhid 123 entered "Helicoverpa" under 1st disease
	d1_raw <- trimws(gsub("\\s+", " ", as.character(r3[[7]])))
	misplaced_pest <- ifelse(tolower(d1_raw) == "helicoverpa", "Helicoverpa", NA_character_)
	d1_raw[tolower(d1_raw) == "helicoverpa"] <- NA_character_

	pests <- rbind(
		pest_rows(r3[[2]], "field"),
		pest_rows(r3[[3]], "field"),
		pest_rows(r3[[4]], "field"),
		pest_rows(r3[[5]], "field"),
		pest_rows(misplaced_pest, "field"),
		pest_rows(r3[[11]], "storage")
	)

	clean_disease <- function(x) {
		z <- tolower(trimws(gsub("\\s+", " ", as.character(x))))
		z[!nzchar(z)] <- NA_character_
		out <- rep(NA_character_, length(z))
		out[grepl("^foliar", z)] <- "leaf diseases"
		out[grepl("^viral", z)] <- "viral diseases (bud necrosis, stem necrosis)"
		out[grepl("^soil borne", z) & grepl("root rot", z)] <- "soil-borne diseases (stem rot, root rot)"
		out[grepl("^soil borne", z) & !grepl("root rot", z)] <- "soil-borne diseases (stem rot, pod rot)"
		out
	}

	dis_rows <- function(x) {
		v <- clean_disease(x)
		keep <- !is.na(v)
		data.frame(hhid = hhid[keep], disease = v[keep])
	}
	diseases <- rbind(dis_rows(d1_raw), dis_rows(r3[[8]]), dis_rows(r3[[9]]))

	animal_rows <- function(x, animal) {
		nhead <- as.numeric(x)
		keep <- !is.na(nhead)
		data.frame(hhid = hhid[keep], animal = animal, heads = nhead[keep])
	}
	livestock <- rbind(
		animal_rows(r2[[11]], "cattle"),
		animal_rows(r2[[12]], "buffalo"),
		animal_rows(r2[[13]], "sheep"),
		animal_rows(r2[[14]], "goat")
	)

	long <- carobiner::bindr(crops, pests, diseases, livestock)
	long$order <- as.integer(ave(seq_len(nrow(long)), long$hhid, FUN = seq_along))

	carobiner::write_files(path, meta, d, long = long)
}
