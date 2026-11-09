# R script for "carob"
# license: GPL (>=3)

## NOTES
# InnovAfrica baseline, 3819 farmers, 12 pilot sites in six countries, collected 2018 (README).
# Questionnaire A/7 asked GPS lat/lon; the digitized file has gps_alt only (no latitude/longitude columns).
# Crop block is wide: up to 11 crops x two seasons (short rains/winter 2016, long rains 2017). Reshaped to one row per household-crop-season.
# Yield columns are already kg/ha (README). Area column cy_*_crpy_*_area is hectares.
# Missing codes in the sheet are "n/a" and "missing".

## ISSUES
# Questionnaire GPS lat/lon were not digitized (gps_alt only). Coordinates are the GADM point-radius centroid of adm1 where the name matches.
# Spelling corrected to GADM: Kirinyanga and Kirinyiga to Kirinyaga; Maachakos, Machachkos, Machakosi and Maxchakos to Machakos; Freestate to Free State.
# No GADM adm1 match, left without coordinates: Ethiopia Kombocha, Kombolcha, Kombolche, Komnolcha, Meta (woredas, not regions); Kenya Kangundo (not a county); Rwanda Eastern, Southern (GADM names are Iburasirazuba and Amajyepfo, not spelling variants).
# Crop "Other" mapped from crop_other when present (banana, cashew, avocado, …); remaining "other" need review.
# Livestock, fodder/pasture, milk, credit, nutrition, gender, extension, and household-member roster (hhmem_1..15) not processed.
# Land parcels lp_1..4 not linked to crop rows; farmland not summed into crop records.
# Some yield values look low or high for kg/ha; left as reported.
# write_files: NA detected: latitude, longitude (regions with no GADM adm1 match); duplicate coordinates (households share a region centroid); remaining invalid crop spellings; elevation 22287 out of bounds (left as reported); NA yield; duplicate household-crop-season possible.

## NEW VARIABLES
# none



## NOT PROCESSED
# Baseline_survey__data_16_May_2022.r1.rar: not read. .rar file. Observation records are missing from d until this file is standardized.
# README_Baseline_16_May_2022.r1.txt: documentation or a codebook, not an observation table.

carob_script <- function(path) {

"InnovAfrica project baseline survey data for Ethiopia, Kenya, Malawi, Rwanda, South Africa and Tanzania

A data set was generated thorugh surveys to establish a baseline inforamtion for a project entitled \"Innovations in Technology, Institutional and Extension Approaches towards Sustainable Agriculture and enhanced Food and Nutrition Security in Africa (Acronym - InnovAfrica)\". The InnovAfrica is a consortium of 16 institutions comprising five institutions from Europe and eleven institutions from Africa and the project was implemented in six countries of eastern and southern Africa namely Ethiopia, Kenya, Malawi, Rwanda, South Africa and Tanzania from June 2017 to November 2021."

	uri <- "doi:10.5061/dryad.2bvq83bs9"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=7, minor=NA,
		data_organization = "ILRI; KALRO; SUA; ARC; RAB",
		publication = NA,
		project = "InnovAfrica",
		design = "12 pilot sites (2 per country); paper questionnaire digitized in KIPUS",
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 25,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Baseline survey  data_16 May 2022.r1.xlsx"]
	# col_types=text avoids readxl type-guess warnings on unused livestock/fodder columns
	r <- carobiner::read.excel(f, sheet="Baseline data", skip=1, na=c("n/a", "N/A", "missing"), col_types="text")

	# skipped: livestock, fodder, milk, credit, nutrition, gender, extension, hhmem_1..15, land parcels lp_*

	to_num <- function(x) {
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "n/a", "N/A", "missing", "NA", "na")] <- NA
		as.numeric(x)
	}

	map_crop <- function(crop, other) {
		crop <- trimws(as.character(crop))
		other <- trimws(as.character(other))
		out <- tolower(crop)
		use_other <- !is.na(out) & out == "other" & !is.na(other) & other != ""
		out[use_other] <- tolower(other[use_other])
		alias <- c(
			bananas="banana", bannana="banana", bnana="banana",
			cashewnut="cashew", cashewnuts="cashew", "cashew nut"="cashew",
			"cashew nuts"="cashew", cahew="cashew", cahewnut="cashew",
			casew="cashew", cashenuts="cashew", cashewnits="cashew",
			cashwnuts="cashew", chashew="cashew",
			groundnuts="groundnut", "ground nuts"="groundnut", "g/nuts"="groundnut",
			grondnuts="groundnut", grundnuts="groundnut", ggroundnut="groundnut",
			graundnut="groundnut", peanuts="groundnut",
			beans="common bean", "common beabs"="common bean",
			"common bean (inter-cropping)"="common bean",
			"soy beans"="soybean", "soya bean"="soybean", "soya beans"="soybean",
			soyabean="soybean", soyabeans="soybean", soybeans="soybean",
			"soy bean"="soybean", soya="soybean",
			"sweet potato"="sweetpotato", "sweet potatoes"="sweetpotato",
			"sweet potatoe"="sweetpotato", "sweet-potatoes"="sweetpotato",
			"sweat potato"="sweetpotato",
			potatoes="potato", potatoe="potato", "irish potato"="potato",
			"irish potatoes"="potato",
			tomatoes="tomato", tomatoe="tomato",
			onions="onion", "0nion"="onion",
			cabbages="cabbage", cabagge="cabbage",
			casava="cassava",
			coconuts="coconut", cofee="coffee",
			paddy="rice", seasame="sesame", semsame="sesame", simsim="sesame",
			whaet="wheat", "wheat( hawi)"="wheat",
			"green gram"="mung bean", greengram="mung bean", greengrain="mung bean",
			"bambara nut"="bambara groundnut", bambalanut="bambara groundnut",
			fababeans="faba bean", lentils="lentil", oat="oats", narley="barley",
			carrote="carrot", carrots="carrot", pumkin="pumpkin", pumpkib="pumpkin",
			sugarcan="sugarcane", yams="yam", pineapples="pineapple",
			"orange maize"="maize", "khat / khat"="khat", "khat or khat"="khat",
			kkhat="khat"
		)
		m <- match(out, names(alias))
		out[!is.na(m)] <- unname(alias[m[!is.na(m)]])
		out
	}

	hh <- as.character(seq_len(nrow(r)))
	blocks <- list()
	seasons <- c("sr_2016", "lr_2017")
	season_name <- c(sr_2016="short rains", lr_2017="long rains")
	season_year <- c(sr_2016="2016", lr_2017="2017")

	k <- 0
	for (i in 1:11) {
		cc <- paste0("cy_", i, "_crpy_crop")
		if (!cc %in% names(r)) next
		for (s in seasons) {
			yc <- paste0("cy_", i, "_crpy_", s, "_yield")
			ac <- paste0("cy_", i, "_crpy_", s, "_area")
			if (!yc %in% names(r) && !ac %in% names(r)) next
			crop <- map_crop(r[[cc]], r[[paste0("cy_", i, "_crpy_crop_other")]])
			has_crop <- !is.na(crop) & crop != ""
			if (!any(has_crop)) next
			k <- k + 1
			yld <- if (yc %in% names(r)) to_num(r[[yc]]) else NA_real_
			area <- if (ac %in% names(r)) to_num(r[[ac]]) else NA_real_
			variety <- r[[paste0("cy_", i, "_crpy_crop_variety")]]
			blocks[[k]] <- data.frame(
				hhid = hh[has_crop],
				country = r$country[has_crop],
				adm1 = carobiner::fix_name(r$region[has_crop], "title"),
				elevation = to_num(r$gps_alt[has_crop]),
				sex = tolower(as.character(r$resp_sex[has_crop])),
				age = to_num(r$resp_age[has_crop]),
				crop = crop[has_crop],
				variety = variety[has_crop],
				field_size = area[has_crop],
				yield = yld[has_crop],
				yield_part = "grain",
				season = unname(season_name[s]),
				planting_date = unname(season_year[s])
			)
		}
	}

	d <- do.call(rbind, blocks)

	d$yield_part[d$crop %in% c("banana", "avocado", "mango", "orange", "apple", "papaya", "tomato", "pumpkin", "pineapple")] <- "fruit"
	d$yield_part[d$crop %in% c("groundnut", "cashew", "soybean", "common bean", "cowpea", "sesame", "coconut", "coffee", "bambara groundnut", "faba bean", "lentil", "sunflower", "mung bean")] <- "seed"
	d$yield_part[d$crop %in% c("potato", "yam")] <- "tubers"
	d$yield_part[d$crop %in% c("cassava", "sweetpotato")] <- "roots"
	d$yield_part[d$crop %in% c("sugarcane")] <- "stems"
	d$yield_part[d$crop %in% c("khat", "spinach")] <- "leaves"

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	d$adm1[which(d$country == "Kenya" & d$adm1 %in% c("Kirinyanga", "Kirinyiga"))] <- "Kirinyaga" # GADM spelling
	d$adm1[which(d$country == "Kenya" & d$adm1 %in% c("Maachakos", "Machachkos", "Machakosi", "Maxchakos"))] <- "Machakos" # GADM spelling
	d$adm1[which(d$adm1 == "Freestate")] <- "Free State" # GADM spelling

	# Coordinates are the GADM point-radius centroid of adm1.
	geo <- data.frame(
		country = c("Kenya", "Kenya", "Malawi", "Malawi", "South Africa", "Tanzania", "Tanzania"),
		adm1 = c("Kirinyaga", "Machakos", "Dedza", "Mzimba", "Free State", "Lindi", "Mbeya"),
		longitude = c(37.3329, 37.4121, 34.3664, 33.6347, 26.8673, 38.4069, 33.7575),
		latitude = c(-0.511, -1.2776, -14.158, -11.7893, -28.6144, -9.4807, -8.16),
		geo_uncertainty = c(84705, 70575, 87223, 104873, 312568, 189486, 172774),
		geo_source = "GADM 4.1, adm1"
	)
	d <- merge(d, geo, by=c("country", "adm1"), all.x=TRUE, sort=FALSE)
	d$geo_from_source <- ifelse(is.na(d$longitude), NA, FALSE)

	carobiner::write_files(path, meta, d)
}
