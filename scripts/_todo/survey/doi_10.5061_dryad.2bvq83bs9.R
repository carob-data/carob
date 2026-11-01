# R script for "carob"
# license: GPL (>=3)

## NOTES
# InnovAfrica baseline, 3819 farmers, 12 pilot sites in six countries, collected 2018 (README).
# Questionnaire A/7 asked GPS lat/lon; the digitized file has gps_alt only (no latitude/longitude columns).
# Crop block is wide: up to 11 crops x two seasons (short rains/winter 2016, long rains 2017). Reshaped to one row per household-crop-season.
# Yield columns are already kg/ha (README). Area column cy_*_crpy_*_area is hectares.
# Missing codes in the sheet are "n/a" and "missing".

## ISSUES
# Human must georeference from region (and pilot sites). Do not geocode in-script. Questionnaire GPS lat/lon were not digitized.
# adm1 = region as recorded; spellings may not match GADM (Mbeya, Lindi, …).
# Crop "Other" mapped from crop_other when present (banana, cashew, avocado, …); remaining "other" need review.
# Livestock, fodder/pasture, milk, credit, nutrition, gender, extension, and household-member roster (hhmem_1..15) not processed.
# Land parcels lp_1..4 not linked to crop rows; farmland not summed into crop records.
# Some yield values look low or high for kg/ha; left as reported.
# write_files: missing latitude/longitude/geo_from_source; remaining invalid crop spellings; elevation 22287 out of bounds (left as reported); NA yield; duplicate household-crop-season possible.

## NEW VARIABLES
# none


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

	f <- ff[grepl("Baseline survey", basename(ff)) & grepl("xlsx$", basename(ff))]
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

	carobiner::write_files(path, meta, d)
}
