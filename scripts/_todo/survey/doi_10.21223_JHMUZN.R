# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey of salt-prone farms in Khulna and Satkhira, Bangladesh, interviewed in 2018. Country Bangladesh is the title. District, upazila, union, and village are columns. District is stored as adm2 because the division is not in the file.
# Section D1 is one row per crop. Area is recorded in decimals. The dictionary unit cell says Number and does not give the hectare factor, so area is not converted and yield is not computed. Harvest and seed amounts that the column name states in kg are kept as kilograms, not as kg/ha.
# Crop names are mapped only where the English name is clear (potato, sweetpotato, rice for Boro and T. Aman, and the vegetable names in the crop list). Other names stay in local_crop_. Child nutrition questions are not copied.

## ISSUES
# No latitude or longitude. GPS altitude from section A is elevation when that column is filled.
# A decimal of land is not converted to hectares.
# write_files: missing carob_contributor, latitude, longitude. unknown variable: local_crop_ (crop?). unknown variables: area_decimal_, harvest_kg_, seed_kg_.

## NEW VARIABLES
# local_crop_: crop name as written when it is not mapped to the crop list.
# area_decimal_: D2.4 area in decimals.
# harvest_kg_: D2.10 quantity harvested, kilograms.
# seed_kg_: D2.8a amount of seed, kilograms. This is not a seed rate.

## NOT PROCESSED
# Sections other than A (location and altitude) and D1 (crop plot). Chemicals, irrigation, labor, assets, food security, and child feeding are not crop measurements with a stated rate.


carob_script <- function(path) {

"
Dataset for: Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh in 2018

RTCs play an important role in diverse Asian agri-food systems and contribute to their resilience and capacity to adapt to different challenging environments.The study is a quantitative assessment to better understand the current farming practice including potato and sweetpotato farming in salt prone areas to finally increase the potato and sweetpotato yield in those areas. The information collected was divided in sixteen sections: A) Basic information and consent, (B) Household Roster, (C) Roster of land and pond/water bodies owned or under operation, (D1) Agriculture Plot Utilization, (D2) Irrigation method (in the last 12 months), (D3) Usage of Agricultural Chemicals, Fertilizers and Pesticides (in the last 12 months), (D4) Rental Cost of Tools, Machinery and Draft Animal (in the last 12 months), (D5) Labor Usage by Gender for Crop Plantation and Harvesting, (D6) Post Harvest Labor, Animal and Tools/ Machinery Usage, (E) Household Assets, (F) Household Access to Resources, (G) Household Access to Extension and Credit, (H) Livestock and Aquaculture, (I) Household annual off-farm Income, (J) Household annual Expenditures, (K) Social networks, (L) Migration, (M) Household Food Security, (N) Constraints to POTATO AND SWEETPOTATO production and Marketing, (O) Risk Perception, (P) Nutrition and Hygiene.
"

	uri <- "doi:10.21223/JHMUZN"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.4
	)

	f <- ff[grepl("^Database_Salt", basename(ff))]
	r <- carobiner::read.excel(f, sheet="Section D1")
	a <- carobiner::read.excel(f, sheet="Section A-F-G-K-L-N-O-P")
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z) | z == "NA"] <- NA_character_
		z
	}
	num <- function(x) {
		z <- txt(x)
		ok <- is.na(z) | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}
	when <- function(x) {
		z <- txt(x)
		ok <- grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$", z)
		out <- rep(NA_character_, length(z))
		if (any(ok)) {
			p <- strsplit(z[ok], "-", fixed=TRUE)
			out[ok] <- vapply(p, function(v) sprintf("%s-%02d-%02d", v[3], as.integer(v[2]), as.integer(v[1])), "")
		}
		out
	}
	crop_of <- function(x) {
		key <- tolower(trimws(sub("\\(.*", "", txt(x))))
		key <- gsub("[[:space:]]+", " ", key)
		c(potato="potato", sweetpotato="sweetpotato", "sweet potato"="sweetpotato",
			tomato="tomato", lentil="lentil", cabbage="cabbage", cauliflower="cauliflower",
			cucumber="cucumber", eggplant="eggplant", bringal="eggplant", brinjal="eggplant",
			ginger="ginger", jute="jute", okra="okra", pumpkin="pumpkin",
			boro="rice", "t.aman"="rice", "t. aman"="rice", aman="rice",
			watermelon="watermelon", "water melon"="watermelon",
			sugarcane="sugarcane", sugurcan="sugarcane", turmeric="turmeric")[key]
	}
	col <- function(nm) names(r)[grepl(nm, names(r), fixed=TRUE)][1]
	alt <- data.frame(hhid=txt(a[["Household ID"]]), elevation=num(a[["GPS Altitude"]]), stringsAsFactors=FALSE)
	alt <- alt[!duplicated(alt$hhid), ]

	d <- data.frame(
		hhid = txt(r[["Household ID"]]),
		date = when(r[["Interview date (DD-MM-YYYY)"]]),
		adm2 = txt(r[["District / province"]]),
		adm3 = txt(r[["Upazilla / Subdistrict"]]),
		location = txt(r[["Village"]]),
		local_crop_ = txt(r[[col("D2.3 Crop name cultivated during")]]),
		variety = txt(r[[col("D2.5 Main variety cultivated")]]),
		area_decimal_ = num(r[[col("D2.4 Area")]]),
		harvest_kg_ = num(r[[col("D2.10 Quantity harvested")]]),
		seed_kg_ = num(r[[col("D2.8a Amount of seed")]]),
		stringsAsFactors = FALSE
	)
	d$crop <- as.character(crop_of(d$local_crop_))
	d$local_crop_[!is.na(d$crop)] <- NA_character_
	d <- merge(d, alt, by="hhid", all.x=TRUE)
	d$country <- "Bangladesh"
	d$geo_from_source <- FALSE
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- "Khulna_Satkhira_2018"
	d <- d[!is.na(d$hhid), ]
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
