# R script for "carob"
# license: GPL (>=3)

## NOTES
# TRIVSA household surveys on adoption of rice varieties. Coding manual: CodingManualTRIVSA.xlsx.
# "HH survey-Data.xlsx", sheet "Varieties": household x parcel x plot x subplot x season x variety for Bangladesh, Nepal (Eastern, Western)
#   and the Indian states West Bengal, Orissa and Chhattisgarh: land type, irrigated/rainfed, variety (names revised by IRRI to match release names),
#   release year, MV/TV, area (ha), establishment method, years growing the variety, seed source. No production or yield.
#   Area is already in ha (sheet "Notes" says West Bengal area is in acre and Orissa in decimal, but the values equal acre/2.47 and dec/250).
#   Household head sex and age from sheet "Basic-Land profile".
#   Crop year per country from sheet "Notes" (crop_year_): Bangladesh 2011-2012, Nepal (Eastern) 2010-2011, Nepal (Western) 2009-2010, West Bengal 2011-2012, Orissa 2010-2011; not given for Chhattisgarh.
# "Bhutan HH survey-Data.xlsx", sheet "Landhold&MVs": household x improved variety (up to 4) with area (acre -> ha) and yield (kg/acre -> kg/ha).
# Records from both files are combined; district -> adm2 (India, Bangladesh) or adm3 (Nepal); Bhutan Dzongkhag -> adm1, Gewog -> adm2.

## ISSUES
# No coordinates. Georeference villages (location) with district and block_ (India: block; Bangladesh: upazila-union; Nepal: ward) or Bhutan gewog.
# planting_date not set; only the crop year per country is known (crop_year_). Bhutan interview dates (mixed Excel serials and "18-2-2011") not processed.
# Bhutan: area grown to traditional varieties (TVArea), landholding, adoption/non-adoption, farming practices (fertilizer as Qty with local units drey/hkawo/pathi), traits, sales not processed.
# HH survey sheets "Adoption", "Disadoption-MV", "Disadoption-TV" (year of first adoption, area in 1st/2nd year, traits, reasons for disadoption) not processed.
# Community survey, Expert estimates, Researchers' survey and Varietal releases files are village-, expert- or variety-level data, not farm records; not processed.
# Seasons "aus" and "spring" are not terminag values. Land types with combinations (e.g. "mediumland, highland") kept as is.
# "Irrigated coastal" (Orissa) mapped to irrigated TRUE.
# Variety names "Hybrids", unknown or local names kept as given; variety_release_note_ holds the IRRI comment on the release status.
# YrsGrown 0 kept (probably first year).
# Remaining write_files messages: missing latitude/longitude; new variables (trailing _) reported as unknown; invalid season aus, spring;
#   duplicate records (same household, field, season, variety and area listed more than once in the source); NA yield (no yield in the South Asia sheet).

## NEW VARIABLES
# crop_year_: crop year the survey refers to
# block_: administrative block / upazila-union / ward below the district
# years_grown_: number of years the household has grown the variety
# variety_release_note_: comment on the release of the variety (e.g. "Released in Andhra Pradesh")
# altitude_class_: altitude category of the village (Bhutan: High/Mid/Low)


carob_script <- function(path) {

"
Tracking Improved Varieties in South Asia (TRIVSA)

To devise practical means for the regular monitoring of variety diffusion and for the generation of widely accessible databases on rice varietal improvement in South Asia.
"

	uri <- "doi:10.7910/DVN/26301"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "TRIVSA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 25,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "HH survey-Data.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Varieties", skip=5, col_names=FALSE, guess_max=100000)
	names(r1) <- c("State", "RespNo", "Zone", "District", "Block", "Village", "Resp", "Parcel", "Plot", "Subplot",
		"Landtype", "Irrig", "Season", "Variety", "RelYear", "RelComment", "MVTV", "Area", "CEMethod", "YrsGrown",
		"SeedSrc", "Comment", "Variety_orig")
	r2 <- carobiner::read.excel(f, sheet="Basic-Land profile", skip=4, guess_max=100000)
	r3 <- carobiner::read.excel(ff[basename(ff) == "Bhutan HH survey-Data.xlsx"], sheet="Landhold&MVs", skip=4, guess_max=100000)

	country <- c("Bangladesh"="Bangladesh", "Nepal (Eastern)"="Nepal", "Nepal (Western)"="Nepal",
		"West Bengal"="India", "Orissa"="India", "Chhattisgarh"="India")
	cropyear <- c("Bangladesh"="2011-2012", "Nepal (Eastern)"="2010-2011", "Nepal (Western)"="2009-2010",
		"West Bengal"="2011-2012", "Orissa"="2010-2011")
	h <- match(paste(r1$State, r1$RespNo), paste(r2$Country, r2$RespNo))
	r1$RelYear[r1$RelYear %in% "no year reported"] <- NA

	d1 <- data.frame(
		country = unname(country[r1$State]),
		adm1 = ifelse(country[r1$State] == "India", r1$State, NA),
		adm2 = r1$District,
		block_ = r1$Block,
		location = r1$Village,
		hhid = paste(r1$State, r1$RespNo, sep="-"),
		field_id = paste(r1$Parcel, r1$Plot, r1$Subplot, sep="-"),
		crop_year_ = unname(cropyear[r1$State]),
		season = tolower(r1$Season),
		landscape_position = tolower(r1$Landtype),
		irrigated = c(Irrigated=TRUE, "Irrigated coastal"=TRUE, Rainfed=FALSE)[r1$Irrig],
		variety = trimws(r1$Variety),
		variety_release_year = as.integer(r1$RelYear),
		variety_release_note_ = r1$RelComment,
		variety_type = c(MV="modern", TV="traditional")[r1$MVTV],
		field_size = r1$Area,
		planting_method = c(TPR="transplanting", DSR="direct seeding", WSR="direct seeding")[r1$CEMethod],
		years_grown_ = as.numeric(r1$YrsGrown),
		seed_source = r1$SeedSrc,
		sex = tolower(r2$Sex[h]),
		age = as.numeric(r2$Age[h])
	)
	d1$adm1[d1$adm1 %in% "Orissa"] <- "Odisha"
	d1$adm3 <- ifelse(d1$country == "Nepal", d1$adm2, NA)
	d1$adm2[d1$country == "Nepal"] <- NA

	## Bhutan: up to 4 improved varieties per household (area in acre, yield in kg/acre)
	r3 <- r3[!is.na(r3$RespNo), ]
	b <- lapply(1:4, \(i) data.frame(
		hhid = paste("Bhutan", r3$RespNo, sep="-"),
		adm1 = r3$DZ,
		adm2 = r3$Geog,
		location = r3$Village,
		altitude_class_ = r3$Altitude,
		sex = tolower(r3$Sex),
		age = as.numeric(r3$Age),
		variety = r3[[paste0("Variety", i)]],
		field_size = as.numeric(r3[[paste0("Area", i)]]) * 0.4046863,
		yield = as.numeric(r3[[paste0("Yield", i)]]) / 0.4046863
	))
	d2 <- do.call(rbind, b)
	d2 <- d2[!is.na(d2$variety), ]
	d2$country <- "Bhutan"
	d2$variety_type <- "modern"

	d <- carobiner::bindr(d1, d2)
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
